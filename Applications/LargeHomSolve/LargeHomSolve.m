(* ::Package:: *)

BeginPackage["LargeHomSolve`"];


(* ------------- PUBLIC (exported) symbols: add BOTH usage messages ------------- *)

SolveLargeHomogeneousLinearSystem::usage =
  "SolveLargeHomogeneousLinearSystem[eqns, vars, opts] solves large sparse homogeneous linear systems with symbolic parameters.";

SafeCoefficientRules::usage =
  "SafeCoefficientRules[expr, vars, opts] returns rules {expVector->coeff,...} for additive terms in expr w.r.t. vars. \
Options: \"Assumptions\" (default True), \"TimeConstraint\" (default 2), \
\"OnNonPolynomial\"->\"Drop\"|\"Keep\"|\"Error\" (default \"Drop\").";


Options[SolveLargeHomogeneousLinearSystem] = {
  "Parameters"      -> {},
  "Assumptions"     -> True,
  "GenericSolve"    -> True,
  "TimeConstraint"  -> 2,          (* seconds or Automatic *)
  "DeduplicateRows" -> False,
  "ParameterHead"   -> Automatic   (* default parameter head symbol (e.g., t); resolved at runtime *)
};

Begin["`Private`"];

(* ------------- PRIVATE: put your implementations here ------------- *)


(* (Re)define safely on reload without wiping Options *)
Unprotect[SolveLargeHomogeneousLinearSystem];
DownValues[SolveLargeHomogeneousLinearSystem] =.;
SubValues[SolveLargeHomogeneousLinearSystem] =.;
UpValues[SolveLargeHomogeneousLinearSystem]  =.;

SolveLargeHomogeneousLinearSystem[eqns_List, vars_List, opts:OptionsPattern[]] := Module[
  {
    (* options *)
    params   = OptionValue["Parameters"],
    asm0     = OptionValue["Assumptions"],
    genericQ = TrueQ @ OptionValue["GenericSolve"],
    tcon     = OptionValue["TimeConstraint"],
    dedupQ   = TrueQ @ OptionValue["DeduplicateRows"],
    phead    = OptionValue["ParameterHead"],

    (* locals *)
    headSym, asm, toZero, buildA, nullBasisRules, built, A2, Nul, rules, denoms, badSet
  },

  (* Resolve parameter head ONCE to a plain symbol *)
  headSym = Replace[phead, {
    Automatic  :> Symbol["t"],           (* default uses Global`t to avoid context shadowing *)
    s_Symbol   :> s,
    str_String :> With[{tmp = ToExpression[str]}, If[SymbolQ[tmp], tmp, Symbol["t"]]],
    _          :> Symbol["t"]
  }];
  If[!SymbolQ[headSym], headSym = Symbol["t"]];

  (* Treat parameters as nonzero for generic pivoting, if requested *)
  asm = If[genericQ && params =!= {}, asm0 && And @@ Thread[params != 0], asm0];

  (* Helper: normalize each equation to LHS-RHS and simplify *)
  toZero = Function[{e},
    Simplify[
      Expand @ If[MatchQ[e, _Equal], First[e] - Last[e], e],
      asm, TimeConstraint -> tcon
    ]
  ];

  (* Helper: build full (#rows \[Times] #vars) coefficient matrix; never drop columns *)
  buildA = Function[{eqs},
    Module[{eq0, paramOnly, paramConstraints, varEqns, rowCoeffs, mat, dropped = 0, nvars, A},
      eq0 = toZero /@ eqs;

      paramOnly        = Select[eq0, FreeQ[#, Alternatives @@ vars] &];
      paramConstraints = Complement[paramOnly, Select[paramOnly, PossibleZeroQ]];
      varEqns          = Complement[eq0, paramOnly];

      If[varEqns === {},
        Return @ <|"A"->SparseArray[{}, {0, Length[vars]}], "Dropped"->0,
                  "ParamConstraints"->(Equal[#,0]&/@paramConstraints),
                  "Rows"->0, "Cols"->Length[vars]|>
      ];

      rowCoeffs[e_] := Simplify[Coefficient[e, #] & /@ vars, asm, TimeConstraint -> tcon];
      mat = rowCoeffs /@ varEqns;                 (* (#eqns) \[Times] (#vars) *)

      If[dedupQ, With[{m0 = Length[mat]}, mat = DeleteDuplicates[mat]; dropped = m0 - Length[mat]]];

      nvars = Length[vars];
      If[mat === {}, A = SparseArray[{}, {0, nvars}], If[VectorQ[mat], mat = {mat}]; A = SparseArray[mat]];

      <|"A"->A, "Dropped"->dropped,
        "ParamConstraints"->(Equal[#,0]&/@paramConstraints),
        "Rows"->If[mat==={},0,Length[mat]], "Cols"->nvars|>
    ]
  ];

  (* Helper: turn nullspace basis into rules vars -> (B . t) *)
  nullBasisRules = Function[{basisIn},
    Module[{n = Length[vars], mat, dims, B, k, tsyms, sol},
      mat = basisIn /. s_SparseArray :> Normal[s];

      Which[
        VectorQ[mat],
          If[Length[mat] =!= n, Return @ Thread[vars -> ConstantArray[0, n]]];
          B = Transpose @ {mat},                             (* n\[Times]1 *)
        ListQ[mat] && AllTrue[mat, VectorQ[#] &],
          If[Length[First@mat] =!= n, Return @ Thread[vars -> ConstantArray[0, n]]];
          B = Transpose @ mat,                               (* rows=basis -> n\[Times]k *)
        MatrixQ[mat],
          dims = Dimensions[mat];
          B = Which[
                dims[[1]] == n, mat,                         (* n\[Times]k *)
                dims[[2]] == n, Transpose @ mat,             (* k\[Times]n -> n\[Times]k *)
                True, Return @ Thread[vars -> ConstantArray[0, n]]
              ],
        True,
          Return @ Thread[vars -> ConstantArray[0, n]]
      ];

      k = Last @ Dimensions[B];
      If[k == 0, Return @ Thread[vars -> ConstantArray[0, n]]];

      (* Stable parameters as Indexed[headSym,{i}] (renders as Subscript[headSym,i]) *)
      tsyms = Array[Indexed[headSym, {#}] &, k];

      sol = Simplify[B . tsyms, asm];                        (* length n *)
      Thread[vars -> sol]
    ]
  ];

  (* Build, solve, assemble *)
  built = buildA[eqns];
  A2    = built["A"];

  Nul = NullSpace[
          A2,
          Method   -> "OneStepRowReduction",
          ZeroTest -> (PossibleZeroQ @ Simplify[#, asm] &)
        ];

  rules  = nullBasisRules[Nul];
  denoms = If[Nul === {}, {}, DeleteCases[Union @ Flatten @ Denominator[Normal /@ Nul], 1] // Factor];
  badSet = (Equal[#, 0] & /@ denoms);

  <|
    "Status" -> If[Nul === {}, "OnlyTrivialSolution", "ParametricSolution"],
    "Nullity" -> Length[Nul],
    "SolutionRules" -> rules,
    "NullSpaceBasis" -> (Normal /@ Nul),
    "ParameterConstraints" -> built["ParamConstraints"],
    "DegeneracyConditions" -> badSet,
    "Diagnostics" -> <|
      "RowCount" -> built["Rows"], "ColCount" -> built["Cols"],
      "DroppedDuplicateRows" -> built["Dropped"], "UsedDedup" -> dedupQ
    |>
  |>
];

Protect[SolveLargeHomogeneousLinearSystem];



(* ==== SafeCoefficientRules (robust, with negative-power toggle) ==== *)

Options[SafeCoefficientRules] = {
  "Assumptions"        -> True,
  "TimeConstraint"     -> 2,
  "OnNonPolynomial"    -> "Drop",      (* "Drop" | "Keep" | "Error" *)
  "AllowNegativePowers"-> True         (* treat x^-k as a valid monomial when True *)
};

SafeCoefficientRules::nonpoly =
  "Found non-polynomial term `1` in variables `2`.";
SafeCoefficientRules::badopt  =
  "Option \"OnNonPolynomial\" -> `1` is invalid. Use \"Drop\", \"Keep\", or \"Error\".";

SafeCoefficientRules[expr_, vars_List, opts:OptionsPattern[]] := Module[
  {asm, tcon, policy, allowNeg, terms, collected = {}, isZeroQ, add, intExp, toKeyList},

  asm     = OptionValue["Assumptions"];
  tcon    = OptionValue["TimeConstraint"];
  policy  = OptionValue["OnNonPolynomial"];
  allowNeg= TrueQ @ OptionValue["AllowNegativePowers"];

  If[!MemberQ[{"Drop","Keep","Error"}, policy],
    Message[SafeCoefficientRules::badopt, policy]; Return[$Failed];
  ];

  (* zero test *)
  isZeroQ[z_] := TrueQ @ PossibleZeroQ @ Simplify[z, asm, TimeConstraint -> tcon];

  (* append one rule *)
  add[key_, val_] := AppendTo[collected, toKeyList[key] -> Simplify[val, asm, TimeConstraint -> tcon]];

  (* coerce any weird \[OpenCurlyDoubleQuote]exponent\[CloseCurlyDoubleQuote] wrapper to a plain integer; else 0 *)
  intExp[e_] := Module[{u = e, i = 0},
    Do[
      If[IntegerQ[u], Return[u]];
      If[MatchQ[u, _[__]],
        u = Quiet @ Check[u[[2]], Return[0]],
        Return[0]
      ],
      {20}
    ];
    If[IntegerQ[u], u, 0]
  ];

  (* ensure keys are always lists (even for 1 variable) *)
  toKeyList[k_] := If[ListQ[k], k, {k}];

  (* Fast path for true polynomials *)
  If[ TrueQ @ PolynomialQ[expr, vars],
    Return @ (Normal @ Association @
      (MapAt[ Simplify[#, asm, TimeConstraint -> tcon] &, #, {2}] & /@
        CoefficientRules[Expand[expr], vars]))
  ];

  terms = List @@ Expand[expr];

  Do[
    Module[{expvRaw, expv, mono, coeff, nonPolyQ},
      (* integer exponents elementwise; respects negatives; bizarre wrappers sanitized *)
      expvRaw = Exponent[term, #] & /@ vars;
      expv    = intExp /@ expvRaw;

      mono  = Times @@ MapThread[Power, {vars, expv}];
      coeff = Simplify[term/mono, asm, TimeConstraint -> tcon];

      (* Non-polynomial iff residual still contains variables, OR negatives forbidden *)
      nonPolyQ = (Not @ allowNeg && AnyTrue[expv, # < 0 &]) ||
                 Not @ FreeQ[coeff, Alternatives @@ vars];

      If[nonPolyQ,
        Which[
          policy === "Drop",  Null,
          policy === "Error", Message[SafeCoefficientRules::nonpoly, term, vars],
          True,               add[expv, coeff]                 (* "Keep" *)
        ],
        add[expv, coeff]                                       (* monomial (Laurent ok if allowNeg) *)
      ]
    ],
    {term, terms}
  ];

  If[collected === {}, Return[{}]];

  Normal @ Association @
    Select[
      Merge[collected, Simplify[Total[#], asm, TimeConstraint -> tcon] &],
      Not @* isZeroQ
    ]
];




Protect[SafeCoefficientRules];

End[];  (* `Private` *)
EndPackage[];

