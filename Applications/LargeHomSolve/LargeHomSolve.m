(* ::Package:: *)

BeginPackage["LargeHomSolve`"];


(* ------------- PUBLIC (exported) symbols: add BOTH usage messages ------------- *)

SolveLargeHomogeneousLinearSystem::usage =
  "SolveLargeHomogeneousLinearSystem[eqs, vars, opts] returns a basis or rules for the solution x such that A.x == 0. \
Options:\n" <>
  "\"Parameters\" -> {}, \"Assumptions\" -> True, \"ParameterSampling\" -> 5, \"SampleSubstitution\" -> Automatic,\n" <>
  "\"DeduplicateRows\" -> True, \"DropZeroRows\" -> True, \"BlockDecomposition\" -> False,\n" <>
  "\"Parallel\" -> True, \"MaxKernels\" -> Automatic, \"Debug\" -> False,\n" <>
  "\"Return\" -> \"Basis\"|\"Association\"|\"Rules\", \"FreeParameterSymbol\" -> \[FormalT].";
SafeCoefficientRules::usage =
  "SafeCoefficientRules[expr, vars, opts] returns rules {expVector->coeff,...} for additive terms in expr w.r.t. vars. \
Options: \"Assumptions\" (default True), \"TimeConstraint\" (default 2), \
\"OnNonPolynomial\"->\"Drop\"|\"Keep\"|\"Error\" (default \"Drop\").";

SafeCoefficientRulesFast::usage =
  "SafeCoefficientRules[expr, vars] returns rules {expVector->coeff,...} for additive terms in expr w.r.t. vars.";


Begin["`Private`"];

(* ------------- PRIVATE: put your implementations here ------------- *)



SetAttributes[debugPrint, HoldAll];
debugPrint[flag_, args__] := If[TrueQ[flag], Print[Style[Row[{DateString[{"Time"}], " | ", args}], {Small, GrayLevel[0.25]}]]];

normalizeEquations[eqs_List] := DeleteCases[Flatten@List@Replace[eqs, {
    Equal[l_, r_] :> (l - r),
    True -> 0
  }, {0, 1}], 0];

Options[buildSparseMatrix] = {"Debug" -> False};
buildSparseMatrix[eqsIn_List, vars_List, OptionsPattern[]] := Module[
  {dbg = OptionValue["Debug"], eqs = normalizeEquations[eqsIn], carr, b, A, dims, nz},
  debugPrint[dbg, "Building sparse matrix..."];
  carr = Quiet@Check[CoefficientArrays[eqs, vars], $Failed];
  If[carr === $Failed,
    debugPrint[dbg, "CoefficientArrays failed. Falling back to direct coefficient extraction."];
    A = SparseArray[Table[Coefficient[eqs[[i]], vars[[j]]], {i, Length@eqs}, {j, Length@vars}]];
    b = ConstantArray[0, Length@eqs];,
    b = Normal@First@carr;
    A = SparseArray@Last@carr;
  ];
  dims = Dimensions[A];
  nz = With[{p = A["NonzeroPositions"]}, If[ListQ[p], Length@p, 0]];
  debugPrint[dbg, "Matrix dimensions: ", dims, ", Nonzeros: ", nz];
  <|"Matrix" -> A, "Constant" -> b|>
]

Options[dropZeroRowsSparse] = {"Debug" -> False};
dropZeroRowsSparse[A_SparseArray, OptionsPattern[]] := Module[
  {dbg = OptionValue["Debug"], nnzByRow, keep, A2},
  nnzByRow = Total[Unitize@Normal@A, {2}];
  keep = Flatten@Position[nnzByRow, _?(# > 0 & )];
  A2 = If[Length@keep < Length@nnzByRow && Length@keep > 0, A[[keep, All]], A];
  debugPrint[dbg, "Dropped zero rows: ", Max[0, Length@nnzByRow - Length@keep], " removed; ", If[Length@keep>0, Length@keep, Length@nnzByRow], " kept."];
  {A2, keep}
]

Options[deduplicateRowsSparse] = {"Debug" -> False, "Substitution" -> Automatic};
deduplicateRowsSparse[A_SparseArray, OptionsPattern[]] := Module[
  {dbg = OptionValue["Debug"], sub = OptionValue["Substitution"], Aeval, keys, groups, idx},
  debugPrint[dbg, "Deduplicating rows by numeric probe..."];
  Aeval = If[sub === Automatic || sub === {},
    N@Normal@A,
    N@Normal@(A /. sub)
  ];
  keys = Hash /@ Aeval;
  groups = GroupBy[Range[Length@keys], keys[[#]] &];
  idx = First /@ Values[groups];
  debugPrint[dbg, "Dedup kept ", Length@idx, " of ", Length@keys, " rows."];
  {A[[idx, All]], idx}
]

defaultSampleGenerator[params_List, k_Integer?Positive] := Module[{vals},
  If[params === {}, Table[<||>, {k}], (vals = RandomChoice[DeleteCases[Range[-97,97],0], {k, Length@params}];
    Table[Thread[params -> vals[[i]]], {i,k}])]
]

Options[intersectNullSpacesIter] = {"Debug" -> False};
intersectNullSpacesIter[A_SparseArray, samples_List, OptionsPattern[]] := Module[
  {dbg = OptionValue["Debug"], n, Bmat, As, k = 1},
  n = Last@Dimensions@A;
  If[Length@samples == 0, Return[SparseArray[{}, {n, 0}]]];
  As = Normal@(A /. First@samples);
  Bmat = Transpose@NullSpace[As]; (* n x d1 *)
  debugPrint[dbg, "Nullity(sample 1): ", If[MatrixQ[Bmat], Last@Dimensions@Bmat, 0]];
  Do[
    If[!MatrixQ[Bmat] || Last@Dimensions@Bmat == 0, Break[]];
    k++; As = Normal@(A /. s);
    Bmat = With[{AsB = As . Bmat}, Bmat . Transpose@NullSpace[AsB]];
    debugPrint[dbg, "Nullity(sample ", k, "): ", If[MatrixQ[Bmat], Last@Dimensions@Bmat, 0]];
  , {s, Rest@samples}];
  If[!MatrixQ[Bmat], SparseArray[{}, {n, 0}], Bmat]
]

Options[variableBlocksFromMatrix] = {"Debug" -> False};
variableBlocksFromMatrix[A_SparseArray, OptionsPattern[]] := Module[
  {dbg = OptionValue["Debug"], pos, n, byRow, edges, g, comps},
  n = Last@Dimensions@A;
  pos = A["NonzeroPositions"];
  byRow = GatherBy[pos, First];
  edges = Flatten[Subsets[Last /@ #, {2}] & /@ byRow, 1];
  g = Graph[Range[n], UndirectedEdge @@@ (Sort /@ edges)];
  comps = ConnectedComponents[g];
  debugPrint[dbg, "Detected ", Length@comps, " variable blocks."];
  comps /; True
]

Options[solveNullspacePerBlock] = {"Debug" -> False, "Samples" -> {}, 
   "Parallel" -> True, "MaxKernels" -> Automatic};
   
solveNullspacePerBlock[A_SparseArray, blocks_List, OptionsPattern[]] :=
   Module[{dbg = OptionValue["Debug"], samples = OptionValue["Samples"],
     par = TrueQ@OptionValue["Parallel"], 
    maxk = OptionValue["MaxKernels"], n = Last@Dimensions@A, pos, 
    blockMats = {}, rowsForBlk, Ablk, Bblk, lift}, 
   pos = A["NonzeroPositions"];
   If[par && $KernelCount == 0, Quiet@LaunchKernels[]];
   Do[rowsForBlk = 
     Union@(First /@ Select[pos, MemberQ[blocks[[bi]], Last@#] &]);
    (*Unconstrained block=>full nullspace=identity on that block*)
    If[rowsForBlk === {} || Length@rowsForBlk == 0, 
     Bblk = IdentityMatrix[Length@blocks[[bi]]], 
     Ablk = A[[rowsForBlk, blocks[[bi]]]];
     Bblk = 
      If[samples === {}, Transpose@NullSpace@Normal@Ablk, 
       Transpose@
        LargeHomSolve`Private`intersectNullSpacesIter[
         A[[All, blocks[[bi]]]], samples, "Debug" -> dbg, 
         "Parallel" -> False]];
     If[! MatrixQ[Bblk], 
      Bblk = ConstantArray[{}, {Length@blocks[[bi]], 0}]];];
    (*Lift block basis (size:n\[Times]dim_blk) and collect*)
    lift = SparseArray[
      Thread[Transpose[{Range[Length@blocks[[bi]]], blocks[[bi]]}] -> 
        1], {Length@blocks[[bi]], n}];
    AppendTo[blockMats, Transpose[lift] . Bblk], {bi, Length@blocks}];
   If[blockMats === {}, SparseArray[{}, {n, 0}], 
    Fold[If[#1 === {}, #2, Join[#1, #2, 2]] &, {}, 
     blockMats]  (*column-wise concat*)]];
     
Options[SolveLargeHomogeneousLinearSystem] = {
  "Parameters" -> {},
  "Assumptions" -> True,
  "ParameterSampling" -> 5,
  "SampleSubstitution" -> Automatic,
  "DeduplicateRows" -> True,
  "DropZeroRows" -> True,
  "BlockDecomposition" -> False,
  "Parallel" -> True,
  "MaxKernels" -> Automatic,
  "Debug" -> False,
  "Return" -> "Basis",
  "FreeParameterSymbol" -> \[FormalT]
};

SolveLargeHomogeneousLinearSystem[eqsIn_List, vars_List, opts : OptionsPattern[]] := Module[
  {
    dbg = OptionValue["Debug"],
    params = OptionValue["Parameters"],
    nsamp = OptionValue["ParameterSampling"],
    sampleFn = OptionValue["SampleSubstitution"],
    dedup = TrueQ@OptionValue["DeduplicateRows"],
    drop0 = TrueQ@OptionValue["DropZeroRows"],
    blockDecomp = TrueQ@OptionValue["BlockDecomposition"],
    par = TrueQ@OptionValue["Parallel"],
    maxk = OptionValue["MaxKernels"],
    ret = OptionValue["Return"],
    freeSym = OptionValue["FreeParameterSymbol"],
    Arec, A, rowsKept, rowsDedupIdx, blocks, samples, subsForDedup,
    basisMat, dim, nz, rhsVec
  },
  debugPrint[dbg, "======== SolveLargeHomogeneousLinearSystem START ========"];
  debugPrint[dbg, "Equations: ", Length@Flatten@eqsIn, " | Variables: ", Length@vars];
  Arec = buildSparseMatrix[eqsIn, vars, "Debug" -> dbg];
  A = Arec["Matrix"];
  If[drop0, {A, rowsKept} = dropZeroRowsSparse[A, "Debug" -> dbg]];
  If[dedup, subsForDedup = Automatic; {A, rowsDedupIdx} = deduplicateRowsSparse[A, "Debug" -> dbg, "Substitution" -> subsForDedup]];
  samples = Which[
    nsamp <= 0, {},
    sampleFn === Automatic, defaultSampleGenerator[params, nsamp],
    Head[sampleFn] === Function, Table[sampleFn[params], {nsamp}],
    MatchQ[sampleFn, {_Rule ..}] || MatchQ[sampleFn, {(_Rule) ..}], ConstantArray[sampleFn, nsamp],
    True, defaultSampleGenerator[params, nsamp]
  ];
  debugPrint[dbg, "Sampling ", Length@samples, " parameter sets."];
  basisMat =
    If[TrueQ@blockDecomp,
      blocks = variableBlocksFromMatrix[A, "Debug" -> dbg];
      debugPrint[dbg, "Solving per-block with ", Length@blocks, " blocks..."];
      solveNullspacePerBlock[A, blocks, "Samples" -> samples, "Debug" -> dbg, "Parallel" -> par, "MaxKernels" -> maxk],
      If[samples === {}, Transpose@NullSpace@Normal@A, intersectNullSpacesIter[A, samples, "Debug" -> dbg]]
    ];
  dim = If[MatrixQ[basisMat], Last@Dimensions@basisMat, 0];
  debugPrint[dbg, Style[Row[{"Final nullity: ", dim}], Darker@Green]];
  debugPrint[dbg, "======== SolveLargeHomogeneousLinearSystem END =========="];
  Which[
    ret === "Association",
      <|"Vars" -> vars, "MatrixDimensions" -> Dimensions@A, "Nonzeros" -> With[{p=A["NonzeroPositions"]}, If[ListQ[p], Length@p, 0]], "Nullity" -> dim, "BasisMatrix" -> basisMat|>,
    ret === "Rules",
      (rhsVec = If[dim==0, ConstantArray[0, Length@vars], basisMat . Table[Indexed[freeSym,{i}], {i, dim}]];
       Thread[vars -> rhsVec]),
    True,
      If[MatrixQ[basisMat], Transpose@basisMat, {}]  (* list of basis vectors *)
  ]
]


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

SafeCoefficientRulesFast[expr_, vars_List] := Module[{terms, rules},(*Step 1:Expand expression into additive terms*) terms = List @@ Expand[expr]; (*Step 2:For each term, compute exponent vector and symbolic coefficient*) rules = Table[ Module[{term = t, exponents, monomial, coeff},(*Get integer exponents for each variable*) exponents = Table[Exponent[term, v], {v, vars}]; (*Reconstruct monomial*) monomial = Times @@ MapThread[Power, {vars, exponents}]; (*Divide to get symbolic coefficient*) coeff = Simplify[term/monomial]; exponents -> coeff], {t, terms}]; Merge[rules, Total] // Normal]

Protect[SafeCoefficientRulesFast];

End[];  (* `Private` *)
EndPackage[];

