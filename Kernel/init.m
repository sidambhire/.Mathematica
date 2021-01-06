(** User Mathematica initialization file **)
Notation`AutoLoadNotationPalette = False
Needs["Notation`"]

Notation[ParsedBoxWrapper[
SubscriptBox["x_", "i_"]] \[DoubleLongLeftRightArrow] ParsedBoxWrapper[
RowBox[{"Indexed", "[", 
RowBox[{"x_", ",", "i_"}], "]"}]]]

Notation[ParsedBoxWrapper[
SubscriptBox["x_", 
RowBox[{"i_", ",", "j_"}]]] \[DoubleLongLeftRightArrow] 
  ParsedBoxWrapper[
RowBox[{"Indexed", "[", 
RowBox[{"x_", ",", 
RowBox[{"{", 
RowBox[{"i_", ",", "j_"}], "}"}]}], "]"}]]]

Notation[ParsedBoxWrapper[
SubscriptBox["x_", 
RowBox[{"i_", ",", "j_", ",", "k_"}]]] \[DoubleLongLeftRightArrow] 
  ParsedBoxWrapper[
RowBox[{"Indexed", "[", 
RowBox[{"x_", ",", 
RowBox[{"{", 
RowBox[{"i_", ",", "j_", ",", "k_"}], "}"}]}], "]"}]]]
