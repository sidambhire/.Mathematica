(** User Mathematica initialization file **)
Notation`AutoLoadNotationPalette = False
Needs["Notation`"]

Notation[ParsedBoxWrapper[SubscriptBox["t_", "i_"]] \[DoubleLongLeftRightArrow] 
   ParsedBoxWrapper[RowBox[{"Indexed", "[", 
      RowBox[{"t_", ",", RowBox[{"List", "[", "i_", "]"}]}], 
      "]"}]]]

Alert[]:=EmitSound[Sound[SoundNote[]]]
