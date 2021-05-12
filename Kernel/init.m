(** User Mathematica initialization file **)
Needs["Notation`"]

Notation[ParsedBoxWrapper[SubscriptBox["t_", "i_"]] \[DoubleLongLeftRightArrow] 
   ParsedBoxWrapper[RowBox[{"Indexed", "[", 
      RowBox[{"t_", ",", RowBox[{"List", "[", "i_", "]"}]}], 
      "]"}]]]

(** Add Notations, if any, before this line **)
Notation`AutoLoadNotationPalette = True
Alert[]:=EmitSound[Sound[SoundNote[]]]
