(* ::Package:: *)

(* Quantum`Occupation` *)

Quantum`Notation`AutoLoadPalette=False;

BeginPackage["Quantum`Occupation`",{"Quantum`Notation`"}] 

Unprotect[
\[Dagger],
\[ScriptCapitalB],\[ScriptCapitalF],\[ScriptA],\[ScriptCapitalN],DownArrow,UpArrow,
zz050Occupation, zz060annihilation, zz040creation, zz050number,
SetOccupationAliases
];

ClearAll[
\[Dagger],
\[ScriptCapitalB],\[ScriptCapitalF],\[ScriptA],\[ScriptCapitalN],DownArrow,UpArrow,
zz050Occupation, zz060annihilation, zz040creation, zz050number,
SetOccupationAliases
];

\[Dagger]::usage=
	"\[Dagger] is used for formating hermitian conjugates";

\[ScriptCapitalB]::usage=
	"\[ScriptCapitalB] is used to label bosonic fields";
	
\[ScriptCapitalF]::usage=
	"\[ScriptCapitalF] is used to label fermionic fields";

\[ScriptA]::usage=
	"\[ScriptA] is used to label ladder (creation and annihilation) operators";

\[ScriptCapitalN]::usage=
	"\[ScriptCapitalN] is used to label number operators";

\[Null]::usage=
	"\[Null] is internally used to label spin-up and spin-down fermionic fields";

DownArrow::usage=
	"DownArrow is used to label spin-down fermionic fields";

UpArrow::usage=
	"UpArrow is used to label spin-up fermionic fields";

zz060annihilation::usage=
	"zz060annihilation is internally used to represent annihilation (lowering) operators"; 

zz040creation::usage=
	"zz040creation is internally used to represent creation (raising) operators";

zz050number::usage=
	"zz050number is internally used to represent number operators";

zz050Occupation::usage=
	"zz050Occupation is internally used to represent occupation numbers";

zz050Occupation::NonInteger=
	"Occupation number `2` for state `1` must be an integer";
	
SetOccupationAliases::usage=
	"SetOccupationAliases[] sets keyboard aliases in the"<>
	" selected notebook. SetOccupationAliases[notebook]"<>
	" sets keyboard aliases in the specified notebook."<>
	" The aliases are keyboard key-combinations"<>
	" for the input of quantum objects in Dirac Notation.";

SetOccupationAliases::aliases=
    "ALIASES:\n"<>
	"[ESC]on[ESC]        Quantum concatenation infix symbol\n"<>
	"[ESC]tp[ESC]        Tensor-product infix symbol\n"<>	
	"[ESC]her[ESC]       hermitian conjugate template\n"<>
	"[ESC]su[ESC]        subscript (occupation number) template\n"<>
	"[ESC]po[ESC]        power template\n"<>
    "[ESC]ket[ESC]       Ket template\n"<>
	"[ESC]bra[ESC]       Bra template\n"<>    
    "[ESC]bket[ESC]      Bosonic-field Ket\n"<>
    "[ESC]bbra[ESC]      Bosonic-field Bra\n"<>    
    "[ESC]fket[ESC]      Fermionic-field Ket\n"<>
    "[ESC]fbra[ESC]      Fermionic-field Bra\n"<>
    "[ESC]up[ESC]        Spin-up label\n"<>
    "[ESC]dn[ESC]        Spin-down label\n"<>    
    "[ESC]b-[ESC]        Bosonic-field annihilation operator\n"<>
    "[ESC]b--[ESC]       Power of bosonic-field annihilation operator\n"<>
    "[ESC]b+[ESC]        Bosonic-field creation operator\n"<>
    "[ESC]b++[ESC]       Power of bosonic-field creation operator\n"<>
    "[ESC]bn[ESC]        Bosonic-field number operator\n"<>
    "[ESC]bnn[ESC]       Power of bosonic-field number operator\n"<>
    "[ESC]f-[ESC]        Fermionic-field annihilation operator\n"<>
    "[ESC]f--[ESC]       Power of fermionic-field annihilation operator\n"<>
    "[ESC]f+[ESC]        Fermionic-field creation operator\n"<>
    "[ESC]f++[ESC]       Power of fermionic-field creation operator\n"<>
    "[ESC]fn[ESC]        Fermionic-field number operator\n"<>
    "[ESC]fnn[ESC]       Power of fermionic-field number operator\n"<>
	"[ESC]comm[ESC]      commutator template\n"<>
	"[ESC]anti[ESC]      anticommutator template\n"<>
    "\nThe quantum concatenation infix symbol [ESC]on[ESC] is"<>
    " used for operator application, inner product and outer product.\n"<>
    "\nSetOccupationAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";

Begin["`Private`"]

(* *** **** **** INPUT ALIASES *)

SetOccupationAliases[]:=
Module[{nb},
	nb:=InputNotebook[];
	SetOccupationAliases[nb]
];

SetOccupationAliases[doc_NotebookObject]:=
Module[{old,new,oldandnew},
	SetQuantumAliases[doc];
    old= InputAliases /. Options[doc] /. InputAliases->{};
    new={
		"up" ->		TagBox[RowBox[{"\[Null]","\[UpArrow]","\[Null]"}],Identity,
							Editable->False,Selectable->False],
		"dn" ->		TagBox[RowBox[{"\[Null]","\[DownArrow]","\[Null]"}],Identity,
							Editable->False,Selectable->False],
		"bket" ->	SubscriptBox[
   						TagBox[
     						RowBox[{"\[VerticalSeparator]", 
        						TagBox["\[Placeholder]",
          							zz080KetArgs,
          							Editable -> True,
          							Selectable -> True,
									  BaseStyle->{ShowSyntaxStyles->True}], "\[RightAngleBracket]"}],
     						zz080Ket,Editable -> False,Selectable -> False,
										BaseStyle->{ShowSyntaxStyles->False}], 
     					RowBox[{"\[ScriptCapitalB]","[","\[Placeholder]","]"}]],
     	"bbra" ->	SubscriptBox[
   						TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox["\[Placeholder]",zz080BraArgs,
							Editable->True,Selectable->True,
							BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
								BaseStyle->{ShowSyntaxStyles->False}], 
     					RowBox[{"\[ScriptCapitalB]","[","\[Placeholder]","]"}]],
		"fket" ->	SubscriptBox[
   						TagBox[
     						RowBox[{"\[VerticalSeparator]", 
        						TagBox["\[Placeholder]",
          							zz080KetArgs,
          							Editable -> True,
          							Selectable -> True,
									BaseStyle->{ShowSyntaxStyles->True}], "\[RightAngleBracket]"}],
     						zz080Ket,Editable -> False,Selectable -> False,
									BaseStyle->{ShowSyntaxStyles->False}], 
     					RowBox[{"\[ScriptCapitalF]","[","\[Placeholder]","]"}]],
     	"fbra" ->	SubscriptBox[
   						TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox["\[Placeholder]",zz080BraArgs,
							Editable->True,Selectable->True,
							BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
								BaseStyle->{ShowSyntaxStyles->False}], 
     					RowBox[{"\[ScriptCapitalF]","[","\[Placeholder]","]"}]],
     	"b-" ->		SubscriptBox["\[ScriptA]", 
   						RowBox[{"\[ScriptCapitalB]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}]],
   		"b--"->		SubsuperscriptBox["\[ScriptA]", 
   						RowBox[{"\[ScriptCapitalB]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}],
   						RowBox[{"\[Placeholder]"}]],
   		"b+"->		SubsuperscriptBox["\[ScriptA]", 
   						RowBox[{"\[ScriptCapitalB]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}],
   						RowBox[{"\[Dagger]"}]], 
   		"b++"->		SubsuperscriptBox[
   						"\[ScriptA]", 
  						RowBox[{"\[ScriptCapitalB]", "[", "\[Placeholder]", "]", 
   								"[","\[Placeholder]", "]"}], 
  						RowBox[{"\[Dagger]"," ","\[Placeholder]"}]],
   		"bn"->		SubscriptBox["\[ScriptCapitalN]", 
   						RowBox[{"\[ScriptCapitalB]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}]],
   		"bnn"->		SubsuperscriptBox["\[ScriptCapitalN]", 
   						RowBox[{"\[ScriptCapitalB]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}],
   						RowBox[{"\[Placeholder]"}]],
     	"f-" ->		SubscriptBox["\[ScriptA]", 
   						RowBox[{"\[ScriptCapitalF]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}]],
   		"f--"->		SubsuperscriptBox["\[ScriptA]", 
   						RowBox[{"\[ScriptCapitalF]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}],
   						RowBox[{"\[Placeholder]"}]],
   		"f+"->		SubsuperscriptBox["\[ScriptA]", 
   						RowBox[{"\[ScriptCapitalF]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}],
   						RowBox[{"\[Dagger]"}]], 
   		"f++"->		SubsuperscriptBox[
   						"\[ScriptA]", 
  						RowBox[{"\[ScriptCapitalF]", "[", "\[Placeholder]", "]", 
   								"[","\[Placeholder]", "]"}], 
  						RowBox[{"\[Dagger]"," ","\[Placeholder]"}]],
   		"fn"->		SubscriptBox["\[ScriptCapitalN]", 
   						RowBox[{"\[ScriptCapitalF]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}]],
   		"fnn"->		SubsuperscriptBox["\[ScriptCapitalN]", 
   						RowBox[{"\[ScriptCapitalF]","[", "\[Placeholder]", "]",
   								"[", "\[Placeholder]", "]"}],
   						RowBox[{"\[Placeholder]"}]]					    	   						 	
					    	   						 	
    	};
	oldandnew=Union[new,old];
	SetOptions[doc,InputAliases->oldandnew];
	SetOccupationAliases::aliases];
    	
(*  *** **** **** OUTPUT AND INPUT FORMATS *)

(* output formats for spin-down and spin-up arrows *)
(* the TagBox Identity argument is used in copy-paste input *)
(* the TagBox Editable and Selectable arguments
    are also used to avoid the selection of the DownArrow alone;
	selection and copy-paste of the arrow alone would create errors 
	because DownArrow is an operator that needs two arguments.
*)
DownArrow /: MakeBoxes[DownArrow[\[Null],\[Null]],form_]:=
	TagBox[RowBox[{"\[Null]","\[DownArrow]","\[Null]"}],Identity,
							Editable->False,Selectable->False];

UpArrow /: MakeBoxes[UpArrow[\[Null],\[Null]],form_]:=
	TagBox[RowBox[{"\[Null]","\[UpArrow]","\[Null]"}],Identity,
							Editable->False,Selectable->False];
							
Unprotect[Power];
Power /: MakeBoxes[Power[UpArrow[\[Null],\[Null]],n_Integer],form_]  :=
	RowBox@Table[TagBox[RowBox[{"\[Null]", "\[UpArrow]", "\[Null]"}], Identity, 
 							Editable -> False, Selectable -> False],{n}]/;n>0 ;
Power /: MakeBoxes[Power[DownArrow[\[Null],\[Null]],n_Integer],form_]  :=
	RowBox@Table[TagBox[RowBox[{"\[Null]", "\[DownArrow]", "\[Null]"}], Identity, 
 							Editable -> False, Selectable -> False],{n}]/; n>0;
Protect[Power];

Unprotect[Times];
Times /: MakeBoxes[Times[targ__,Power[UpArrow[\[Null],\[Null]],n_Integer]],form_]  :=
	RowBox[Join[{MakeBoxes[targ,form]},
				Table[TagBox[RowBox[{"\[Null]", "\[UpArrow]", "\[Null]"}], Identity, 
 							Editable -> False, Selectable -> False],{n}]]]/;n>0 ;
Times /: MakeBoxes[Times[targ__,Power[DownArrow[\[Null],\[Null]],n_Integer]],form_]  :=
	RowBox[Join[{MakeBoxes[targ,form]},
				Table[TagBox[RowBox[{"\[Null]", "\[DownArrow]", "\[Null]"}], Identity, 
 							Editable -> False, Selectable -> False],{n}]]]/;n>0 ;
Protect[Times];

(* ouput and input of occupation numbers *)

zz050Occupation /: MakeBoxes[zz050Occupation[{index__},Plus[symbols__]],form_]:=
	SubscriptBox[RowBox[{"(",MakeBoxes[Plus[symbols],form],")"}],
		RowBox[Drop[Flatten[Map[{",",MakeBoxes[#, form]}&, {index}]],1]]];

zz050Occupation /: MakeBoxes[zz050Occupation[{index__},Times[symbols__]],form_]:=
	SubscriptBox[RowBox[{"(",MakeBoxes[Times[symbols],form],")"}],
		RowBox[Drop[Flatten[Map[{",",MakeBoxes[#, form]}&, {index}]],1]]];

zz050Occupation /: MakeBoxes[zz050Occupation[{index__},number_],form_]:=
	SubscriptBox[MakeBoxes[number,form],
		RowBox[Drop[Flatten[Map[{",",MakeBoxes[#, form]}&, {index}]],1]]];
		
Unprotect[zz050Subscript];
(* Subscripts inside a (special type of) subscripted ket become
occupation data. Next definition is enough for kets and bras,
because a bra always tries the evaluation of the corresponding ket and
if it changes then it becomes the bra of the changed ket*)
zz050Subscript/:
zz080Ket[
	subs:zz050Subscript[
		{__?(MemberQ[{#},Subscript[__],Infinity]&)}, 
		{(\[ScriptCapitalB]|\[ScriptCapitalF])[___]}],
	otherKetArgs___ ]:=
	Module[{fieldInfo},
		fieldInfo=subs/.
			Subscript[number_,index__]:>zz050Occupation[{index},number];
		zz080Ket[fieldInfo,otherKetArgs]
	];
Protect[zz050Subscript];

(* output and input of the vacuum ket *)

Unprotect[zz080Ket];
zz080Ket /: 
	MakeBoxes[zz080Ket[
				zz050Subscript[{}, 
					{field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]}],
				otherKetArgs___],
			form_]:=
		MakeBoxes[zz080Ket[zz050Subscript[{0}, {field}],otherKetArgs],form];
		
zz080Ket /:
	zz080Ket[zz050Subscript[{0}, 
				{field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]}],
			otherKetArgs___]:=
	zz080Ket[zz050Subscript[{}, {field}],otherKetArgs];
Protect[zz080Ket];

(* output and input of number operator *)

zz050number /:
MakeBoxes[zz050number[{index__},
			field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]],
		form_]:=
	MakeBoxes[Subscript[\[ScriptCapitalN], field[index]],form];

\[ScriptCapitalN] /:
	Subscript[\[ScriptCapitalN], 
		(field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___])[index__]]:=
		zz050number[{index},field];

(* output and input of annihilation operators *)

zz060annihilation /:
MakeBoxes[zz060annihilation[{index__},
			field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]],
		form_]:=
	MakeBoxes[Subscript[\[ScriptA], field[index]],form];

\[ScriptA] /:
	Subscript[\[ScriptA], (field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___])[index__]]:=
		zz060annihilation[{index},field];

(* output of creation operator: *)

zz040creation /:
MakeBoxes[
	zz040creation[{index__},
		field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]],form_]:=
	SubsuperscriptBox[
		MakeBoxes[\[ScriptA],form],
		MakeBoxes[field[index],form],
		MakeBoxes[\[Dagger],form]];

(* the input of a creation operator is the hermitian conjugate
 of annihilation of operator, therefore it does not need to be specified *)

(* output of a power of a creation operator *)

Unprotect[Power];
Power /:
	MakeBoxes[
		Power[
			zz040creation[{index__},
				field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]],exp_],form_]:=
		SubsuperscriptBox[
			MakeBoxes[\[ScriptA],form],
			MakeBoxes[field[index],form],
			RowBox[{MakeBoxes[\[Dagger],form],MakeBoxes[exp,form]}]];	
Protect[Power];

(* the input of a power of a creation operator uses 
 definitions made in Quantum`Notation` for exponents that include one or more
 daggers 
 *)

(*  *** **** **** ****** **** ********** *** **** ******* **** ******* **** **** *)
(*  *** **** **** MAIN CALCULATION RULES *** **** ******* **** ******* **** **** *)
(*  *** **** **** ****** **** ********** *** **** ******* **** ******* **** **** *)

(* Warning: I think that UpArrow and DownArrow 
 must Not be QuantumObjects. Right? *)
SetQuantumObject[zz060annihilation,zz040creation,zz050number];

(*  *** **** **** MAIN CALCULATION RULES *** **** ******* **** ******* **** **** *)

(* ********** Occupation number rules *******************)

Unprotect[zz050Subscript];

(* Multiplications (therefore change of sign) 
to occupation numbers go to the number *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{ss1___,Times[zz050Occupation[{index__},x_],targ__],ss2___}, 
   		{field : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   	restket___]:=
zz080Ket[
	zz050Subscript[
		{ss1,zz050Occupation[{index},Times[x,targ]],ss2}, 
   		{field}], 
   	restket];
(* Additions to occupation numbers go to the number *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{ss1___,Plus[zz050Occupation[{index__},x_],parg__],ss2___}, 
   		{field : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   	restket___]:=
zz080Ket[
	zz050Subscript[
		{ss1,zz050Occupation[{index},Plus[x,parg]],ss2}, 
   		{field}], 
   	restket];
(* Additions of multiplications (therefore substractions)
 to occupation numbers go to the number *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{ss1___,Plus[Times[zz050Occupation[{index__},x_],targ__],parg__],ss2___}, 
   		{field : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   	restket___]:=
zz080Ket[
	zz050Subscript[
		{ss1,zz050Occupation[{index},Plus[Times[x,targ],parg]],ss2}, 
   		{field}], 
   	restket];

(* Numeric Occupation numbers must be integers.
floating point numbers equal to integers get rounded and accepted,
other floating point numbers lead to Abort[]
 *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{ss1___,zz050Occupation[{index__},x_?NumericQ],ss2___}, 
   		{field : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   	restket___]:=
If[TrueQ[x==Round[x]],
	zz080Ket[
		zz050Subscript[
			{ss1,zz050Occupation[{index},Round[x]],ss2}, 
   			{field}], 
   		restket],
	Message[zz050Occupation::NonInteger,{index},x];Abort[]] /; 
Not[IntegerQ[x]];

(* A (final) negative integer occupation number annihilates the ket *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{ss1___,zz050Occupation[{index__},x_?NumericQ],ss2___}, 
   		{field : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   	restket___]:=0 /; 
And[IntegerQ[x],x<0,Not[MemberQ[{ss1,ss2},zz050Occupation[{index},_]]]];

(* Pauli exclusion: A (final) occupation number larger 
then one in a fermion field annihilates the ket *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{ss1___,zz050Occupation[{index__},x_?NumericQ],ss2___}, 
   		{field : \[ScriptCapitalF][___]}], 
   	restket___]:=0 /; 
And[IntegerQ[x],x>1,Not[MemberQ[{ss1,ss2},zz050Occupation[{index},_]]]];

(* the same index cannot have two different occupation numbers 
   therefore, all the occupation numbers with the same index 
   are added to get a unique occupation number for each index *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{	ss1___,
			zz050Occupation[{sameindex__},x1_],
			ssi___,
			zz050Occupation[{sameindex__},x2_],
			ss2___}, 
   		{field : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   	restket___]:=
zz080Ket[
	zz050Subscript[
		{	ss1,
			zz050Occupation[{sameindex},x1+x2],
			ssi,
			ss2}, 
   		{field}], 
   	restket];   
   
(* Occupation numbers equal to zero dissapear *)
zz050Subscript/:
zz080Ket[
	zz050Subscript[
		{ss1___,zz050Occupation[{index__},0],ss2___}, 
   		{field : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   	restket___]:=
zz080Ket[zz050Subscript[{ss1,ss2}, {field}],restket];

Protect[zz050Subscript];

(* ********** Field kets and bras rules *******************)

Unprotect[zz075NonCommutativeTimes];
(* Bra and Ket of Different Fields live in different subspaces
   therefore something that looks like a bra-ket evolves to
   a ket-bra *)
HoldPattern[zz075NonCommutativeTimes[
  after___,
  zz080Ket[
  		zz050Subscript[{occuket:(zz050Occupation[{__},_]..)}, 
   			{fieldket : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   		restket___], 
  zz080Bra[
  		zz050Subscript[{occubra:(zz050Occupation[{__},_]..)}, 
  			{fieldbra : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
  		restbra___],
  before___
  ]]:=
	zz075NonCommutativeTimes[
			after,
			zz080Ket[restket],
			zz080Bra[zz050Subscript[{occubra}, {fieldbra}]],
			zz080Ket[zz050Subscript[{occuket}, {fieldket}]],
			zz080Bra[restbra],
			before
		] /; fieldket=!=fieldbra

(* Internal product of bra and ket of the same field.
Occupation states are orthonormal.
It takes into account that a missing occupation number is
actually a zero occupation number, 
and symbolic variables CAN be used as occupation numbers.
On the other hand, each state index is NOT consider as
a possible variable even if it is a symbol.
*)
HoldPattern[zz075NonCommutativeTimes[
		after___,
		zz080Ket[
  			zz050Subscript[{occuket:(zz050Occupation[{__},_]..)}, 
   				{samefield : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
   			restket___],
		zz080Bra[
  			zz050Subscript[{occubra:(zz050Occupation[{__},_]..)}, 
  				{samefield : (\[ScriptCapitalB] | \[ScriptCapitalF])[___]}], 
  			restbra___],
		before___
		]]:= 
Module[{m1,m2,indices},
	indices = Union[{occuket}[[All, 1]], {occubra}[[All, 1]]];
	m1 = Flatten[Map[Cases[{occuket}, zz050Occupation[#, x_] :> x] &, indices] /. {} -> {0}];
	m2 = Flatten[Map[Cases[{occubra}, zz050Occupation[#, x_] :> x] &, indices] /. {} -> {0}];
	Inner[KroneckerDelta[#1-#2] &, m1, m2, Times]*
	zz075NonCommutativeTimes[
		after,
		zz080Ket[restket],
		zz080Bra[restbra],
		before
		]
];

(* ********** General rules (bosons and fermions) for annihilation and creation operators ** *)

(* creation and annihilation operators are hermitian conjugates *)

zz060annihilation /:
zz080HermitianConjugate[zz060annihilation[
  {index__}, field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]]]:=
zz040creation[{index},field];

zz040creation /:
zz080HermitianConjugate[zz040creation[
  {index__}, field:(\[ScriptCapitalB]|\[ScriptCapitalF])[___]]]:=
zz060annihilation[{index},field];

zz050number /:
zz080HermitianConjugate[zz050number[args__]]:=zz050number[args];

(* floating-point powers of operators get rounded if the foating-point
is considered by Mathematica as mathematically equal to its rounded integer *)

zz060annihilation/:
Power[zz060annihilation[args__],
	n_?NumericQ]:=
		Power[zz060annihilation[args],
			Round[n]] /;
	And[Not[IntegerQ[n]],n==Round[n]];

zz040creation/:
Power[zz040creation[args__],
	n_?NumericQ]:=
		Power[zz040creation[args],
			Round[n]] /;
	And[Not[IntegerQ[n]],n==Round[n]];

(* ***************************************************************************** *)
(* BOSONIC RULES **************************************************************** *)

zz050Commutator[
	zz040creation[{index1__},field1:\[ScriptCapitalB][___]],
	zz040creation[{index2__},field2:\[ScriptCapitalB][___]] ]:=0;

zz050Commutator[
	zz060annihilation[{index1__},field1:\[ScriptCapitalB][___]],
	zz060annihilation[{index2__},field2:\[ScriptCapitalB][___]] ]:=0;

zz050Commutator[
	zz060annihilation[{index1__},field1:\[ScriptCapitalB][___]],
	zz040creation[{index2__},field2:\[ScriptCapitalB][___]] ]:=
	(	If[field1=!=field2,
			0,
			KroneckerDelta[index1-index2]
		]
	); 

zz050number /:
zz050Commutator[
	zz050number[{index1__},field1:\[ScriptCapitalB][___]],
	secondarg_ ]:=	
zz050Commutator[
	zz075NonCommutativeTimes[
		zz060annihilation[{index1},field1],
		zz040creation[{index1},field1]
	],
	secondarg ];
	
zz050number /:	
zz050Commutator[
	firstarg_,
	zz050number[{index1__},field1:\[ScriptCapitalB][___]]
]:=	
zz050Commutator[
	firstarg,
	zz075NonCommutativeTimes[
		zz060annihilation[{index1},field1],
		zz040creation[{index1},field1]
	]
];
	
(* OPERATORS ON KETS AND BRAS (BOSONS) **********************************************)

(* Number operator on ket *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 zz080Ket[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___], 
 Power[zz050number[{index__}, samefield_],m_.],
 nctarg2___]]:=
Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{index}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(Power[n,m])*
 		zz080Ket[
 			zz050Subscript[
   				{ssarg}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
];

(* Number operator on bra *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 Power[zz050number[{index__}, samefield_],m_.],
 zz080Bra[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___],
 nctarg2___]]:=
 Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{index}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(Power[n,m])*
 		zz080Bra[
 			zz050Subscript[
   				{ssarg}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
];

(* (Power of creation-annihilation)-Bket of the 
 same state (same field and same index)
 this rule is necessary for symbolic exponent m *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 zz080Ket[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___], 
 Power[HoldPattern[
 	zz075NonCommutativeTimes[zz060annihilation[{sameindex__}, samefield_],
 							zz040creation[{sameindex__}, samefield_]]],m_],  
 nctarg2___]]:=
 Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{sameindex}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(Power[n,m])*
 		zz080Ket[
 			zz050Subscript[
   				{ssarg}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
];

(* BBra-(Power of creation-annihilation) of the 
 same state (same field and same index)
 this rule is necessary for symbolic exponent m *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 Power[HoldPattern[
 	zz075NonCommutativeTimes[zz060annihilation[{sameindex__}, samefield_],
 							zz040creation[{sameindex__}, samefield_]]],m_],
 zz080Bra[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___],
 nctarg2___]]:=
 Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{sameindex}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(Power[n,m])*
 		zz080Bra[
 			zz050Subscript[
   				{ssarg}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
];

(* Bosonic annihilation operator on kets *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 zz080Ket[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___], 
 Power[zz060annihilation[{index__}, samefield_],m_.],
 nctarg2___]]:=
Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{index}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(FunctionExpand[Sqrt[Factorial[n]/Factorial[n-m]]] /. 
 		Gamma[x_] :> Factorial[x - 1])*
 		zz080Ket[
 			zz050Subscript[
   				{ssarg, zz050Occupation[{index}, -m]}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
]/; daggerFreeQ[m];

(* Bosonic creation operator on kets *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 zz080Ket[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___], 
 Power[zz040creation[{index__}, samefield_],m_.],
 nctarg2___]]:=
Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{index}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(FunctionExpand[Sqrt[Factorial[n+m]/Factorial[n]]] /. 
 		Gamma[x_] :> Factorial[x - 1])*
 		zz080Ket[
 			zz050Subscript[
   				{ssarg, zz050Occupation[{index}, m]}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
];

(* Bosonic creation operator on bras *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 Power[zz040creation[{index__}, samefield_],m_.],
 zz080Bra[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___],
 nctarg2___]]:=
 Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{index}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(FunctionExpand[Sqrt[Factorial[n]/Factorial[n-m]]] /. 
 		Gamma[x_] :> Factorial[x - 1])*
 		zz080Bra[
 			zz050Subscript[
   				{ssarg, zz050Occupation[{index}, -m]}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
];

(* Bosonic annihilation operator on bras *)
HoldPattern[zz075NonCommutativeTimes[
 nctarg1___,
 Power[zz060annihilation[{index__}, samefield_],m_.],
 zz080Bra[
 	zz050Subscript[{ssarg___}, 
   		{samefield : \[ScriptCapitalB][___]}],
   ketarg___],
 nctarg2___]]:=
 Module[{n,nlist},
	nlist=Cases[{ssarg},zz050Occupation[{index}, x_]:>x];
	n=Switch[Length[nlist],
			0,0,
			_,Plus@@nlist
	];
	zz075NonCommutativeTimes[
 		nctarg1,
 		(FunctionExpand[Sqrt[Factorial[n+m]/Factorial[n]]] /. 
 		Gamma[x_] :> Factorial[x - 1])*
 		zz080Bra[
 			zz050Subscript[
   				{ssarg, zz050Occupation[{index}, m]}, 
   				{samefield}],
   			ketarg], 
 		nctarg2]
]/; daggerFreeQ[m];

Protect[zz075NonCommutativeTimes];

(*  *** **** **** FINISHING THE PACKAGE *)

SetAttributes[
{ 	SetOccupationAliases,\[Dagger],
	\[ScriptCapitalB],\[ScriptCapitalF],\[ScriptA],\[ScriptCapitalN],DownArrow,UpArrow,
	zz050Occupation, zz060annihilation, zz040creation, zz050number 
},
{Protected,ReadProtected}
];

End[];

EndPackage[];
Column[{    
"Welcome to Quantum`Occupation`\n"<>
"A Mathematica package for Second Quantization"<>
" QFT in Occupation Number bra-ket notation\n"<>
"by Jos\[EAcute] Luis G\[OAcute]mez-Mu\[NTilde]oz"<>
"\n\nExecute SetOccupationAliases[] in order"<>
" to use the keyboard to enter second quantization objects"<>
" in Dirac's notation\n"<>
"SetOccupationAliases[] must be executed again in"<>
" each new notebook that is created",
"\nMATHEMATICA "<>$Version,
"\nQUANTUM version EN CONSTRUCCION",
"\nTODAY IS "<>DateString[]
}]


