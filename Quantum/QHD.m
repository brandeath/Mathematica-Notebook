(* ::Package:: *)

(* Quantum`QHD`
 
   Copyright 2011 
   Jose Luis Gomez-Munoz and Francisco Delgado-Cepeda
   ITESM-CEM,   
   Departamento de Ciencias Basicas (Matematicas)
   Carretera Lago de Guadalupe Km. 3.5, 
   Atizapan de Zaragoza, Estado de Mexico, 
   C.P. 52926
   Mexico
   jose.luis.gomez@itesm.mx
   http://homepage.cem.itesm.mx/lgomez/quantum
   
   A package for Quantized Hamilton Dynamics (Heisenberg Equations of
   Motion) in Mathematica.
   This package may be freely redistributed under the condition that the   
   copyright notices (including this entire header) are not removed
   and no compensation is received.  Private, research, and institutional     
   use is free.  You may distribute modified versions of this code UNDER  
   THE CONDITION THAT THIS CODE AND ANY MODIFICATIONS MADE TO IT IN THE   
   SAME FILE REMAIN UNDER COPYRIGHT OF THE ORIGINAL AUTHOR, SOURCE
   CODE IS MADE FREELY AVAILABLE WITHOUT CHARGE, AND CLEAR    
   NOTICE IS GIVEN OF THE MODIFICATIONS.  Distribution of this code as
   part of a commercial system is permissible ONLY BY DIRECT ARRANGEMENT
   WITH THE AUTHOR.  (If you are not directly supplying this code to a
   customer, and you are instead telling them how they can obtain it for
   free, then you are not required to make any arrangement with me.)
  
*)

Quantum`Notation`AutoLoadPalette=False;

BeginPackage["Quantum`QHD`",
	{"Quantum`Notation`","DifferentialEquations`InterpolatingFunctionAnatomy`"}] 

Unprotect[AngleBracket,zz050Expected,
	SetQHDAliases,
	QHDSymmetrize, QHDExpand, 
	QHDHBar,
	QHDApproximantFunction, QHDMaxOrder,
	QHDCrossTermsApproximant,
	QHDOrder, QHDEOM,
	QHDClosure,  
	QHDHierarchy,QHDVariables,
	QHDForm,
	QHDDifferentialEquations,
	QHDInitialConditionsTemplate,
	QHDNDSolve,
	QHDConnectivity,QHDGraphPlot,
	QHDFunction,
	QHDPlot,QHDParametricPlot,QHDParametricPlot3D,
	QHDSymbolForTime,
	QHDClosureStyle,
	QHDNeedClosureStyle,
	zz050D,
	zz075Symmetric, QHD, QHDLabel,
	\[SelectionPlaceholder], \[ScriptT], \[HBar], \[ScriptS]
];
ClearAll[AngleBracket,zz050Expected,
	SetQHDAliases,
	QHDSymmetrize, QHDExpand, 
	QHDHBar,
	QHDApproximantFunction, QHDMaxOrder,
	QHDCrossTermsApproximant,
	QHDOrder, QHDEOM,
	QHDClosure,  
	QHDHierarchy,QHDVariables,
	QHDForm,
	QHDDifferentialEquations,
	QHDInitialConditionsTemplate,
	QHDNDSolve,
	QHDConnectivity,QHDGraphPlot,
	QHDFunction,
	QHDPlot,QHDParametricPlot,QHDParametricPlot3D,
	QHDSymbolForTime,
	QHDClosureStyle,
	QHDNeedClosureStyle,
	zz050D,
	zz075Symmetric,QHD, QHDLabel,
	\[SelectionPlaceholder], \[ScriptT], \[HBar], \[ScriptS]
];

Options[QHDClosure]={QHDApproximantFunction->QHDCrossTermsApproximant};

Options[QHDEOM]=
	Join[{QHDHBar->\[HBar],QHDLabel->Automatic},Options[QHDClosure]];

Options[QHDHierarchy]=Join[{QHDMaxOrder->2},Options[QHDEOM]];

Options[QHDGraphPlot]=
	Join[{
		QHDLabel->Automatic,
		QHDClosureStyle->{Smaller,Plain,Darker[Red,0.4]},
		QHDNeedClosureStyle->{Larger,Bold,Darker[Blue,0.4]} },
		ReplaceAll[Options[GraphPlot],
		{	(DirectedEdges->_)->(DirectedEdges->True),
			(VertexLabeling->_)->(VertexLabeling->True),
			(VertexRenderingFunction->_)->(VertexRenderingFunction->(Text[#2,#1]&)),
			(Method->_)->(Method->"CircularEmbedding")}]];

Options[QHDForm]=
	Join[{
		QHDLabel->Automatic,
		QHDSymbolForTime -> \[ScriptT],
		QHDClosureStyle->{},
		QHDNeedClosureStyle->{} },
		ReplaceAll[Options[Column],
			{(Dividers->_)->(Dividers->All)}]];

Options[QHDDifferentialEquations]=
	{QHDSymbolForTime -> \[ScriptT]};

Options[QHDNDSolve]=
	Join[{QHDSymbolForTime -> \[ScriptT]},
		Options[NDSolve]
		];

Options[QHDFunction]=
	{QHDSymbolForTime -> \[ScriptT]};

Options[QHDPlot]=
	Join[{QHDSymbolForTime -> \[ScriptT]},
		ReplaceAll[Options[Plot],
			{(PlotStyle->_)->(PlotStyle->Thick),
			(Frame->_)->(Frame->True),
			(FrameLabel->_)->(FrameLabel->Automatic),
			(Axes->_)->(Axes->False)}],
		Options[GraphicsGrid]
		];

Options[QHDParametricPlot]=
	Join[{QHDSymbolForTime -> \[ScriptT]},
		ReplaceAll[Options[ParametricPlot],
			{(PlotStyle->_)->(PlotStyle->Thick),
			(Frame->_)->(Frame->True),
			(FrameLabel->_)->(FrameLabel->Automatic),
			(Axes->_)->(Axes->False)}]
		];

Options[QHDParametricPlot3D]=
	Join[{QHDSymbolForTime -> \[ScriptT]},
		ReplaceAll[Options[ParametricPlot3D],
			{
				(PlotStyle->_)->(PlotStyle->{Thick,Hue[0.67, 0.6, 0.6]}), 
				(Boxed->_)->(Boxed->True), 
				(BoxStyle->_)->(BoxStyle -> Dashed),  
				(Axes->_)-> (Axes -> True),
				(AxesEdge->_)->(AxesEdge->{{-1,-1},{-1,-1},{-1,-1}}),
				(AxesLabel->_)->(AxesLabel->Automatic),
				(AxesStyle->_)->(AxesStyle -> Thick)}]
		];

SetQHDAliases::usage=
					"SetQHDAliases[] sets keyboard aliases in the"<>
					" selected notebook. SetQHDAliases[notebook]"<>
					" sets keyboard aliases in the specified notebook."<>
					" The aliases are keyboard key-combinations"<>
					" for the input of QHD objects.";
SetQHDAliases::aliases=
    "ALIASES:\n"<>
	"[ESC]on[ESC]     \[CenterDot] Quantum concatenation symbol\n"<>
	"[ESC]time[ESC]   \[ScriptT] Time symbol\n"<>
	"[ESC]hb[ESC]     \[HBar] Reduced Planck's constant (h bar)\n"<>
	"[ESC]ii[ESC]     \[ImaginaryI] Imaginary I symbol\n"<>
	"[ESC]inf[ESC]    \[Infinity] Infinity symbol\n"<>
	"[ESC]->[ESC]     \[Rule] Option (Rule) symbol\n"<>
	"[ESC]ave[ESC]    \[LeftAngleBracket]\[Placeholder]\[RightAngleBracket] Quantum average template\n"<>
	"[ESC]expec[ESC]  \[LeftAngleBracket]\[Placeholder]\[RightAngleBracket] Quantum average template\n"<>
	"[ESC]symm[ESC]   (\[Placeholder]\[CenterDot]\[Placeholder]\!\(\*SubscriptBox[\()\), \(\[ScriptS]\)]\) Symmetrized quantum product template\n"<>
	"[ESC]comm[ESC]   \[LeftDoubleBracket]\[Placeholder],\[Placeholder]\!\(\*SubscriptBox[\(\[RightDoubleBracket]\), \(-\)]\) Commutator template\n"<>
	"[ESC]po[ESC]     (\[Placeholder]\!\(\*SuperscriptBox[\()\), \(\[Placeholder]\)]\) Power template\n"<>
	"[ESC]su[ESC]     \!\(\*SubscriptBox[\(\[Placeholder]\), \(\[Placeholder]\)]\) Subscripted variable template\n"<>
	"[ESC]posu[ESC]   \!\(\*SubsuperscriptBox[\(\[Placeholder]\), \(\[Placeholder]\), \(\[Placeholder]\)]\) Power of a subscripted variable template\n"<>
	"[ESC]fra[ESC]    \!\(\*FractionBox[\(\[Placeholder]\), \(\[Placeholder]\)]\) Fraction template\n"<>
	"[ESC]eva[ESC]    \[Placeholder]/.{\[Placeholder]\[Rule]\[Placeholder],\[Placeholder]\[Rule]\[Placeholder]} Evaluation (ReplaceAll) template\n"<>
	"\nSetQHDAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
AngleBracket::usage=
	"AngleBracket is used for the format of"<>
	" quantum averages in the Heisenberg representation.";
zz050Expected::usage=
	"zz050Expected is used for expected values";
QHDSymmetrize::usage=
	"QHDSymmetrize[expr] symmetrizes quantum products in expr";
QHDExpand::usage=
	"QHDExpand[expr] unsymmetrizes quantum products in expr";
\[ScriptS]::usage=
	"\[ScriptS] is a tag used in the format of Weyl symmetrized quantum products";
QHDCrossTermsApproximant::usage=
	"QHDCrossTermsApproximant[expr] approximates expected values"<>
	" of crossterms \[LeftAngleBracket]\!\(\*SubsuperscriptBox[\"p\", \"1\", \"n\"]\)\[CenterDot]\!\(\*SubsuperscriptBox[\"p\", \"2\", \"m\"]\)\[RightAngleBracket]\[TildeTilde]\[LeftAngleBracket]\!\(\*SubsuperscriptBox[\"p\", \"1\", \"n\"]\)\[RightAngleBracket]\[LeftAngleBracket]\!\(\*SubsuperscriptBox[\"p\", \"2\", \"m\"]\)\[RightAngleBracket] in expr\n";
QHDOrder::usage=
	"QHDOrder[expr] gives the QHD-order of expr";
QHDClosure::usage=
	"QHDClosure[order,expr] applies the QHD-order closure scheme to expr.\n"<>
	"QHDClosure[{var1,var2,...},expr] applies the closure scheme to expr"<>
	" in terms of the dynamical variables {var1,var2,...}";
QHDClosure::notimpl="The requested QHD order `1` has not been implemented";
QHDClosure::exprord="The expresion `1` has QHD order `2` which is too large";
QHDClosure::fail="Could not decompose `1` in terms of `2`";
QHDClosure::nonope="Some variables in `1` are not quantum operators."<>
	" Use the command SetQuantumObject to define quantum operators.";
QHDEOM::usage=
	"QHDEOM[order,var,hamiltonian] gives the"<>
	" expression for the Equation of Motion (EOM)"<>
	" of dynamical variable var in the QHD-order formalism,"<>
	" using approximants (as specified by its option"<>
	" QHDApproximantFunction), and for the"<>
	" specified hamiltonian.\n"<>
	"The order can be infinite (no closure). "<>
	"A list of dynamical variables {var1,var2,...} can be specified"<>
	" instead of order.\n"<>
	" Its output is in the same format as the output of"<>
	" QHDHierarchy";
QHDEOM::closure="The closure procedure modified `1` into `2`."<>
	" The last expression will be used to build the hierarchy";
QHD::VerifySubscripts=
	"WARNING: Possible typing error, the following variables"<>
	" were expected to have subscripts: `1`";
QHDHierarchy::usage=
	"QHDHierarchy[order,var,hamiltonian] gives the"<>
	" expressions for the hierarchy of dynamic variables and"<>
	" their equations of motion (EOM)"<>
	" that are needed for the EOM"<>
	" of dynamical variable var in the QHD-order formalism,"<>
	" using approximants (as specified by its option"<>
	" QHDApproximantFunction), and for the"<>
	" specified hamiltonian.\n"<>
	"The order can be infinite (no closure). "<>
	"A list of dynamical variables {var1,var2,...} can be specified"<>
	" instead of order.";
QHDVariables::usage=
	"QHDVariables[expr] gives"<>
	" the set (list) of QHD dynamical variables in expr";
QHDForm::usage=
	"QHDForm[hierarchy]"<>
	" where hierarchy is the output of QHDHierarchy" <>
	" shows the hierarchy of equations of motion (EOM)"<>
	" in a format for display and export purposes only (TraditionalForm).";
QHDDifferentialEquations::usage=
	"QHDDifferentialEquations[hierarchy]"<>
	" where hierarchy is the output of QHDHierarchy" <>
	" gives the hierarchy of equations of motion (EOM)"<>
	" in a format suitable for input of NDSolve.";
QHDInitialConditionsTemplate::usage=
	"QHDInitialConditionsTemplate[hierarchy,initialtime],"<>
	" where hierarchy is the output of QHDHierarchy," <>
	" gives the initial conditions template for the hierarchy"<>
	" in a format suitable for input of NDSolve.";
QHDNDSolve::usage=
	"QHDNDSolve[hierarchy,inicond,tmin,tmax],"<>
	" where hierarchy is the output of QHDHierarchy and"<>
	" inicond has the format of the output of QHDInitialConditionsTemplate,"<>
	" finds a numerical solution to the diferential equations of"<>
	" hierarchy for time in the range tmin to tmax.";
QHDConnectivity::usage=
	"QHDConnectivity[hierarchy],"<>
	" where hierarchy is the output of QHDHierarchy," <>
	" represents the QHD hierarchy as edge-rules that can be used"<>
	" as input for GraphPlot and LayeredGraphPlot";
QHDParametricPlot3D::usage=
	"QHDParametricPlot3D[{expr1,expr2,expr3},sol],"<>
	" where sol is the output of NDSolve, "<>
	" plots a phase-space plot of {expr1,expr2,expr3}.";
QHDParametricPlot::usage=
	"QHDParametricPlot[{expr1,expr2},sol], where sol is the output of NDSolve,"<>
	" plots a phase-space plot of {expr1,expr2}.";
QHDFunction::usage=
	"QHDFunction[expr,sol], where sol is the output of NDSolve,"<>
	" generates the Mathematica Function corresponding to expr."<>
	" That Function can be evaluated, numerically derivated or integrated,"<>
	" be used as input for FindRoot, etc.";
QHDPlot::usage=
	"QHDPlot[expr,sol], where sol is the output of NDSolve, "<>
	" plots expr as a function of time. QHDPlot[All,sol] plots "<>
	" all the dynamical variables as functions of time.";
QHDGraphPlot::usage=
	"QHDGraphPlot[hierarchy]"<>
	" where hierarchy is the output of QHDHierarchy" <>
	" shows the hierarchy of equations of motion (EOM)"<>
	" as a GraphPlot.";
QHDSymbolForTime::usage=
	"QHDSymbolForTime is an option for several QHD commands"<>
	" that specifies the symbolo to be used for the time variable."<>
	" The default value is QHDSymbolForTime\[Rule]"<>
	ToString[QHDSymbolForTime/.Options[QHDNDSolve]];
QHDClosureStyle::usage=
	"QHDClosureStyle is an option for QHDGraphPlot and QHDForm"<>
	" which specifies the style to apply to"<>
	" expected values that do NOT need closure";
QHDNeedClosureStyle::usage=
	"QHDNeedClosureStyle"<>
	" is an option of QHDGraphPlot and QHDForm"<>
	" which specifies the style to apply to"<>
	" expected values that DO need closure";
QHDMaxOrder::usage=
	"QHDMaxOrder is an option for"<>
	" QHDHierarchy which"<>
	" specifies the order where to stop (truncate)"<>
	" an infinite-order hierarchy if"<>
	" it has not stop by itself."<>
	" The default value is QHDMaxOrder\[Rule]"<>
	ToString[QHDMaxOrder/.Options[QHDHierarchy]];
QHDHBar::usage=
	"QHDHBar is an option for"<>
	" QHDHierarchy,"<>
	" QHDEOM which"<>
	" specifies the symbol or value used for Planck reduced constant."<>
	" The default value is QHDHBar\[Rule]"<>
	ToString[QHDHBar/.Options[QHDHierarchy]];
QHDApproximantFunction::usage=
	"QHDApproximantFunction is an option for"<>
	" QHDHierarchy,"<>
	" QHDEOM which"<>
	" specifies the approximation transformation(s) that must be applied"<>
	" before and after the closure procedure."<>
	" Its default value is:"<>
	" QHDApproximantFunction\[Rule]"<>
	ToString[QHDApproximantFunction/.Options[QHDEOM]];
zz050D::usage=
	"zz050D is used in the output format of QHDForm."<>
	" It is not meant to be used by the final user.";
zz075Symmetric::usage=
	"zz075Symmetric is an internal Quantum object";
\[SelectionPlaceholder]::usage=
	"\[SelectionPlaceholder] is used in QHDInitialConditionsTemplate";
\[ScriptT]::usage=
	"\[ScriptT] is used as time symbol by default in QHD equations";
\[HBar]::usage=
	"\[HBar] is used as the reduced Planck constant"<>
	" by default in QHD calculations";
QHDLabel::usage=
	"QHDLabel is an option for QHDEOM, QHDHierarchy,"<>
	" QHDGraphPlot and QHDForm that specifies an overall label for the output.";


Begin["`Private`"]

SetQuantumObject[zz075Symmetric];

(* *** **** **** INPUT ALIASES *)

SetQHDAliases[]:=
Module[{nb},
	nb:=InputNotebook[];
	SetQHDAliases[nb]
];

SetQHDAliases[doc_NotebookObject]:=
Module[{new,old,oldandnew},
	SetQuantumAliases[doc];
	old=InputAliases /. Options[InputNotebook[],InputAliases] /. InputAliases->{};
    new={
		"ave" ->     RowBox[{"\[LeftAngleBracket]","\[Placeholder]","\[RightAngleBracket]"}],
		"expec" ->   RowBox[{"\[LeftAngleBracket]","\[Placeholder]","\[RightAngleBracket]"}],
		"symm"->     SubscriptBox[RowBox[{"(",RowBox[{"\[Placeholder]","\[CenterDot]","\[Placeholder]"}],")"}],"\[ScriptS]"],
		"time"->     RowBox[{"\[ScriptT]"}],
		"supo" ->	SubsuperscriptBox["\[Placeholder]","\[Placeholder]","\[Placeholder]"],
		"posu" ->	SubsuperscriptBox["\[Placeholder]","\[Placeholder]","\[Placeholder]"],
		"fra" ->	FractionBox["\[Placeholder]","\[Placeholder]"],
		"eva" ->	RowBox[{"\[Placeholder]","/.",RowBox[{"{",RowBox[{
						RowBox[{"\[Placeholder]","\[Rule]","\[Placeholder]"}],",",RowBox[{"\[Placeholder]","\[Rule]","\[Placeholder]"}]}],"}"}]}]
	};
	oldandnew=Union[old,new];
	SetOptions[doc,InputAliases->oldandnew];
	SetQHDAliases::aliases];

(* *** **** **** OUTPUT AND INPUT FORMATS *)

AngleBracket[args___]:=zz050Expected[args];

zz050Expected /: MakeBoxes[zz050Expected[args___],form_]:=
	MakeBoxes[AngleBracket[args],form];

zz075Symmetric /: MakeBoxes[zz075Symmetric[arg1_,argrest__],form_]:=
	MakeBoxes[Subscript[zz075NonCommutativeTimes[arg1,argrest],\[ScriptS]],form]/.
		SubscriptBox[box_,"\[ScriptS]"]:>SubscriptBox[RowBox[{"(",box,")"}],"\[ScriptS]"];
zz075Symmetric /: MakeBoxes[zz075Symmetric[arg1_],form_]:=
	MakeBoxes[Subscript[arg1,\[ScriptS]],form]/.
		SubscriptBox[box_,"\[ScriptS]"]:>SubscriptBox[RowBox[{"(",box,")"}],"\[ScriptS]"];

\[ScriptS] /: Subscript[arg_,\[ScriptS]]:= zz075Symmetric[arg];

(** TraditinalForm formats **)

zz050D /: MakeBoxes[zz050D["QHDLabel",arg2_],TraditionalForm]:=
	MakeBoxes["QHDLabel",TraditionalForm];

zz050D /: MakeBoxes[zz050D[arg1_?(#=!="QHDLabel" &),arg2_],TraditionalForm]:=
	MakeBoxes[D[arg1,arg2],TraditionalForm]/."\[PartialD]"->"d";

zz050Expected /: MakeBoxes[
	zz050Expected[zz075Symmetric[arg1_,argrest__]],TraditionalForm]:=
		MakeBoxes[Subscript[
			zz050Expected[zz075NonCommutativeTimes[arg1,argrest]],\[ScriptS]],
			TraditionalForm];
zz050Expected /: MakeBoxes[
	zz050Expected[zz075Symmetric[arg1_]],TraditionalForm]:=
		MakeBoxes[Subscript[
			zz050Expected[arg1],\[ScriptS]],
			TraditionalForm];


(*  *** **** **** MAIN CALCULATION RULES *)

zz075Symmetric[s_?QuantumScalarQ]:=s;

zz075Symmetric[args1___,s_?QuantumScalarQ,args2___]:=
	s*zz075Symmetric[args1,args2]/;{args1,args2}=!={};

zz075Symmetric[HoldPattern[zz075NonCommutativeTimes[args__]]]:=
	zz075Symmetric[args];

zz075Symmetric[HoldPattern[Plus[args__]]]:=
	Plus@@Map[zz075Symmetric,{args}];

zz075Symmetric[HoldPattern[Times[args__]]]:=
	Times@@Map[zz075Symmetric,{args}];

zz075Symmetric[arg1_,argrest__]:=
With[{revlist=Reverse[Sort[List@@zz075NonCommutativeTimes[arg1,argrest]]]},
	zz075Symmetric@@revlist /; {arg1,argrest}=!=revlist
];

zz050symmprod[HoldPattern[zz075NonCommutativeTimes[args__]]]:=
Module[{symm,expr},
	symm=zz075Symmetric[args];
	expr=zz075NonCommutativeTimes[args]+symm-QHDExpand[symm];
	FixedPoint[x\[Function]Expand[CommutatorExpand[x]],expr]
];

QHDSymmetrize[expr_]:=
Module[{expr0},
	expr0=expr /. p_zz075NonCommutativeTimes:>zz050symmprod[p];
	Expand[expr0]
];

QHDExpand[expr_]:=
	Expand[expr /. 
		HoldPattern[zz075Symmetric[args__]] :> 
			(zz075NonCommutativeTimes[args]+
				zz075NonCommutativeTimes@@Reverse[{args}])/2];

zz050Expected[{elements__}]:=Map[zz050Expected,{elements}];

zz050Expected[a_+b_]:=zz050Expected[a]+zz050Expected[b];

zz050Expected[a_*b_]:=
	a*zz050Expected[b] /; QuantumScalarQ[a];

zz050Expected[a_]:=With[{expd=Expand[a]},zz050Expected[expd]/;(expd=!=a)];

zz050Expected[a_]:=a /; QuantumScalarQ[a];

QHDVariables[expression_]:=
	Module[{expr},
		expr=QHDSymmetrize[ QHDExpand[\[LeftAngleBracket]expression\[RightAngleBracket]] ];
		Union[Cases[{expr},
				HoldPattern[zz050Expected[v_]],Infinity]]
	];



QHDCrossTermsApproximant[expr_]:=
Expand[
QHDSymmetrize[
ReplaceAll[
QHDExpand[expr],
{(*Remember zz075NonCommutativeTimes arguments in reverse order*)
	HoldPattern[zz050Expected[zz075NonCommutativeTimes[
			Power[Subscript[v1_, s1_],n1_.],
			Power[Subscript[v2_, s2_],n2_.]]]]/;
		s1=!=s2 :> 
		\[LeftAngleBracket]Power[Subscript[v1, s1],n1]\[RightAngleBracket]*
		\[LeftAngleBracket]Power[Subscript[v2, s2],n2]\[RightAngleBracket],
	HoldPattern[zz050Expected[zz075NonCommutativeTimes[
			Power[Subscript[v1_, s1_],n1_.],
			Power[Subscript[v2_, s2_],n2_.],
			Power[Subscript[v3_, s3_],n3_.]]]]/;
		s1=!=s3&&s1=!=s2 :>
		\[LeftAngleBracket]Power[Subscript[v3, s3],n3]\[CenterDot]Power[Subscript[v2, s2],n2]\[RightAngleBracket]*
		\[LeftAngleBracket]Power[Subscript[v1, s1],n1]\[RightAngleBracket],
	HoldPattern[zz050Expected[zz075NonCommutativeTimes[
			Power[Subscript[v1_, s1_],n1_.],
			Power[Subscript[v2_, s2_],n2_.],
			Power[Subscript[v3_, s3_],n3_.]]]]/;
		s3=!=s1&&s3=!=s2 :>
		\[LeftAngleBracket]Power[Subscript[v3, s3],n3]\[RightAngleBracket]*
		\[LeftAngleBracket]Power[Subscript[v2, s2],n2]\[CenterDot]Power[Subscript[v1, s1],n1]\[RightAngleBracket],
	HoldPattern[zz050Expected[zz075NonCommutativeTimes[
			Power[Subscript[v1_, s1_],n1_.],
			Power[Subscript[v2_, s2_],n2_.],
			Power[Subscript[v3_, s3_],n3_.],
			Power[Subscript[v4_, s4_],n4_.]]]]/;
		s1=!=s4&&s1=!=s3&&s1=!=s2 :>
		\[LeftAngleBracket]Power[Subscript[v4, s4],n4]\[CenterDot]
			Power[Subscript[v3, s3],n3]\[CenterDot]
			Power[Subscript[v2, s2],n2]\[RightAngleBracket]*
		\[LeftAngleBracket]Power[Subscript[v1, s1],n1]\[RightAngleBracket],
	HoldPattern[zz050Expected[zz075NonCommutativeTimes[
			Power[Subscript[v1_, s1_],n1_.],
			Power[Subscript[v2_, s2_],n2_.],
			Power[Subscript[v3_, s3_],n3_.],
			Power[Subscript[v4_, s4_],n4_.]]]]/;
		s4=!=s1&&s4=!=s2&&s4=!=s3 :>
		\[LeftAngleBracket]Power[Subscript[v4, s4],n4]\[RightAngleBracket]*
		\[LeftAngleBracket]Power[Subscript[v3, s3],n3]\[CenterDot]
			Power[Subscript[v2, s2],n2]\[CenterDot]
			Power[Subscript[v1, s1],n1]\[RightAngleBracket]
		} 
] ] ];


QHDOrder[expr_]:=
Module[{nct,nctexpr,orders,theorder},
	SetQuantumObject[nct];	
	nct[
		right___,
		Power[a_,n_Integer],
		left___
	]:=
		nct[right,Apply[Sequence,Table[a,{n}]],left] /; n>0;

	nctexpr=QHDExpand[expr];
	nctexpr=nctexpr/.
		{HoldPattern[zz075NonCommutativeTimes[args__]]:>nct[args],
		HoldPattern[Power[a_?(Not[QuantumScalarQ[#]]&),n_]]:>nct[Power[a,n]]};
(* This trick is necessary so that Cases works also in very simple nctexpr *)	
	nctexpr=nctexpr+nct[]; 
	orders=Cases[nctexpr,HoldPattern[nct[args___]]:>Length[{args}],Infinity];
	Clear[nct];
	theorder=Max[orders];
	If[theorder==0,theorder=1];
	theorder
];


(* Start of QHD for a given set of dynamical variables (instead of integer order) *)
Options[zz050DecomposeExpected]:=Options[QHDClosure];

zz050DecomposeExpected[listav:{HoldPattern[zz050Expected[_]]..},
					expr:HoldPattern[zz050Expected[_]],opts:OptionsPattern[]]:=
Module[{exprorder,exprclos,correctedlist,a,b},
	correctedlist=listav/.
		HoldPattern[zz075NonCommutativeTimes[args__]]:>
			zz075Symmetric[args];
	If[MemberQ[correctedlist,expr],Return[expr]];
	exprorder=QHDOrder[expr];
	If[exprorder==1,Return[expr]];
	exprclos=QHDClosure[exprorder-1,expr,FilterRules[{opts},Options[QHDClosure]]];
	exprclos /. HoldPattern[zz050Expected[arg_]]:>  
	zz050DecomposeExpected[correctedlist,zz050Expected[arg]]
];
QHDClosure[listav:{HoldPattern[zz050Expected[_]]..},generalexpro_,
			opts:OptionsPattern[]]:=
Module[{result,failed,resultvars,correctedlist,generalexpr,approximant},
	approximant=OptionValue[QHDApproximantFunction];
	generalexpr=approximant[QHDSymmetrize[QHDExpand[approximant[\[LeftAngleBracket]generalexpro\[RightAngleBracket]]]]];
	correctedlist=listav/.
		HoldPattern[zz075NonCommutativeTimes[args__]]:>
			zz075Symmetric[args];
	result=generalexpr/. HoldPattern[zz050Expected[arg_]]:>  
			zz050DecomposeExpected[correctedlist,zz050Expected[arg]];
	resultvars=Union[Cases[{result},HoldPattern[zz050Expected[_]],Infinity]];
	failed=Cases[resultvars,_?(Not[MemberQ[correctedlist,#]]&)];
	If[Length[failed]!=0,Message[QHDClosure::fail,failed,correctedlist]];
	approximant[Expand[result]]
];
QHDClosure[lista_List,generalexpro_,opts:OptionsPattern[]]:=
Module[{listwithoutexpected,somenonoperator,generalexpr},
	generalexpr=QHDSymmetrize[QHDExpand[\[LeftAngleBracket]generalexpro\[RightAngleBracket]]];
	listwithoutexpected=lista/.
		HoldPattern[zz050Expected[arg_]]:>arg;
	somenonoperator=Apply[Or,QuantumScalarQ[listwithoutexpected]];
	If[somenonoperator,
		Message[QHDClosure::nonope,listwithoutexpected];Abort[]];
	QHDClosure[Map[zz050Expected,lista],generalexpr,
			FilterRules[{opts},Options[QHDClosure]]]
];
(* End of QHD for a given set of dynamical variables (instead of integer order) *)



QHDClosure[\[Infinity],expr_,opts:OptionsPattern[]]:=
Module[{approximant},
	approximant=OptionValue[QHDApproximantFunction];
	approximant[QHDSymmetrize[QHDExpand[approximant[\[LeftAngleBracket]expr\[RightAngleBracket]]]]]
];

QHDClosure[order_Integer,expro_,opts:OptionsPattern[]]:=
Module[{nct,nctexpr,exprorder,expr,approximant},
	approximant=OptionValue[QHDApproximantFunction];
	expr=approximant[QHDSymmetrize[QHDExpand[approximant[\[LeftAngleBracket]expro\[RightAngleBracket]]]]];
	
(* Abort if order>=5 was requested *)
	If[Or[order<=0,order>=5],
		Message[QHDClosure::notimpl,order];Abort[]];

(* Abort if the expression has order>=5+1 *)
	exprorder=QHDOrder[expr];
	If[exprorder>=5+1,
		Message[QHDClosure::exprord,expr,exprorder];Abort[]];

	SetQuantumObject[nct];	
	nct[
		right___,
		Power[a_,n_Integer],
		left___
	]:=
		nct[right,Apply[Sequence,Table[a,{n}]],left] /; n>0;

	nctexpr=QHDExpand[expr];

	If[order<=4 && exprorder>4,
		nctexpr=nctexpr/.
			{HoldPattern[zz075NonCommutativeTimes[args__]]:>nct[args],
			HoldPattern[Power[a_?(Not[QuantumScalarQ[#]]&),n_]]:>nct[Power[a,n]]
		};
		nctexpr=ReplaceAll[nctexpr, 
			HoldPattern[zz050Expected[nct[a_,b_,c_,d_,e_]]]:>
				-24\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]+
				6( \[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,b]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,c]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,d]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[a,e]\[RightAngleBracket]+
					\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[b,c]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[b,d]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[b,e]\[RightAngleBracket]+
					\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[c,d]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[c,e]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]nct[d,e]\[RightAngleBracket] )-
				2( \[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,b]\[RightAngleBracket]\[LeftAngleBracket]nct[c,d]\[RightAngleBracket]+\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,c]\[RightAngleBracket]\[LeftAngleBracket]nct[b,d]\[RightAngleBracket]+\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,d]\[RightAngleBracket]\[LeftAngleBracket]nct[b,c]\[RightAngleBracket]+
					\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[a,b]\[RightAngleBracket]\[LeftAngleBracket]nct[c,e]\[RightAngleBracket]+\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[a,c]\[RightAngleBracket]\[LeftAngleBracket]nct[b,e]\[RightAngleBracket]+\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[a,e]\[RightAngleBracket]\[LeftAngleBracket]nct[b,c]\[RightAngleBracket]+
					\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]nct[a,b]\[RightAngleBracket]\[LeftAngleBracket]nct[d,e]\[RightAngleBracket]+\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]nct[a,d]\[RightAngleBracket]\[LeftAngleBracket]nct[b,e]\[RightAngleBracket]+\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]nct[a,e]\[RightAngleBracket]\[LeftAngleBracket]nct[b,d]\[RightAngleBracket]+
					\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]nct[a,c]\[RightAngleBracket]\[LeftAngleBracket]nct[d,e]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]nct[a,d]\[RightAngleBracket]\[LeftAngleBracket]nct[c,e]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]nct[a,e]\[RightAngleBracket]\[LeftAngleBracket]nct[c,d]\[RightAngleBracket]+
					\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]nct[b,c]\[RightAngleBracket]\[LeftAngleBracket]nct[d,e]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]nct[b,d]\[RightAngleBracket]\[LeftAngleBracket]nct[c,e]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]nct[b,e]\[RightAngleBracket]\[LeftAngleBracket]nct[c,d]\[RightAngleBracket]+
					\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,c]\[RightAngleBracket]+\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,d]\[RightAngleBracket]+\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,e]\[RightAngleBracket]+
					\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,c,d]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[a,c,e]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]nct[a,d,e]\[RightAngleBracket]+
					\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[b,c,d]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[b,c,e]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]nct[b,d,e]\[RightAngleBracket]+
					\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]nct[c,d,e]\[RightAngleBracket] ) +
				\[LeftAngleBracket]nct[d,e]\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,c]\[RightAngleBracket]+\[LeftAngleBracket]nct[c,e]\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,d]\[RightAngleBracket]+\[LeftAngleBracket]nct[c,d]\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,e]\[RightAngleBracket]+
				\[LeftAngleBracket]nct[b,e]\[RightAngleBracket]\[LeftAngleBracket]nct[a,c,d]\[RightAngleBracket]+\[LeftAngleBracket]nct[b,d]\[RightAngleBracket]\[LeftAngleBracket]nct[a,c,e]\[RightAngleBracket]+\[LeftAngleBracket]nct[b,c]\[RightAngleBracket]\[LeftAngleBracket]nct[a,d,e]\[RightAngleBracket]+
				\[LeftAngleBracket]nct[a,e]\[RightAngleBracket]\[LeftAngleBracket]nct[b,c,d]\[RightAngleBracket]+\[LeftAngleBracket]nct[a,d]\[RightAngleBracket]\[LeftAngleBracket]nct[b,c,e]\[RightAngleBracket]+\[LeftAngleBracket]nct[a,c]\[RightAngleBracket]\[LeftAngleBracket]nct[b,d,e]\[RightAngleBracket]+
				\[LeftAngleBracket]nct[a,b]\[RightAngleBracket]\[LeftAngleBracket]nct[c,d,e]\[RightAngleBracket]+
				\[LeftAngleBracket]e\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,c,d]\[RightAngleBracket]+\[LeftAngleBracket]d\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,c,e]\[RightAngleBracket]+\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]nct[a,b,d,e]\[RightAngleBracket]+\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]nct[a,c,d,e]\[RightAngleBracket]+\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]nct[b,c,d,e]\[RightAngleBracket]
			];
		nctexpr=ReplaceAll[nctexpr,
						nct[args___]:>zz075NonCommutativeTimes[args]]
	];

	If[order<=3 && exprorder>3,
		nctexpr=nctexpr/.
			{HoldPattern[zz075NonCommutativeTimes[args__]]:>nct[args],
			HoldPattern[Power[a_?(Not[QuantumScalarQ[#]]&),n_]]:>nct[Power[a,n]]
		};
		nctexpr=ReplaceAll[nctexpr, 
			HoldPattern[zz050Expected[nct[a_,b_,c_,d_]]]:>
				\[LeftAngleBracket]nct[a,b,c]\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]+\[LeftAngleBracket]nct[a,b,d]\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]+\[LeftAngleBracket]nct[a,c,d]\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]+\[LeftAngleBracket]nct[b,c,d]\[RightAngleBracket]\[LeftAngleBracket]a\[RightAngleBracket]+
				\[LeftAngleBracket]nct[a,b]\[RightAngleBracket]\[LeftAngleBracket]nct[c,d]\[RightAngleBracket]+\[LeftAngleBracket]nct[a,c]\[RightAngleBracket]\[LeftAngleBracket]nct[b,d]\[RightAngleBracket]+\[LeftAngleBracket]nct[a,d]\[RightAngleBracket]\[LeftAngleBracket]nct[b,c]\[RightAngleBracket]-
				2\[LeftAngleBracket]nct[a,b]\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]-2\[LeftAngleBracket]nct[a,c]\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]-
				2\[LeftAngleBracket]nct[b,c]\[RightAngleBracket]\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]-2\[LeftAngleBracket]nct[a,d]\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]-
				2\[LeftAngleBracket]nct[b,d]\[RightAngleBracket]\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]-2\[LeftAngleBracket]nct[c,d]\[RightAngleBracket]\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]+
				6\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]\[LeftAngleBracket]d\[RightAngleBracket]];
		nctexpr=ReplaceAll[nctexpr,
						nct[args___]:>zz075NonCommutativeTimes[args]]
	];

	If[order<=2 && exprorder>2,
		nctexpr=nctexpr/.
			{HoldPattern[zz075NonCommutativeTimes[args__]]:>nct[args],
			HoldPattern[Power[a_?(Not[QuantumScalarQ[#]]&),n_]]:>nct[Power[a,n]]
		};
		nctexpr=ReplaceAll[nctexpr, 
			HoldPattern[zz050Expected[nct[a_,b_,c_]]]:>
				\[LeftAngleBracket]nct[\[LeftAngleBracket]a\[RightAngleBracket],b,c]\[RightAngleBracket]+\[LeftAngleBracket]nct[a,\[LeftAngleBracket]b\[RightAngleBracket],c]\[RightAngleBracket]+\[LeftAngleBracket]nct[a,b,\[LeftAngleBracket]c\[RightAngleBracket]]\[RightAngleBracket]-2\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]\[LeftAngleBracket]c\[RightAngleBracket]];
		nctexpr=ReplaceAll[nctexpr,
						nct[args___]:>zz075NonCommutativeTimes[args]]
	];

	If[order==1 && exprorder>1,
		nctexpr=nctexpr/.
			{HoldPattern[zz075NonCommutativeTimes[args__]]:>nct[args],
			HoldPattern[Power[a_?(Not[QuantumScalarQ[#]]&),n_]]:>nct[Power[a,n]]
		};
		nctexpr=ReplaceAll[nctexpr, 
			HoldPattern[zz050Expected[nct[a_,b_]]]:>\[LeftAngleBracket]a\[RightAngleBracket]\[LeftAngleBracket]b\[RightAngleBracket]];
		nctexpr=ReplaceAll[nctexpr,
						nct[args___]:>zz075NonCommutativeTimes[args]]
	];
	Clear[nct];
	approximant[QHDSymmetrize[ nctexpr ]]
];


Options[QHDEOMRHS]=
	Join[{QHDHBar->\[HBar]},Options[QHDClosure]];

QHDVerifySubscripts[var_,expr_]:=
Module[{subsVarsInExpr,nonSubsVarsInVar,
		nonsubs,
		inter,a,b,foo},
	
	subsVarsInExpr=Union[Cases[expr,
						HoldPattern[Subscript[a_,b_]]:>a,
						Infinity]];
	
	nonsubs=Hold[{var}]/.HoldPattern[Subscript[_,_]]:>foo;
	
	nonSubsVarsInVar=Cases[Level[nonsubs,{-1}],Except[foo]];
	
	inter=Intersection[subsVarsInExpr,nonSubsVarsInVar];
	
	If[Length[inter]>0,
		Message[QHD::VerifySubscripts,inter]
	]
];


QHDEOMRHS[orderORlist_,HoldPattern[zz050Expected[var_]],
				hamilt_,opts:OptionsPattern[]]:=
	QHDEOMRHS[orderORlist,var,hamilt,opts];


QHDEOMRHS[orderORlist_,var_,hamilt_,opts:OptionsPattern[]]:=
Module[{expr,hbar,approximant,expr2},
	hbar=OptionValue[QHDHBar];
	approximant=OptionValue[QHDApproximantFunction];
	QHDVerifySubscripts[var,hamilt];

	expr=approximant[zz050Expected[zz050Commutator[var,hamilt]]/(I*hbar)];

	expr2=QHDClosure[orderORlist,expr,FilterRules[{opts},Options[QHDClosure]]];

	approximant[expr2]
];


QHDEOM[orderORlist_,expression_,hamilt_,opts:OptionsPattern[]]:=
Module[{qhdexpr,qhdexpr2,v,vars,hier,infmsg,infmsg1,infmsg2},
	qhdexpr=QHDSymmetrize[QHDExpand[\[LeftAngleBracket]expression\[RightAngleBracket]]];
	qhdexpr2=QHDClosure[orderORlist,qhdexpr,
				FilterRules[{opts},Options[QHDClosure]]];
	If[qhdexpr=!=qhdexpr2,
		Message[QHDEOM::closure,
				TraditionalForm[qhdexpr],TraditionalForm[qhdexpr2]]];
	vars=QHDVariables[qhdexpr2]/.HoldPattern[zz050Expected[v_]]:>v;
	hier=Map[{zz050Expected[#],
		 QHDEOMRHS[orderORlist,#,hamilt,
							FilterRules[{opts},Options[QHDEOMRHS]]]}&, vars];
	If[OptionValue[QHDLabel]===None, Return[hier]];

	If[OptionValue[QHDLabel]===Automatic,
		infmsg1=If[(OptionValue[QHDApproximantFunction]===Identity)||
					((OptionValue[QHDApproximantFunction]===
							QHDCrossTermsApproximant)&&
					FreeQ[vars,Subscript] ),
				".",
				"QHDApproximantFunction->"<>
						ToString[OptionValue[QHDApproximantFunction]]<>
						". "];
		infmsg2=If[orderORlist===\[Infinity],
					"No closure was applied",
					If[TrueQ[IntegerQ[orderORlist]],
						"Closure procedure was applied to order "<>
							ToString[orderORlist],
						Row[{"Dynamical variables: ",
							\[LeftAngleBracket]orderORlist\[RightAngleBracket] /. 
								HoldPattern[zz075NonCommutativeTimes[args__]]:>
									zz075Symmetric[args]}]
					]		
				];
		infmsg=If[infmsg1===".",infmsg2,Column[{infmsg2,infmsg1}]],
		(*else (not Automatic)*)
		infmsg=OptionValue[QHDLabel]
	];
	Join[{{"QHDLabel",infmsg}},hier]
];


QHDHierarchy::closure="The closure procedure modified `1` into `2`."<>
	" The last expression will be used to build the hierarchy";


QHDHierarchy[orderORlist_,expression_,hamilt_,opts:OptionsPattern[]]:=
Module[{vars,eqs,varnum,roc,rocvars,newvars,dynvars,
		qhdexpr,qhdexpr2,inivars,v,hier,infmsg,infmsg1,infmsg2},
	qhdexpr=QHDSymmetrize[QHDExpand[\[LeftAngleBracket]expression\[RightAngleBracket]]];
	qhdexpr2=QHDClosure[orderORlist,qhdexpr,
				FilterRules[{opts},Options[QHDClosure]]];
	If[qhdexpr=!=qhdexpr2,
		Message[QHDHierarchy::closure,
				TraditionalForm[qhdexpr],TraditionalForm[qhdexpr2]]];
	
	inivars=QHDVariables[qhdexpr2]/.HoldPattern[zz050Expected[v_]]:>v;
	If[orderORlist===\[Infinity],
		inivars=Select[inivars,QHDOrder[#]<=OptionValue[QHDMaxOrder]&]];
	
	For[vars=inivars;
		eqs={};varnum=1,
		varnum<=Length[vars], varnum++,
			roc=QHDEOMRHS[orderORlist,vars[[varnum]],hamilt,
							FilterRules[{opts},Options[QHDEOMRHS]]];
			AppendTo[eqs,roc];
			rocvars=QHDVariables[roc]/.HoldPattern[zz050Expected[v_]]:>v;
			If[orderORlist===\[Infinity],
				rocvars=Select[rocvars,QHDOrder[#]<=OptionValue[QHDMaxOrder]&]];
			newvars=Select[rocvars,Not[MemberQ[vars,#]]&];
			vars=Join[vars,newvars]
	];
	dynvars=Map[zz050Expected,vars];
	hier=Transpose[{dynvars,eqs}];

	If[OptionValue[QHDLabel]===None, Return[hier]];

	If[OptionValue[QHDLabel]===Automatic,
		infmsg1=If[(OptionValue[QHDApproximantFunction]===Identity)||
					((OptionValue[QHDApproximantFunction]===
							QHDCrossTermsApproximant)&&
					FreeQ[vars,Subscript] ),
				".",
				"QHDApproximantFunction->"<>
						ToString[OptionValue[QHDApproximantFunction]]<>
						". "];
		infmsg2=If[orderORlist===\[Infinity],
					If[QHDOrder[hier]<OptionValue[QHDMaxOrder],
						"Calculations stopped without the need of closure.",
						"Calculations were stopped at order QHDMaxOrder\[Rule]"<>
							ToString[OptionValue[QHDMaxOrder]]
					],
					If[TrueQ[IntegerQ[orderORlist]],
						"Closure procedure was applied to order "<>
							ToString[orderORlist],
						Row[{"Dynamical variables: ",
							\[LeftAngleBracket]orderORlist\[RightAngleBracket] /. 
								HoldPattern[zz075NonCommutativeTimes[args__]]:>
									zz075Symmetric[args]}]
					]		
				];
		infmsg=If[infmsg1===".",infmsg2,Column[{infmsg2,infmsg1}]],
		(*else (not Automatic)*)
		infmsg=OptionValue[QHDLabel]
	];
	Join[{{"QHDLabel",infmsg}},hier]
];


QHDDifferentialEquations[qhdhierarchy_,opts:OptionsPattern[]]:=
Module[{left,right,time,eqs,hierarchy},
	time=OptionValue[QHDSymbolForTime];
	hierarchy=DeleteCases[qhdhierarchy,{"QHDLabel",_}];
	eqs=Map[Function[{pair},
			D[pair[[1]][time],time]==
			(pair[[2]]/.
				HoldPattern[zz050Expected[arg_]]:>
					zz050Expected[arg][time])
			],
		hierarchy];
	eqs
];


QHDInitialConditionsTemplate[qhdhierarchy_,initialtime_:\[SelectionPlaceholder]]:=
Module[{eqs,hierarchy},
	hierarchy=DeleteCases[qhdhierarchy,{"QHDLabel",_}];
	eqs=Map[Function[{pair},pair[[1]][initialtime]==\[SelectionPlaceholder]], hierarchy];
	eqs
];


QHDConnectivity[hierarchy_List]:=
Module[{listaexpected},
	With[{expected=
			Function[{expr},
				Union[Cases[{expr},HoldPattern[zz050Expected[v_]],Infinity]]],
			makerules=Function[{record},Map[(#->record[[1]])&,record[[2]] ]] },
		listaexpected=Map[{#[[1]],expected[#[[2]]]}&,
						DeleteCases[hierarchy,{"QHDLabel",_}]];
		Flatten[Map[makerules,listaexpected]]
]  ];


QHDEmphasizeClosure[expr_,hierarchy_List,
	closedfunct_,needfunct_]:=
Module[{vars},
	vars=Part[Transpose[DeleteCases[hierarchy,{"QHDLabel",_}]],1];
	expr /. HoldPattern[zz050Expected[v_]]:>
		If[TrueQ[MemberQ[vars,zz050Expected[v]]],
			closedfunct[zz050Expected[v]],
			needfunct[zz050Expected[v]]
		]
];


QHDGraphPlot[hierarchy_List, opts:OptionsPattern[]]:=
Module[{connect,mssg,lbl,dispfunction,closurefunction,rep},
	dispfunction=
		If[OptionValue[QHDClosureStyle]==={},
			Identity,
			Function[Style[#,rep]]/.rep->OptionValue[QHDClosureStyle]
		];
	closurefunction=
		If[OptionValue[QHDNeedClosureStyle]==={},
			Identity,
			Function[Style[#,rep]]/.rep->OptionValue[QHDNeedClosureStyle]
		];
	connect=QHDConnectivity[hierarchy];
	connect=QHDEmphasizeClosure[connect,hierarchy,
		dispfunction,closurefunction];
	GraphPlot[
		connect,
		FilterRules[{opts,
				If[OptionValue[QHDLabel]===Automatic,
					lbl=Cases[hierarchy,{"QHDLabel",mssg_}:>mssg];
					If[lbl==={},
						PlotLabel->None,
						PlotLabel->Part[lbl,1]
					],
					If[OptionValue[QHDLabel]===None,
						QHDLabel->OptionValue[QHDLabel],
						PlotLabel->OptionValue[QHDLabel]
					]
				],
				Sequence@@Options[QHDGraphPlot]},
			Options[GraphPlot]]]
];


QHDForm[qhdhierarchy_List,opts:OptionsPattern[]]:=
Module[{eqslist,left,right,time,ti,hierarchy,rep,dispfunction,closurefunction},
	dispfunction=
		If[OptionValue[QHDClosureStyle]==={},
			Identity,
			Function[Style[#,rep]]/.rep->OptionValue[QHDClosureStyle]
		];
	closurefunction=
		If[OptionValue[QHDNeedClosureStyle]==={},
			Identity,
			Function[Style[#,rep]]/.rep->OptionValue[QHDNeedClosureStyle]
		];
(* Notice the use of = instead of ==, so that it does Not work if the 
user tries to copy-paste, otherwise it will work to a wrong equation,
because the TraditionalForm-derivative is interpreted as a division *)
	If[OptionValue[QHDLabel]===None,
		hierarchy=DeleteCases[qhdhierarchy,{"QHDLabel",_}],
		If[OptionValue[QHDLabel]===Automatic,
			hierarchy=qhdhierarchy,
			hierarchy=Join[{{"QHDLabel",OptionValue[QHDLabel]}},
						DeleteCases[qhdhierarchy,{"QHDLabel",_}]]
		]];
	eqslist=Map[Function[{pair},
			If[pair[[1]]==="QHDLabel",
				TraditionalForm[ pair[[2]] ],
				TraditionalForm[
					HoldForm[zz050D[left,time]=right]]/.
					{time->OptionValue[QHDSymbolForTime],
					left->QHDEmphasizeClosure[pair[[1]],
								hierarchy,
								dispfunction,
								closurefunction],
					right->QHDEmphasizeClosure[pair[[2]],
								hierarchy,
								dispfunction,
								closurefunction]}
			] ],
		hierarchy];
	
	Column[eqslist,
		FilterRules[{opts,Sequence@@Options[QHDForm]},Options[Column]]]
];


QHDNDSolve[qhdhierarchy_,initialcond_,initialtime_,finaltime_,
			opts:OptionsPattern[]]:=
	With[{time=OptionValue[QHDSymbolForTime],
			eqs=Join[QHDDifferentialEquations[qhdhierarchy,
						FilterRules[{opts},Options[QHDDifferentialEquations]]],
					initialcond],
			vars=Part[Transpose[DeleteCases[qhdhierarchy,{"QHDLabel",_}]],1] },
		NDSolve[eqs,vars,{time,initialtime,finaltime},
				FilterRules[{opts,Sequence@@Options[QHDNDSolve]},Options[NDSolve]] ]
];


QHDFunction[expression_,sol:{{HoldPattern[Rule[__]]..}..},opts:OptionsPattern[]]:=
Module[{time, qhdexpr, qhdexprtime, plotexpr, v,args,domain,lbls,myfun},
		time=OptionValue[QHDSymbolForTime];
		QHDVerifySubscripts[expression,sol];
		qhdexpr=\[LeftAngleBracket]expression\[RightAngleBracket] /. 
			HoldPattern[zz075NonCommutativeTimes[args__]]:>
				zz075Symmetric[args];
		qhdexprtime = 
			qhdexpr /. HoldPattern[zz050Expected[v_]]:>zz050Expected[v][time];
		plotexpr= qhdexprtime /. sol[[1]];
		domain=Join[{time},
					Flatten@InterpolatingFunctionDomain[ sol[[1,1,2]] ] ];
		myfun[{time},plotexpr] /. myfun->Function
];


QHDPlot[All,sol:{{HoldPattern[Rule[__]]..}..},opts:OptionsPattern[]]:=
GraphicsGrid[Transpose[Map[QHDPlot[#[[1]],sol,opts]&,
	sol,{2}]],
	Evaluate[FilterRules[{opts, ImageSize->Full, Dividers->All},
					Options[GraphicsGrid]]]];


QHDPlot[expression_,sol:{{HoldPattern[Rule[__]]..}..},opts:OptionsPattern[]]:=
Module[{time, qhdexpr, qhdexprtime, plotexpr, v,args,domain,lbls},
		time=OptionValue[QHDSymbolForTime];
		QHDVerifySubscripts[expression,sol];
		qhdexpr=\[LeftAngleBracket]expression\[RightAngleBracket] /. 
			HoldPattern[zz075NonCommutativeTimes[args__]]:>
				zz075Symmetric[args];
		qhdexprtime = 
			qhdexpr /. HoldPattern[zz050Expected[v_]]:>zz050Expected[v][time];
		plotexpr= qhdexprtime /. sol;
		domain=Join[{time},
					Flatten@InterpolatingFunctionDomain[ sol[[1,1,2]] ] ];
		If[OptionValue[FrameLabel]===Automatic,
			lbls=(FrameLabel->{time,qhdexpr}),
			lbls=(FrameLabel->OptionValue[FrameLabel])
		];
		Plot[Evaluate[plotexpr],Evaluate[domain],
			Evaluate[
				FilterRules[{lbls,opts,Sequence@@Options[QHDPlot]},Options[Plot]]]]
];


QHDParametricPlot[expression_,sol:{{HoldPattern[Rule[__]]..}..},
				opts:OptionsPattern[]]:=
Module[{time, qhdexpr, qhdexprtime, plotexpr, v,args,domain,lbls},
		time=OptionValue[QHDSymbolForTime];
		QHDVerifySubscripts[expression,sol];
		qhdexpr=\[LeftAngleBracket]expression\[RightAngleBracket] /. 
			HoldPattern[zz075NonCommutativeTimes[args__]]:>
				zz075Symmetric[args];
		qhdexprtime = 
			qhdexpr /. HoldPattern[zz050Expected[v_]]:>zz050Expected[v][time];
		plotexpr= qhdexprtime /. sol;
		domain=Join[{time},
					Flatten@InterpolatingFunctionDomain[ sol[[1,1,2]] ] ];
		If[OptionValue[FrameLabel]===Automatic,
			lbls=(FrameLabel->qhdexpr),
			lbls=(FrameLabel->OptionValue[FrameLabel])
		];
		ParametricPlot[Evaluate[plotexpr],Evaluate[domain],
			Evaluate[FilterRules[{lbls,opts,Sequence@@Options[QHDParametricPlot]},
							Options[ParametricPlot]]]]
];


QHDParametricPlot3D[expression_,sol:{{HoldPattern[Rule[__]]..}..},
				opts:OptionsPattern[]]:=
Module[{time, qhdexpr, qhdexprtime, plotexpr, v,args,domain,lbls},
		time=OptionValue[QHDSymbolForTime];
		QHDVerifySubscripts[expression,sol];
		qhdexpr=\[LeftAngleBracket]expression\[RightAngleBracket] /. 
			HoldPattern[zz075NonCommutativeTimes[args__]]:>
				zz075Symmetric[args];
		qhdexprtime = 
			qhdexpr /. HoldPattern[zz050Expected[v_]]:>zz050Expected[v][time];
		plotexpr= qhdexprtime /. sol;
		domain=Join[{time},
					Flatten@InterpolatingFunctionDomain[ sol[[1,1,2]] ] ];
		If[OptionValue[AxesLabel]===Automatic,
			lbls=(AxesLabel->qhdexpr),
			lbls=(AxesLabel->OptionValue[FrameLabel])
		];
		ParametricPlot3D[Evaluate[plotexpr],Evaluate[domain],
			Evaluate[FilterRules[{lbls,opts,Sequence@@Options[QHDParametricPlot3D]},
							Options[ParametricPlot3D]]]]
];


SetAttributes[
{ SetQHDAliases, AngleBracket, zz050Expected,
  QHDSymmetrize, QHDExpand, 
  QHDHBar,
  QHDApproximantFunction, QHDMaxOrder,
  QHDCrossTermsApproximant,
  QHDOrder, QHDEOM,
  QHDClosure,  
  QHDHierarchy, QHDVariables,
  QHDForm, 
  QHDDifferentialEquations,
  QHDInitialConditionsTemplate,
  QHDNDSolve,
  QHDConnectivity,QHDGraphPlot,
  QHDFunction,
  QHDPlot,QHDParametricPlot,QHDParametricPlot3D,
  QHDSymbolForTime,
  QHDClosureStyle,
  QHDNeedClosureStyle,
  zz050D,
  zz075Symmetric, QHD, QHDLabel,
  \[SelectionPlaceholder], \[ScriptT], \[HBar], \[ScriptS]
 },
{Protected,ReadProtected}
];

If[TrueQ[$VersionNumber>=6.0],
 NotebookOpen["QHDPalette.nb"]
];


End[];

EndPackage[];
Column[{    
"Quantum`QHD` \n"<>
"A Mathematica package for Quantized Hamilton Dynamics"<>
" approximation to Heisenberg Equations of Motion\n"<> 
"by Jos\[EAcute] Luis G\[OAcute]mez-Mu\[NTilde]oz\n"<>       
"based on the original idea of Kirill Igumenshchev\n"<>  
"\nThis add-on does NOT work properly with the debugger turned on."<>
" Therefore the debugger must NOT be checked in the Evaluation menu of"<>
" Mathematica.\n"<>     
"\nExecute SetQHDAliases[] in order"<>
" to use the keyboard to enter QHD objects\n"<>
"SetQHDAliases[] must be executed again in"<>
" each new notebook that is created",
"\nMATHEMATICA "<>$Version,
"\nQUANTUM version EN CONSTRUCCION",
"\nTODAY IS "<>DateString[]
}]

