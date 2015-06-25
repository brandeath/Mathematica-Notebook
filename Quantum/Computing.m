(* ::Package:: *)

(* Quantum`Computing`

   Copyright 2007-2010
   Jose Luis Gomez-Munoz and Francisco Delgado-Cepeda
   ITESM-CEM,   
   Departamento de Ciencias Basicas (Matematicas)
   Carretera Lago de Guadalupe Km. 3.5, 
   Atizapan de Zaragoza, Estado de Mexico, 
   C.P. 52926
   Mexico
   jose.luis.gomez@itesm.mx
   http://homepage.cem.itesm.mx/lgomez/quantum
   
   A package for Quantum Computing in Mathematica.
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

BeginPackage["Quantum`Computing`",{"Quantum`Notation`"}] 

Unprotect[
	zz050Bell,zz050Superpos,\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR],
	QuantumEvaluate, PauliExpand,
	QuantumMatrix,QuantumMatrixForm,QuantumPlot,
	QuantumEigensystem, QuantumEigensystemForm,
			MatrixQuantum, TensorQuantum,
            QubitLabels,QubitList,PauliIdentities,
			QuantumGatePowers,QuantumGateShifting,
			QuantumBackground,
			QuantumMeterStyle,
			QuantumWireStyle,
			QuantumNotStyle,
			QuantumControlStyle,
			QuantumSwapStyle,
			QuantumConnectionStyle,
			QuantumGateStyle,
			QuantumTextStyle,
			QuantumVerticalTextStyle,
			QuantumPlot3D,
	QuantumSparseArray,QuantumTable,QuantumTableForm,QuantumTensor,
	QuantumTensorForm,SetQuantumGate,SetComputingAliases,
	\[Sigma],\[ScriptCapitalB],\[CapitalPsi],\[CapitalPhi],
	QubitMeasurement,
	QuantumMeter,
	zz020CeroOneQ,zz020Controlled,zz020MultiQubit,
	zz020TensorPower,zz020TwoScalarsListQ,
	DecToQubit,QubitToDec,
	\[ScriptK]\[ScriptE]\[ScriptT],
	\[ScriptCapitalI],\[ScriptCapitalX],\[ScriptCapitalY],\[ScriptCapitalZ],\[ScriptZero],
	\[ScriptZero]\[ScriptZero],\[ScriptZero]\[ScriptOne],\[ScriptOne]\[ScriptZero],\[ScriptOne]\[ScriptOne],
	\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT],
	\[ScriptCapitalP],\[ScriptCapitalH],\[ScriptCapitalS],\[ScriptCapitalT],\[ScriptCapitalC],
	\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT],
	\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP],
	\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI],
	\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN]
];

ClearAll[
	zz050Bell,zz050Superpos,\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR],
	QuantumEvaluate,PauliExpand,
	QuantumMatrix,QuantumMatrixForm,QuantumPlot,
	QuantumEigensystem, QuantumEigensystemForm,
			MatrixQuantum, TensorQuantum,
            QubitLabels,QubitList,PauliIdentities,
			QuantumGatePowers,QuantumGateShifting,
			QuantumBackground,
			QuantumMeterStyle,
			QuantumWireStyle,
			QuantumNotStyle,
			QuantumControlStyle,
			QuantumSwapStyle,
			QuantumConnectionStyle,
			QuantumGateStyle,
			QuantumTextStyle,
			QuantumVerticalTextStyle,
			QuantumPlot3D,
	QuantumSparseArray,QuantumTable,QuantumTableForm,QuantumTensor,
	QuantumTensorForm,SetQuantumGate,SetComputingAliases,
	\[Sigma],\[ScriptCapitalB],\[CapitalPsi],\[CapitalPhi],
	QubitMeasurement,
	QuantumMeter,
	zz020CeroOneQ,zz020Controlled,zz020MultiQubit,
	zz020TensorPower,zz020TwoScalarsListQ,
	DecToQubit, QubitToDec,
	\[ScriptK]\[ScriptE]\[ScriptT],
	\[ScriptCapitalI],\[ScriptCapitalX],\[ScriptCapitalY],\[ScriptCapitalZ],\[ScriptZero],
	\[ScriptZero]\[ScriptZero],\[ScriptZero]\[ScriptOne],\[ScriptOne]\[ScriptZero],\[ScriptOne]\[ScriptOne],
	\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT],
	\[ScriptCapitalP],\[ScriptCapitalH],\[ScriptCapitalS],\[ScriptCapitalT],\[ScriptCapitalC],
	\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT],
	\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP],
	\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI],
	\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN]
	
];

SetComputingAliases::usage=
					"SetComputingAliases[] sets keyboard aliases in the"<>
					" selected notebook. SetComputingAliases[notebook]"<>
					" sets keyboard aliases in the specified notebook."<>
					" The aliases are keyboard key-combinations"<>
					" for the input of quantum objects in Dirac Notation.";
SetComputingAliases::aliases=
    "ALIASES:\n"<>
	"[ESC]on[ESC]        Quantum concatenation symbol"<>
	" (operator application, inner product and outer product)\n"<>
	"[ESC]qket0[ESC]     Ket of qubit 0 template\n"<>
	"[ESC]qbra0[ESC]     Bra of qubit 0 template\n"<>
	"[ESC]qket1[ESC]     Ket of qubit 1 template\n"<>
	"[ESC]qbra1[ESC]     Bra of qubit 1 template\n"<>
	"[ESC]qket[ESC]      Ket of qubit template\n"<>
	"[ESC]qqket[ESC]     Ket of two qubits template\n"<>
	"[ESC]qqqket[ESC]    Ket of three qubits template\n"<>
	"[ESC]qbra[ESC]      Bra of qubit template\n"<>
	"[ESC]qqbra[ESC]     Bra of two qubits template\n"<>
	"[ESC]qqqbra[ESC]    Bra of three qubits template\n"<>
	"[ESC]toqb[ESC]      Base-10 Integer to binary qubit template\n"<>
	"[ESC]ket[ESC]       Ket template\n"<>
	"[ESC]bra[ESC]       Bra template\n"<>
	"[ESC]qb[ESC]        Qubit template\n"<>
	"[ESC]qv[ESC]        Qubit-value template\n"<>
	"[ESC]qketbra[ESC]   Element of a one-qubit operator template\n"<>
	"[ESC]qqketbra[ESC]  Element of a two-qubits operator template\n"<>
	"[ESC]qqqketbra[ESC] Element of a three-qubits operator template\n"<>
	"[ESC]k+[ESC]        Plus ket (eigenstate of the first Pauli matrix)\n"<>
	"[ESC]b+[ESC]        Plus bra\n"<>
	"[ESC]k-[ESC]        Minus ket (eigenstate of the first Pauli matrix)\n"<>
	"[ESC]b-[ESC]        Minus bra\n"<>
	"[ESC]k00[ESC]       Ket of Bell State 00\n"<>
	"[ESC]k01[ESC]       Ket of Bell State 01\n"<>
	"[ESC]k10[ESC]       Ket of Bell State 10\n"<>
	"[ESC]k11[ESC]       Ket of Bell State 11\n"<>	
	"[ESC]b00[ESC]       Bra of Bell State 00\n"<>
	"[ESC]b01[ESC]       Bra of Bell State 01\n"<>
	"[ESC]b10[ESC]       Bra of Bell State 10\n"<>
	"[ESC]b11[ESC]       Bra of Bell State 11\n"<>	
	"[ESC]kphi+[ESC]     Ket of Bell State phi+\n"<>
	"[ESC]kpsi+[ESC]     Ket of Bell State psi+\n"<>
	"[ESC]kphi-[ESC]     Ket of Bell State phi-\n"<>
	"[ESC]kpsi-[ESC]     Ket of Bell State psi-\n"<>	
	"[ESC]bphi+[ESC]     Bra of Bell State phi+\n"<>
	"[ESC]bpsi+[ESC]     Bra of Bell State psi+\n"<>
	"[ESC]bphi-[ESC]     Bra of Bell State phi-\n"<>
	"[ESC]bpsi-[ESC]     Bra of Bell State psi-\n"<>	
	"[ESC]her[ESC]       Hermitian conjugate template\n"<>
	"[ESC]con[ESC]       Complex conjugate template\n"<>
	"[ESC]norm[ESC]      Quantum norm template\n"<>
	"[ESC]trace[ESC]     Partial trace template\n"<>
	"[ESC]tp[ESC]        Tensor-product symbol\n"<>	
	"[ESC]tprod[ESC]     Tensor-product template\n"<>	
	"[ESC]tprodqb[ESC]   Tensor-product of Qubit template\n"<>	
	"[ESC]tpow[ESC]      Tensor-power template\n"<>
	"[ESC]tpowqb[ESC]    Tensor-power of Qubit template\n"<>
	"[ESC]s0[ESC]        0th-Pauli operator (Identity) template\n"<>
	"[ESC]s1[ESC]        1st-Pauli operator (X) template\n"<>
	"[ESC]s2[ESC]        2nd-Pauli operator (Y) template\n"<>
	"[ESC]s3[ESC]        3rd-Pauli operator (Z) template\n"<>
	"[ESC]so[ESC]        0th-Pauli operator (Identity) template\n"<>
	"[ESC]sx[ESC]        1st-Pauli operator (X) template\n"<>
	"[ESC]sy[ESC]        2nd-Pauli operator (Y) template\n"<>
	"[ESC]sz[ESC]        3rd-Pauli operator (Z) template\n"<>
	"[ESC]sp[ESC]        General Pauli operator template\n"<>
	"[ESC]ig[ESC]        Identity gate template\n"<>
	"[ESC]xg[ESC]        Pauli-X gate\n"<>	
	"[ESC]yg[ESC]        Pauli-Y gate\n"<>	
	"[ESC]zg[ESC]        Pauli-Z gate\n"<>	
	"[ESC]hg[ESC]        Haddamard gate\n"<>	
	"[ESC]pg[ESC]        Parametric phase gate\n"<>	
	"[ESC]sg[ESC]        S Phase gate\n"<>	
	"[ESC]tg[ESC]        T \[Pi]/8 gate \n"<>	
	"[ESC]swap[ESC]      Swap gate\n"<>	
	"[ESC]cgate[ESC]     Controlled-Gate template\n"<>
	"[ESC]ccgate[ESC]    Controlled-controlled-Gate template\n"<>
	"[ESC]cccgate[ESC]   Controlled-controlled-controlled-Gate template\n"<>
	"[ESC]cnot[ESC]      Controlled-Not template\n"<>
	"[ESC]ccnot[ESC]     Controlled-controlled-Not template\n"<>
	"[ESC]cccnot[ESC]    Controlled-controlled-controlled-Not template\n"<>
	"[ESC]toff[ESC]      Toffoli gate\n"<>	
	"[ESC]fred[ESC]      Fredkin gate\n"<>	
	"[ESC]qg[ESC]        Quantum gate of one argument \n"<>
	"[ESC]qqg[ESC]       Quantum gate of one argument applied to two qubits\n"<>
	"[ESC]qqqg[ESC]      Quantum gate of one argument applied to three qubits\n"<>
	"[ESC]qgg[ESC]       Quantum gate of two arguments \n"<>
	"[ESC]qggg[ESC]      Quantum gate of three arguments \n"<>
	"[ESC]pqg[ESC]       Parametric quantum gate of one argument \n"<>
	"[ESC]qr[ESC]        Quantum register template \n"<>
	"[ESC]qrg[ESC]       Quantum-register gate template \n"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
zz020TensorPower::usage=
	"Execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]tpow[ESC]      Tensor-power template\n"<>
	"SetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
zz020TP::usage=
	"Internal Quantum object";
zz020TPini::usage=
	"Internal Quantum object";
zz020TPend::usage=
	"Internal Quantum object";
zz020TPdat::usage=
	"Internal Quantum object";
zz020MultiQubit::usage=
	"Execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]tpow[ESC]      Tensor-power template\n"<>
	"SetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
zz020CeroOneQ::usage=
	"Internal Quantum object";
zz020TwoScalarsListQ::usage=
	"Internal Quantum object";
QubitMeasurement::usage=
	"QubitMeasurement[ket,{q1,q2,q3...}] represents the measurement\n"<>
	" of qubits q1,q2,... in ket. It can be used inside QuantumPlot and QuantumEvaluate";
QuantumMeter::usage=
	"QuantumMeter is used to plot measurement devices in"<>
	" quantum computing circuits";
zz020Controlled::usage=
	"Execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]cgate[ESC]     Controlled-Gate template\n"<>
	"SetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
zz020Controlled::rptdqb="A qubit appears both as a controll qubit and as a controlled qubit";
QubitToDec::usage="QubitToDec[ket] transforms a ket of qubits"<>
	" to a decimal integer";
DecToQubit::usage=
	"Execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]toqb[ESC]     Base-10 Integer to binary qubit template\n"<>
	"SetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
zz050Superpos::usage=
	"Execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]k+[ESC]        Plus ket\n"<>
	"[ESC]k-[ESC]        Minus ket\n"<>
	"SetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
zz050Bell::usage=
	"Execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]k00[ESC]       Bell State ket 00\n"<>
	"[ESC]k01[ESC]       Bell State ket 01\n"<>
	"[ESC]k10[ESC]       Bell State ket 10\n"<>
	"[ESC]k11[ESC]       Bell State ket 11\n"<>	
	"SetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR]::usage=
	"\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] is used to input registes of qubits";
\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR]::syntx=
	"Wrong \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] syntax. Please write \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imin,imax,di] or"<>
	" \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imin,imax,di,s], where imin, imax, di are integers"<>
	" and the optional label s is a symbol or integer";
QuantumEvaluate::usage=
	"QuantumEvaluate[expr] gives Dirac Kets and Bras for expr.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
PauliExpand::usage=
	"PauliExpand[expr] expands expr into Pauli operators for each qubit.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
PauliExpand::nonunit=
	"PauliExpand can only expand unitary operators\n"<>
	"The expression `1` is or might be nonunitary for some complex values"<>
	" of its parameters.";
QuantumPlot::usage=
	"QuantumPlot[expr] plots the quantum circuit for expr.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumSparseArray::usage=
	"QuantumSparseArray[expr] gives the SparseArray for expr.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumTensor::usage=
	"QuantumTensor[expr] gives the Tensor for expr in List format.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumTensorForm::usage=
	"QuantumTensorForm[expr] gives the Tensor for expr in MatrixForm format.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumMatrix::usage=
	"QuantumMatrix[expr] gives the Matrix for expr in List format.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumEigensystem::usage=
	"QuantumEigensystem[expr] gives the eigenvalues and eigenvectors"<>
	" for expr in the format {eigenvalues,eigenvectos}.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumEigensystemForm::usage=
	"QuantumEigensystemForm[expr] gives the eigenvalues and eigenvectors"<>
	" for expr in a TableForm format.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
MatrixQuantum::usage=
	"MatrixQuantum[mat] gives the Dirac bra-ket expr for matrix mat";
TensorQuantum::usage=
	"TensorQuantum[ten] gives the Dirac bra-ket expr for tensor ten";
QuantumMatrixForm::usage=
	"QuantumMatrixForm[expr] gives the Matrix for expr in MatrixForm format.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumTable::usage=
	"QuantumTable[expr] gives the Truth-Table for expr in List format.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
QuantumTableForm::usage=
	"QuantumTableForm[expr] gives the Truth-Table for expr in TableForm format.\n"<>
	"Notice that expr is made of quantum gates connected"<>
	" by the quantum product \[CenterDot]\n In order to enter"<>
	" the quantum product \[CenterDot]"<>
	" execute SetComputingAliases[]. Then press:\n"<>
	"[ESC]on[ESC]        Quantum product template"<>
	"\nSetComputingAliases[] must be executed again in"<>
	" each new notebook that is created, only one time per notebook.";
SetQuantumGate::usage=
	"After evaluating SetQuantumGate[symbol,narg]"<>
	" symbol will be"<>
	" treated as quantum gate of narg arguments (qubits)"<>
	" by QuantumEvaluate[] and other functions\n"<>
	"SetQuantumGate[symbol,narg,Function[{q1,q2...},diracexpr]]"<>
	" replaces symbol with diracexpr (evaluated in q1,q2...) when"<>
	" symbol is part of the argument of QuantumEvaluate.\n"<>
	"SetQuantumGate[symbol,{n1,n2}] and"<>
	" SetQuantumGate[symbol,{n1,n2},Function[{q1,q2...},diracexpr]]"<>
	" define symbol as a quantum gate with a number of arguments n1<=narg<=n2";
SetQuantumGate::nonu=
	"The gate `1` produced an operator that might be nonunitary"<>
	" for some complex values of its parameters. Quantum gates MUST be unitary\n `2` ";
SetQuantumGate::notation1=
	"The gate `1` is not a gate of exactly one qubit.\n"<>
	"Notations that use or generate curly brackets in the subindex \!\(\*SubscriptBox[\"g\", 
RowBox[{\"{\", \"}\"}]]\),\!\(\*SubscriptBox[\"g\", 
RowBox[{\"{\", 
RowBox[{\"1\", \",\", \"2\"}], \"}\"}]]\),\!\(\*SubscriptBox[\"g\", \"integer\"]\), etc."<>
	" can only be used with gates of exactly one qubit";
SetQuantumGate::nqb=
	"Quantum gate `1` was called with `4` argument(s):`5`."<>
	" It must have a number na of arguments such that `2`<=na<=`3`";
SetQuantumGate::args="SetQuantumGate must have two or three arguments";
SetQuantumGate::gateonearg="Quantum gate `1` must have as argument"<>
	" a qubit \!\(\*OverscriptBox[\"q\", \"^\"]\) or a list of qubits"<>
	" DELIMITED with curly"<>
	" brackets {\!\(\*OverscriptBox[\"q1\", \"^\"]\),\!\(\*OverscriptBox[\"q2\", \"^\"]\),...}";
SetQuantumGate::gatetwoarg="Quantum gate `1` must have as argument"<>
	" two qubits \!\(\*OverscriptBox[\"q1\", \"^\"]\),\!\(\*OverscriptBox[
RowBox[{\" \", \"q2\"}], \"^\"]\)";
SetQuantumGate::error="Internal Quantum Computing Error";
QubitLabels::usage=
	"QuantumPlot[expr,QubitLabels->False] plots the quantum circuit of"<>
	" expr without the labels for the qubits. ";
QubitList::usage=
	"QuantumPlot[expr,QubitList->{q1,q2,q3,...}] plots the quantum circuit of"<>
	" expr with qubits in the order specified by {q1,q2,q3...}. ";
PauliIdentities::usage=
	"PauliIdentities is an option of PauliExpand to generate explicit \!\(\*SubscriptBox[\"\[Sigma]\", \"0\"]\) operators";
QuantumGatePowers::usage=
	"QuantumPlot[expr,QuantumGatePowers->False] plots the quantum circuit of"<>
	" expr with integer powers of gates represented as repeated gates";
QuantumGateShifting::usage=
	"QuantumPlot[expr,QuantumGateShifting->False] plots the quantum circuit of"<>
	" expr without shifting (moving) the gates to the left";


Options[QuantumPlot] = 
	Join[{  QubitList->{}, 
			QubitLabels->True,
			QuantumGatePowers->True,
			QuantumGateShifting->True,
			QuantumBackground->Lighter[LightGray],
			QuantumMeterStyle->Directive[Darker[Darker[Blue]],Thickness[0.003],
								Arrowheads[Small]],
			QuantumWireStyle->Directive[Darker[Darker[Blue]],Thickness[0.006]],
			QuantumConnectionStyle->Directive[Darker[Darker[Red]],Thickness[0.004]],
			QuantumNotStyle->Directive[Darker[Green],Thickness[0.004]],
			QuantumControlStyle->Directive[Green,
										EdgeForm[{Darker[Darker[Red]],Thickness[0.006]}]],
			QuantumSwapStyle->Directive[Darker[Magenta],Thickness[0.006]],
			QuantumGateStyle->Directive[LightYellow,
										EdgeForm[{Thickness[0.006],Darker[Darker[Red]]}]],
			QuantumTextStyle->Directive[Small,Darker[Darker[Blue]],
										FontFamily->"Verdana",
										Background->LightYellow],
			QuantumVerticalTextStyle->Directive[Darker[Darker[Red]],
										FontFamily->"Verdana"],
			QuantumPlot3D->False},
		Options[Graphics],Options[Graphics3D]];

Options[QuantumPlot3D] = Options[QuantumPlot];

Options[QuantumTable] = {QubitList->{}};

Options[QuantumTableForm]=Options[QuantumTable];

Options[MatrixQuantum]= {QubitList->{}};

Options[TensorQuantum]= {QubitList->{}};

Options[QuantumMatrix] = {QubitList->{}};

Options[QuantumMatrixForm]=Options[QuantumMatrix];

Options[QuantumTensor] = {QubitList->{}};

Options[QuantumTensorForm]=Options[QuantumTensor];

Options[QuantumSparseArray] = {QubitList->{}};

Options[QuantumEigensystem] = Join[{QubitList->{}},Options[DiracEigensystem]];

Options[QuantumEigensystemForm] = Join[Options[QuantumEigensystem],
										Options[Grid]];

Options[QubitMeasurement] = Options[QuantumMeasurement];

Options[PauliExpand] = {PauliIdentities->True};

SetAttributes[QuantumEvaluate,Listable];

Begin["`Private`"]

SetAttributes[internalEvaluate,Listable];

(* *** **** **** INPUT ALIASES *)

myCircleTimes=
	If[TrueQ[$VersionNumber==6||$VersionNumber==7],
		AdjustmentBox["\[CircleTimes]",
			BoxBaselineShift->(-3/4)],
		"\[CircleTimes]"
	];

SetComputingAliases[]:=
Module[{nb},
	nb:=InputNotebook[];
	SetComputingAliases[nb]
];

SetComputingAliases[doc_NotebookObject]:=
Module[{new,old,oldandnew},
	ClearAliases[doc];
	SetQuantumAliases[doc];
	old=InputAliases /. Options[InputNotebook[],InputAliases] /. InputAliases->{};
    new={
    	"on"  -> 	"\[CenterDot]",
    	"tp"  -> 	"\[CircleTimes]",
		"qb"  -> 	OverscriptBox["\[Placeholder]", "^"],
    	"her"  -> 	SuperscriptBox[
						RowBox[{
							"(","\[Placeholder]",")"}], "\[Dagger]" ],
		"con"  -> 	SuperscriptBox[
						RowBox[{
							"(","\[Placeholder]",")"}], "*" ],
		"norm" -> 	RowBox[{"\[LeftDoubleBracketingBar]", "\[Placeholder]",
							"\[RightDoubleBracketingBar]"}],
		"tprod" ->	
				TagBox[
					RowBox[{
						UnderoverscriptBox[
								"\[CircleTimes]", 
								TagBox[
									RowBox[{"\[Placeholder]", "=", 
											"\[Placeholder]"}],
									zz020TPNotationini,Editable->True,Selectable->True], 
								TagBox[
									"\[Placeholder]",
									zz020TPNotationend,Editable->True,Selectable->True]],
						TagBox["\[Placeholder]",
									zz020TPNotationdat,Editable->True,Selectable->True]}],
					zz020TPNotation,Editable->False,Selectable->False],
		"tprodqb" -> 
				TagBox[
					RowBox[{
						UnderoverscriptBox[
								"\[CircleTimes]", 
								TagBox[
									RowBox[{"n", "=", 
											"\[Placeholder]"}],
									zz020TPNotationini,Editable->True,Selectable->True], 
								TagBox[
									"\[Placeholder]",
									zz020TPNotationend,Editable->True,Selectable->True]],
						TagBox[TagBox[RowBox[{"\[VerticalSeparator]", 
    							TagBox[SubscriptBox["\[Placeholder]", 
									OverscriptBox["n", "^"]],
     								zz080KetArgs,
     								Editable->True,
									Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}], 
								"\[RightAngleBracket]"}],zz080Ket,
								Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
									zz020TPNotationdat,Editable->True,Selectable->True]}],
					zz020TPNotation,Editable->False,Selectable->False],
    	"tpow" ->	TagBox[
    					SuperscriptBox[	
    						RowBox[{
    							"(", 
								TagBox[	"\[Placeholder]",zz020TPdat,
										Editable->True,Selectable->True], 
								")"}],
							RowBox[{
								myCircleTimes,
								TagBox[	"\[Placeholder]",zz020TPend,
										Editable->True,Selectable->True]
							}]],
						zz020TP,Editable->False,Selectable->False],						
		"tpowqb" ->	TagBox[
    					SuperscriptBox[	
    						RowBox[{
    							"(", 
								TagBox[	
									TagBox[RowBox[{
										"\[VerticalSeparator]",
										TagBox[RowBox[{		
											SubscriptBox["\[Placeholder]",
												OverscriptBox["\[Placeholder]", "^"]]}],
										zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
										"\[RightAngleBracket]" }],
										zz080Ket,Editable->False,Selectable->False,
										BaseStyle->{ShowSyntaxStyles->False}],
									zz020TPdat,Editable->True,Selectable->True], 
								")"}],
							RowBox[{
								myCircleTimes,
								TagBox[	"\[Placeholder]",zz020TPend,
										Editable->True,Selectable->True]
							}]],
						zz020TP,Editable->False,Selectable->False],
		"qket" ->	TagBox[
						RowBox[{"\[VerticalSeparator]",
      						TagBox[SubscriptBox["\[Placeholder]",
      								OverscriptBox["\[Placeholder]","^"]],
        						zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
      						"\[RightAngleBracket]"}],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qqket" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qqqket" -> TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qbra" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["\[Placeholder]",
      								OverscriptBox["\[Placeholder]","^"]],
      							zz080BraArgs,
							Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qqbra" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qqqbra" -> TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qket0" ->	TagBox[
						RowBox[{"\[VerticalSeparator]",
      						TagBox[SubscriptBox["0",
      								OverscriptBox["\[Placeholder]","^"]],
        						zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
      						"\[RightAngleBracket]"}],
      					zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
      	"qbra0" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["0",
      								OverscriptBox["\[Placeholder]","^"]],
      							zz080BraArgs,
							Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qket1" ->	TagBox[
						RowBox[{"\[VerticalSeparator]",
      						TagBox[SubscriptBox["1",
      								OverscriptBox["\[Placeholder]","^"]],
        						zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
      						"\[RightAngleBracket]"}],
      					zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
      	"qbra1" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["1",
      								OverscriptBox["\[Placeholder]","^"]],
      							zz080BraArgs,
							Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qlbl" ->	SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
		"qv" ->	SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
		"qn" ->	SubscriptBox["\[Placeholder]","\[Placeholder]"],
		"qg" ->	SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
		"qqg" ->	SubscriptBox["\[Placeholder]",
						RowBox[{OverscriptBox["\[Placeholder]","^"],",",
								OverscriptBox["\[Placeholder]","^"]}]],
		"qqqg" ->	SubscriptBox["\[Placeholder]",
						RowBox[{OverscriptBox["\[Placeholder]","^"],",",
								OverscriptBox["\[Placeholder]","^"],",",
								OverscriptBox["\[Placeholder]","^"]}]],
		"qgg" ->	SubscriptBox["\[Placeholder]",
						RowBox[{"{",RowBox[{OverscriptBox["\[Placeholder]","^"],",",
											OverscriptBox["\[Placeholder]","^"]}],"}"}]],
		"qggg" ->    SubscriptBox["\[Placeholder]",
						RowBox[{"{",RowBox[{OverscriptBox["\[Placeholder]","^"],",",
											OverscriptBox["\[Placeholder]","^"],",",
											OverscriptBox["\[Placeholder]","^"]}],"}"}]],
		"pqg" ->     RowBox[{
						SubscriptBox["\[Placeholder]", 
							OverscriptBox["\[Placeholder]", "^"]], 
						"[", "\[Placeholder]", "]"}],
		"pqgg" ->    RowBox[{
						SubscriptBox["\[Placeholder]", 
							RowBox[{"{", RowBox[{
								OverscriptBox["\[Placeholder]", "^"], ",", 
								OverscriptBox["\[Placeholder]", "^"]}], "}"}]], 
						"[","\[Placeholder]", "]"}],
		"pqggg" ->   RowBox[{
						SubscriptBox["\[Placeholder]", 
							RowBox[{"{", RowBox[{
								OverscriptBox["\[Placeholder]", "^"], ",", 
								OverscriptBox["\[Placeholder]", "^"], ",", 
								OverscriptBox["\[Placeholder]", "^"]}], "}"}]], 
						"[", "\[Placeholder]", "]"}],
		"pqqg" ->    RowBox[{
						SubscriptBox["\[Placeholder]", 
							RowBox[{
								OverscriptBox["\[Placeholder]", "^"], ",", 
								OverscriptBox["\[Placeholder]", "^"]}]], 
						"[", "\[Placeholder]", "]"}],
		"pqqqg" ->   RowBox[{
						SubscriptBox["\[Placeholder]", 
							RowBox[{
								OverscriptBox["\[Placeholder]", "^"], ",", 
								OverscriptBox["\[Placeholder]", "^"], ",", 
								OverscriptBox["\[Placeholder]", "^"]}]], 
						"[", "\[Placeholder]", "]"}],
		"qketbra" -> RowBox[{
					TagBox[RowBox[{"\[VerticalSeparator]",
          					TagBox[SubscriptBox["\[Placeholder]",
              					OverscriptBox["\[Placeholder]","^"]],zz080KetArgs,
            					Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
          					"\[RightAngleBracket]"}],
          				zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
          			"\[CenterDot]",
    				TagBox[RowBox[{"\[LeftAngleBracket]",
          					TagBox[SubscriptBox["\[Placeholder]",
              					OverscriptBox["\[Placeholder]","^"]],zz080BraArgs,
            					Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
          					"\[VerticalSeparator]"}],
          				zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}]}],
         "qqketbra" -> RowBox[{
         			TagBox[RowBox[{"\[VerticalSeparator]",
          					TagBox[RowBox[{SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]],",",
                				SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]]}],zz080KetArgs,
            					Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
          					"\[RightAngleBracket]"}],
          			zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
          			"\[CenterDot]",
					TagBox[RowBox[{"\[LeftAngleBracket]",
          					TagBox[RowBox[{SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]],",",
                				SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]]}],zz080BraArgs,
            					Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
          					"\[VerticalSeparator]"}],
          				zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}]}],
          "qqqketbra" -> RowBox[{
          			TagBox[RowBox[{"\[VerticalSeparator]",
          					TagBox[RowBox[{SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]],",",
                				SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]],",",
                				SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]]}],zz080KetArgs,
            					Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
          					"\[RightAngleBracket]"}],
          				zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
          			"\[CenterDot]",
    				TagBox[RowBox[{"\[LeftAngleBracket]",
          					TagBox[RowBox[{SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]],",",
                				SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]],",",
                				SubscriptBox["\[Placeholder]",
                  				OverscriptBox["\[Placeholder]","^"]]}],zz080BraArgs,
            					Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
          					"\[VerticalSeparator]"}],
          				zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}]}],
		"cngate"	->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									"\[Placeholder]",
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cgate"	->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",OverscriptBox["\[Placeholder]", "^"],"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cng"	->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									"\[Placeholder]",
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cg"	->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",OverscriptBox["\[Placeholder]", "^"],"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"ccgate"->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"ccg"->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cccgate"->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cccg"->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								RowBox[{"\[Placeholder]"}],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cnnot"	->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									"\[Placeholder]",
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								TagBox[
									SubscriptBox[
										"\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT]",
										TagBox[OverscriptBox["\[Placeholder]", "^"],
											zz020TPdat,Editable->True,Selectable->True]],
									zz020TP,Editable->False,Selectable->False],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cnot"	->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",OverscriptBox["\[Placeholder]", "^"],"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								TagBox[
									SubscriptBox[
										"\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT]",
										TagBox[OverscriptBox["\[Placeholder]", "^"],
											zz020TPdat,Editable->True,Selectable->True]],
									zz020TP,Editable->False,Selectable->False],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"ccnot"	->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								TagBox[
									SubscriptBox[
										"\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT]",
										TagBox[OverscriptBox["\[Placeholder]", "^"],
											zz020TPdat,Editable->True,Selectable->True]],
									zz020TP,Editable->False,Selectable->False],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"cccnot"->	TagBox[
						RowBox[{
							SuperscriptBox["\[ScriptCapitalC]", 
								TagBox[
									RowBox[{"{",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										",",
										OverscriptBox["\[Placeholder]", "^"],
										"}"}],
									zz020TPend,Editable->True,Selectable->True]], 
							"[",
							TagBox[
								TagBox[
									SubscriptBox[
										"\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT]",
										TagBox[OverscriptBox["\[Placeholder]", "^"],
											zz020TPdat,Editable->True,Selectable->True]],
									zz020TP,Editable->False,Selectable->False],
								zz020TPdat,Editable->True,Selectable->True],
							"]"}],
						zz020TP,Editable->False,Selectable->False],
		"toqb" ->	SubscriptBox[
						TagBox[RowBox[{"\[VerticalSeparator]", 
							TagBox["\[Placeholder]",zz080KetArgs,
								Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}], 
							"\[RightAngleBracket]"}],zz080Ket,
							Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}], 
						"\[Placeholder]"],
		"s0" ->		SubscriptBox["\[Sigma]",RowBox[{"0",",",OverscriptBox["\[Placeholder]","^"]}]],
		"s1" ->		SubscriptBox["\[Sigma]",RowBox[{"1",",",OverscriptBox["\[Placeholder]","^"]}]],
		"s2" ->		SubscriptBox["\[Sigma]",RowBox[{"2",",",OverscriptBox["\[Placeholder]","^"]}]],
		"s3" ->		SubscriptBox["\[Sigma]",RowBox[{"3",",",OverscriptBox["\[Placeholder]","^"]}]],
		"s4" ->		SubscriptBox["\[Sigma]",RowBox[{"4",",",OverscriptBox["\[Placeholder]","^"]}]],
		"sO" ->		SubscriptBox["\[Sigma]",RowBox[{"\[ScriptZero]",",",OverscriptBox["\[Placeholder]","^"]}]],
		"so" ->		SubscriptBox["\[Sigma]",RowBox[{"\[ScriptZero]",",",OverscriptBox["\[Placeholder]","^"]}]],
		"sx" ->		SubscriptBox["\[Sigma]",RowBox[{"\[ScriptCapitalX]",",",OverscriptBox["\[Placeholder]","^"]}]],
		"sy" ->		SubscriptBox["\[Sigma]",RowBox[{"\[ScriptCapitalY]",",",OverscriptBox["\[Placeholder]","^"]}]],
		"sz" ->		SubscriptBox["\[Sigma]",RowBox[{"\[ScriptCapitalZ]",",",OverscriptBox["\[Placeholder]","^"]}]],
		"sp" ->		SubscriptBox["\[Sigma]",RowBox[{"\[Placeholder]",",",OverscriptBox["\[Placeholder]","^"]}]],

		"in" ->		SubscriptBox["\[ScriptCapitalI]", "\[Placeholder]"],
		"ig" ->		SubscriptBox["\[ScriptCapitalI]", OverscriptBox["\[Placeholder]", "^"]],
		"igg" ->	   SubscriptBox["\[ScriptCapitalI]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"iggg" ->	  SubscriptBox["\[ScriptCapitalI]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"id" ->		SubscriptBox["\[ScriptCapitalI]", OverscriptBox["\[Placeholder]", "^"]],
		"idd" ->	   SubscriptBox["\[ScriptCapitalI]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"iddd" ->	  SubscriptBox["\[ScriptCapitalI]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"xn" ->		SubscriptBox["\[ScriptCapitalX]", "\[Placeholder]"],
		"xg" ->		SubscriptBox["\[ScriptCapitalX]", OverscriptBox["\[Placeholder]", "^"]],
		"xgg" ->	   SubscriptBox["\[ScriptCapitalX]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"xggg" ->	  SubscriptBox["\[ScriptCapitalX]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"yn" ->		SubscriptBox["\[ScriptCapitalY]", "\[Placeholder]"],
		"yg" ->		SubscriptBox["\[ScriptCapitalY]", OverscriptBox["\[Placeholder]", "^"]],
		"ygg" ->	   SubscriptBox["\[ScriptCapitalY]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"yggg" ->	  SubscriptBox["\[ScriptCapitalY]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"zn" ->		SubscriptBox["\[ScriptCapitalZ]", "\[Placeholder]"],
		"zg" ->		SubscriptBox["\[ScriptCapitalZ]", OverscriptBox["\[Placeholder]", "^"]],
		"zgg" ->	   SubscriptBox["\[ScriptCapitalZ]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"zggg" ->	  SubscriptBox["\[ScriptCapitalZ]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"hn" ->		SubscriptBox["\[ScriptCapitalH]", "\[Placeholder]"],
		"hg" ->		SubscriptBox["\[ScriptCapitalH]", OverscriptBox["\[Placeholder]", "^"]],
		"hgg" ->	   SubscriptBox["\[ScriptCapitalH]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"hggg" ->	  SubscriptBox["\[ScriptCapitalH]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"pn" ->		RowBox[{SubscriptBox["\[ScriptCapitalP]", "\[Placeholder]"], "[", "\[Placeholder]", "]"}],
		"pg" ->		RowBox[{SubscriptBox["\[ScriptCapitalP]", OverscriptBox["\[Placeholder]", "^"]], "[", "\[Placeholder]", "]"}],
		"pgg" ->	   RowBox[{SubscriptBox["\[ScriptCapitalP]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]], "[", "\[Placeholder]", "]"}],
		"pggg" ->	  RowBox[{SubscriptBox["\[ScriptCapitalP]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]], "[", "\[Placeholder]", "]"}],
		"sn" ->		SubscriptBox["\[ScriptCapitalS]", "\[Placeholder]"],
		"sg" ->		SubscriptBox["\[ScriptCapitalS]", OverscriptBox["\[Placeholder]", "^"]],
		"sgg" ->	   SubscriptBox["\[ScriptCapitalS]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"sggg" ->	  SubscriptBox["\[ScriptCapitalS]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"tn" ->		SubscriptBox["\[ScriptCapitalT]", "\[Placeholder]"],
		"tg" ->		SubscriptBox["\[ScriptCapitalT]", OverscriptBox["\[Placeholder]", "^"]],
		"tgg" ->	   SubscriptBox["\[ScriptCapitalT]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"tggg" ->	  SubscriptBox["\[ScriptCapitalT]",RowBox[{"{",
							RowBox[{OverscriptBox["\[Placeholder]","^"],",",OverscriptBox["\[Placeholder]","^"],
									",",OverscriptBox["\[Placeholder]","^"]}],
							"}"}]],
		"qf" ->        SubscriptBox["\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]", "\[Placeholder]"],
		"qft" ->        SubscriptBox["\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]", 
								OverscriptBox["\[Placeholder]", "^"]],
		"qqft" ->       SubscriptBox["\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]", 
								RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}]],
		"qqqft" ->      SubscriptBox["\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]", 
								RowBox[{OverscriptBox["\[Placeholder]", "^"], ",",
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}]],
		"qqqqft" ->      SubscriptBox["\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]", 
								RowBox[{OverscriptBox["\[Placeholder]", "^"], ",",
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}]],
		"qqqqqft" ->      SubscriptBox["\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]", 
								RowBox[{OverscriptBox["\[Placeholder]", "^"], ",",
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}]],
		"swap" ->	SubscriptBox["\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP]", 
								RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}]],
		"toff" ->	SubscriptBox["\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI]", 
								RowBox[{OverscriptBox["\[Placeholder]", "^"], ",",
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}]],
		"fred" ->	SubscriptBox["\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN]", 
								RowBox[{OverscriptBox["\[Placeholder]", "^"], ",",
										OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}]],
		"k00" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptZero]\[ScriptZero]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
								zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"k01" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptZero]\[ScriptOne]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"k10" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptOne]\[ScriptZero]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"k11" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptOne]\[ScriptOne]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"b00" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptZero]\[ScriptZero]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
								zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"b01" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptZero]\[ScriptOne]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"b10" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptOne]\[ScriptZero]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"b11" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["\[ScriptCapitalB]", 
										RowBox[{"\[ScriptOne]\[ScriptOne]", ",", 
											OverscriptBox["\[Placeholder]", "^"], ",", 
											OverscriptBox["\[Placeholder]", "^"]}]],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"kphi+" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubsuperscriptBox["\[CapitalPhi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "+"],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"kphi-" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubsuperscriptBox["\[CapitalPhi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "-"],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"kpsi+" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubsuperscriptBox["\[CapitalPsi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "+"],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"kpsi-" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubsuperscriptBox["\[CapitalPsi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "-"],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],

		"bphi+" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubsuperscriptBox["\[CapitalPhi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "+"],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"bphi-" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubsuperscriptBox["\[CapitalPhi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "-"],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"bpsi+" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubsuperscriptBox["\[CapitalPsi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "+"],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"bpsi-" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubsuperscriptBox["\[CapitalPsi]",
									RowBox[{OverscriptBox["\[Placeholder]", "^"], ",", 
										OverscriptBox["\[Placeholder]", "^"]}], "-"],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"k+" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubscriptBox["+", 
								OverscriptBox["\[Placeholder]", "^"],
								BaseStyle->{ShowSyntaxStyles->False}],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"b+" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["+", 
								OverscriptBox["\[Placeholder]", "^"],
								BaseStyle->{ShowSyntaxStyles->False}],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"k-" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[SubscriptBox["-", 
								OverscriptBox["\[Placeholder]", "^"],
								BaseStyle->{ShowSyntaxStyles->False}],
							zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[RightAngleBracket]" }],
						zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"b-" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[SubscriptBox["-", 
								OverscriptBox["\[Placeholder]", "^"],
								BaseStyle->{ShowSyntaxStyles->False}],
							zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
							"\[VerticalSeparator]" }],
						zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}],
		"qr" ->     RowBox[{"\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR]","[","\[Placeholder]","]"}],
		"qrg" ->	SubscriptBox["\[Placeholder]",
									RowBox[{"\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR]","[","\[Placeholder]","]"}]]		
	};
	oldandnew=Union[old,new];
	SetOptions[doc,InputAliases->oldandnew];
	SetComputingAliases::aliases];

(* *** **** **** OUTPUT AND INPUT FORMATS *)

(* *** **** **** TRADITIONAL FORM FORMATS *)




zz050Bell /: MakeBoxes[zz050Bell[0,0,q1_,q2_,"Beta"],TraditionalForm]:=
Module[{boxes,replaceme},
	boxes=MakeBoxes[Subscript[Global`\[Beta],\[ScriptZero]\[ScriptZero],q1,q2],
									TraditionalForm];
	ReleaseHold[Hold[InterpretationBox[replaceme,zz050Bell[0,0,q1,q2,"Beta"]]] /.
		replaceme->boxes]
];

zz050Bell /: MakeBoxes[zz050Bell[0,1,q1_,q2_,"Beta"],TraditionalForm]:=
Module[{boxes,replaceme},
	boxes=MakeBoxes[Subscript[Global`\[Beta],\[ScriptZero]\[ScriptOne],q1,q2],
									TraditionalForm];
	ReleaseHold[Hold[InterpretationBox[replaceme,zz050Bell[0,1,q1,q2,"Beta"]]] /.
		replaceme->boxes]
];

zz050Bell /: MakeBoxes[zz050Bell[1,0,q1_,q2_,"Beta"],TraditionalForm]:=
Module[{boxes,replaceme},
	boxes=MakeBoxes[Subscript[Global`\[Beta],\[ScriptOne]\[ScriptZero],q1,q2],
									TraditionalForm];
	ReleaseHold[Hold[InterpretationBox[replaceme,zz050Bell[1,0,q1,q2,"Beta"]]] /.
		replaceme->boxes]
];

zz050Bell /: MakeBoxes[zz050Bell[1,1,q1_,q2_,"Beta"],TraditionalForm]:=
Module[{boxes,replaceme},
	boxes=MakeBoxes[Subscript[Global`\[Beta],\[ScriptOne]\[ScriptOne],q1,q2],
									TraditionalForm];
	ReleaseHold[Hold[InterpretationBox[replaceme,zz050Bell[1,1,q1,q2,"Beta"]]] /.
		replaceme->boxes]
];


Unprotect[zz080Operator];
zz080Operator /: MakeBoxes[zz080Operator[arg_],TraditionalForm]:=
Module[{boxes,replaceme},
	boxes=MakeBoxes[arg,TraditionalForm];
	ReleaseHold[Hold[InterpretationBox[replaceme,zz080Operator[arg]]] /.
		replaceme->boxes]
];
Protect[zz080Operator];

zz020Controlled /: 
	MakeBoxes[zz020Controlled[gate_,{controlqbits__}],
		TraditionalForm]:=
Module[{preboxes,boxes,replaceme,ib},
	preboxes=RowBox[{
		"\[ScriptCapitalC]",
		MakeBoxes[Subscript[gate,{controlqbits}],TraditionalForm]
		}]/."\[ScriptCapitalC]"->StringJoin@@Table["\[ScriptCapitalC]",{Length[{controlqbits}]}];
	boxes=(preboxes/.InterpretationBox -> ib /.
			RowBox[{c_,SubscriptBox[RowBox[{"(",ib[SubscriptBox[b_String,ss__],i_],")"}],sb__]}] :>
				SubscriptBox[ib[SubscriptBox[c<>b,ss],i],sb] /.ib -> InterpretationBox);

	ReleaseHold[Hold[InterpretationBox[replaceme,
			zz020Controlled[gate,{controlqbits}]]] /.
		replaceme->boxes]
];



(* TraditionalForm of kets of qubits *)
Unprotect[zz080Ket];
zz080Ket /: MakeBoxes[zz080Ket[arg:(zz080Eigenstate[_,_]..)], TraditionalForm] :=
Module[{boxes,replaceme},
	boxes=TagBox[RowBox[{
				"\[VerticalSeparator]",
					TagBox[DeleteCases[
								Replace[				
									MakeBoxes[{arg}, TraditionalForm],","->"\[InvisibleSpace]",4], 
								"{" | "}" | ",", 2] /. 
								SubscriptBox[base_,s_]:>base,
						zz080KetArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
				"\[RightAngleBracket]" }],
			zz080Ket,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}];
	ReleaseHold[Hold[InterpretationBox[replaceme,zz080Ket[arg]]] /.
		replaceme->boxes]
] /; Not[MemberQ[Hold[arg],zz050Subscript[__]]];
Protect[zz080Ket];

Unprotect[zz080Bra];
zz080Bra /: MakeBoxes[zz080Bra[arg:(zz080Eigenstate[_,_]..)], TraditionalForm] :=
Module[{boxes,replaceme},
	boxes=TagBox[RowBox[{
				"\[LeftAngleBracket]",
					TagBox[DeleteCases[
								Replace[				
									MakeBoxes[{arg}, TraditionalForm],","->"\[InvisibleSpace]",4], 
								"{" | "}" | ",", 2] /. 
								SubscriptBox[base_,s_]:>base,
						zz080BraArgs,Editable->True,Selectable->True,
									BaseStyle->{ShowSyntaxStyles->True}],
				"\[VerticalSeparator]" }],
			zz080Bra,Editable->False,Selectable->False,
									BaseStyle->{ShowSyntaxStyles->False}];
	ReleaseHold[Hold[InterpretationBox[replaceme,zz080Bra[arg]]] /.
		replaceme->boxes]
] /; Not[MemberQ[Hold[arg],zz050Subscript[__]]];
Protect[zz080Bra];



(* Output and input of zz020Controlled *)

zz020Controlled /: MakeBoxes[zz020Controlled[u_,n_],form_]:=
	TagBox[
		RowBox[{
			SuperscriptBox["\[ScriptCapitalC]", 
				TagBox[MakeBoxes[n,form],
						zz020TPend,Editable->True,Selectable->True]], 
			"[",
			TagBox[MakeBoxes[u,form],zz020TPdat,Editable->True,Selectable->True],
			"]"}],
		zz020TP,Editable->False,Selectable->False];

MakeExpression[
	TagBox[
		RowBox[{
			SuperscriptBox["\[ScriptCapitalC]", 
				TagBox[n_,
						zz020TPend,opts0___]], 
			"[", 
			TagBox[u__,zz020TPdat,opts1___],
			"]"}],
		zz020TP,opts2___],
  	form_]:=
MakeExpression[RowBox[{"zz020Controlled","[",u,",",n,"]"}],form];

(* Next input format is for compatiblity with old versions,
and it is also good to have it so that a structure build from pieces
with the shape of a controlled gate is interpreted as a controlled gate*)
MakeExpression[
	RowBox[{SuperscriptBox["\[ScriptCapitalC]", n_], 
			"[", u_, "]"}],
  	form_]:=
MakeExpression[RowBox[{"zz020Controlled","[",u,",",n,"]"}],form];

(* Next input format is for compatiblity with old versions,
and it is also good to have it so that a structure build from pieces
with the shape of tensor product is interpreted as a tensor product*)
MakeExpression[
	RowBox[{
		UnderoverscriptBox["\[CircleTimes]", RowBox[{j_, "=", ini_}], end_],
		data_}],
	form_]:=
MakeExpression[
	RowBox[{"zz090TensorProduct","[",data,",{",j,",",ini,",",end,"}]"}],form]


(* Output and input of zz020TensorPower *)

zz020TensorPower /: MakeBoxes[zz020TensorPower[a_,n_],form_]:=
	TagBox[
		SuperscriptBox[	RowBox[{"(", 
								TagBox[	MakeBoxes[a,form],zz020TPdat,
										Editable->True,Selectable->True], 
								")"}],
						RowBox[{myCircleTimes,
								TagBox[	MakeBoxes[n,form],zz020TPend,
										Editable->True,Selectable->True]
							}]],
		zz020TP,Editable->False,Selectable->False
	];

(* notice two almost identical MakeExpression: with and without AdjustmentBox.
The AdjustmentBox is in the definition of myCircleTimes *)
MakeExpression[
	TagBox[
		SuperscriptBox[	RowBox[{"(",
								TagBox[a_,zz020TPdat,opts1___],
								")"}],
						RowBox[{myCircleTimes,
								TagBox[n_,zz020TPend,opts2___]}]],
		zz020TP,opts0___],
					form_]:=
MakeExpression[RowBox[{"zz020TensorPower","[",a,",",n,"]"}],form];

MakeExpression[
	TagBox[
		SuperscriptBox[	RowBox[{"(",
								TagBox[a_,zz020TPdat,opts1___],
								")"}],
						RowBox[{"\[CircleTimes]",
								TagBox[n_,zz020TPend,opts2___]}]],
		zz020TP,opts0___],
					form_]:=
MakeExpression[RowBox[{"zz020TensorPower","[",a,",",n,"]"}],form];

(* Next input format is for compatiblity with old versions,
and it is also good to have it so that a structure build from pieces
with the shape of tensor power is interpreted as a tensor power*)
MakeExpression[
	SuperscriptBox[a_,RowBox[{"\[CircleTimes]",n_}]],
					form_]:=
MakeExpression[RowBox[{"zz020TensorPower","[",a,",",n,"]"}],form];

(* Output and Input of Base-10-Integer to binary-qubit template*)
(* A bug in Mathematica's editor forced me to use _?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &) *)
Unprotect[zz050Subscript];
zz050Subscript /:
	zz080Ket[zz050Subscript[{a_Integer}, 
					{arg1:(_List|_Integer|_zz080Operator|_?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &)),args___}]]:=
		DecToQubit[a,arg1,args];
Protect[zz050Subscript];

(* Notation for states ket+ ket- *)

MakeExpression[
	SubscriptBox["+", j_,opts___],
	form_]:=MakeExpression[RowBox[{"zz050Superpos", "[", j, ",","\"plus\"","]"}],form];

MakeExpression[
	SubscriptBox["-", j_,opts___],
	form_]:=MakeExpression[RowBox[{"zz050Superpos", "[", j, ",","\"minus\"","]"}],form];

zz050Superpos /: MakeBoxes[zz050Superpos[arg_,"plus"],form_]:=
	SubscriptBox["+", MakeBoxes[arg,form],BaseStyle->{ShowSyntaxStyles->False}];

zz050Superpos /: MakeBoxes[zz050Superpos[arg_,"minus"],form_]:=
	SubscriptBox["-", MakeBoxes[arg,form],BaseStyle->{ShowSyntaxStyles->False}];



(* Notation for BELL STATES *)

subwasprot=Unprotect[Subscript];

Subscript[\[ScriptCapitalB],i_,j_,q1_,q2_]:= zz050Bell[i,j,q1,q2,"Beta"];
Subscript[\[ScriptCapitalB],\[ScriptZero]\[ScriptZero],q1_,q2_]:=zz050Bell[0,0,q1,q2,"Beta"];
Subscript[\[ScriptCapitalB],\[ScriptZero]\[ScriptOne],q1_,q2_]:=zz050Bell[0,1,q1,q2,"Beta"];
Subscript[\[ScriptCapitalB],\[ScriptOne]\[ScriptZero],q1_,q2_]:=zz050Bell[1,0,q1,q2,"Beta"];
Subscript[\[ScriptCapitalB],\[ScriptOne]\[ScriptOne],q1_,q2_]:=zz050Bell[1,1,q1,q2,"Beta"];

Protect@@subwasprot;

zz050Bell /: MakeBoxes[zz050Bell[0,0,q1_,q2_,"Beta"],form_]:=
	MakeBoxes[Subscript[\[ScriptCapitalB],\[ScriptZero]\[ScriptZero],q1,q2],form];
zz050Bell /: MakeBoxes[zz050Bell[0,1,q1_,q2_,"Beta"],form_]:=
	MakeBoxes[Subscript[\[ScriptCapitalB],\[ScriptZero]\[ScriptOne],q1,q2],form];
zz050Bell /: MakeBoxes[zz050Bell[1,0,q1_,q2_,"Beta"],form_]:=
	MakeBoxes[Subscript[\[ScriptCapitalB],\[ScriptOne]\[ScriptZero],q1,q2],form];
zz050Bell /: MakeBoxes[zz050Bell[1,1,q1_,q2_,"Beta"],form_]:=
	MakeBoxes[Subscript[\[ScriptCapitalB],\[ScriptOne]\[ScriptOne],q1,q2],form];

superwasp=Unprotect[SuperPlus,SuperMinus];

SuperPlus[Subscript[\[CapitalPhi],q1_,q2_]]:=zz050Bell[0,0,q1,q2];

zz050Bell /: MakeBoxes[zz050Bell[0,0,q1_,q2_],form_]:=
	SubsuperscriptBox["\[CapitalPhi]", 
		RowBox[{
			MakeBoxes[q1,form], ",", 
			MakeBoxes[q2,form]}], "+"];

SuperPlus[Subscript[\[CapitalPsi],q1_,q2_]]:=zz050Bell[0,1,q1,q2];

zz050Bell /: MakeBoxes[zz050Bell[0,1,q1_,q2_],form_]:=
	SubsuperscriptBox["\[CapitalPsi]", 
		RowBox[{
			MakeBoxes[q1,form], ",", 
			MakeBoxes[q2,form]}], "+"];

SuperMinus[Subscript[\[CapitalPhi],q1_,q2_]]:=zz050Bell[1,0,q1,q2];

zz050Bell /: MakeBoxes[zz050Bell[1,0,q1_,q2_],form_]:=
	SubsuperscriptBox["\[CapitalPhi]", 
		RowBox[{
			MakeBoxes[q1,form], ",", 
			MakeBoxes[q2,form]}], "-"];

SuperMinus[Subscript[\[CapitalPsi],q1_,q2_]]:=zz050Bell[1,1,q1,q2];

zz050Bell /: MakeBoxes[zz050Bell[1,1,q1_,q2_],form_]:=
	SubsuperscriptBox["\[CapitalPsi]", 
		RowBox[{
			MakeBoxes[q1,form], ",", 
			MakeBoxes[q2,form]}], "-"];

Protect@@superwasp;

(* Old formats for compatibility with old versions *)
MakeExpression[
	SubscriptBox["\[Beta]", 
		RowBox[{"00", ",", 
				q1_, ",", q2_}]],
      form_]:=
      MakeExpression[RowBox[{"zz050Bell", "[", 0,",",0,",",q1,",",q2,",\"Beta\" ","]"}],form];

MakeExpression[
	SubscriptBox["\[Beta]", 
		RowBox[{"01", ",", 
				q1_, ",", q2_}]],
      form_]:=
      MakeExpression[RowBox[{"zz050Bell", "[", 0,",",1,",",q1,",",q2,",\"Beta\" ","]"}],form];

MakeExpression[
	SubscriptBox["\[Beta]", 
		RowBox[{"10", ",", 
				q1_, ",", q2_}]],
      form_]:=
      MakeExpression[RowBox[{"zz050Bell", "[", 1,",",0,",",q1,",",q2,",\"Beta\" ","]"}],form];

		
MakeExpression[
	SubscriptBox["\[Beta]", 
		RowBox[{"11", ",", 
				q1_, ",", q2_}]],
      form_]:=
      MakeExpression[RowBox[{"zz050Bell", "[", 1,",",1,",",q1,",",q2,",\"Beta\" ","]"}],form];



(*  *** **** **** MAIN CALCULATION RULES *)



HoldPattern[zz050Bell[i1_, i2_, qsame_zz080Operator, qsame_, b1___]]:= 
	Module[
      {},
		Message[zz080Eigenstate::ketrepop];
		QuantumDebug["debugbell"];Abort[];
      "Errorbellket"
	];

kwasp=Unprotect[zz080Ket];

(* it is not necessary to define this rule for bra
because bra automatically tries ket, therefore
this verification takes place: *) 
HoldPattern[zz080Ket[
		zz050Superpos[q1_zz080Operator,"plus"],
        rest___]]:= 
	Module[
      {},
      Message[zz080Eigenstate::ketrepop];
	  QuantumDebug[{"debug zz050Superposition ket+",
      	zz050Superpos[q1,"plus"]}];
	  Abort[];
      "Errorket+"
] /; And[FreeQ[{q1},Pattern],Not[FreeQ[{rest},q1]]];

(* it is not necessary to define this rule for bra
because bra automatically tries ket, therefore
this verification takes place: *) 
HoldPattern[zz080Ket[
		zz050Superpos[q1_zz080Operator,"minus"],
        rest___]]:= 
	Module[
      {},
      Message[zz080Eigenstate::ketrepop];
	  QuantumDebug[{"debug zz050Superposition ket-",
      	zz050Superpos[q1,"minus"]}];
	  Abort[];
      "Errorket-"
] /; And[FreeQ[{q1},Pattern],Not[FreeQ[{rest},q1]]];

HoldPattern[zz080Ket[zz050Bell[1, 1, 
		q1_zz080Operator, q2_zz080Operator, b1___],rest___]]:=
	(-1)*zz080Ket[zz050Bell[1,1,q2,q1,b1],rest] /;
	Not[OrderedQ[{q1,q2}]];

HoldPattern[zz080Ket[zz050Bell[i1_Integer, i2_Integer, 
		q1_zz080Operator, q2_zz080Operator, b1___],rest___]]:=
	(+1)*zz080Ket[zz050Bell[i1,i2,q2,q1,b1],rest] /;
	And[{i1,i2}=!={1,1},Not[OrderedQ[{q1,q2}]]];

HoldPattern[zz080Ket[
	zz050Bell[i1_, i2_, q1_zz080Operator, q2_zz080Operator, b1___], rest___]]:= 
	Module[
      {},
		Message[zz080Eigenstate::ketrepop];
		QuantumDebug["debugbell"];Abort[];
      "Errorbellket"
	]/; 
	And[FreeQ[{q1,q2},Pattern],
			Or[ Not[FreeQ[{rest},q1]], Not[FreeQ[{rest},q2]] ]];

HoldPattern[zz080Ket[
	zz080Eigenstate[q1_zz080Operator, ev_], rest___]]:= 
	Module[
      {},
		Message[zz080Eigenstate::ketrepop];
		QuantumDebug["debugqbit"];Abort[];
      "Errorqubitket"
	]/; 
	And[FreeQ[{q1},Pattern],Not[FreeQ[{rest},q1]]];


Protect@@kwasp;

ntwasp=Unprotect[zz075NonCommutativeTimes];

(* bra-ket that (at least some part) becomes 
external product ket-bra because
there are not repeated qubits, 
see the symbol === in the condition after the symbol /; 
*)
HoldPattern[
zz075NonCommutativeTimes[
	after___,
	zz080Ket[kfun:((zz050Superpos|zz050Bell|
		zz080Eigenstate)[kf1___,qk_zz080Operator,kf2___]),restket___],
	zz080Bra[bfun:((zz050Superpos|zz050Bell|
		zz080Eigenstate)[bf1___,qb_zz080Operator,bf2___]),restbra___],
	before___
]]:=
zz075NonCommutativeTimes[(*remember args are in reverse order*)
	after,
	zz080Ket[restket],
	zz080Bra[bfun], (*for sure external product ket-bra*)
	zz080Ket[kfun], (*for sure external product ket-bra*)
	zz080Bra[restbra],
	before
]/;
Module[{kc,bc},
	kc=Cases[kfun,_zz080Operator,Infinity];
	bc=Cases[bfun,_zz080Operator,Infinity];
	Length[kc]+Length[bc]===Length[Union[kc,bc]]
]; (* it has to be checked with this weird procedure
because of the operators that can exist in kf1,kf2,bf1,bf2,
(specially in Bell states). 
Given that ALL operators are different, 
then (at least some part of) bra-ket can become
ket-bra, remember that the arguments of
zz075NonCommutativeTimes are in reverse order *)

(* bra-ket where (at least) part of it becomes
a number because
there are repeated qubits, 
see the symbol =!= in the condition after the symbol /; *)
HoldPattern[
zz075NonCommutativeTimes[
	after___,
	zz080Ket[kfun:((zz050Superpos|zz050Bell|
		zz080Eigenstate)[kf1___,qk_zz080Operator,kf2___]),restket___],
	zz080Bra[bfun:((zz050Superpos|zz050Bell|
		zz080Eigenstate)[bf1___,qb_zz080Operator,bf2___]),restbra___],
	before___
]]:=
Module[{ev},
	ev=zz075NonCommutativeTimes[(*remember args are in reverse order*)
			QuantumEvaluate[ zz080Ket[kfun] ], (*number or ket-bra*)
			QuantumEvaluate[ zz080Bra[bfun] ] (*number or ket-bra*)
		];
	zz075NonCommutativeTimes[(*remember args are in reverse order*)
		after,
		zz080Ket[restket],
		ev,
		zz080Bra[restbra],
		before
	]
]/;
Module[{kc,bc},
	kc=Cases[kfun,_zz080Operator,Infinity];
	bc=Cases[bfun,_zz080Operator,Infinity];
	Length[kc]+Length[bc]=!=Length[Union[kc,bc]]
];(* it has to be checked with this weird procedure
because of the operators that can exist in kf1,kf2,bf1,bf2,
(specially in Bell states). 
Given that SOME operators are the same, 
then at least some part of the bra-ket becomes
a number (although the particular ev part could become an
external product ket-bra, this definition works in either case), 
remember that the arguments of
zz075NonCommutativeTimes are in reverse order *)

zz075NonCommutativeTimes /:
	HoldPattern[Power[	zz075NonCommutativeTimes[
 				zz080Bra[rightbra___,mbra:(zz050Superpos|zz050Bell)[___],leftbra___], 
 				zz080Ket[rightket___,mket:(zz050Superpos|zz050Bell)[___],leftket___]],
			exponent_?(IntegerQ[#]&&(#>1) &)]]:=
		zz075NonCommutativeTimes[ 
			Sequence @@ Flatten[ Table[
							{zz080Bra[rightbra,mbra,leftbra], 
 							 zz080Ket[rightket,mket,leftket]},
							{exponent}
						]]
		 ];



Protect@@ntwasp;

(* Transformation between integers and*)
(* the binary notation in qubits*)
(* and Ket0 and Ket1 transformed to the proper qubit *)
QubitToDec[
	zz080Ket[qubits:(zz080Eigenstate[zz080Operator[_], (0|1)]..)],
	listausr_List:{}
]:=
Module[{listaqubits, listafromket, listaord, listaqbord, listaval,
		listausr2},
	listaqubits={qubits};
	listafromket=(listaqubits/.zz080Eigenstate[zz080Operator[ope_],_]:>ope);
	listausr2=(listausr/. zz080Operator[ope_]:>ope); 
	listaord=Join[listausr2,Complement[listafromket,listausr2]];
	listaqbord=Flatten[Map[
		Cases[listaqubits,
			zz080Eigenstate[zz080Operator[#],_]]&,
		listaord]];
	listaval=listaqbord/.zz080Eigenstate[zz080Operator[_], val_]:>val;
	FromDigits[listaval,2]
];

DecToQubit[numero_Integer]:=
	DecToQubit[numero,Length[IntegerDigits[numero,2]]]; 

DecToQubit[first_,second_]:=
	DecToQubit[first,second,1];

DecToQubit[numero_Integer,ope_zz080Operator,offset_]:=
	DecToQubit[numero,{ope},offset];

DecToQubit[numero_Integer,\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___],offset_]:=
	DecToQubit[numero,registerList[args],offset];

DecToQubit[numero_Integer,n_Integer,offset_]:=
	DecToQubit[numero,Range[n],offset];

DecToQubit[numero_Integer,{},offset_]:=1;

DecToQubit[numero_Integer,qubitList_List,offset_]:=
Module[{lista,j,dif,nqubits},
	lista=IntegerDigits[numero,2];
	nqubits=Length[qubitList];
	dif=nqubits-Length[lista];
	lista=Which[
			dif>0,
				Flatten[Prepend[lista,Table[0,{j,1,dif}]]],
			dif<0,  
				Take[lista,-nqubits],
			True,
				lista];
	Inner[zz080Eigenstate[ zz080Operator[ (#2-1+offset)/.zz080Operator->Sequence ],#1 ]&,
		lista,qubitList,zz080Ket]
];


(* Defining Quantum Gates *)

(* Notice the delayed assignment *)
$QuantumMaxUnitaryDefinitions:=$QuantumMaxCachedDefinitions;
uStoredDef=0;
autoresetUnitaryQ[]:=
Module[{},
	If[uStoredDef>$QuantumMaxUnitaryDefinitions,
		resetUnitaryQ[] ];
	uStoredDef++
];

resetUnitaryQ[]:=
Module[{},
	Clear[unitaryQ];
	uStoredDef=0;
	unitaryQ[m_List?MatrixQ] :=
		( autoresetUnitaryQ[];
		  unitaryQ[m] = (* dynamic programing *)
			Simplify[Conjugate@Transpose@m.m ==IdentityMatrix@Length@m] );
];

(* First Time: *)
resetUnitaryQ[];


(* Quantum registers *)

\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] /: zz080Operator[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]]:=\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args];

\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___,s_Symbol]:=\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args,SymbolName[s]];

\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imax_Integer]:=\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][1,imax];

\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imin_Integer,imax_Integer]:=\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imin,imax,1];

\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imax_Integer,s_String]:=\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][1,imax,s];

\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imin_Integer,imax_Integer,s_String]:=\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][imin,imax,1,s];

registerList[imin_Integer,imax_Integer,di_Integer]:=
	Map[zz080Operator,Range[imin,imax,di]];

registerList[imin_Integer,imax_Integer,di_Integer,s_String]:=
	Map[zz080Operator[Symbol[s<>IntegerString[#]]]&,Range[imin,imax,di]];

registerList[anyother___]:=
Module[{},
	Message[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR]::syntx];
	Abort[]
];

(* SetQuantumGate *)
SetQuantumGate[onearg_]:=
	Module[{},Message[SetQuantumGate::args];Abort[]];

diracFunctionToRules[fun_Function,nq_Integer]:=
With[{q=Table[Unique["q"],{nq}]},
	Module[{x,qargs,qrules},
		qargs=Apply[Sequence,q];
		qrules=Map[Function[{u},u:>u_],q];
		Map[Function[{pair},
					RuleDelayed@@{(pair[[1]]/.qrules)/;OrderedQ[q],pair[[2]]}],
			QuantumTable[fun[qargs]]]
]];

hermitianFunctionToRules[fun_Function,nq_Integer]:=
With[{q=Table[Unique["q"],{nq}]},
	Module[{x,qargs,qrules},
		qargs=Apply[Sequence,q];
		qrules=Map[Function[{u},u:>u_],q];
		Map[Function[{pair},
					RuleDelayed@@{(pair[[1]]/.qrules)/;OrderedQ[q],pair[[2]]}],
			QuantumTable[zz080HermitianConjugate[fun[qargs]]]]
]];

SetQuantumGate[gate_,nq_Integer,functiongate_Function]:=
Module[{msg, gaterules, hermitianrules},

	msg=SetQuantumGate[gate,nq];

	(* QuantumEvaluate *)
	
	internalEvaluate[gate]:=
		QuantumEvaluate[functiongate];

	internalEvaluate[HoldPattern[gate[gargs___]]]:=
		QuantumEvaluate[functiongate[gargs]];

	internalEvaluate[HoldPattern[gate[gargs___][params___]]]:=
		QuantumEvaluate[functiongate[gargs][params]];

	If[TrueQ[MatchQ[functiongate,HoldPattern[Function[vars___,_Function]]]],
		{internalEvaluate[HoldPattern[
			zz075NonCommutativeTimes[
				right___,
				zz080Ket[kargs__],
				Power[gate[gargs__?(FreeQ[#,List]&)],expon_.]]]]:=
			internalEvaluate[Function[QuantumEvaluate[
			zz075NonCommutativeTimes[
				right,
				zz080Ket[kargs],
				Power[gate[gargs][##],expon]]]]];

		internalEvaluate[HoldPattern[
			zz075NonCommutativeTimes[
				right___,
				zz080Ket[kargs__],
				Power[zz080HermitianConjugate[
					gate[gargs__?(FreeQ[#,List]&)]],expon_.]]]]:=
			internalEvaluate[Function[QuantumEvaluate[
			zz075NonCommutativeTimes[
				right,
				zz080Ket[kargs],
				Power[zz080HermitianConjugate[
					gate[gargs][##]],expon]]]]];
		},
		{gaterules=diracFunctionToRules[functiongate,nq];	
		hermitianrules=hermitianFunctionToRules[functiongate,nq];
	
		DefineOperatorOnKets[qFast[gate],gaterules];

		DefineOperatorOnKets[qFastHermitian[gate],hermitianrules];

		internalEvaluate[HoldPattern[
			zz075NonCommutativeTimes[
				right___,
				zz080Ket[kargs__],
				Power[gate[gargs__?(FreeQ[#,List]&)],expon_.]]]]:=
		Module[
			{kqbs,gqbs,klist,klistg,klistnotg,knog,kg,ftkg,knogftkg,resultado},
			kqbs=Cases[{kargs},zz080Operator[q_],Infinity];
			gqbs=Cases[{gargs},zz080Operator[q_],Infinity];
			klist=FactorKetList[QuantumEvaluate[zz080Ket[kargs]]];
			klistg=Cases[klist,
				zz080Ket[zz080Eigenstate[zz080Operator[b_],a_]]/;
					MemberQ[gqbs,zz080Operator[b]]];
			klistnotg=Complement[klist,klistg];
			resultado=If[TrueQ[And[IntegerQ[expon],Length[klistg]==Length[gqbs]]],
					knog=Apply[zz075NonCommutativeTimes,klistnotg];	
					kg=Apply[zz075NonCommutativeTimes,klistg];
					ftkg=zz075NonCommutativeTimes[kg,
								Power[qFast[gate],expon]];
					knogftkg=zz075NonCommutativeTimes[right,knog,ftkg];
					QuantumEvaluate[knogftkg],
				QuantumEvaluate[zz075NonCommutativeTimes[
					right,
					zz080Ket[kargs],
					Power[functiongate[gargs],expon]
					] ] ];
			Chop[CollectKet[Expand[resultado]]]
		];

		internalEvaluate[HoldPattern[
			zz075NonCommutativeTimes[
				right___,
				zz080Ket[kargs__],
				Power[zz080HermitianConjugate[
					gate[gargs__?(FreeQ[#,List]&)]],expon_.]]]]:=
		Module[
			{kqbs,gqbs,klist,klistg,klistnotg,knog,kg,ftkg,knogftkg,resultado},
			kqbs=Cases[{kargs},zz080Operator[q_],Infinity];
			gqbs=Cases[{gargs},zz080Operator[q_],Infinity];
			klist=FactorKetList[QuantumEvaluate[zz080Ket[kargs]]];
			klistg=Cases[klist,
				zz080Ket[zz080Eigenstate[zz080Operator[b_],a_]]/;
					MemberQ[gqbs,zz080Operator[b]]];
			klistnotg=Complement[klist,klistg];
			resultado=If[TrueQ[And[IntegerQ[expon],Length[klistg]==Length[gqbs]]],
					knog=Apply[zz075NonCommutativeTimes,klistnotg];	
					kg=Apply[zz075NonCommutativeTimes,klistg];
					ftkg=zz075NonCommutativeTimes[kg,
							Power[qFastHermitian[gate],expon]];
					knogftkg=zz075NonCommutativeTimes[right,knog,ftkg];
					QuantumEvaluate[knogftkg],
				QuantumEvaluate[zz075NonCommutativeTimes[
					right,
					zz080Ket[kargs],
					Power[zz080HermitianConjugate[functiongate[gargs]],expon]
					] ] ];
			Chop[CollectKet[Expand[resultado]]]
		];
	}];

	internalEvaluate[HoldPattern[
		zz075NonCommutativeTimes[
			right___,
			zz080Ket[kargs__],
			Power[gate[gargs__?(FreeQ[#,List]&)][params___],expon_.]]]]:=
	Module[
		{resultado},
		resultado=QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				zz080Ket[kargs],
				Power[functiongate[gargs][params],expon]
				] ];
		Chop[CollectKet[Expand[resultado]]]
	];

	internalEvaluate[HoldPattern[
		zz075NonCommutativeTimes[
			right___,
			zz080Ket[kargs__],
			Power[zz080HermitianConjugate[
				gate[gargs__?(FreeQ[#,List]&)][params___]],expon_.]]]]:=
	Module[
		{resultado},
		resultado=QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				zz080Ket[kargs],
				Power[zz080HermitianConjugate[functiongate[gargs][params]],expon]
				] ];
		Chop[CollectKet[Expand[resultado]]]
	];
	
	msg
];

SetQuantumGate[x_,nq:{_Integer,_Integer},qex_]:=
Module[{qewp, msg},
	msg=SetQuantumGate[x,nq];	
	(* QuantumEvaluate *)

	internalEvaluate[x]:=QuantumEvaluate[qex];
	
	internalEvaluate[HoldPattern[x[args___]]]:=
		With[{qexargs=qex[args]},
			If[TrueQ[unitaryQ[QuantumMatrix[qexargs]]],
				QuantumEvaluate[qexargs],
				Message[SetQuantumGate::nonu,x,qexargs];QuantumEvaluate[qexargs]
			]];

	internalEvaluate[HoldPattern[x[args___][params___]]]:=
		With[{qexargs=qex[args][params]},
			If[TrueQ[unitaryQ[QuantumMatrix[qexargs]]],
				QuantumEvaluate[qexargs],
				Message[SetQuantumGate::nonu,x,qexargs];QuantumEvaluate[qexargs]
			]];

	internalEvaluate[HoldPattern[
		zz075NonCommutativeTimes[
			right___,
			zz080Ket[kargs__],
			Power[x[args__?(FreeQ[#,List]&)],expon_.]]]]:=
		With[{qexargs=qex[args]},
			If[Not[TrueQ[unitaryQ[QuantumMatrix[qexargs]]]],
				Message[SetQuantumGate::nonu,x,qexargs]
			];
			QuantumEvaluate[
				zz075NonCommutativeTimes[
					right,
					zz080Ket[kargs],
					Power[qexargs,expon]]
		]];

	internalEvaluate[HoldPattern[
		zz075NonCommutativeTimes[
			right___,
			zz080Ket[kargs__],
			Power[x[args__?(FreeQ[#,List]&)][params___],expon_.]]]]:=
		With[{qexargsparams=qex[args][params]},
			If[Not[TrueQ[unitaryQ[QuantumMatrix[qexargs]]]],
				Message[SetQuantumGate::nonu,x,qexargs]
			];
			QuantumEvaluate[
				zz075NonCommutativeTimes[
					right,
					zz080Ket[kargs],
					Power[qexargsparams,expon]]
		]];

	internalEvaluate[HoldPattern[
		zz075NonCommutativeTimes[
			right___,
			zz080Ket[kargs__],
			Power[zz080HermitianConjugate[x[args__?(FreeQ[#,List]&)]],expon_.]]]]:=
		With[{qexargs=qex[args]},
			If[Not[TrueQ[unitaryQ[QuantumMatrix[qexargs]]]],
				Message[SetQuantumGate::nonu,x,qexargs]
			];
			QuantumEvaluate[
				zz075NonCommutativeTimes[
					right,
					zz080Ket[kargs],
					Power[zz080HermitianConjugate[qexargs],expon]]
		]];

	internalEvaluate[HoldPattern[
		zz075NonCommutativeTimes[
			right___,
			zz080Ket[kargs__],
			Power[zz080HermitianConjugate[x[args__?(FreeQ[#,List]&)][params___]],expon_.]]]]:=
		With[{qexargsparams=qex[args][params]},
			If[Not[TrueQ[unitaryQ[QuantumMatrix[qexargs]]]],
				Message[SetQuantumGate::nonu,x,qexargs]
			];
			QuantumEvaluate[
				zz075NonCommutativeTimes[
					right,
					zz080Ket[kargs],
					Power[zz080HermitianConjugate[ qexargsparams ],expon]]
		]];

	msg
];

(* Initial value of ReplaceAfterShifting
 it contains the replacements to be made after "gate shifting"
 of quantum gates in a quantum plot. A new replacement is
 added for each new quantum gate of one argument (qubit) in
order to allow the notation that plots several similar gates
in one column regardless of the gate shifting
*)
ReplaceAfterShifting=
	{{QuantumMeter[{args__}],col_}:>
					Sequence@@Map[{QuantumMeter[#],col}&,{args}]};
ReplaceBeforeShifting={};

SetQuantumGate[x_,nqbits:(_Integer|{_Integer,_Integer})]:=
Module[{nctwasp, objeto, werep, n1, n2},
	ClearAll[x];
	SetQuantumObject[x];

	If[IntegerQ[nqbits],
			n1=n2=nqbits,
			{n1,n2}=Sort[nqbits]
		];

	objeto=Which[
		Head[x] === Symbol,x,
		Head[Head[x]]===Symbol,Head[x],
		True,Print["ERROR: Could Not make Quantum Object "];
		Message[QuantumScalarQ::error];QuantumDebug["debug467"];Abort[]
	];

	werep = Unprotect[Evaluate[objeto]];
	With[{obj=objeto},
		subiwp=Unprotect[Subscript];
		Set[Subscript[x,args__],rhs_]^:=
			(Message[Set::wrsym,x];Abort[]);
		SetDelayed[Subscript[x,args__],rhs_]^:=
			(Message[SetDelayed::wrsym,x];Abort[]);
		Protect@@subiwp;
		obj /: MakeBoxes[x[args__],form_]:=MakeBoxes[Subscript[x,args],form];
		obj /: Subscript[x,args__]:=x[args];
		obj /: HoldPattern[x[args__]]:=
				(Message[SetQuantumGate::nqb,x,n1,n2,Length[{args}],{args}];
				Abort[])/;
			And[FreeQ[{args},\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR]],FreeQ[{args},registerList],
				Not[n1<=Length[{args}]<=n2]];
		obj /: HoldPattern[x[args__][params___]]:=
				(Message[SetQuantumGate::nqb,x,n1,n2,Length[{args}],{args}];
				Abort[])/;
			And[FreeQ[{args},\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR]],FreeQ[{args},registerList],
				Not[n1<=Length[{args}]<=n2]];
		obj /: HoldPattern[x[n_Integer]]:=
					If[n1===n2===1,
						x[Range[n]],
						Message[SetQuantumGate::notation1,x];Abort[]];
		obj /: HoldPattern[x[n_Integer][params___]]:=
					If[n1===n2===1,
						x[Range[n]][params],
						Message[SetQuantumGate::notation1,x];Abort[]];
		obj /: HoldPattern[x[{}]]:=
					If[n1===n2===1,
						1,
						Message[SetQuantumGate::notation1,x];Abort[]];
		obj /: HoldPattern[x[{}][params___]]:=
					If[n1===n2===1,
						1,
						Message[SetQuantumGate::notation1,x];Abort[]];

		obj /: HoldPattern[x[{args__Integer}]]:=
					If[n1===n2===1,
						x[Map[zz080Operator,{args}]],
						Message[SetQuantumGate::notation1,x];Abort[]];
		obj /: HoldPattern[x[{args__Integer}][params___]]:=
					If[n1===n2===1,
						x[Map[zz080Operator,{args}]][params],
						Message[SetQuantumGate::notation1,x];Abort[]];

		obj /: internalEvaluate[HoldPattern[x[{args__zz080Operator}]]]:=
					If[n1===n2===1,
						QuantumEvaluate[zz075NonCommutativeTimes@@Map[x,Reverse[{args}]]],
						Message[SetQuantumGate::notation1,x];Abort[]];
		obj /: internalEvaluate[HoldPattern[x[{args__zz080Operator}][params___]]]:=
					If[n1===n2===1,
						QuantumEvaluate[zz075NonCommutativeTimes@@Map[x[#][params]&,
																	Reverse[{args}]]],
						Message[SetQuantumGate::notation1,x];Abort[]];

		obj /: internalEvaluate[HoldPattern[x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]]]]:=
					If[n1===n2===1,
						QuantumEvaluate[x[registerList[args]]],
						QuantumEvaluate[x[Sequence@@registerList[args]]]];
		obj /: internalEvaluate[HoldPattern[x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]][params___]]]:=
					If[n1===n2===1,
						QuantumEvaluate[ x[registerList[args]][params] ],
						QuantumEvaluate[ x[Sequence@@registerList[args]][params] ]];

		If[Or[n1=!=1,n2=!=1],
			(obj /: HoldPattern[x[{args__zz080Operator}]]:=
				(Message[SetQuantumGate::notation1,x];Abort[]);
			obj /: HoldPattern[x[{args__zz080Operator}][params___]]:=
				(Message[SetQuantumGate::notation1,x];Abort[]) )
		];
	];
	Protect@@werep;

	AppendTo[ReplaceAfterShifting,
		{HoldPattern[Power[x[{args__}],expon_.]],col_}:>
					Sequence@@Map[{Power[x[#],expon],col}&,{args}]];

	AppendTo[ReplaceAfterShifting,
		{HoldPattern[Power[x[{args__}][params___],expon_.]],col_}:>
					Sequence@@Map[{Power[x[#][params],expon],col}&,{args}]];


	AppendTo[ReplaceAfterShifting,
		{HoldPattern[Power[zz080HermitianConjugate[x[{args__}]],expon_.]],col_}:>
					Sequence@@Map[{Power[zz080HermitianConjugate[x[#]],
									expon],col}&,{args}]];
	AppendTo[ReplaceAfterShifting,
		{HoldPattern[Power[zz080HermitianConjugate[x[{args__}][params___]],expon_.]],col_}:>
					Sequence@@Map[{Power[zz080HermitianConjugate[x[#][params]],
									expon],col}&,{args}]];


	AppendTo[ReplaceAfterShifting,
		{HoldPattern[
			zz020Controlled[
				Power[x[{args__}],expon_.], 
				other__]],col_}:>
		{zz020Controlled[Apply[zz075NonCommutativeTimes,
							Reverse[Map[Power[x[#],expon]&,{args}]]], 
			other],col}];

	AppendTo[ReplaceAfterShifting,
		{HoldPattern[
			zz020Controlled[
				Power[x[{args__}][params___],expon_.], 
				other__]],col_}:>
		{zz020Controlled[Apply[zz075NonCommutativeTimes,
							Reverse[Map[Power[x[#][params],expon]&,{args}]]], 
			other],col}];


	AppendTo[ReplaceAfterShifting,
		{HoldPattern[
			zz020Controlled[
				Power[zz080HermitianConjugate[x[{args__}]],expon_.], 
				other__]],col_}:>
		{zz020Controlled[Apply[zz075NonCommutativeTimes,
							Reverse[Map[Power[zz080HermitianConjugate[x[#]],expon]&,
										{args}]]], 
			other],col}];

	AppendTo[ReplaceAfterShifting,
		{HoldPattern[
			zz020Controlled[
				Power[zz080HermitianConjugate[x[{args__}][params___]],expon_.], 
				other__]],col_}:>
		{zz020Controlled[Apply[zz075NonCommutativeTimes,
							Reverse[Map[Power[zz080HermitianConjugate[x[#][params]],expon]&,
										{args}]]], 
			other],col}];


	If[n1===n2===1,
		{AppendTo[ReplaceBeforeShifting,
			x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]]:>x[registerList[args]] ];
		AppendTo[ReplaceBeforeShifting,
			x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]][params___]:>x[registerList[args]][params] ]},

		{AppendTo[ReplaceBeforeShifting,
			x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]]:>x[Sequence@@registerList[args]] ];
		AppendTo[ReplaceBeforeShifting,
			x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]][params___]:>x[Sequence@@registerList[args]][params] ]}
	];

	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[x[{args__zz080Operator}],expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				zz075NonCommutativeTimes@@Map[Power[x[#],expo]&,Reverse[{args}]]]],
			Message[SetQuantumGate::notation1,x];Abort[]];

	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[zz080HermitianConjugate[x[{args__zz080Operator}]],expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				zz075NonCommutativeTimes@@Map[
					Power[zz080HermitianConjugate[x[#]],expo]&,
					Reverse[{args}]]]],
			Message[SetQuantumGate::notation1,x];Abort[]];

	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]],expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[x[registerList[args]],expo] ]],
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[x[Sequence@@registerList[args]],expo] ]]];

	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[zz080HermitianConjugate[x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]]],expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[zz080HermitianConjugate[x[registerList[args]]],expo] ]],
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[zz080HermitianConjugate[x[Sequence@@registerList[args]]],expo] ]]];


	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[ x[{args__zz080Operator}][params___] ,expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				zz075NonCommutativeTimes@@Map[Power[x[#][params],expo]&,Reverse[{args}]]]],
			Message[SetQuantumGate::notation1,x];Abort[]];

	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[zz080HermitianConjugate[
					x[{args__zz080Operator}][params___] ],expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				zz075NonCommutativeTimes@@Map[
					Power[zz080HermitianConjugate[x[#][params]],expo]&,
					Reverse[{args}]]]],
			Message[SetQuantumGate::notation1,x];Abort[]];

	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]][params___],expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[x[registerList[args]][params] ,expo] ]],
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[ x[Sequence@@registerList[args]][params] ,expo] ]]];

	internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right__,
			Power[zz080HermitianConjugate[
					x[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]][params___] ],expo_.]
		]]]:=
	If[n1===n2===1,
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[zz080HermitianConjugate[
						x[registerList[args]][params] ],expo] ]],
			QuantumEvaluate[zz075NonCommutativeTimes[
				right,
				Power[zz080HermitianConjugate[
						x[Sequence@@registerList[args]][params] ],expo] ]]];


	nctwasp=Unprotect[zz075NonCommutativeTimes];
	zz075NonCommutativeTimes /: QuantumEvaluate[ HoldPattern[zz075NonCommutativeTimes[
			after___,
			zz080Ket[ei:zz080Eigenstate[zz080Operator[opket_],evket_],restket___],
			g:Power[x[argx__],expo_.],
			before___
		]] ]:=
		QuantumEvaluate[ zz075NonCommutativeTimes[
			after,
			zz080Ket[restket],
			g,
			zz080Ket[ei],
			before
		] ]/;
	And[Not[FreeQ[g,zz080Operator]],FreeQ[g,zz080Operator[opket]]];

	zz075NonCommutativeTimes /: QuantumEvaluate[ HoldPattern[zz075NonCommutativeTimes[
			after___,
			zz080Ket[ei:zz080Eigenstate[zz080Operator[opket_],evket_],restket___],
			g:Power[zz080HermitianConjugate[x[argx__]],expo_.],
			before___
		]] ]:=
		QuantumEvaluate[ zz075NonCommutativeTimes[
			after,
			zz080Ket[restket],
			g,
			zz080Ket[ei],
			before
		] ]/;
	And[Not[FreeQ[g,zz080Operator]],FreeQ[g,zz080Operator[opket]]];
	
	zz075NonCommutativeTimes /: QuantumEvaluate[ HoldPattern[zz075NonCommutativeTimes[
			after___,
			g:Power[x[argx__],expo_.],
			zz080Bra[ei:zz080Eigenstate[zz080Operator[opket_],evket_],restket___],
			before___
		]] ]:=
		QuantumEvaluate[ zz075NonCommutativeTimes[
			after,
			zz080Bra[ei],
			g,
			zz080Bra[restket],
			before
		] ]/;
	And[Not[FreeQ[g,zz080Operator]],FreeQ[g,zz080Operator[opket]]];

	zz075NonCommutativeTimes /: QuantumEvaluate[ HoldPattern[zz075NonCommutativeTimes[
			after___,
			g:Power[zz080HermitianConjugate[x[argx__]],expo_.],
			zz080Bra[ei:zz080Eigenstate[zz080Operator[opket_],evket_],restket___],
			before___
		]] ]:=
		QuantumEvaluate[ zz075NonCommutativeTimes[
			after,
			zz080Bra[ei],
			g,
			zz080Bra[restket],
			before
		] ]/;
	And[Not[FreeQ[g,zz080Operator]],FreeQ[g,zz080Operator[opket]]];
	Protect@@nctwasp;

	If[n1===n2,
		"The expression "<>ToString[x]<>" is"<>
		" a quantum gate of "<>ToString[n1]<>" arguments (qubits)",
		"The expression "<>ToString[x]<>" is"<>
		" a quantum gate with a number na of arguments (qubits)"<>
		" such that "<>ToString[n1]<>"\[LessEqual]na\[LessEqual]"<>ToString[n2]
	]
];

SetQuantumObject[{
	zz020TensorPower,zz020MultiQubit,zz020Controlled,
	QubitMeasurement,
	QuantumMeter,
	DecToQubit,\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR],
	PauliExpand,\[ScriptK]\[ScriptE]\[ScriptT],QuantumEvaluate, internalEvaluate
}];

(* Properties of QubitList *)

QubitList /: Rule[QubitList,zz080Operator[qb_]]:=
	Rule[QubitList,{zz080Operator[qb]}];

QubitList /: Rule[QubitList,n_Integer]:=
	Rule[QubitList,Range[n]];

QubitList /: Rule[QubitList,{before___,m:Except[zz080Operator[_]],after___}]:=
	Rule[QubitList,{before,zz080Operator[m],after}];

QubitList /: RuleDelayed[QubitList,zz080Operator[qb_]]:=
	RuleDelayed[QubitList,{zz080Operator[qb]}];

QubitList /: RuleDelayed[QubitList,n_Integer]:=
	RuleDelayed[QubitList,Range[n]];

QubitList /: RuleDelayed[QubitList,
				{before___,m:Except[zz080Operator[_]],after___}]:=
	RuleDelayed[QubitList,{before,zz080Operator[m],after}];

(* This code is necessary for compatibity with old versions,
so that some TagBoxes generated with old versions of Quantum are
correctly interpreted *)
zz020TP[args___]:=Identity[args];
zz020TPdat[args___]:=Identity[args];
MakeExpression[
	TagBox[
		RowBox[{
			"\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP]",
			"[",
			TagBox[
				arg_,
				zz020TPdat,Editable->True,Selectable->True],
			"]"}],
		zz020TP,Editable->False,Selectable->False],
  	form_]:=
	MakeExpression[RowBox[{"\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP]","[",arg,"]"}],form];
MakeExpression[
	TagBox[
		RowBox[{
			"\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI]",
			"[",
			TagBox[
				arg_,
				zz020TPdat,Editable->True,Selectable->True],
			"]"}],
		zz020TP,Editable->False,Selectable->False],
  	form_]:=
	MakeExpression[RowBox[{"\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI]","[",arg,"]"}],form];
MakeExpression[
	TagBox[
		RowBox[{
			"\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN]",
			"[",
			TagBox[
				arg_,
				zz020TPdat,Editable->True,Selectable->True],
			"]"}],
		zz020TP,Editable->False,Selectable->False],
  	form_]:=
MakeExpression[RowBox[{"\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN]","[",arg,"]"}],form];
(* end of code for compatibility with old versions *)

(*Some auxiliar functions, including MultiQubit by Francisco Delgado*)

Unprotect[qwire];
Clear[qwire];
Protect[qwire];

zz020CeroOneQ[x_]:=
If[Or[x===1,x===0],True,False];

zz020TwoScalarsListQ[{x_,y_}]:=QuantumScalarQ[x]&&QuantumScalarQ[y];

zz020TwoScalarsListQ[_]:=False;

zz020MultiQubit[state:{(_?zz020CeroOneQ|_?zz020TwoScalarsListQ)..},
			channelsList:{__}]:=
Module[{tensorproduct,i,n},
	tensorproduct=
		If[zz020CeroOneQ[ state[[1]] ],
			zz080Ket[zz080Eigenstate[
						zz080Operator[ channelsList[[1]] ],state[[1]] ]],
			state[[1,1]]*			
			zz080Ket[zz080Eigenstate[
						zz080Operator[ channelsList[[1]] ],0 ]]+
			state[[1,2]]*			
			zz080Ket[zz080Eigenstate[
						zz080Operator[ channelsList[[1]] ], 1 ]]  ];
	n=Length[state];
	For[i=2,i<=n,i++,
		tensorproduct=
			zz075NonCommutativeTimes[
				If[zz020CeroOneQ[ state[[i]] ],
					zz080Ket[zz080Eigenstate[
								zz080Operator[ channelsList[[i]] ],
											state[[i]] ]],
					state[[i,1]]*			
					zz080Ket[zz080Eigenstate[
								zz080Operator[ channelsList[[i]] ],
											0 ]]	
					+state[[i,2]]*			
					zz080Ket[zz080Eigenstate[
								zz080Operator[ channelsList[[i]] ],
											1 ]]  ],
				tensorproduct]
	];
	tensorproduct
];

(* Tensor Power *)

zz020TensorPower[graph:((Graphics|Graphics3D)[___]), exponents__]:=
With[{qinfo=Cases[graph, Tooltip[g_List, e_] :> e, Infinity]}, 
	zz020TensorPower[Part[qinfo,1], exponents] /;
	Length[qinfo]===1
];

zz020TensorPower[anything_,0]:=1;

zz020TensorPower[anything_,{}]:=1;

(*If the exponent is an operator (qubit), it becomes a 
list containing that operator*)
zz020TensorPower[anything_,expo_zz080Operator]:=
	zz020TensorPower[anything,{expo}];

(*If the exponent is a positive integer n,
  then the qubits are {1,2,3,...,n-1,n}*)
zz020TensorPower[anything_,expo_Integer]:=
	zz020TensorPower[anything,Range[expo]] /; expo>0;

(*Register notation   qpower
the pattern _ \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] does not work properly after
modifiying and saving this file (bug in Mathematica's editor),
therefore we had to use _?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &) *)
zz020TensorPower[anything_,reg_?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &)]:=
	zz020TensorPower[anything,QuantumEvaluate[reg]];

(*Register notation for QuantumProduct
the pattern _ \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] does not work properly after
modifiying and saving this file (bug in Mathematica's editor),
therefore we had to use _?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &) *)
qprowasp=Unprotect[QuantumProduct];
QuantumProduct[anything_,
	{before___, reg_?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &), after___}]:=
	QuantumProduct[anything,{before, QuantumEvaluate[reg], after}];
Protect@@qprowasp;

(* Tensor power of a normal ket on the specified qubits,
   the operator becomes an offset common to all channels *)
zz020TensorPower[zz080Ket[zz080Eigenstate[zz080Operator[op_], ev_?zz020CeroOneQ]],
				{channels__}]:=
	zz020MultiQubit[Table[ev,{Length[{channels}]}],
					{channels}+op-1];

(* Tensor power of a "generalized" ket on the specified qubits *)
zz020TensorPower[zz080Ket[zz080Eigenstate[zz080Operator[op_], 
   state:{(_?zz020CeroOneQ|_?zz020TwoScalarsListQ)..}]], {channels__}]:=
	zz020MultiQubit[state,{channels}+op-1];

zz020TensorPower[zz080Ket[args__], {channels__}] :=        
 zz075NonCommutativeTimes@@ 
   Reverse@Map[zz020TensorPower[zz080Ket[#], {channels}] &, {args}] /; (Length[{args}] > 1);

(* Tensor power of a Bra on the specified channels *)
zz020TensorPower[zz080Bra[braargs__],{channels__}]:=
	zz080HermitianConjugate[
		zz020TensorPower[zz080Ket[braargs],{channels}]];

(* Tensor power of a normal (commutative) multiplication with a scalar*)
zz020TensorPower[Times[a_?QuantumScalarQ,b_],{channels__}]:=
	Times[Power[a,Length[{channels}]],
			zz020TensorPower[b,{channels}]];

(* Tensor power of a quantum (noncommutative) multiplication.
Notice that b can be one or several arguments b__ *)
zz020TensorPower[HoldPattern[zz075NonCommutativeTimes[a_,b__]], 
				{channels__}] :=
 zz075NonCommutativeTimes@@Reverse@Map[
 	zz020TensorPower[zz075NonCommutativeTimes[a, b], {#1}] &, {channels}]/;
(Length[{channels}]>1);

(* Notice that b can be one or several arguments b__ *)
zz020TensorPower[HoldPattern[zz075NonCommutativeTimes[a_,b__]], {n_}] := 
	zz075NonCommutativeTimes[zz020TensorPower[a, {n}], 
							zz020TensorPower[zz075NonCommutativeTimes[b], {n}]];

(* Tensor power of a sum. Remember that a "normal" Power is
   a "normal" Times (for example FullForm[Times[a,a,a]]->Power[a,3]).
   In a similar (inverse) way, a TensorPower is actually an
   abreviated NonCommutativeTimes, but in different channels (subspaces). 
   Therefore, in the tensor power of a sum, the equivalent of
   Newton's binomial expantion is implemented, with the extra difficulty
   that the channels have to be correctly assigned.
   In other words, the fact that ab is not the same as ba is
   not encoded in the order, but in the channel.
*)
zz020TensorPower[Plus[a_,b_],{channels__}]:=
Module[{expo,tuplas,myf,tuplasPow,prodlist,j},
	expo=Length[{channels}];
	If[$VersionNumber>=5.2,
		tuplas=Tuples[{a,b},expo],
		tuplas=Table[IntegerDigits[j,2,expo],{j,0,2^expo -1}]/.{0->a,1->b}
	];
	myf[qobj_,{listNumber_,elementNumber_}]:=
    	zz020TensorPower[qobj,{ {channels}[[elementNumber]] } ];
	tuplasPow=MapIndexed[myf,tuplas,{2}];
	prodlist=zz075NonCommutativeTimes @@@ Reverse /@ tuplasPow;
	Apply[Plus,prodlist]
];

(*Tensor power of a symbol that is assumed to be a Quantum Gate *)
zz020TensorPower[
	f_Symbol[fbefore___?(FreeQ[#,zz080Operator]&),
			fqubits__zz080Operator,
			fafter___?(FreeQ[#,zz080Operator]&)], 
	{channels__}] :=
 Module[{lista, fq2, lista2},
  fq2 = {fqubits} /. zz080Operator :> Sequence;
  lista = Map[fq2 - 1 + # &, {channels}];
  lista2 = Map[zz080Operator, lista, {2}];
  Apply[zz075NonCommutativeTimes, Reverse[Apply[f[fbefore,##,fafter]&, lista2, {1}]]]];

(*Tensor power of a symbol that is assumed to be a 
    PARAMETRIC Quantum Gate *)
zz020TensorPower[
	f_Symbol[fbefore___?(FreeQ[#,zz080Operator]&),
			fqubits__zz080Operator,
			fafter___?(FreeQ[#,zz080Operator]&)][params___], 
	{channels__}] :=
 Module[{lista, fq2, lista2},
  fq2 = {fqubits} /. zz080Operator :> Sequence;
  lista = Map[fq2 - 1 + # &, {channels}];
  lista2 = Map[zz080Operator, lista, {2}];
  Apply[zz075NonCommutativeTimes, 
	Reverse[Apply[f[fbefore,##,fafter][params]&, lista2, {1}]]]];

(*Tensor power of a controlled Quantum Gate*)
zz020TensorPower[
	zz020Controlled[f_Symbol[fqubits__zz080Operator], {controlqubits__zz080Operator}], 
	{channels__}]:=
Module[{listadata,listacontrol,fq2,cq2,ld2,lc2},
  	fq2 = {fqubits} /. zz080Operator :> Sequence;
	listadata=Map[fq2-1+#&,{channels}];
	cq2 = {controlqubits} /. zz080Operator :> Sequence;
	listacontrol=Map[cq2-1+#&,{channels}];
	ld2 = Map[zz080Operator, listadata, {2}];
	lc2 = Map[zz080Operator, listacontrol, {2}];
	Apply[zz075NonCommutativeTimes,
		Reverse[MapThread[zz020Controlled[f[Sequence@@#1],{Sequence@@#2}]&,
					{ld2,lc2}]]]
];

(* controlled gates "algebra" *)

HoldPattern[zz020Controlled[gate_,{}]]:=gate;

HoldPattern[zz020Controlled[gate_,
		entero_Integer]]:=
	zz020Controlled[gate,Range[entero]];

HoldPattern[zz020Controlled[gate_,
		ope_zz080Operator]]:=
	zz020Controlled[gate,{ope}];

HoldPattern[zz020Controlled[anything_,{before___,
		n_?(And[Head[#]=!=zz080Operator,Head[#]=!=qwire]&),after___}]]:=
	zz020Controlled[anything,{before,zz080Operator[n],after}];

HoldPattern[zz020Controlled[g_,{c__}]]:=
With[{controlledqbits=Cases[g,_zz080Operator,Infinity,Heads->True]},
	(* weird With behavior: second argument cannot be 
	a compound expresion a;b;c it was necessary to make it
	one expression {a;b;c}, otherwise a; and b; get evaluated
	even if c doesn't, very weird*)
	{Message[zz020Controlled::rptdqb]; Abort[]} /;
	Intersection[{c},controlledqbits]=!={}
];

HoldPattern[zz020Controlled[g_,{c__}]]:=
With[{sorted=Union[{c}]},
	zz020Controlled[g,sorted] /; sorted=!={c}
];

HoldPattern[zz020Controlled[zz020Controlled[g_,{c1__}],{c2__}]]:=
	zz020Controlled[g,{c2,c1}];

zz020Controlled /: HoldPattern[zz080HermitianConjugate[zz020Controlled[g_,{c__}]]]:=
	zz020Controlled[zz080HermitianConjugate[g],{c}];

zz020Controlled /: HoldPattern[Power[zz020Controlled[g_,{c__}],expon_.]]:=
	zz020Controlled[Power[g,expon],{c}];



(* Truth Tables *)

SetAttributes[QuantumTableForm,Listable];

QuantumTableForm[op__,opts:OptionsPattern[]]:=
Module[{ctt},
	ctt=QuantumTable[op,opts];
	TableForm[	ctt,
				TableSpacing->{1,3},
				TableHeadings->{Range[Length[ctt]]-1,{"  Input","  Output"}}]
];

SetAttributes[QuantumTable,Listable];

QuantumTable[graph:((Graphics|Graphics3D)[___]),opts:OptionsPattern[]]:=
With[{qinfo=Cases[graph, Tooltip[g_List, e_] :> e, Infinity]},
	QuantumTable[Part[qinfo,1],opts] /;
	Length[qinfo]===1
];

QuantumTable[ex_,opts:OptionsPattern[]]:=
Module[{nqubits,j,k,listaQubits,lines,ta,
		orderedQubits,expressionQubits,extraQubits},
   
	(* Next we take advantage of the OneIdentity attribute of zz080Operator
	 so that the userlist works the same if the user writes or not
	 the qubits inside the qubit template (zz080Operator) *)
	orderedQubits=Map[zz080Operator,OptionValue[QubitList]];
	(* The ordered list of qubits is created, including those qubits whose order
	 was specified by the user and those qubits whose order wasn't specified*) 
	expressionQubits = 
		Union[Cases[{ex /. ReplaceBeforeShifting }, zz080Operator[_], Infinity, Heads->True]];
	extraQubits = Select[expressionQubits, Not[MemberQ[orderedQubits, #]] &];
	listaQubits = Join[orderedQubits, extraQubits];
	nqubits=Length[listaQubits];

	ta=If[	NumericQ[nqubits],
		Table[   k=DecToQubit[j,listaQubits]; 
				{k,QuantumEvaluate[zz075NonCommutativeTimes[k,ex]]},
				{j,0,(2^nqubits)-1,1}],
		"Cannot create truth table"
	];
	
	ta
];

(* Quantum gates and ket-bras to SparseArrays, Tensors and Matrices*)

TensorQuantum[ten_?ArrayQ,opts:OptionsPattern[]]:=
	MatrixQuantum[FixedPoint[ArrayFlatten,ten],opts];

MatrixQuantum[vec_?VectorQ,opts:OptionsPattern[]]:=
	MatrixQuantum[Map[List,vec],opts];

MatrixQuantum[mat_?MatrixQ,opts:OptionsPattern[]]:=
Module[{orderedQubits, rules, dimen, j,
	expressionQubits, extraQubits, listaQubits,
	nqubits},

	(* Next we take advantage of the OneIdentity attribute of zz080Operator
	 so that the userlist works the same if the user writes or not
	 the qubits inside the qubit template (zz080Operator) *)
	orderedQubits=Map[zz080Operator,OptionValue[QubitList]];
	(* The ordered list of qubits is created, including those qubits whose order
	 was specified by the user and those qubits whose order wasn't specified*) 
	expressionQubits = Table[zz080Operator[j],{j,Log[ 2, Dimensions[mat][[1]] ]}];
	extraQubits = Select[expressionQubits, Not[MemberQ[orderedQubits, #]] &];
	listaQubits = Join[orderedQubits, extraQubits];
	nqubits=Length[listaQubits];

	rules=MapIndexed[zz080Operator[First[#2]]->zz080Operator[#1]&,listaQubits];

	dimen=Table[2,{nqubits}];

	If[Length[rules]===0,
		MatrixToDirac[mat,dimen],
		MatrixToDirac[mat,dimen,rules]]
];

SetAttributes[QuantumSparseArray,Listable];

QuantumSparseArray[arg_,opts:OptionsPattern[]]:=
	SparseArray[QuantumTensor[arg,opts]];

SetAttributes[QuantumMatrixForm,Listable];

QuantumMatrixForm[arg_,opts:OptionsPattern[]]:=
	MatrixForm[QuantumMatrix[arg,opts]];

SetAttributes[QuantumMatrix,Listable];

QuantumMatrix[n_?NumericQ,rest___]:=n;

QuantumMatrix[arg_,opts:OptionsPattern[]]:=
Module[{ct},
 ct=QuantumTensor[arg,opts];
 If[$VersionNumber>=6.0, 
 	NestWhile[ArrayFlatten,ct,(UnsameQ[#1,#2]&&Head[#2]=!=ArrayFlatten)&,2,1024],
 	Print["Not implemented for Mathematica versions before version 6"]; ct
 ]
];

SetAttributes[QuantumTensorForm,Listable];

QuantumTensorForm[arg_,opts:OptionsPattern[]]:=
	MatrixForm[QuantumTensor[arg,opts]];

SetAttributes[QuantumTensor,Listable];

QuantumTensor[ex_,opts:OptionsPattern[]]:=
Module[{orderedQubits,expressionQubits,extraQubits,
		listaQubits,nqubits,lista,evex},

	evex=If[FreeQ[ex,\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]],QuantumEvaluate[ex],ex];

	(* Next we take advantage of the OneIdentity attribute of zz080Operator
	 so that the userlist works the same if the user writes or not
	 the qubits inside the qubit template (zz080Operator) *)
	orderedQubits=Map[zz080Operator,OptionValue[QubitList]];
	(* The ordered list of qubits is created, including those qubits whose order
	 was specified by the user and those qubits whose order wasn't specified*) 
	expressionQubits = Union[Cases[{evex /. ReplaceBeforeShifting }, zz080Operator[_], Infinity,Heads->True]];
	extraQubits = Select[expressionQubits, Not[MemberQ[orderedQubits, #]] &];
	listaQubits = Join[orderedQubits, extraQubits];
	nqubits=Length[listaQubits];

	lista=listaQubits /. zz080Operator[ope_]:> 
		{zz080Eigenstate[zz080Operator[ope], 0], zz080Eigenstate[zz080Operator[ope], 1]};
	DiracToTensor[evex,lista]
];

(* For n eigenvalues. The default value n=0 gives all eigenvalues *)
QuantumEigensystemForm[ex_,n_:0,opts:OptionsPattern[]]:=
Module[{ctt},
	ctt=Join[{{"Eigenvalue","Eigenvector"}},
			Transpose[QuantumEigensystem[ex,n,
						FilterRules[{opts},Options[QuantumEigensystem]]]]];
	Grid[ctt,Dividers->All,FilterRules[{opts},Options[Grid]]]
];


(* For n eigenvalues. The default value n=0 gives all eigenvalues *)
QuantumEigensystem[ex_,n_:0,opts:OptionsPattern[]]:=
Module[{orderedQubits,expressionQubits,extraQubits,
		listaQubits,nqubits,lista,evex},

	(*evex=If[FreeQ[ex,\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]],QuantumEvaluate[ex],ex];*)
	evex=ex;
	
	(* Next we take advantage of the OneIdentity attribute of zz080Operator
	 so that the userlist works the same if the user writes or not
	 the qubits inside the qubit template (zz080Operator) *)
	orderedQubits=Map[zz080Operator,OptionValue[QubitList]];
	(* The ordered list of qubits is created, including those qubits whose order
	 was specified by the user and those qubits whose order wasn't specified*) 
	expressionQubits = Union[Cases[{evex /. ReplaceBeforeShifting }, zz080Operator[_], Infinity,Heads->True]];
	extraQubits = Select[expressionQubits, Not[MemberQ[orderedQubits, #]] &];
	listaQubits = Join[orderedQubits, extraQubits];
	nqubits=Length[listaQubits];

	lista=listaQubits /. zz080Operator[ope_]:> 
		{zz080Eigenstate[zz080Operator[ope], 0], zz080Eigenstate[zz080Operator[ope], 1]};
	DiracEigensystem[evex,lista,n,FilterRules[{opts},Options[DiracEigensystem]]]
];



(* PauliExpand: gates to Pauli matrices *)

SetAttributes[PauliExpand,Listable];

PauliExpand[graph:((Graphics|Graphics3D)[___]),opts:OptionsPattern[]]:=
With[{qinfo=Cases[graph, Tooltip[g_List, e_] :> e, Infinity]}, 
	PauliExpand[Part[qinfo,1],opts] /;
	Length[qinfo]===1
];

(* Expand both sides of an equation *)
PauliExpand[a_==b_,opts:OptionsPattern[]]:=
	(PauliExpand[a,opts]==PauliExpand[b,opts]);

(* Expand commutators *)
PauliExpand[HoldPattern[zz050Commutator[args__]],opts:OptionsPattern[]]:=
	PauliExpand[EvaluateAllCommutators[zz050Commutator[args]],opts];

(* Expand anticommutators *)
PauliExpand[HoldPattern[zz050AntiCommutator[args__]],opts:OptionsPattern[]]:=
	PauliExpand[EvaluateAllCommutators[zz050AntiCommutator[args]],opts];

(* Multiplication with a scalar that is NOT a braket *)
PauliExpand[a_?(And[QuantumScalarQ[#],Head[#]=!=zz075NonCommutativeTimes]&)*b_,
		opts:OptionsPattern[]]:=
	Expand[a*PauliExpand[b,opts]];

(* A scalar that is NOT a braket *)
PauliExpand[a_?(And[QuantumScalarQ[#],Head[#]=!=zz075NonCommutativeTimes]&),
		opts:OptionsPattern[]]:=a;

(* Addition of quantum gates (they do Not have ket-bra) *)
PauliExpand[a_?(And[FreeQ[#,zz080Ket],FreeQ[#,zz080Bra]]&)+
			b_?(And[FreeQ[#,zz080Ket],FreeQ[#,zz080Bra]]&),
			opts:OptionsPattern[]]:=
	PauliExpand[a,opts]+PauliExpand[b,opts];

(* Power of quantum gates (they do Not have ket-bra) *)
PauliExpand[(a_?(And[FreeQ[#,zz080Ket],FreeQ[#,zz080Bra]]&))^n_Integer,
			opts:OptionsPattern[]]:= 
	Expand[PauliExpand[a,opts]^n]; 
 
(* Noncommutative aplication of gates (they do Not have ket-bra) *)
PauliExpand[HoldPattern[
	zz075NonCommutativeTimes[x__?(And[FreeQ[#,zz080Ket],FreeQ[#,zz080Bra]]&)]],
	opts:OptionsPattern[]]:=
Module[{lista,primero,resto},
	lista=Map[PauliExpand[#,opts]&,{x}];
	primero=First[lista];
	resto=Rest[lista];
	Fold[Expand[zz075NonCommutativeTimes[#1,#2]]&,primero,resto]
];

(* PauliExpand based on code by Francisco Delgado *)
PauliExpand[HH_,opts:OptionsPattern[]]:=
Module[{H,matH,n,ev,h,d,invd,n2,n4,i,j,b,sub,matb,diagmatb,coefb,
		expressionQubits,rules,hrules},
	H=QuantumEvaluate[HH];
	expressionQubits = Union[Cases[{H /. ReplaceBeforeShifting }, zz080Operator[_], Infinity,Heads->True]];
	rules = MapIndexed[(zz080Operator@@(#2))->#1&,expressionQubits];
	matH=QuantumMatrix[H];
	If[NumericQ[matH],Return[matH]];
	If[Not[MatrixQ[matH,QuantumScalarQ]],Return[HH]];
	If[Not[TrueQ[unitaryQ[matH]]],Message[PauliExpand::nonunit,HH];Abort[]];
	n=Log[2, Length[matH]];
	ev=Eigenvalues[matH];
	h=0;
	d=Simplify[Inverse[Transpose[Eigenvectors[matH]]]];
	invd=Inverse[d];
	n2=2^n;
	n4=4^n;
	For[i=0,i<=n4-1,i++,
		sub=IntegerDigits[i,4,n];
		b[sub]=QuantumProduct[
			Which[sub[[j]] == 0, \[Sigma][0, zz080Operator[j]], 
				sub[[j]] == 1, \[Sigma][1, zz080Operator[j]], 
				sub[[j]] == 2, \[Sigma][2, zz080Operator[j]],
				sub[[j]] == 3, \[Sigma][3, zz080Operator[j]]], {j, 1, n}];
		matb=QuantumMatrix[QuantumEvaluate[b[sub]]];
		diagmatb=d.matb.invd;
		coefb=Simplify[(Sum[ev[[k]]diagmatb[[k,k]],{k,1,n2}])/n2];
		h=h+coefb b[sub] ]; 
	hrules=h/.rules;
	If[TrueQ[OptionValue[PauliIdentities]],
		hrules,
		hrules //. \[Sigma][0, _]:>1  ]
];

(**********************************************************)
(* Quantum Evaluation: gates to ket-bras QuantumEvaluate *)
(**********************************************************)

QuantumEvaluate[graph:((Graphics|Graphics3D)[___]),opts:OptionsPattern[]]:=
With[{qinfo=Cases[graph, Tooltip[g_List, e_] :> e, Infinity]}, 
	QuantumEvaluate[Part[qinfo,1],opts] /;
	Length[qinfo]===1
];

internalEvaluate[graph:((Graphics|Graphics3D)[___]),opts:OptionsPattern[]]:=
With[{qinfo=Cases[graph, Tooltip[g_List, e_] :> e, Infinity]}, 
	QuantumEvaluate[Part[qinfo,1],opts] /;
	Length[qinfo]===1
];


QuantumEvaluate[HoldPattern[zz075NonCommutativeTimes[arg1_,arg2_,args__]]]:=
Module[{},
	Fold[CollectKet[Expand[
			internalEvaluate[zz075NonCommutativeTimes[#1,#2]]]]&,
		arg1,{arg2,args}]
];

QuantumEvaluate[anyother_]:=CollectKet[Expand[internalEvaluate[anyother]]];

internalEvaluate[\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR][args___]]:=QuantumEvaluate[registerList[args]];

internalEvaluate[HoldPattern[
	zz075NonCommutativeTimes[right_?(And[FreeQ[#,internalEvaluate],FreeQ[#,zz080Ket]]&),
							left_?(FreeQ[#,internalEvaluate]&)]]]:=
	Module[{ier,iel},
		ier=QuantumEvaluate[right];
		If[ier=!=right,
			QuantumEvaluate[zz075NonCommutativeTimes[ier,left]],
			iel=QuantumEvaluate[left];
			If[iel=!=left,
				QuantumEvaluate[zz075NonCommutativeTimes[right,iel]],
				zz075NonCommutativeTimes[right,left]
			]
		]
	];

(* checking for ketq of hermitianconjugate of bra 
instead of just checking that the head is zz080bra is
better because it includes linear superpositions of bras *)
internalEvaluate[bra_]:=
With[{conjugate=zz080HermitianConjugate[bra]},
zz080HermitianConjugate[internalEvaluate[conjugate]]/;KetQ[conjugate]];

internalEvaluate[HoldPattern[zz080HermitianConjugate[x_]]]:=
	zz080HermitianConjugate[QuantumEvaluate[x]];

internalEvaluate[suma_Plus]:=Map[QuantumEvaluate,suma];

internalEvaluate[mult_Times]:=Map[QuantumEvaluate,mult];

internalEvaluate[equ_Equal]:=Map[QuantumEvaluate,equ];

internalEvaluate[a_^n_Integer]:= Expand[QuantumEvaluate[a]^n]; 

internalEvaluate[expresion_]:=
	With[{expanded=Expand[expresion]},
		internalEvaluate[expanded]
		/;expanded=!=expresion]

internalEvaluate[HoldPattern[
	zz075NonCommutativeTimes[zz080Bra[bargs__], zz080Ket[kargs__]]]]:=
	zz075NonCommutativeTimes[zz080Bra[bargs], zz080Ket[kargs]];

internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
		right___,
		k_?KetQ,
		cg:zz020Controlled[ 
			Power[ controlledgate_, expo_.],
			{controlqbits__zz080Operator} ]]]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[right,k,preEvaluate[cg]]];

internalEvaluate[HoldPattern[cg:zz020Controlled[ 
		Power[ controlledgate_, expo_.],
					{controlqbits__zz080Operator} ]]]:=QuantumEvaluate[preEvaluate[cg]];

preEvaluate[ HoldPattern[zz020Controlled[ 
		Power[ controlledgate_, expo_.],
					{controlqbits__zz080Operator} ]] ]:=
Module[
	{   controlledchannels,controlchannels,
		ncontrolqbits,ncontrolledqbits,
		ones,offcombinations,oncombinations,
		offreprules,onreprules,a,j,k,
		offketbralist,onketbralist,
		offoperator,onoperator,
		identityoperator,controlledoperator,
		resultado},

	controlledchannels=Cases[controlledgate,_zz080Operator,Infinity,Heads->True];
	controlchannels:={controlqbits};
	
	ncontrolqbits=Length[controlchannels];
	ncontrolledqbits=Length[controlledchannels];

	ones=Table[1,{ncontrolqbits}];

	offcombinations=
		DeleteCases[
			If[$VersionNumber>=5.2,
				Tuples[{0,1},ncontrolqbits],
				Table[IntegerDigits[j,2,ncontrolqbits],{j,0,2^ncontrolqbits -1}]
			],
			ones];
	oncombinations={ones};

	offreprules=
		Table[	a[ controlchannels[[k]] ] -> offcombinations[[j,k]],
				{j,1,Length[offcombinations]},{k,1,ncontrolqbits}];
	onreprules=
		Table[	a[ controlchannels[[k]] ] -> oncombinations[[j,k]],
				{j,Length[oncombinations]},{k,ncontrolqbits}];

	offketbralist=
		Map[zz075NonCommutativeTimes[
				zz080Bra[zz080Eigenstate[zz080Operator[#],a[#] ]],
				zz080Ket[zz080Eigenstate[zz080Operator[#],a[#] ]]]&,
			controlchannels];
	onketbralist=
		Map[zz075NonCommutativeTimes[
				zz080Bra[zz080Eigenstate[zz080Operator[#],a[#] ]],
				zz080Ket[zz080Eigenstate[zz080Operator[#],a[#] ]]]&,
			controlchannels];

	offoperator=
		Apply[Plus,Apply[zz075NonCommutativeTimes,
						ReplaceAll[offketbralist,offreprules],{1}]];
	onoperator=
		Apply[Plus,Apply[zz075NonCommutativeTimes,
						ReplaceAll[onketbralist,onreprules],{1}]];

	identityoperator=
		Apply[	zz075NonCommutativeTimes,
				Table[\[ScriptCapitalI][ controlledchannels[[j]] ],
						{j,ncontrolledqbits}]];

	resultado=Expand[zz075NonCommutativeTimes[offoperator,identityoperator] + 
			Power[zz075NonCommutativeTimes[onoperator,controlledgate],expo]];
	
	resultado
];

(* finally, if internalEvaluate could do nothing, it just disapears *)

internalEvaluate[x_]:=x;

(* Slow (Dirac) and fast (Fourier[]) implementations of the
Quantum Fourier Transform. Both are needed to let QFT be a Quantum gate
usable in any possible way as any other gate, and at the same time
having a fast calculation when possible *)

QuantumFourier[ket_zz080Ket]:=
Module[{qubits,numq},
qubits=Cases[ket,zz080Operator[q_],Infinity];
numq=Length[qubits];
Chop[Dot[Fourier[Table[
zz075NonCommutativeTimes[ket,zz080HermitianConjugate[DecToQubit[i,qubits]]],
{i,0,2^numq-1}]] ,Table[DecToQubit[j,qubits],{j,0,2^numq-1}]]]]/;
Not[FreeQ[ket,zz080Eigenstate[zz080Operator[_],_]]];

QuantumInverseFourier[ket_zz080Ket]:=
Module[{qubits,numq},
qubits=Cases[ket,zz080Operator[q_],Infinity];
numq=Length[qubits];
Chop[Dot[InverseFourier[Table[
zz075NonCommutativeTimes[ket,zz080HermitianConjugate[DecToQubit[i,qubits]]],
{i,0,2^numq-1}]] ,Table[DecToQubit[j,qubits],{j,0,2^numq-1}]]]]/;
Not[FreeQ[ket,zz080Eigenstate[zz080Operator[_],_]]];

DefineOperatorOnKets[QFTOperator,{\!\(\*
TagBox[
RowBox[{"\[VerticalSeparator]", 
TagBox["x_",
Quantum`Notation`zz080KetArgs,
BaseStyle->{ShowSyntaxStyles -> True},
Editable->True,
Selectable->True], "\[RightAngleBracket]"}],
Quantum`Notation`zz080Ket,
BaseStyle->{ShowSyntaxStyles -> False},
Editable->False,
Selectable->False]\):>QuantumFourier[\!\(\*
TagBox[
RowBox[{"\[VerticalSeparator]", 
TagBox["x",
Quantum`Notation`zz080KetArgs,
BaseStyle->{ShowSyntaxStyles -> True},
Editable->True,
Selectable->True], "\[RightAngleBracket]"}],
Quantum`Notation`zz080Ket,
BaseStyle->{ShowSyntaxStyles -> False},
Editable->False,
Selectable->False]\)]}];

DefineOperatorOnKets[QFTInverseOperator,{\!\(\*
TagBox[
RowBox[{"\[VerticalSeparator]", 
TagBox["x_",
Quantum`Notation`zz080KetArgs,
BaseStyle->{ShowSyntaxStyles -> True},
Editable->True,
Selectable->True], "\[RightAngleBracket]"}],
Quantum`Notation`zz080Ket,
BaseStyle->{ShowSyntaxStyles -> False},
Editable->False,
Selectable->False]\):>QuantumInverseFourier[\!\(\*
TagBox[
RowBox[{"\[VerticalSeparator]", 
TagBox["x",
Quantum`Notation`zz080KetArgs,
BaseStyle->{ShowSyntaxStyles -> True},
Editable->True,
Selectable->True], "\[RightAngleBracket]"}],
Quantum`Notation`zz080Ket,
BaseStyle->{ShowSyntaxStyles -> False},
Editable->False,
Selectable->False]\)]}];

SetQuantumGate[QFTDirac,{1,2^20},
	Function[
		Module[{m},
			m=2^Length[{##}];
			Sum[ExpToTrig[Exp[2*Pi*I*j*k/m]]*
				zz075NonCommutativeTimes[zz080Bra[zz050Subscript[{j},{{##1}}]],
										zz080Ket[zz050Subscript[{k},{{##1}}]]],
				{k,0,m-1},{j,0,m-1}]/Sqrt[m] ]]];

SetQuantumGate[\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT],{1,2^20}];

internalEvaluate[\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT]]:=
	QuantumEvaluate[ QFTDirac];

internalEvaluate[\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT][gargs__]]:=
	QuantumEvaluate[ QFTDirac[gargs]];

internalEvaluate[
HoldPattern[zz075NonCommutativeTimes[
	after___,
	zz080Ket[kargs__],
	Power[\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT][gargs__],expon_.]]]
]:=
Module[{kqbs,gqbs,klist,klistg,klistnotg,
		knog,kg,ftkg,knogftkg,
		resultado},
	kqbs=Cases[{kargs},zz080Operator[q_],Infinity];
	gqbs=Cases[{gargs},zz080Operator[q_],Infinity];
	klist=FactorKetList[QuantumEvaluate[zz080Ket[kargs]]];
	klistg=Cases[klist,
		zz080Ket[zz080Eigenstate[zz080Operator[b_],a_]]/;
			MemberQ[gqbs,zz080Operator[b]]];
	klistnotg=Complement[klist,klistg];
	resultado=If[TrueQ[Length[klistg]==Length[gqbs]],
				knog=Apply[zz075NonCommutativeTimes,klistnotg];	
				kg=Apply[zz075NonCommutativeTimes,klistg];
				ftkg=zz075NonCommutativeTimes[kg,Power[QFTOperator,expon]];
				knogftkg=zz075NonCommutativeTimes[knog,ftkg];
				QuantumEvaluate[zz075NonCommutativeTimes[
					after,
					 knogftkg] 
				],
				QuantumEvaluate[zz075NonCommutativeTimes[
					after,
					zz080Ket[kargs],
					Power[QFTDirac[gargs],expon]] 
				] ]; 
	Chop[CollectKet[Expand[resultado]]]
];

internalEvaluate[
HoldPattern[zz075NonCommutativeTimes[
	after___,
	zz080Ket[kargs__],
	Power[zz080HermitianConjugate[\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT][gargs__]],expon_.]]]
]:=
Module[{kqbs,gqbs,klist,klistg,klistnotg,
		knog,kg,ftkg,knogftkg,
		resultado},
	kqbs=Cases[{kargs},zz080Operator[q_],Infinity];
	gqbs=Cases[{gargs},zz080Operator[q_],Infinity];
	klist=FactorKetList[QuantumEvaluate[zz080Ket[kargs]]];
	klistg=Cases[klist,
		zz080Ket[zz080Eigenstate[zz080Operator[b_],a_]]/;
			MemberQ[gqbs,zz080Operator[b]]];
	klistnotg=Complement[klist,klistg];
	resultado=If[TrueQ[Length[klistg]==Length[gqbs]],
				knog=Apply[zz075NonCommutativeTimes,klistnotg];	
				kg=Apply[zz075NonCommutativeTimes,klistg];
				ftkg=zz075NonCommutativeTimes[kg,
						Power[QFTInverseOperator,expon]];
				knogftkg=zz075NonCommutativeTimes[knog,ftkg];
				QuantumEvaluate[zz075NonCommutativeTimes[
					after,
					 knogftkg] 
				],
				QuantumEvaluate[zz075NonCommutativeTimes[
					after,
					zz080Ket[kargs], 
					Power[zz080HermitianConjugate[QFTDirac[gargs]],expon]] 
				] ]; 
	Chop[CollectKet[Expand[resultado]]]
];

(* End of Quantum Fourier *)



(* Pauli Operators *)

SetQuantumGate[\[Sigma],2];

\[Sigma] /: MakeBoxes[\[Sigma][0,q_],form_]:=MakeBoxes[\[Sigma][\[ScriptZero],q],form];
\[Sigma] /: MakeBoxes[\[Sigma][1,q_],form_]:=MakeBoxes[\[Sigma][\[ScriptCapitalX],q],form];
\[Sigma] /: MakeBoxes[\[Sigma][2,q_],form_]:=MakeBoxes[\[Sigma][\[ScriptCapitalY],q],form];
\[Sigma] /: MakeBoxes[\[Sigma][3,q_],form_]:=MakeBoxes[\[Sigma][\[ScriptCapitalZ],q],form];

\[Sigma][\[ScriptZero],arg___]:=\[Sigma][0,arg];
\[Sigma][\[ScriptCapitalX],arg___]:=\[Sigma][1,arg];
\[Sigma][\[ScriptCapitalY],arg___]:=\[Sigma][2,arg];
\[Sigma][\[ScriptCapitalZ],arg___]:=\[Sigma][3,arg];
\[Sigma][4,arg___]:=\[Sigma][0,arg];
\[Sigma][\[ScriptCapitalI],arg___]:=\[Sigma][0,arg];
\[Sigma][O,arg___]:=\[Sigma][0,arg]; (*letter O to number 0*)
\[Sigma][Global`o,arg___]:=\[Sigma][0,arg];

\[Sigma] /: MakeBoxes[\[Sigma][id:(\[ScriptZero]|\[ScriptCapitalX]|\[ScriptCapitalY]|\[ScriptCapitalZ]),q_],TraditionalForm]:=
Module[{boxes,replaceme},
	boxes=MakeBoxes[Subscript[\[Sigma],q]^id,TraditionalForm];
	ReleaseHold[Hold[InterpretationBox[replaceme,\[Sigma][id,q]]] /.
		replaceme->boxes]
];

subscriptwasp=Unprotect[Subscript];
Subscript /: MakeBoxes[Subscript[\[Sigma],id_,q_],TraditionalForm]:=
Module[{boxes,replaceme},
	boxes=MakeBoxes[Subscript[\[Sigma],q]^id,TraditionalForm];
	ReleaseHold[Hold[InterpretationBox[replaceme,Subscript[\[Sigma],id,q]]] /.
		replaceme->boxes]
];
Protect@@subscriptwasp;

\[Sigma]/:
HoldPattern[zz075NonCommutativeTimes[
after___,
\[Sigma][0,zz080Operator[ope_]],
before___
]]:=zz075NonCommutativeTimes[after,before] /;
Not[FreeQ[{after,before},zz080Operator[ope]]];

\[Sigma]/:
HoldPattern[zz075NonCommutativeTimes[
after___,
\[Sigma][b_,zz080Operator[opesame_]],
\[Sigma][a_,zz080Operator[opesame_]],
before___
]]:=zz075NonCommutativeTimes[
after,
KroneckerDelta[a,b]*\[Sigma][0,zz080Operator[opesame]]+
I*Sum[Signature[{a,b,c}]*\[Sigma][c,zz080Operator[opesame]],{c,1,3}],
before] /;
And[MemberQ[{1,2,3},a],MemberQ[{1,2,3},b]];

\[Sigma]/:
HoldPattern[zz075NonCommutativeTimes[
after___,
\[Sigma][b_,zz080Operator[ope2_]],
\[Sigma][a_,zz080Operator[ope1_]],
before___
]]:=zz075NonCommutativeTimes[
after,
\[Sigma][a,zz080Operator[ope1]],
\[Sigma][b,zz080Operator[ope2]],
before] /;
And[ope1=!=ope2,Not[OrderedQ[{ope1,ope2}]],
MemberQ[{0,1,2,3},a],MemberQ[{0,1,2,3},b]];

\[Sigma]/:
HoldPattern[zz080HermitianConjugate[\[Sigma][args___]]]:=\[Sigma][args];

\[Sigma]/:
HoldPattern[Power[\[Sigma][id_,zz080Operator[ope_]],expo_?EvenQ]]:=
\[Sigma][0,zz080Operator[ope]];

\[Sigma]/:
HoldPattern[Power[\[Sigma][id_,zz080Operator[ope_]],expo_?OddQ]]:=
\[Sigma][id,zz080Operator[ope]];

\[Sigma]/:
HoldPattern[
zz050Commutator[
\[Sigma][a_,zz080Operator[opesame_]],
\[Sigma][b_,zz080Operator[opesame_]]]]:=
If[a===0,0,1]*If[b===0,0,1]*
2*I*Sum[Signature[{a,b,c}]*\[Sigma][c,zz080Operator[opesame]],{c,1,3}] /;
And[MemberQ[{0,1,2,3},a],MemberQ[{0,1,2,3},b]];

\[Sigma]/:
HoldPattern[
zz050Commutator[
\[Sigma][a_,zz080Operator[ope1_]],
\[Sigma][b_,zz080Operator[ope2_]]]]:= 0 /;
ope1=!=ope2; 

\[Sigma]/:
HoldPattern[
zz050AntiCommutator[
\[Sigma][a_,zz080Operator[opesame_]],
\[Sigma][b_,zz080Operator[opesame_]]]]:=
2*KroneckerDelta[a,b]*\[Sigma][0,zz080Operator[opesame]] /;
And[MemberQ[{1,2,3},a],MemberQ[{1,2,3},b]];

\[Sigma]/:
HoldPattern[
zz050AntiCommutator[
\[Sigma][a_,zz080Operator[ope1_]],
\[Sigma][b_,zz080Operator[ope2_]]]]:= 
	2*zz075NonCommutativeTimes[
	\[Sigma][a,zz080Operator[ope1]],
	\[Sigma][b,zz080Operator[ope2]]]/;
Or[ope1=!=ope2,a===0,b===0]; 

internalEvaluate[\[Sigma][0,args___]]:=	QuantumEvaluate[\[ScriptCapitalI][args]];
internalEvaluate[\[Sigma][1,args___]]:=	QuantumEvaluate[\[ScriptCapitalX][args]];
internalEvaluate[\[Sigma][2,args___]]:=	QuantumEvaluate[\[ScriptCapitalY][args]];
internalEvaluate[\[Sigma][3,args___]]:=	QuantumEvaluate[\[ScriptCapitalZ][args]];

internalEvaluate[HoldPattern[zz075NonCommutativeTimes[right__,\[Sigma][0,args___]]]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[right,\[ScriptCapitalI][args]]];
internalEvaluate[HoldPattern[zz075NonCommutativeTimes[right__,\[Sigma][1,args___]]]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[right,\[ScriptCapitalX][args]]];
internalEvaluate[HoldPattern[zz075NonCommutativeTimes[right__,\[Sigma][2,args___]]]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[right,\[ScriptCapitalY][args]]];
internalEvaluate[HoldPattern[zz075NonCommutativeTimes[right__,\[Sigma][3,args___]]]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[right,\[ScriptCapitalZ][args]]];

(* End of Pauli Operators *)



QubitMeasurement/:
internalEvaluate[QubitMeasurement[ksup_,rest___]]:=
	QuantumMeasurement[QuantumEvaluate[ksup],rest];

internalEvaluate[HoldPattern[zz075NonCommutativeTimes[
			right___,
			QubitMeasurement[ksup_,rest___],
			left___ ]]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[
		right,
		QuantumEvaluate[QubitMeasurement[ksup,rest]],
		left]];

QubitMeasurement/:
QuantumDensityOperator[QubitMeasurement[ksup_,rest___]]:=
	QuantumDensityOperator[QuantumMeasurement[QuantumEvaluate[ksup],rest]];

QubitMeasurement[ksup_,n_Integer,rest___]:=
	QubitMeasurement[ksup,Map[zz080Operator,Range[n]],rest];

(* A bug in Mathematica's editor forced me to use _?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &)
instead of _ \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] *)
QubitMeasurement[ksup_,r_?(Head[#]=== \[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR] &), rest___]:=
	QubitMeasurement[ksup,
		QuantumEvaluate[r],rest];

(* Quantum Evaluation of zz050Superposition States ket[+] and ket[-] *)

internalEvaluate[ zz080Ket[zz050Superpos[k_,"plus"],restket___] ]:=
	QuantumEvaluate[zz075NonCommutativeTimes[
		zz080Ket[restket],
		(zz080Ket[zz080Eigenstate[k, 0]] + 
		zz080Ket[zz080Eigenstate[k, 1]])/Sqrt[2]]];

internalEvaluate[ zz080Ket[zz050Superpos[k_,"minus"],restket___] ]:=
	QuantumEvaluate[zz075NonCommutativeTimes[
		zz080Ket[restket],
		(zz080Ket[zz080Eigenstate[k, 0]] - 
		zz080Ket[zz080Eigenstate[k, 1]])/Sqrt[2]]];

(* Quantum Evaluation of Bell States *)

internalEvaluate[zz080Ket[zz050Bell[0, 0, q1_, q2_,beta___],restket___]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[
		zz080Ket[restket],
		(zz080Ket[zz080Eigenstate[q1, 0], 
			zz080Eigenstate[q2, 0]] + 
		zz080Ket[zz080Eigenstate[q1, 1], 
			zz080Eigenstate[q2, 1]])/Sqrt[2]]];

internalEvaluate[zz080Ket[zz050Bell[0, 1, q1_, q2_,beta___],restket___]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[
		zz080Ket[restket],
		(zz080Ket[zz080Eigenstate[q1, 0], 
			zz080Eigenstate[q2, 1]] + 
		zz080Ket[zz080Eigenstate[q1, 1], 
			zz080Eigenstate[q2, 0]])/Sqrt[2]]];

internalEvaluate[zz080Ket[zz050Bell[1, 0, q1_, q2_,beta___],restket___]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[
		zz080Ket[restket],
		(zz080Ket[zz080Eigenstate[q1, 0], 
   		zz080Eigenstate[q2, 0]] - 
		zz080Ket[zz080Eigenstate[q1, 1], 
			zz080Eigenstate[q2, 1]])/Sqrt[2]]];

internalEvaluate[zz080Ket[zz050Bell[1, 1, q1_, q2_,beta___],restket___]]:=
	QuantumEvaluate[zz075NonCommutativeTimes[
		zz080Ket[restket],
		(zz080Ket[zz080Eigenstate[q1, 0], 
			zz080Eigenstate[q2, 1]] - 
		zz080Ket[zz080Eigenstate[q1, 1], 
			zz080Eigenstate[q2, 0]])/Sqrt[2]]];
	

wasp=Unprotect[QuantumTr];	
QuantumTr /: internalEvaluate[HoldPattern[QuantumTr[args___,sub_]]]:=
	QuantumTr[QuantumEvaluate[args],sub];
Protect@@wasp;

(***************************************)
(* Quantum plotting: gates to Graphics *)
(***************************************)

SetAttributes[QuantumPlot,Listable];

QuantumPlot3D[args___]:=QuantumPlot[args,QuantumPlot3D->True];

QuantumPlot[expr_?(FreeQ[#,_?KetQ]&),opts:OptionsPattern[]]:=
With[{expanded=Expand[expr]},
		QuantumPlot[expanded,opts] /;
	expanded=!=expr
];

QuantumPlot[expr_?(Not[FreeQ[#,QubitMeasurement[_,_,___]]]&),opts:OptionsPattern[]]:=
With[{expanded=expr/.
	QubitMeasurement[HoldPattern[zz075NonCommutativeTimes[
			ket_?KetQ,opes:(_?(FreeQ[#,_?KetQ]&)..)]],q_,opqm___]:>
	QubitMeasurement[zz075NonCommutativeTimes[
			ket,Expand[zz075NonCommutativeTimes[opes]]],q,opqm]},
		QuantumPlot[expanded,opts] /;
	expanded=!=expr
];

QuantumPlot[
	HoldPattern[
		zz075NonCommutativeTimes[
			ket_?KetQ,expr:(_?(FreeQ[#,_?KetQ]&)..)]],opts:OptionsPattern[]]:=
With[{expanded=Expand[zz075NonCommutativeTimes[expr]]},
		QuantumPlot[zz075NonCommutativeTimes[ket,expanded],opts] /;
	expanded=!=zz075NonCommutativeTimes[expr]
];

QuantumPlot[HoldPattern[zz050Commutator[args__]],opts:OptionsPattern[]]:=
	QuantumPlot[EvaluateAllCommutators[zz050Commutator[args]],opts];
QuantumPlot[HoldPattern[zz050AntiCommutator[args__]],opts:OptionsPattern[]]:=
	QuantumPlot[EvaluateAllCommutators[zz050AntiCommutator[args]],opts];


QuantumPlot[a_+b_,opts:OptionsPattern[]]:=QuantumPlot[a,opts]+QuantumPlot[b,opts];

QuantumPlot[qmr:QuantumMeasurementResults[args___],opts:OptionsPattern[]]:=
Module[{},
	ListPlot[
		Tooltip[ Apply[Function[{
			QubitToDec[
				Apply[zz080Ket,#2[[1]]]],#1}],qmr[[1,All,{1,2}]],{1}] ],
		FilterRules[{opts},Options[ListPlot]],
		Filling->Axis,
		FillingStyle->Directive[Thick,Darker[Red]],
		PlotStyle->Directive[Darker[Blue],PointSize[Large]],
		Axes->False,
		Frame->True,
		PlotRange->{0,1.05}]
];

QuantumPlot3D[qmr:QuantumMeasurementResults[args___],opts:OptionsPattern[]]:=
Module[{},
	BarChart3D[
		qmr[[1,All,1]],
		FilterRules[{opts},Options[BarChart3D]],
		ChartStyle->ColorData["Charting"][[-2]],
		ChartLegends->Map[TraditionalForm[#[[1]]]&,qmr[[1,All,2]]],
		PlotRange->{0,1.05}]
];

QuantumPlot[ncex_?(Not[QuantumScalarQ[#]]&),opts:OptionsPattern[]]:=
Module[{listafinal,lines,lista,listaQubits,x,listameasuredwire,
		labels,orderedQubits,gpowers,
		listIndexed,qwireoccupation,element,
		elementwires,newpos,rules,j,k,columns,ex,exx,
		expressionQubits,extraQubits,
		ifex},
		
	labels=OptionValue[QubitLabels];
	gpowers=OptionValue[QuantumGatePowers];
	(* Next we take advantage of the OneIdentity attribute of zz080Operator
	 so that the userlist works the same if the user writes or not
	 the qubits inside the qubit template (zz080Operator) *)
	orderedQubits=Map[zz080Operator,OptionValue[QubitList]];
	(* The ordered list of qubits is created, including those qubits whose order
	 was specified by the user and those qubits whose order wasn't specified*) 
	expressionQubits = 
		Union[Cases[{ncex /. ReplaceBeforeShifting }, zz080Operator[_], Infinity, Heads->True]];

	extraQubits = Select[expressionQubits, Not[MemberQ[orderedQubits, #]] &];
	listaQubits = Join[orderedQubits, extraQubits];
	lines=Length[listaQubits];
	
	exx=EvaluateAllCommutators[ncex];

	If[exx=!=ncex,Return[QuantumPlot[exx,opts]]];
	
	If[Head[exx]===zz075NonCommutativeTimes,
		ex=Sequence@@Reverse[Flatten[Apply[List,exx]]],
		If[Head[exx]===Times,
			ex=Apply[Sequence,(Apply[List,exx]//.
				HoldPattern[zz075NonCommutativeTimes[args__]]:>
					Sequence@@Reverse[Flatten[{args}]] //.
				{b___,s_?QuantumScalarQ,a___,k_?KetQ}:>{b,a,s*k}//.
				{b___,s_?QuantumScalarQ,a___}:>{b,a} )],
			ex=Sequence[exx]
	]];

	ex=Apply[Sequence,
			Flatten[{ex}//.
				QubitMeasurement[expre_,lista_,opqm___]:>
					{QuantumMeter[lista],
					 If[Head[expre]===zz075NonCommutativeTimes,
						Reverse[Flatten[Apply[List,expre]]],
						expre	
						]
					}
			]
		];

	If[Not[gpowers], 
		ex=Apply[Sequence,Flatten[{ex}/.
			{Power[g_,n_Integer]:>Table[g,{n}],
			 HoldPattern[zz020Controlled[Power[g_,n_Integer],c_]]:>
			 			Table[zz020Controlled[g,c],{n}] 
			} ]] ];	

  	(* x is the Dirac expression ex with 
  	   all the zz080Operator[qb] (qubit labels) replaced with the 
  	   number of wire, which is not (in general) the same as the label
  	   of qubit *)
	x=Sequence@@({ex} /. ReplaceBeforeShifting /.{zz080Operator[qb_] :> 
  		qwire[Sequence@@Flatten@Position[listaQubits, zz080Operator[qb]]]});

	listIndexed=MapIndexed[{#1,First[#2]}&,Reverse[{x}]];
	
	If[OptionValue[QuantumGateShifting],
		qwireoccupation=Table[0,{lines}];
		Do[
			element=listIndexed[[j]];	
			elementwires=Sort[Cases[element,qwire[n_]:>n,Infinity, Heads->True]];
			elementwires=Range[First[elementwires],Last[elementwires]];
			newpos=1+Max[qwireoccupation[[elementwires]]];
			element[[2]]=newpos;
			listIndexed[[j]]=element;
			rules=Map[#->newpos&,elementwires];		
			qwireoccupation=ReplacePart[qwireoccupation,rules],
		{j,Length[listIndexed]}]
	];
	
	listIndexed=listIndexed //. ReplaceAfterShifting;

	If[Not[FreeQ[listIndexed,{_?KetQ,1}]],
		listIndexed=listIndexed/.{x_,n_Integer}:>{x,n-1}/.{k_?KetQ,0}:>{k,-1}];
	
	columns=Max[listIndexed[[All,2]]];
	
	listameasuredwire=
		Cases[listIndexed,
			{QuantumMeter[qwire[j_]],k_}:>
				QQMeasuredWire[j,k,columns+1.3,opts]];

	lista=Map[ToColumn[First[#],Last[#],lines,opts]&,listIndexed];
	
	listafinal=Join[If[	TrueQ[labels],
						{QQWires[columns,lines,opts],
						 QQuantumColumn[
							MapIndexed[qtext[Sequence @@ #2, #1] &, 
										listaQubits], 
							0, lines,opts]},
						{QQWires[columns,lines,opts]}
					],
					listameasuredwire,
					lista];
	
	(* The command Show makes the user-defined options
		overwrite the default options, for example the Background *)
	ifex=If[FreeQ[ncex,zz080Ket],ncex,ncex/.zz080Ket->\[ScriptK]\[ScriptE]\[ScriptT]];				
	Show[If[TrueQ[OptionValue[QuantumPlot3D]],Graphics3D,Graphics][
					Tooltip[listafinal,ifex], 
					Background -> OptionValue[QuantumBackground],
					If[TrueQ[OptionValue[QuantumPlot3D]],
						{Boxed->False},
						{PlotRange -> All,
						PlotRangeClipping->True, 
						AspectRatio -> Automatic, 
						ContentSelectable->False,
						Frame->False}]],
		If[TrueQ[OptionValue[QuantumPlot3D]],
			FilterRules[{opts}, Options[Graphics3D]],
			FilterRules[{opts}, Options[Graphics]]]
		]
];

(* From quantum Dirac objects kets, gates, controlled-gates, etc to 
"Quantum Column Primitives" qline, qnot, etc. *)

Options[ToColumn]=Options[QuantumPlot];

ToColumn[ QuantumMeter[qwire[x_]],pos_,lines_,opts:OptionsPattern[] ]:=
Module[{},
	QQuantumColumn[{qmeter[x]},pos,lines,opts]
];

ToColumn[ HoldPattern[\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP][qwire[x_],qwire[y_]]],
			pos_,lines_,opts:OptionsPattern[] ]:=
Module[{},
	QQuantumColumn[{qswap[x],qswap[y],qline[x,y]},pos,lines,opts]
];

ToColumn[k:zz080Ket[zz080Eigenstate[__]..],
		pos_,lines_,opts:OptionsPattern[]]:=
Module[{lista},
	lista=ReplaceAll[ List@@k,
	(*remmeber that operator is replaced with qwire in QuantumPlot*)
				zz080Eigenstate[qwire[wire_],value_] :> 
					qtextket[wire,value]];
	QQuantumColumn[lista,pos,lines,opts]
];

ToColumn[k_?KetQ,
		pos_,lines_,opts:OptionsPattern[]]:=
Module[{lista,wm},
	wm=Min[ Cases[k,qwire[wire_]:>wire,
			Infinity] ];
	lista={qverticaltext[wm,FactorKet[k] /. Hold->HoldForm //.
								qwire[wire_]:>zz080Operator[wire]]};
	QQuantumColumn[lista,pos,lines,opts]
];

ToColumn[ Power[HoldPattern[zz020Controlled[g_,c_]],expo_],
			pos_,lines_,opts:OptionsPattern[] ]:=
	ToColumn[zz020Controlled[Power[g,expo],c],pos,lines,opts];

ToColumn[HoldPattern[
	Power[
		zz080HermitianConjugate[
			HoldPattern[zz020Controlled[g_,c_]]],expo_.]],
			pos_,lines_,opts:OptionsPattern[] ]:=
	ToColumn[
		zz020Controlled[
			Power[zz080HermitianConjugate[g],expo],c],pos,lines,opts];

ToColumn[ HoldPattern[zz020Controlled[
		Power[controlledgate_,expo_.],
	{controlqbits__qwire}] ],
pos_,lines_,opts:OptionsPattern[] ]:=
Module[{lista,wires,
		listaqline,listacontrolqbits,listaall,listaqcontrol,patt,
		resultado},
	
	wires=Cases[controlledgate,qwire[patt___]:>patt,Infinity,Heads->True];
	listacontrolqbits=({controlqbits}/.qwire[patt___]:>patt);		
	listaqcontrol=Map[qcontrol,listacontrolqbits];
	listaall=Sort[Join[wires,listacontrolqbits]];
	listaqline={qline[	First[listaall],
						Last[listaall]]};
	lista=Join[listaqline,listaqcontrol];
	
	resultado=Join[QQuantumColumn[lista,pos,lines,opts],
		ToColumn[Power[controlledgate,expo],pos,lines,opts]];	
	resultado
];

ToColumn[ Power[gate_,expo_.],pos_,lines_,opts:OptionsPattern[] ]:=
Module[{lista,wires,listaconexionesagrupadas,listaconexionesordenadas,
		listaqline,listagate,displaygate,patt,head,b,a},	

	wires=Cases[gate,qwire[patt___]:>patt,Infinity, Heads->True];
	listaconexionesordenadas=Sort[wires]; 

	listaconexionesagrupadas=
		Map[{First[#],Last[#]}&,
			Split[listaconexionesordenadas,-1<=#1-#2<=1&]];

	displaygate=gate //. 
		HoldPattern[zz075NonCommutativeTimes[patt___]]:>
			HoldForm[CenterDot[patt]] //. 
		HoldPattern[zz080HermitianConjugate[patt___]]:>
			HoldForm[SuperDagger[patt]] //.
		HoldPattern[head_[b___,qwire[patt___],a___]]:>
			HoldForm[head[b,a]] //. 
		HoldForm[head_[]]:>HoldForm[head] //. 
		HoldForm[head_[List]]:>HoldForm[head];

	listagate=
		If[	And[displaygate===HoldForm[\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT]],expo===1],
			Map[qnot,wires],
			Apply[qgate[Power[displaygate,expo],#1,#2]&,
					listaconexionesagrupadas,{1}]
		];

	listaqline={qline[	First[listaconexionesordenadas],
						Last[listaconexionesordenadas]]};
	lista=Join[listagate,listaqline];

	QQuantumColumn[lista,pos,lines,opts]
];

(* From "Quantum Column Primitives" qline, qnot, etc, to
normal Mathematica primitives Line[], Rectangle[], etc.  *)

QQuantumDx =1.0;
QQuantumDy =-1.0;
QQuantumDz =1.0;

QQuantumCoordinates[{gatePos_Integer,wire_Integer}] := 
	{gatePos*QQuantumDx,wire*QQuantumDy};

Options[QQWires]=Options[QuantumPlot];
Options[QQMeasuredWire]=Options[QQWires];

QQMeasuredWire[wire_,inicol_,endcol_,opts:OptionsPattern[]]:=
Module[{},
	{OptionValue[QuantumWireStyle],
	If[TrueQ[OptionValue[QuantumPlot3D]],
		If[TrueQ[$VersionNumber>=7],
			Tube[{{inicol*QQuantumDx,(wire+0.12)*QQuantumDy,0},
				{endcol*QQuantumDx,(wire+0.12)*QQuantumDy,0}},0.03*QQuantumDz],
			Line[{{inicol*QQuantumDx,(wire+0.12)*QQuantumDy,0},
				{endcol*QQuantumDx,(wire+0.12)*QQuantumDy,0}}]],
		Line[{{inicol*QQuantumDx,(wire+0.08)*QQuantumDy},
			{endcol*QQuantumDx,(wire+0.08)*QQuantumDy}}]
	] }
];

QQWires[columns_,lines_,opts:OptionsPattern[]]:=
Module[{j},
	{OptionValue[QuantumWireStyle],
	If[TrueQ[OptionValue[QuantumPlot3D]],
		Table[If[TrueQ[$VersionNumber>=7],
				Tube[{{(-0.5)*QQuantumDx,j*QQuantumDy,0},
					{(columns+1.3)*QQuantumDx,j*QQuantumDy,0}},0.03*QQuantumDz],
				Line[{{(-0.5)*QQuantumDx,j*QQuantumDy,0},
					{(columns+1.3)*QQuantumDx,j*QQuantumDy,0}}]],
			{j,1,lines}],
		Table[Line[
			{{(-0.5)*QQuantumDx,j*QQuantumDy},
			{(columns+1.3)*QQuantumDx,j*QQuantumDy}}],{j,1,lines}]
	] }
];

Options[QQuantumColumn]=Options[QuantumPlot];

QQuantumColumn[
	conexiones:{(
		qline[_Integer,_Integer]|
		qnot[_Integer]|
		qnull|
		qtextket[_Integer,_]|
		qtextbra[_Integer,_]|
		qverticaltext[_Integer,_]|
		qtext[_Integer,_]|
		qcontrol[_Integer]|
		qswap[_Integer]|
		qmeter[_Integer]|
		(qgate[_,_Integer,_Integer])
		)..},
	gatePos_Integer,numberOfWires_Integer, opts:OptionsPattern[]]:=
Module[
	{
	lista,poswireLista,coordsLista,primitivaLista,
	listaNoLabel,
	controla,poswireControla,coordsControla,primitivaControla,
	niega,poswireNiega,coordsNiega,primitivaNiega,
	mide,poswireMide,coordsMide,primitivaMide,
	swapea,poswireSwapea,coordsSwapea,primitivaSwapea,
	compuertas,poswireCompuertas,coordsCompuertas,primitivaCompuertas,
	primitivaTextosKet,textosket,
	primitivaTextosBra,textosbra,
	primitivaTextos,textos,
	primitivaVTextos,vtextos,
	compuertasNoLabel
	},

    (* vertical connections *)
	lista=Cases[conexiones,qline[_,_]];
	listaNoLabel=lista/.qline[x_Integer,y_Integer]:>{x,y};
	poswireLista=Map[ {gatePos,#}&,listaNoLabel,{2}];
	coordsLista=Map[QQuantumCoordinates,poswireLista,{2}];
	primitivaLista=
		{	OptionValue[QuantumConnectionStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				Map[If[TrueQ[$VersionNumber>=7],
						Tube[{{#[[1,1]],#[[1,2]],0},{#[[2,1]],#[[2,2]],0}},
							0.035*QQuantumDz],
						Line[{{#[[1,1]],#[[1,2]],0},{#[[2,1]],#[[2,2]],0}}]]&,
					coordsLista],
				Map[Line[{#[[1]],#[[2]]}]&,coordsLista]]};

	niega=Cases[conexiones,qnot[_]];
	poswireNiega=Map[ {gatePos,#}&,niega/.qnot->Sequence];
	coordsNiega=Map[QQuantumCoordinates,poswireNiega];
	primitivaNiega={
		{	OptionValue[QuantumGateStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				Map[Sphere[{#[[1]],#[[2]],0},
						{Abs[QQuantumDx/4],Abs[QQuantumDy/4],Abs[QQuantumDz/4]}]&,
					coordsNiega],
				Map[Disk[#,{Abs[QQuantumDx/4],Abs[QQuantumDy/4]}]&,coordsNiega]]},
		{	OptionValue[QuantumNotStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				{Map[Cylinder[{ {#[[1]],#[[2]],QQuantumDx/50},
								{#[[1]],#[[2]],-QQuantumDx/50}},QQuantumDx/3.5]&,
					coordsNiega],
				Map[Cylinder[{ {#[[1]]-QQuantumDx/50,#[[2]],0},
								{#[[1]]+QQuantumDx/50,#[[2]],0}},QQuantumDx/3.5]&,
					coordsNiega],
				Map[Cylinder[{  {#[[1]],#[[2]]-QQuantumDx/50,0},
								{#[[1]],#[[2]]+QQuantumDx/50,0}},QQuantumDx/3.5]&,
					coordsNiega]},
				{Map[Line[{	#-{QQuantumDx/4,0},
							#+{QQuantumDx/4,0}}]&,coordsNiega],
				Map[Line[{	#-{0,QQuantumDy/4},
							#+{0,QQuantumDy/4}}]&,coordsNiega]}]}			
	};
	
	controla=Cases[conexiones,qcontrol[_]];
	poswireControla=Map[ {gatePos,#}&,controla/.qcontrol->Sequence];
	coordsControla=Map[QQuantumCoordinates,poswireControla];
	primitivaControla=
		{   OptionValue[QuantumControlStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				Map[Sphere[{#[[1]],#[[2]],0},
						{Abs[QQuantumDx/8],Abs[QQuantumDy/8],Abs[QQuantumDz/8]}]&,
					coordsControla],
				Map[Disk[#,{Abs[QQuantumDx/8],Abs[QQuantumDy/8]}]&,coordsControla]
			]};

	(* Measurement meters *)
	mide=Cases[conexiones,qmeter[_]];
	poswireMide=Map[ {gatePos,#}&,mide/.qmeter->Sequence];
	coordsMide=Map[QQuantumCoordinates,poswireMide];
	primitivaMide={
		{   OptionValue[QuantumGateStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				Map[Cuboid[{#[[1]]-QQuantumDx/3,#[[2]]-QQuantumDy/3,-QQuantumDz/10},
							{#[[1]]+QQuantumDx/3,#[[2]]+QQuantumDy/3,QQuantumDz/10}]&,
					coordsMide],
				Map[Rectangle[#-{QQuantumDx,QQuantumDy}/3,
							#+{QQuantumDx,QQuantumDy}/3]&,coordsMide] ]},
		{   OptionValue[QuantumMeterStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				Map[{
					If[TrueQ[$VersionNumber>=7],
						{Arrow[Tube[{{#[[1]],#[[2]]+QQuantumDy/3,QQuantumDz/9},
							{#[[1]]+Abs[QQuantumDx/6],#[[2]]+Abs[QQuantumDy/3.5],QQuantumDz/9}},
								0.01*QQuantumDz]],
						 Tube[BezierCurve[{
								{#[[1]]-QQuantumDx/3.5,#[[2]]+QQuantumDy/4,QQuantumDz/9},
								{#[[1]],#[[2]]-QQuantumDy/5,QQuantumDz/9},
								{#[[1]]+QQuantumDx/3.5,#[[2]]+QQuantumDy/4,QQuantumDz/9}}],
							0.01*QQuantumDz]},
						{Arrow[{{#[[1]],#[[2]]+QQuantumDy/3,QQuantumDz/9},
							{#[[1]]+Abs[QQuantumDx/6],#[[2]]+Abs[QQuantumDy/3.5],QQuantumDz/9}}]}
					]}&,coordsMide],
				Map[{Circle[#+{0,QQuantumDy}/3.5,
						{Abs[QQuantumDx/3],Abs[QQuantumDy/3]},
							{Pi/6, 5 Pi/6}],
					Arrow[{#+{0,QQuantumDy}/3,
						#+{Abs[QQuantumDx/6],Abs[QQuantumDy/3.5]}}]}&,
					coordsMide]
			]
	 }};

	swapea=Cases[conexiones,qswap[_]];
	poswireSwapea=Map[ {gatePos,#}&,swapea/.qswap->Sequence];
	coordsSwapea=Map[QQuantumCoordinates,poswireSwapea];
	primitivaSwapea=
		{   OptionValue[QuantumSwapStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				{Map[If[TrueQ[$VersionNumber>=7],
						Tube[{{#[[1]]-QQuantumDx/4,#[[2]]+QQuantumDx/4,0},
							{#[[1]]+QQuantumDx/4,#[[2]]-QQuantumDx/4,0}},
							0.05*QQuantumDz],
						Line[{{#[[1]]-QQuantumDx/4,#[[2]]+QQuantumDx/4,0},
							{#[[1]]+QQuantumDx/4,#[[2]]-QQuantumDx/4,0}}]]&,
					coordsSwapea],
				Map[If[TrueQ[$VersionNumber>=7],
						Tube[{{#[[1]]+QQuantumDx/4,#[[2]]-QQuantumDy/4,0},
							{#[[1]]-QQuantumDx/4,#[[2]]+QQuantumDy/4,0}},
							0.05*QQuantumDz],
						Line[{{#[[1]]+QQuantumDx/4,#[[2]]-QQuantumDy/4,0},
							{#[[1]]-QQuantumDx/4,#[[2]]+QQuantumDy/4,0}}]]&,
					coordsSwapea]},
				{Map[Line[{	#+{-QQuantumDx/4,QQuantumDx/4},
							#+{QQuantumDx/4,-QQuantumDx/4}}]&,coordsSwapea],
				Map[Line[{	#+{QQuantumDx/4,-QQuantumDy/4},
							#+{-QQuantumDx/4,QQuantumDy/4}}]&,coordsSwapea]}
			] };

	textos=Cases[conexiones,qtext[_Integer,_]];
	primitivaTextos=
		ReplaceAll[textos,qtext[wire_,mensaje_] :> 
					Style[Text[mensaje,
							If[TrueQ[OptionValue[QuantumPlot3D]],
								Flatten[{QQuantumCoordinates[{gatePos,wire}],0}]+
									{0.0*QQuantumDx,-QQuantumDy/4,QQuantumDz/5},
								QQuantumCoordinates[{gatePos,wire}]+
									{0.0*QQuantumDx,-QQuantumDy/4} ] ],
						OptionValue[QuantumTextStyle]]];

	vtextos=Cases[conexiones,qverticaltext[_Integer,_]];
	primitivaVTextos=
		ReplaceAll[vtextos,qverticaltext[wire_,mensaje_] :> 
					Style[Text[mensaje,
							If[TrueQ[OptionValue[QuantumPlot3D]],
								Flatten[{QQuantumCoordinates[{gatePos,wire}],0}],
								QQuantumCoordinates[{gatePos,wire}]+{0,-QQuantumDy/3} ],
							{-1,-1},{0,-1} ],
						OptionValue[QuantumVerticalTextStyle]]];

	textosket=Cases[conexiones,qtextket[_Integer,_]];
	primitivaTextosKet=
		ReplaceAll[textosket,qtextket[wire_,mensaje_] :> 
					Style[Text[zz080Ket[mensaje],
							If[TrueQ[OptionValue[QuantumPlot3D]],
								Flatten[{QQuantumCoordinates[{gatePos,wire}],0}]+
									{0,-QQuantumDy/4,QQuantumDz/5},
								QQuantumCoordinates[{gatePos,wire}]+
									{0,0} ] ],
						OptionValue[QuantumTextStyle] ]];

	textosbra=Cases[conexiones,qtextbra[_Integer,_]];
	primitivaTextosBra=
		ReplaceAll[textosbra,qtextbra[wire_,mensaje_] :> 
					Style[Text[zz080Bra[mensaje],
							If[TrueQ[OptionValue[QuantumPlot3D]],
								Flatten[{QQuantumCoordinates[{gatePos,wire}],0}]+
									{-QQuantumDx/2,-QQuantumDy/4,QQuantumDz/5},
								QQuantumCoordinates[{gatePos,wire}]+
									{-QQuantumDx/2,-QQuantumDy/4} ] ],
						OptionValue[QuantumTextStyle] ]];

	compuertas=Cases[conexiones,qgate[_,_,_]];
	compuertasNoLabel=compuertas/.qgate[_,x_Integer,y_Integer]->{x,y};
	poswireCompuertas=Map[ {gatePos,#}&,compuertasNoLabel,{2}];
	coordsCompuertas=Map[QQuantumCoordinates,poswireCompuertas,{2}];
	primitivaCompuertas={
		{	OptionValue[QuantumGateStyle],
			If[TrueQ[OptionValue[QuantumPlot3D]],
				Map[Cuboid[{#[[1,1]]-QQuantumDx/3,#[[1,2]]-QQuantumDy/3,-QQuantumDz/10},
							{#[[2,1]]+QQuantumDx/3,#[[2,2]]+QQuantumDy/3,QQuantumDz/10}]&,
					coordsCompuertas],
				Map[Rectangle[#[[1]]-{QQuantumDx,QQuantumDy}/3,
							#[[2]]+{QQuantumDx,QQuantumDy}/3]&,coordsCompuertas] ]},
		{Style[	
			Table[Text[compuertas[[j,1]],
						If[TrueQ[OptionValue[QuantumPlot3D]],
								Flatten[{(coordsCompuertas[[j,1]]+coordsCompuertas[[j,2]])/2,
										QQuantumDz/5}],
								(coordsCompuertas[[j,1]]+coordsCompuertas[[j,2]])/2 ],
						{0,0},
						If[coordsCompuertas[[j,1]]===coordsCompuertas[[j,2]],
							{1,0}, {0,-1}] ],
				{j,1,Length[compuertas]}],OptionValue[QuantumTextStyle]]}
	};
	Flatten[{	primitivaLista,primitivaControla,primitivaSwapea,
		primitivaCompuertas,primitivaMide,primitivaNiega,
		primitivaTextosKet,primitivaTextosBra,primitivaTextos,primitivaVTextos}]
];

QQuantumColumn[anythingelse_,
	gatePos_Integer,numberOfWires_Integer,opts:OptionsPattern[]]:=
Module[{poswireCompuertas,coordsCompuertas,primitivaCompuertas},
	poswireCompuertas={{{gatePos,1},{gatePos,numberOfWires}}};
	coordsCompuertas=Map[QQuantumCoordinates,poswireCompuertas,{2}];
	primitivaCompuertas={
		{	RGBColor[0,0,0.5],
			Map[Rectangle[#[[1]]-{QQuantumDx,QQuantumDy}/3,
							#[[2]]+{QQuantumDx,QQuantumDy}/3]&,coordsCompuertas]},
		{	GrayLevel[1],
			Map[Rectangle[#[[1]]-{QQuantumDx,QQuantumDy}/3.5,
							#[[2]]+{QQuantumDx,QQuantumDy}/3.5]&,coordsCompuertas]},
		{	GrayLevel[0],
			Text["ERROR:\n"<>ToString[Head[anythingelse]],
						(coordsCompuertas[[1,1]]+coordsCompuertas[[1,2]])/2 ]}
	};
	{primitivaCompuertas}
];

QQuantumColumn[wrongformat___]:=
Module[{},
	{GrayLevel[0],Text["Wrong Format\n"<>ToString[{wrongformat}]]}
];


(* Internal representation of Quantum Gates *)
(* THEY TAKE ADVANTAGE OF THE ATTRIBUTES OF zz080Operator  *)
(* zz080Operator[zz080Operator[a]] gives zz080Operator[a] *)

SetQuantumGate[\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT]]; Abort[],
zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]]]]
];

\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT]]; 
	Abort[]
];

SetQuantumGate[\[ScriptCapitalX],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalX]]; Abort[],
zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]]]]
];

\[ScriptCapitalX][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalX]]; 
	Abort[]
];

\[ScriptCapitalX]/:
HoldPattern[zz080HermitianConjugate[\[ScriptCapitalX][args___]]]:=\[ScriptCapitalX][args];

SetQuantumGate[\[ScriptCapitalY],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalY]]; Abort[],
-I*zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]] + 
 	I*zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]]]]
];

\[ScriptCapitalY][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalY]]; 
	Abort[]
];

\[ScriptCapitalY]/:
HoldPattern[zz080HermitianConjugate[\[ScriptCapitalY][args___]]]:=\[ScriptCapitalY][args];

SetQuantumGate[\[ScriptCapitalZ],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalZ]]; Abort[],
-zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]]]]
];

\[ScriptCapitalZ][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalZ]]; 
	Abort[]
];

\[ScriptCapitalZ]/:
HoldPattern[zz080HermitianConjugate[\[ScriptCapitalZ][args___]]]:=\[ScriptCapitalZ][args];

SetQuantumGate[\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP],2,
Function[
If[Length[{##}]!=2,
Message[SetQuantumGate::gatetwoarg,\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP]]; Abort[],
zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0], 
   					zz080Eigenstate[zz080Operator[#2], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0], 
   					zz080Eigenstate[zz080Operator[#2], 0]]] + 
 	zz075NonCommutativeTimes[
 		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0],
 					zz080Eigenstate[zz080Operator[#2], 1]], 
		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1],
					zz080Eigenstate[zz080Operator[#2], 0]]] + 
	zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1], 
					zz080Eigenstate[zz080Operator[#2], 0]], 
		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0], 
					zz080Eigenstate[zz080Operator[#2], 1]]] + 
	zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1], 
					zz080Eigenstate[zz080Operator[#2], 1]],
		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1], 
					zz080Eigenstate[zz080Operator[#2], 1]]]]
]];

HoldPattern[\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP][q1_,q2_,q3__]]:=
Module[{},
	Message[SetQuantumGate::gatetwoarg,\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP]]; 
	Abort[]
];

(* The quantum gate \[ScriptZero] is an identity gate used
in the notation for Pauli operators *)
SetQuantumGate[\[ScriptZero],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptZero]]; Abort[],
zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]]]]
];

\[ScriptZero][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptZero]]; 
	Abort[]
];

\[ScriptZero]/:
HoldPattern[zz080HermitianConjugate[\[ScriptZero][args___]]]:=\[ScriptZero][args];

SetQuantumGate[\[ScriptCapitalI],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalI]]; Abort[],
zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]]]]
];

\[ScriptCapitalI][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalI]]; 
	Abort[]
];

\[ScriptCapitalI]/:
HoldPattern[zz080HermitianConjugate[\[ScriptCapitalI][args___]]]:=\[ScriptCapitalI][args];

SetQuantumGate[\[ScriptCapitalP],1,
Function[{qu},
	Function[{\[Alpha]},
		If[TrueQ[NumericQ[\[Alpha]]],ExpToTrig[Exp[I*\[Alpha]]],Exp[I*\[Alpha]]]*
		zz075NonCommutativeTimes[
			zz080Bra[zz080Eigenstate[zz080Operator[qu], 1]], 
			zz080Ket[zz080Eigenstate[zz080Operator[qu], 1]]] + 
		zz075NonCommutativeTimes[
			zz080Bra[zz080Eigenstate[zz080Operator[qu], 0]], 
			zz080Ket[	zz080Eigenstate[zz080Operator[qu], 0]]] ]]
];

SetQuantumGate[\[ScriptCapitalH],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalH]]; Abort[],
(-zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]])/Sqrt[2]]]
];

\[ScriptCapitalH][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalH]]; 
	Abort[]
];

SetQuantumGate[\[ScriptCapitalS],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalS]]; Abort[],
I*zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]]]]
];

\[ScriptCapitalS][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalS]]; 
	Abort[]
];

SetQuantumGate[\[ScriptCapitalT],1,
Function[
If[Length[{##}]!=1,
Message[SetQuantumGate::gateonearg,\[ScriptCapitalT]]; Abort[],
ExpToTrig[Exp[I*Pi/4]]*zz075NonCommutativeTimes[
		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 1]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 1]]] + 
 	zz075NonCommutativeTimes[
  		zz080Bra[	zz080Eigenstate[zz080Operator[#1], 0]], 
  		zz080Ket[	zz080Eigenstate[zz080Operator[#1], 0]]]]]
];

\[ScriptCapitalT][q1_,q2__]:=
Module[{},
	Message[SetQuantumGate::gateonearg,\[ScriptCapitalT]]; 
	Abort[]
];

SetQuantumGate[\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN],3];

SetQuantumGate[\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI],3];

\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN][a_,b_,c_]:=zz020Controlled[\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP][b, c], {a}];

\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI][a_,b_,c_]:=zz020Controlled[\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT][c], {a,b}];



SetAttributes[
{  
	zz050Bell,zz050Superpos,
	\[ScriptCapitalR]\[ScriptE]\[ScriptG]\[ScriptI]\[ScriptS]\[ScriptT]\[ScriptE]\[ScriptR],
	QuantumEvaluate,PauliExpand,
	QuantumMatrix,QuantumMatrixForm,QuantumPlot,
	QuantumEigensystem, QuantumEigensystemForm,
			MatrixQuantum, TensorQuantum,
            QubitLabels,QubitList,PauliIdentities,
			QuantumGatePowers,QuantumGateShifting,
			QuantumBackground,
			QuantumWireStyle,
			QuantumMeterStyle,
			QuantumNotStyle,
			QuantumControlStyle,
			QuantumSwapStyle,
			QuantumConnectionStyle,
			QuantumGateStyle,
			QuantumTextStyle,
			QuantumVerticalTextStyle,
			QuantumPlot3D,
	QuantumSparseArray,QuantumTable,QuantumTableForm,QuantumTensor,
	QuantumTensorForm,SetQuantumGate,SetComputingAliases,
	\[Sigma],\[ScriptCapitalB],\[CapitalPsi],\[CapitalPhi],
	QubitMeasurement,
	QuantumMeter,
	zz020CeroOneQ,zz020Controlled,zz020MultiQubit,
	zz020TensorPower,zz020TwoScalarsListQ,
	DecToQubit,QubitToDec,
	\[ScriptK]\[ScriptE]\[ScriptT],
	\[ScriptCapitalI],\[ScriptCapitalX],\[ScriptCapitalY],\[ScriptCapitalZ],\[ScriptZero],
	\[ScriptZero]\[ScriptZero],\[ScriptZero]\[ScriptOne],\[ScriptOne]\[ScriptZero],\[ScriptOne]\[ScriptOne],
	\[ScriptCapitalQ]\[ScriptCapitalF]\[ScriptCapitalT],
	\[ScriptCapitalP],\[ScriptCapitalH],\[ScriptCapitalS],\[ScriptCapitalT],\[ScriptCapitalC],
	\[ScriptCapitalN]\[ScriptCapitalO]\[ScriptCapitalT],
	\[ScriptCapitalS]\[ScriptCapitalW]\[ScriptCapitalA]\[ScriptCapitalP],
	\[ScriptCapitalT]\[ScriptCapitalO]\[ScriptCapitalF]\[ScriptCapitalF]\[ScriptCapitalO]\[ScriptCapitalL]\[ScriptCapitalI],
	\[ScriptCapitalF]\[ScriptCapitalR]\[ScriptCapitalE]\[ScriptCapitalD]\[ScriptCapitalK]\[ScriptCapitalI]\[ScriptCapitalN]
},
{Protected,ReadProtected}
];

If[TrueQ[$VersionNumber>=6.0],
 NotebookOpen["QuantumComputingCommands.nb"];
 NotebookOpen["QuantumComputingKets.nb"];
 NotebookOpen["QuantumComputingGates.nb"]
];



End[];

EndPackage[];
Column[{   
"Quantum`Computing` \n"<>
"A Mathematica package for Quantum Computing in Dirac bra-ket notation"<> 
" and plotting of quantum circuits\n"<>
"by Jos\[EAcute] Luis G\[OAcute]mez-Mu\[NTilde]oz\n"<>
"\nThis add-on does NOT work properly with the debugger turned on."<>
" Therefore the debugger must NOT be checked in the Evaluation menu of"<>
" Mathematica.\n"<>       
"Execute SetComputingAliases[] in order"<>
" to use the keyboard to enter quantum objects"<>
" in Dirac's notation\n"<>
"SetComputingAliases[] must be executed again in"<>
" each new notebook that is created",
"\nMATHEMATICA "<>$Version,
"\nQUANTUM version EN CONSTRUCCION",
"\nTODAY IS "<>DateString[]
}]

