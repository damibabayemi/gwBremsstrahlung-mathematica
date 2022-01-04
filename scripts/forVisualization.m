(* ::Package:: *)

(* ::Section::Closed:: *)
(*to Python converter*)


(* ::Text:: *)
(*python conversion from "Export to Python.nb" by Juan Jos\[EAcute] Garc\[IAcute]a Ripoll https://juanjose.garciaripoll.com/blog/converting-mathematica-to-python/*)


Clear[ToPython];
ToPython[expression_, extravars_ : {}, outputvar_ : "output", indent_ : ""] :=
  Block[{
    (* Python code that precedes our expression.
     Includes auxiliary vars and functions *)
    PythonBuffer = "",
    (* Last number of defined variable *)
    PythonVar = 0,
    (* Spaces to indent each line of Python code *)
    PythonIndent = indent,
    (* Was Sqrt[] used? Then we have to define it *)
    PythonSqrt = False,
    (* Was Pi used? We define \[Pi] variable in Python *)
    PythonPi = False},
   (* We begin by parsing 'extravars' which is a list
   {{var1, exp1},{var2,exp2},...}
   of variables that are used in our formula. This is used for
   simplifying expressions, as shown later on. *)
   Do[
    Module[{var = def[[1]], value = def[[2]]},
     PythonBuffer = PythonBuffer <> PythonIndent <> ToString[var] <> "=" <> ToPython2[value] <> ";\n"],
    {def, extravars}];
   (*
     The actual conversion takes place here, recursively
    calling the function ToPython2, which does the work.
    *)
   Module[{aux = ToPython2[expression]},
    (* If Sqrt[] was used, we introduce a function that works with
      complex numbers, since np.sqrt does not *)
    If[PythonSqrt,
     PythonBuffer = PythonIndent <> "def mysqrt(x): return np.sqrt((1.+0j)*x)\n\n" <> PythonBuffer];
    (* Define \[Pi] as a Python variable *)
    If[PythonPi,
     PythonBuffer = PythonIndent <> "\[Pi]=math.pi;\n\n" <> PythonBuffer];
    (* Output Python code preceded by all variable definitions. *)
    PythonBuffer <> PythonIndent <> outputvar <> "=" <> aux <> ";\n"]];


Clear[ToPythonVar];
ToPythonVar[a_]:=
Module[{name="aux"<>ToString[PythonVar]},
PythonBuffer=PythonBuffer<>PythonIndent<>name<>"="<>a<>";\n";
PythonVar=PythonVar+1;
name];

Clear[AlreadyWrapped];
AlreadyWrapped[s_]:=(StringPosition[s,"("]=={{1,1}})&&(StringPosition[s,")"]=={{StringLength[s],StringLength[s]}});

Clear[PythonWrap];
PythonWrap[expa_,limit_:70]:=
Module[{a=ToPython2[expa]},
If[StringLength[a]>limit,
ToPythonVar[a],
If[Not[AtomQ[expa]]&&!AlreadyWrapped[a],
"("<>a<>")",
a]]];


(* Conversions for the most common expressions *)
Clear[ToPython2];
(*ToPython2[Sqrt[x_]]:=Module[{},
PythonSqrt=True;
"mysqrt("<>PythonWrap[x]<>")"];*)
ToPython2[Log[x_]]:="np.log("<>PythonWrap[x]<>")"
ToPython2[Exp[x_]]:="np.exp("<>PythonWrap[x]<>")"
ToPython2[Sin[x_]]:="np.sin("<>PythonWrap[x]<>")"
ToPython2[Cos[x_]]:="np.cos("<>PythonWrap[x]<>")"
ToPython2[E^x_]:="np.exp("<>PythonWrap[x]<>")"
ToPython2[E^x_]:="np.exp("<>PythonWrap[x]<>")"
ToPython2[a_+b_]:=ToPythonOp["+",a,b]
ToPython2[a_*b_]:=ToPythonOp["*",a,b]
ToPython2[a_-b_]:=ToPythonOp["-",a,b]
ToPython2[a_/b_]:=ToPythonOp["/",a,b]
ToPython2[a_^2]:="("<>PythonWrap[a]<>"**2)"
ToPython2[a_^b_]:=PythonWrap[ToPythonOp["**",a,b]]
ToPython2[x_?NumberQ]:=
If[Head[x]===Complex,
"("<>ToPython2[Re[x]]<>"+"<>ToPython2[Im[x]]<>"j)",
ToString[N[x]]];
ToPython2[-x_]:="(-"<>ToPython2[x]<>")"
ToPython2[\[Pi]]:=Module[{},PythonPi=True;"\[Pi]"]
ToPython2[\[Gamma]]:="\[Gamma]"
ToPython2[\[CapitalOmega]]:="\[CapitalOmega]"
ToPython2[\[CapitalDelta]]:="\[CapitalDelta]"
ToPython2[x_]:=ToString[x];

(* This is for converting inline operations *)
Clear[ToPythonOp];
ToPythonOp[op_,expa_,expb_]:=
PythonWrap[expa]<>op<>PythonWrap[expb];


(* ::Section::Closed:: *)
(*from Gustav*)


getGammas = {\[Beta]1 -> (Sqrt[-1 + \[Gamma]^2]*m[2])/(m[1] + \[Gamma]*m[2]), \[Beta]2 -> (Sqrt[-1 + \[Gamma]^2]*m[1])/(\[Gamma]*m[1] + m[2]), \[Gamma]1 -> (m[1] + \[Gamma]*m[2])/Sqrt[m[1]^2 + 2*\[Gamma]*m[1]*m[2] + m[2]^2], 
 \[Gamma]2 -> (\[Gamma]*m[1] + m[2])/Sqrt[m[1]^2 + 2*\[Gamma]*m[1]*m[2] + m[2]^2]};


getProperTimes = {u[1] -> (-(mB*Cos[\[Phi]]*Sin[\[Theta]])/2 + u[0])/(\[Gamma]1 - \[Beta]1*\[Gamma]1*Cos[\[Theta]]), u[2] -> ((mB*Cos[\[Phi]]*Sin[\[Theta]])/2 + u[0])/(\[Gamma]2 + \[Beta]2*\[Gamma]2*Cos[\[Theta]])};


getLengths = {mBt[0] -> Sqrt[mB^2 - u[1]^2 + 2*\[Gamma]*u[1]*u[2] - u[2]^2], mBt[1] -> Sqrt[mB^2 + (-1 + \[Gamma]^2)*u[2]^2], mBt[2] -> Sqrt[mB^2 + (-1 + \[Gamma]^2)*u[1]^2]};


waveformP = (2*mB^2*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]2*\[Gamma]2*Cos[\[Theta]]^3*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]^3*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Gamma]1*Cos[\[Theta]]^3*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]^3*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 - \[Beta]1*Cos[\[Theta]])) - (8*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 - \[Beta]1*Cos[\[Theta]])) + 
 (4*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 + \[Beta]2*Cos[\[Theta]])) - (8*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 + \[Beta]2*Cos[\[Theta]])) + 
 (6*\[Beta]2^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - (4*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + 
 (2*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - (4*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - 
 (6*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + (4*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + (2*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - 
 (4*\[Beta]2^3*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + (12*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[1]) + (4*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[1]) + 
 (2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - (4*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - 
 (4*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2^2*\[Gamma]2^3*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1^3*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (16*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (16*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]2^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2^3*\[Gamma]2^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (6*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - (12*mB^2*\[Beta]1*\[Beta]2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - (12*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - (8*mB^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + (4*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Beta]2^3*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + (16*mB^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2^2*\[Gamma]2^3*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (16*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (16*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (24*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (24*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]2^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Beta]2^3*\[Gamma]*\[Gamma]2^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2^3*\[Gamma]2^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]1^3*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (20*mB^2*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (16*mB^2*\[Beta]1*\[Beta]2^3*\[Gamma]*\[Gamma]2^2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2^4*\[Gamma]2^3*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (12*mB^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (12*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (16*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (16*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (12*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (12*mB^2*\[Beta]2^3*\[Gamma]*\[Gamma]2^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Beta]2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) - (4*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + 
 (6*\[Beta]1^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) - (4*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) - 
 (2*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + (4*\[Beta]1^3*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + (6*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[2]) + (12*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[2]) - (2*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + 
 (4*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + (8*mB^2*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - (8*mB^2*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1^3*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (16*mB^2*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]2^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - (4*mB^2*\[Beta]1^2*\[Gamma]1^3*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - (2*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + (16*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^3*\[Gamma]1^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]1^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]2^3*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^2*\[Gamma]1^3*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (24*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1^3*\[Gamma]1^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1^3*\[Gamma]*\[Gamma]1^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (24*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]1^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (20*mB^2*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^4*\[Gamma]1^3*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*mB^2*\[Beta]1^3*\[Beta]2*\[Gamma]*\[Gamma]1^2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1*\[Beta]2^3*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*mB^2*\[Beta]1*\[Beta]2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - (12*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1^2*m[1]*m[2]*Sin[\[Theta]]^2)/
  ((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (6*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB^2*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB^2*\[Beta]1^3*\[Gamma]*\[Gamma]1^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3)/(mB*Sqrt[-1 + \[Gamma]^2]*(1 - \[Beta]1*Cos[\[Theta]])^2) + (4*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3)/(mB*Sqrt[-1 + \[Gamma]^2]*(1 - \[Beta]1*Cos[\[Theta]])^2) + 
 (2*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3)/(mB*Sqrt[-1 + \[Gamma]^2]*(1 + \[Beta]2*Cos[\[Theta]])^2) - (4*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3)/(mB*Sqrt[-1 + \[Gamma]^2]*(1 + \[Beta]2*Cos[\[Theta]])^2) + 
 (\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[1]) - (2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (mB^2*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/
  (\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^5*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (2*mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/
  (\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^5*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]2^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (2*mB^2*\[Beta]2^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/
  (\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + (6*mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/
  (\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - (3*mB^2*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1^3*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (5*mB^2*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^4*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^5*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^3*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^5*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Beta]2^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (9*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (9*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1^3*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1^2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (6*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]2^3*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (13*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (5*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (9*mB^2*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (7*mB^2*\[Beta]1*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*mB^2*\[Beta]1*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (7*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (11*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (14*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (5*mB^2*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (7*mB^2*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (17*mB^2*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[2]) - (2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[2]) + 
 (2*mB^2*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - (2*mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - (mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^5*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (2*mB^2*\[Beta]1^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - (mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^5*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + (6*mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - (3*mB^2*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (5*mB^2*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^4*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^3*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2^3*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^3*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (9*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (9*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2^3*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (5*mB^2*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (7*mB^2*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (17*mB^2*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (11*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (14*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^5*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1^2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^3*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^5*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (7*mB^2*\[Beta]1*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB^2*\[Beta]1*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (7*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (13*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (5*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (9*mB^2*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4)/(2*(-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^4)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (3*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(4*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]2^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(4*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(4*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(4*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^4*m[1]*m[2]*Sin[\[Theta]]^6)/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB^2*\[Gamma]2*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Gamma]*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Gamma]1*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Gamma]*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Phi]]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (mB^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]^2)/
  (\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - (2*mB^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]^2)/
  (\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + (mB^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]^2)/
  (\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - (2*mB^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]^2)/
  (\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (4*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/(mB*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - 
 (8*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/(mB*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) - (8*mB*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + (8*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + (8*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + (4*mB*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - (4*mB*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + (8*mB*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + (2*mB*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - (4*mB*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + (4*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/(mB*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) - 
 (8*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/(mB*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + (8*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - (8*mB*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + (8*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + (8*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - (8*mB*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (8*mB*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (4*mB*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - (8*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - (2*mB*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + (4*mB*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + (4*mB*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - (8*mB*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]*u[0])/
  (\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + (2*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(mB*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(mB*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[1]) - (4*mB*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/
  (\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (4*mB*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^2*\[Gamma]^5*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/
  (\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (4*mB*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^5*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]2^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (8*mB*\[Beta]2^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/
  (\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*mB*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + (20*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/
  (\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - (4*mB*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (24*mB*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (24*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*mB*\[Beta]1*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1^3*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (20*mB*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^4*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^5*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*mB*\[Beta]1*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^3*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*mB*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^5*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*mB*\[Beta]1*\[Beta]2^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]1^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (24*mB*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (12*mB*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]1^3*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]1^2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (16*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^3*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (18*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*mB*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*mB*\[Beta]1^2*\[Gamma]*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (20*mB*\[Beta]1*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (2*mB*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Beta]2^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (9*mB*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*mB*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(mB*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[2]) + (4*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(mB*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[2]) - 
 (8*mB*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (8*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (4*mB*\[Beta]1^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Gamma]^5*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - (8*mB*\[Beta]1^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (4*mB*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^5*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - (20*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + (4*mB*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (24*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (24*mB*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (20*mB*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^4*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*mB*\[Beta]1^3*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2^3*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1*\[Beta]2^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^3*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (24*mB*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*mB*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]2^3*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*mB*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*mB*\[Beta]1*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (16*mB*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^5*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (16*mB*\[Beta]1^2*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^3*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (16*mB*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^5*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (20*mB*\[Beta]1*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (18*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*mB*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (9*mB*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^3*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]2^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (9*mB*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (9*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]2^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]2^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]1^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(2*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (9*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (9*mB*\[Beta]2^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]1^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]1^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]1^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (9*mB*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (9*mB*\[Beta]1^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]2^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(2*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (9*mB*\[Beta]1^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (9*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^5*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*mB*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (2*mB*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]^2*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]2^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (4*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/
  (\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (4*\[Beta]2^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]^5*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (8*\[Beta]2^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1*\[Beta]2^2*\[Gamma]^5*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - (8*\[Beta]2^3*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/
  (\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (8*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + (16*\[Beta]2^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]2^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2^2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (24*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (24*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]1*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^3*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (20*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^4*\[Gamma]2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^5*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]1*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^3*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^3*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^2*\[Beta]2*\[Gamma]^5*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]1*\[Beta]2^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (24*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1^3*\[Gamma]*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1^2*\[Beta]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^3*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (10*\[Beta]2^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (14*\[Beta]2^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (2*\[Beta]1^2*\[Gamma]*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (2*\[Beta]1^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (2*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]2^2*\[Gamma]^2*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (24*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (18*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (24*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - (8*\[Beta]1^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (4*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/
  (\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - (4*\[Beta]1^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]^5*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (8*\[Beta]1^3*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/
  (\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - (4*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Beta]2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^2*\[Beta]2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Beta]2*\[Gamma]^5*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + (16*\[Beta]1^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) - (8*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/
  ((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (24*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (24*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (20*\[Beta]1^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^4*\[Gamma]1*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]1^3*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2^3*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]2^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]2^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1*\[Beta]2^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^3*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^3*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (24*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]2^3*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]2^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*\[Beta]2^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (2*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]1^2*\[Gamma]^2*\[Gamma]1*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]1*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1*\[Beta]2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^5*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1^2*\[Beta]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^3*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^3*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1*\[Beta]2^2*\[Gamma]*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2^2*\[Gamma]^3*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2^2*\[Gamma]^5*\[Gamma]2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (10*\[Beta]1^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (14*\[Beta]1^2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (2*\[Beta]2^2*\[Gamma]*\[Gamma]2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (24*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (18*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (24*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (18*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (18*\[Beta]1*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]2^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]2^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (3*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (18*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (24*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (9*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]1^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]1^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (18*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (18*\[Beta]1^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*\[Beta]2^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (9*\[Beta]1^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (18*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (24*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^4*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*\[Gamma]*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Gamma]^3*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*\[Gamma]^2*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Gamma]^2*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Gamma]*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Gamma]^3*m[1]*m[2]*Sin[\[Phi]]^2*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (16*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]1*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1^2*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^2*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]2^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (2*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^3*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (2*\[Beta]2^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]2^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]1^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]1^2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1^2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^3*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]);


waveformX = (-4*mB^2*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2*\[Gamma]^2*\[Gamma]2*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1*\[Gamma]^2*\[Gamma]1*Cos[\[Theta]]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 - \[Beta]1*Cos[\[Theta]])) + (8*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 - \[Beta]1*Cos[\[Theta]])) - 
 (4*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 + \[Beta]2*Cos[\[Theta]])) + (8*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]])/(mB*Sqrt[-1 + \[Gamma]^2]*(1 + \[Beta]2*Cos[\[Theta]])) - 
 (2*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + (4*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2*\[Gamma]^2*\[Gamma]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1^2*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (4*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (2*mB^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (2*\[Beta]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) - (4*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1*\[Gamma]^2*\[Gamma]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/((-1 + \[Gamma]^2)*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (2*mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (mB^2*\[Beta]2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (mB^2*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (mB^2*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (3*mB^2*\[Beta]1*\[Gamma]*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(2*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (2*mB^2*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]^3*m[1]*m[2]*Sin[\[Theta]]^4*Sin[\[Phi]])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(mB*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + (8*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(mB*\[Gamma]2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[1]) + 
 (8*mB*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*mB*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*mB*\[Beta]1*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (16*mB*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*mB*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (8*mB*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]1*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(mB*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) + (8*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(mB*\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*mBt[2]) - 
 (8*mB*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]1*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (4*mB*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*mB*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*mB*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*mB*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (16*mB*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*mB*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (9*mB*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*mB*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (3*mB*\[Beta]1*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (3*mB*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (3*mB*\[Beta]1*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]2*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*mB*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (3*mB*\[Beta]2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (6*mB*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (9*mB*\[Beta]1*\[Gamma]*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*mB*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]^2*m[1]*m[2]*Sin[\[Theta]]^3*Sin[\[Phi]]*u[0])/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (16*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) + 
 (16*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[1]) - 
 (8*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (16*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (24*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (18*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (24*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (6*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (6*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (6*\[Beta]1*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]2*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (6*\[Beta]2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (24*\[Beta]2*\[Gamma]^2*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (12*\[Beta]2*\[Gamma]^4*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (18*\[Beta]1*\[Gamma]*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (24*\[Beta]1*\[Gamma]^3*Cos[\[Phi]]*m[1]*m[2]*Sin[\[Theta]]^2*Sin[\[Phi]]*u[0]^2)/(\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (8*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/
  (mB*\[Gamma]2^3*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + (8*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]2^2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2^2*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^4*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1^2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]1*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]2^3*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (12*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) - 
 (16*\[Beta]2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])*(1 + \[Beta]2*Cos[\[Theta]])^3*mBt[0]^2*mBt[1]) + 
 (4*\[Beta]1*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (4*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) + 
 (8*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[1]) - 
 (8*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (8*\[Beta]1*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/
  (mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*mBt[0]^2*mBt[2]) + (8*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2^2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]2^2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) - 
 (4*\[Beta]1*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1*\[Gamma]2^2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]2*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^2*(1 + \[Beta]2*Cos[\[Theta]])^2*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^2*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1*\[Beta]2*\[Gamma]^4*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]1^2*\[Gamma]*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (8*\[Beta]1^2*\[Gamma]^3*Cos[\[Theta]]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^4*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (4*\[Beta]2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]2*\[Gamma]^2*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (8*\[Beta]2*\[Gamma]^4*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^3*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) + 
 (12*\[Beta]1*\[Gamma]*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]) - 
 (16*\[Beta]1*\[Gamma]^3*m[1]*m[2]*Sin[\[Theta]]*Sin[\[Phi]]*u[0]^3)/(mB*\[Gamma]1^2*\[Gamma]2*(1 - \[Beta]1*Cos[\[Theta]])^3*(1 + \[Beta]2*Cos[\[Theta]])*mBt[0]^2*mBt[2]);


(* ::Section:: *)
(*do the conversion*)


(*$RecursionLimit = 10000;*)


myAux = {
	gamma^2-1 -> gm1, Sqrt[gm1] -> rgm1,
	(m1^2 + m2^2 + 2 m1 m2 gamma)^(-1/2) -> em
};
myAux2 = {1/Sqrt[gm1] -> 1/rgm1};
myAuxInv = myAux /. Rule[a_,b_] :> {b, a};
defs = {
	{r, Sqrt[x^2+y^2+z^2]},
	{costh, x/r}, {sinth, Sqrt[1-costh^2]},
	{cosph, y/(r sinth)}, {sinph, z/(r sinth)}, {tr, t-r}
};
\[Beta]rule = {1 - beta1 costh -> beta1cos, - 1 + beta1 costh -> - beta1cos, 1 + beta2 costh -> beta2cos};
\[Beta]Inv = {{beta1cos, 1 - beta1 costh}, {beta2cos, 1 + beta2 costh}};


ToPythonNotation[expr_] := expr /. Rule -> List /. {
	m[1] -> m1, m[2] -> m2,
	mBt[0] -> mBt0, mBt[1] -> mBt1, mBt[2] -> mBt2,
	u[1] -> u1, u[2] -> u2,
	u[0] -> tr, mB -> b,
	Cos[\[Theta]] -> costh, Sin[\[Theta]] -> sinth,
	Cos[\[Phi]] -> cosph, Sin[\[Phi]] -> sinph,
	\[Gamma] -> gamma, \[Gamma]1 -> gamma1, \[Gamma]2 -> gamma2, \[Beta]1 -> beta1, \[Beta]2 -> beta2
} //. Join[myAux, myAux2];


vars = Join[defs, myAuxInv, Join[getGammas, getProperTimes, getLengths] // ToPythonNotation, \[Beta]Inv]


(* simplify waveform *)
waveformSimp = Collect[{waveformP, waveformX} // ToPythonNotation, {r, m1, m2, mBt0, mBt1, mBt2, b, tr}, Simplify] /. myAux /. \[Beta]rule;
(* generate code *)
waveformPPython = ToPython[waveformSimp[[1]] // ToPythonNotation, vars, "pluspol", "    "] <> "    return pluspol";
waveformXPython = ToPython[waveformSimp[[2]] // ToPythonNotation, vars, "crosspol", "    "] <> "    return crosspol";


(* put together *)
code = (
"import numpy as np
import numba

@numba.njit(cache=True)
def waveformPlus(x, y, z, t, b, gamma, m1, m2):
" <> waveformPPython <> "

@numba.njit(cache=True)
def waveformCross(x, y, z, t, b, gamma, m1, m2):
" <> waveformXPython);


(* save python code *)
Export[NotebookDirectory[]<>"waveform.py", code, "Text", CharacterEncoding -> "UTF8"];


(* Cython version, does not seem to be faster *)
codeC = (
"# cython: infer_types=True
cimport cython
import numpy as np
# cimport numpy as np

def waveformPlus(float x, float y, float z, float t, float b, float gamma, float m1, float m2):
" <> waveformPPython <> "

def waveformCross(float x, float y, float z, float t, float b, float gamma, float m1, float m2):
" <> waveformXPython
);


(*Export[NotebookDirectory[]<>"waveformC.pyx", codeC, "Text", CharacterEncoding -> "UTF8"];*)
