(* ::Package:: *)

(*    Mathematica  tools for element tests with BBM in isomorphic PQ space  by A.Niemunis (2011)

This  notebook produces stress/strain path according to the Barcelona Basic model.
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

AN modifications in 2015:
-- better graphics: system origin, unified call to all graphic routines,
-- simplified all plots (no multiple Paths)
-- undo option rendered silly-proof,
-- RunBbmTest checks \[Lambda] < o in plastic solution and if so interrupts calculations
-- RunBbmTest works with AppendTo[ ] instead of fixed length of path
-- repaired Normalization of flow rule
-- new routine to completeState[ state , Mat] for default initial conditions and for the very first incr.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (= BBM.m) to the new directory.
4) Begin a Mathematica session with:    Needs["PQ`BBM2015`"]
*)


BeginPackage["PQ`BBM2015`",{"PQ`BBMgraphics`"}]

(* Off[General::spell,General::spell1,Solve::ifun]; *)
(*---------------------------------------------usages--------------------------------------------------------------------*)



completeState::usage = "state = completeState[ state , Mat]; calculate consistent values of 4 variables \[Epsilon]P,epor, P0, \[Epsilon]pP  on the list of state variables \
 \n  {P,Q,P0s, \[Epsilon]P,\[Epsilon]Q,epor, suc,P0, \[Epsilon]pP, \[Epsilon]pQ}  using  the remaining values of state as correct ones. The mat constants \
 \n   Mat ={\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef, eporRef} are all correct.    ";

 RunBbmTest::usage = "RunBbmTest[  Mat,ICond,{ntime,deps,dT}] \
 \n  RunBbmTest returns the sequence of states {P, Q, Pc, \[Epsilon]P, \[Epsilon]Q, epor, suc, P0}
 \n  which are incrementally calculated according to BBM model within the module \
 \n  using the material parameters Mat = {\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s,sucRef,eporRef} (Butterfield's compression + swelling) \
 \n  and the loading programme  {ntime,deps,dsuc, dT}. \
 \n  The loading programme  consists of : \
 \n  ntime = the number of repetitions,  \
 \n  deps = {depsP, depsQ} = strain increment, \
 \n  dT = {dP,dQ} = stress increment. \
 \n  dsuc = suction increment. \
 \n  Two of the four incremental values: depsP, depsQ, dP, dQ  must be prescribed (given as numbers) \
 \n  The suction increment dsuc must be always prescribed \
 \n  and the remaining two must be calculated and thus they should be specified as symbols.   \
 \n  " ;

getElasticIsomorph::usage = "getElasticIsomorph[Mat,state]  returns the isotropic hypoelastic stiffness for the isomorphic P-Q-space    \
\n  in the form {{3K,0},{0,2G}} with the bulk modulus K and shear modulus G, both barotropic.  \
";
controlPanelBBM::usage = "controlPanelBBM[ ] loads a graphic interface to perform element tests using BBM.   \
\n The initial conditions and parameters must be saved. \
\n Two stress or strain or mixed increments must be prescribed. The suction increment must be prescribed.
\n Push the button calculate to see the results after the increment.  \
\n The number of increments is ntime, but can be modified writing a new number in the cell ntime. \
\n If you want to start with a brand new simulation, click on Reset. \
\n If you wish to undo the last step, please click on the button Undo. \  ";

inputCheck::usaage = " inputCheck[Mat_, Icond_, Loading_]   check the ranges ";
symbolQ::usaage = " symbolQ[t_]     returns True if argument is symbolic";
inRange::usaage = " inRange[x_, range_]   returns True if argument is within the range ";

Mat::usage=" Mat returns the list of BBM meterial constants:  {\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef, eporRef}  ";
ICond::usage=" a global variable stores  the initial conditions =  a list of  state variables {P  ,Q, P0s, \[Epsilon]P,\[Epsilon]Q, epor, suc, P0,   \[Epsilon]pP,\[Epsilon]pQ}  ";
path::usage=" a global variable stores the chronological sequence of states   {P  ,Q, P0s, \[Epsilon]P,\[Epsilon]Q, epor, suc, P0,   \[Epsilon]pP,\[Epsilon]pQ} ";
dynamicCell::usage="set dynamicCell=True in order to Enable dynamic cell effects";


g1::usage="  public graphics   g1    "
g2::usage="  public graphics   g2    "
g3::usage="  public graphics   g3    "
g4::usage="  public graphics   g4    "
g5::usage="  public graphics   g5    "
g6::usage="  public graphics    g6   "
g7::usage="  public graphics    g7   "
g8::usage="  public graphics    g8   "
g9::usage="  public graphics    g9   "





Begin["`Private`"]

(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];
inRange[x_, range_] := IntervalMemberQ[Interval[range], x];
getElasticIsomorph[Mat_,state_ ] := Module[{elastG,elastK, \[Nu],\[Kappa], P },
              {\[Nu],\[Kappa] }= Mat[[1;;2]]; P= state[[1]];
             elastK=P/(\[Kappa]*Sqrt[3]);       elastG=elastK (3.0-6.0 \[Nu])/(2.0+2.0 \[Nu]);
            Simplify[{{3 elastK,0},{0,2 elastG}}]   ];

(* calculate consistent values of  P0 \[Epsilon]P \[Epsilon]pP  *)
completeState[ state_ , Mat_] := Module[
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef, eporRef,P,Q,P0s,x\[Epsilon]P,\[Epsilon]Q,epor, suc,P0,x\[Epsilon]pP, xP0,xepor, \[Epsilon]pQ,\[Lambda]s,\[Epsilon]s,\[Epsilon]vol,\[Epsilon]plvol,sq3=1.7320508075688772 }   ,
 {\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef, eporRef} =    Mat;
 {P,Q,P0s,x\[Epsilon]P, \[Epsilon]Q,xepor, suc,xP0,x\[Epsilon]pP, \[Epsilon]pQ}   = state ;
 \[Lambda]s=\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr);
  P0=Pref*(P0s/Pref)^((\[Lambda]0-\[Kappa])/(\[Lambda]s-\[Kappa]));
  \[Epsilon]s =  \[Kappa]s Log[(suc+ sucRef)/sucRef];
  \[Epsilon]vol = \[Epsilon]s + \[Lambda]s Log[P0/Pref]  - \[Kappa] Log[P0/P];
  \[Epsilon]plvol = \[Epsilon]vol  - \[Epsilon]s  - \[Kappa] Log[P/Pref]  ;
  epor =  (1 + eporRef)*Exp[-\[Epsilon]vol]-1;
 { P,Q,P0s, \[Epsilon]vol/sq3 ,\[Epsilon]Q, epor, suc, P0, \[Epsilon]plvol/sq3,\[Epsilon]pQ }
];

RunBbmTest[Mat_,ICond_,Loading_]:=
       Module[{sq3 = 1.732050807569, states,\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks ,Pref,\[Kappa]s ,sucRef,MC,ME,M, P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor,suc,
                     P0,itime,elastStiff,  unknowns,solution,  d\[Epsilon]Trial ,dTTrial,PTrial,QTrial ,yield, mflow, Ks,\[Epsilon]vol, \[Epsilon]s,
                     loadingQ , \[Lambda], P1,Q1,P0s1, eq1a, eq1b, eq2a ,eq2b, eq3, eq4, eq5,unknowns1, eporRef, ntime, d\[Epsilon], dsuc,d\[Epsilon]s, dT,
                     approxTF, approx,approxi, unknowns1approxi ,solution1, dT2 ,d\[Epsilon]2,   \[Lambda]2,s1,s2,\[Lambda]s, Pss,P01,P02,\[Alpha], \[Epsilon]pP, \[Epsilon]pQ},
{ntime, d\[Epsilon], dsuc, dT}=  N[ Loading[[1;;4]] ];     (* recover loading *)
If[Length[Mat] != 11, Print["Error: expected 11 material constants"]; Abort[];  ];
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks , Pref,  \[Kappa]s , sucRef, eporRef}=Mat;   (* recover the mat. constants,  \[Alpha] is scalar factor to adjust NAFR  to K0 conditions with Jaky's equation*)
MC = Sqrt[2] /3*(6.0* Sin[\[CurlyPhi]])/( 3-Sin[\[CurlyPhi]] ) ;  ME = Sqrt[2] /3*( 6.0 *Sin[\[CurlyPhi]]   )/( 3+Sin[\[CurlyPhi]] ) ;   \[Alpha]=1;
states = { ICond //N }; (*Initial conditions*)

For[itime=1, itime <= ntime,
{P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P0,\[Epsilon]pP,\[Epsilon]pQ} =  states[[-1]] // N;
suc+=dsuc; suc = Max[suc,0];
Ks = (  suc+ sucRef  )/\[Kappa]s ;  d\[Epsilon]s = { dsuc /( Ks*sq3 ), 0 };
elastStiff=getElasticIsomorph[Mat, states[[-1]] ]; (*Elastic matrix*)
unknowns=Select[Flatten[{d\[Epsilon],dT}],symbolQ];
solution = Solve[ dT==elastStiff.( d\[Epsilon]-d\[Epsilon]s ),unknowns][[1]]; (*Trial el. increment with  strain incr. diminished by suction*)
{d\[Epsilon]Trial ,dTTrial}= {d\[Epsilon],dT}  /. solution;  (*Trial step solution*)
M=If[Q+ dTTrial[[2]]>= 0,MC,ME]; (*Critical state slope for compression and extension*)
{PTrial,QTrial} = {P,Q} + dTTrial;
\[Lambda]s=\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr); (*  Lambda depending on suction  *)
P0=Pref*(P0s /Pref)^((\[Lambda]0-\[Kappa])/(\[Lambda]s-\[Kappa])) ; (*  Preconsolidation pressure with suction  *)
Pss=ks *suc; (*  Tensile strength  *)
yield=(QTrial /M)^2-(PTrial+Pss)*(P0-PTrial); (*  Yield surface  *)
If[yield>0.0 PTrial^2,   loadingQ = True, loadingQ=False] ;
If[\[Not]loadingQ ,(****************  Elastic step *****************)
 {\[Epsilon]P,\[Epsilon]Q}+=  d\[Epsilon]Trial ;
epor+=-(1.0+epor)*d\[Epsilon]Trial[[1]]*sq3;
{P,Q}+= dTTrial;   Goto[nextInc] ;
] ;
(*****************  Plastic step ******************)
P01=Pref*(P0s1/Pref)^(( \[Lambda]0-\[Kappa]  )/( \[Lambda]s-\[Kappa] )); (*P0s1 is the unknown saturated preconsolidation  at the end of the increment *)
mflow={2*P1+Pss-P01,((2*Q1) *\[Alpha] )/M^2} // Normalize  ;
(*Set of equations to be solved numerically *)
eq1a = dT[[1]]  == ( elastStiff. (d\[Epsilon] - d\[Epsilon]s - \[Lambda] * mflow) )[[1]];  (*dP implicit*)
eq1b = dT[[2]]  == ( elastStiff. (d\[Epsilon] - d\[Epsilon]s - \[Lambda] * mflow) )[[2]]; (*dQ implicit*)
eq2a = P1  == P  + dT[[1]] ;  (*P implicit*)
eq2b = Q1  == Q  + dT[[2]]; (*Q implicit*)
eq3 = (Q1 /M)^2-(P1+Pss)*(P01-P1) ==0; (* Yield surface *)
eq4=P0s1==P0s + P0s *\[Lambda] * mflow[[1]] * sq3  /( \[Lambda]0-\[Kappa] ) ;   (* volumetric hardening *)
unknowns1 =  {unknowns, \[Lambda], P0s1, P1,Q1 } //Flatten;
approxTF = symbolQ[#]& /@  (Flatten[{d\[Epsilon], dT}]);
approx = Pick[ Flatten[ {d\[Epsilon]Trial, dTTrial}], approxTF];
approxi  = {approx,0, P0s, PTrial,QTrial} //Flatten ;
unknowns1approxi = Transpose[{unknowns1, approxi}];
solution1 = FindRoot[{eq1a,eq1b, eq2a,eq2b, eq3,eq4}, unknowns1approxi,AccuracyGoal-> 4 ] ;
If[Evaluate[\[Lambda] /. solution1] < 0, Print["Exited loop after  plastic solution with \[Lambda] < 0" ]; Goto[exitInc];  ];
{dT2 ,d\[Epsilon]2,   \[Lambda]2 , P0s  ,P  ,  Q}={ dT ,  d\[Epsilon] ,  \[Lambda] , P0s1,  P1, Q1}/. solution1;
{\[Epsilon]P,\[Epsilon]Q} += d\[Epsilon]2;
{\[Epsilon]pP,\[Epsilon]pQ}+=\[Lambda] * mflow/. solution1;
Label[nextInc];  (* common for elastic & plastic case  *)
P02 = Pref*(P0s/Pref)^((\[Lambda]0-\[Kappa])/(\[Lambda]s-\[Kappa]));
\[Epsilon]s =\[Kappa]s Log[(suc+sucRef)/(0 + sucRef)] ;
\[Epsilon]vol = \[Epsilon]s + \[Lambda]s Log[P0/Pref]  - \[Kappa] Log[P0/P];
epor  = (1+eporRef) Exp[-\[Epsilon]vol]-1  ;
AppendTo[ states, {P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P02,\[Epsilon]pP,\[Epsilon]pQ} ];
itime++;
];
Label[exitInc];
(* return the path and exit *)   states
]  ;


 inputCheck[Mat_, Icond_, Loading_]:= Module[
  {P, Q, \[Nu], \[Kappa], \[Lambda]0, \[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef, P0s, \[Epsilon]P, \[Epsilon]Q, epor, ntime, deP, deQ, dP, dQ, suc, dsuc, P0,\[Epsilon]pP,\[Epsilon]pQ,eporRef,OCR,PcPlus,MC,ME,M,check1,check2},
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef,eporRef }=Mat;
{P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P0,\[Epsilon]pP,\[Epsilon]pQ} = Icond;
{ ntime, {deP,deQ}, dsuc, {dP,dQ} } =   Loading[[1;;4]]// N  ;
 If[symbolQ[ntime] , Print[ "number of increments  ntime =  ", ntime," undefined"   ]]  ;
  If[ ntime < 10 , Print[ "suspiciously small number of increments  ntime =  ", ntime   ]]  ;
  If[ deP > 0.005 || deQ > 0.005 , Print[ "Strain increments seem to be large {deP, deQ} = ", {deP, deQ}  , " Keep them at  0.1% level"   ]]  ;
  If[ dP > 10 || dQ > 10 , Print[ "Stress increments seem to be large {dP, dQ} = ", {dP, dQ}  , " Keep them below  10 kPa "   ]]  ;
  If[\[Nu] ~inRange~ {0, 0.5} , Null , Print[ "Suspicious \[Nu] =  ",\[Nu]   ]]  ;
  If[ \[Kappa] ~inRange~ {0, 0.2} , Null , Print[ "Suspicious \[Kappa] =  ",\[Kappa]  ]]  ;
  If[\[Lambda]0 ~inRange~ {  \[Kappa] , 20  \[Kappa] } , Null , Print[ "Suspicious \[Kappa]/\[Lambda]0 =  ",\[Kappa] / \[Lambda]0 ]]  ;
  If[ \[CurlyPhi] ~inRange~ {5\[Degree], 50\[Degree]} , Null , Print[ "Suspicious \[CurlyPhi]=  ", \[CurlyPhi], "rad"  ]]  ;
  If[ rr ~inRange~ {0, 1} , Null , Print[ "Suspicious rr=  ", rr  ]]  ;
  If[ \[Beta] ~inRange~ {0, 10000000} , Null , Print[ "Suspicious \[Beta] =  ", \[Beta] ]]  ;
  If[ ks ~inRange~ {0, 1} , Null , Print[ "Suspicious ks =  ", ks]]  ;
  If[ Pref ~inRange~ {0, 10000} , Null  , Print[ "Suspicious Pref =  ", Pref]]  ;
  If[ \[Kappa]s ~inRange~ {0, 1000} , Null , Print[ "Suspicious \[Kappa]s =  ", \[Kappa]s]]  ;
  If[ sucRef ~inRange~ {0, 100000} , Null , Print[ "Suspicious sucRef =  ", sucRef]]  ;
  If[P < 0 , Print[ "Warning: negative initial  P =  ", P   ]]      ;
  If[P0s < P , Print[ "Warning:  initial  P0s =  ", P0s , "<", P, " =P" ]]  ;
  If[P0 < P , Print[ "Warning:  initial  P0s =  ", P0s , "<", P, " =P" ]]  ;
  MC=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3-Sin[\[CurlyPhi]]));   ME=(Sqrt[2] (6  Sin[\[CurlyPhi]]))/(3 (3+Sin[\[CurlyPhi]]));   M=If[Q >=0,MC,ME];
  If[ P^2+(Q/M)^2-P*P0s >  P0s*P0s *10^-6, Print[ "Warning: initial OCR<1 :  P^2+(Q/M)^2-P*P0s  = ", P^2+(Q/M)^2-P*P0s  , "> 0" ] ]  ;
  PcPlus  =  P +(Q/M)^2 /P ;     OCR = P0s/PcPlus;
  If[ OCR ~inRange~ {1.2,2},  Print[ "Initial  OCR =", OCR, ", soil was lightly overconsolidated " ] ];
  If[ OCR ~inRange~ {1.0,1.01},  Print[ "Initial OCR =", OCR, ", soil was  normally consolidated " ] ];
  If[ OCR ~inRange~ {1.01,1.2},  Print[ "Initial OCR =", OCR, ", soil was  almost  normally consolidated " ] ];
  If[ OCR ~inRange~ {2,4},  Print[ "Initial OCR =", OCR, ", soil was  overconsolidated " ] ];
  If[ OCR ~inRange~ {4, Infinity},  Print[ "Initial OCR =", OCR, ", soil was heavily overconsolidated " ] ];
  If[epor  ~inRange~ {0.01, 3} , Null , Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 3} "  ]];
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True] > 2,  Print["Error: Too many unknowns in Loading = ", Loading ];  Abort[]; ];
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True]  < 2,  Print["Error: Too few unknowns in Loading = ", Loading ];  Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, True, False, True},  Print["Error: You cannot prescribe deP and dP in Loading simultaneously"];  Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == {True, False, True,False},  Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously"];  Abort[]; ];
 PrescribedStressPath  =  Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, False, True, True} ;
 If[ PrescribedStressPath  ,
      P +=  ntime *dP;      Q += ntime*dQ;
      M=If[Q >=0,MC,ME];
      If[ P^2+(Q/M)^2-P*P0s >  P0s*P0s *10^-6, Print[ "Warning: the stress path ends above CSL; strain or mixed control recommended " ] ]  ;
 ];
   If[Length[path] > 1,
              { P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor,suc,P0, \[Epsilon]pP,\[Epsilon]pQ }= Last[path];
                M=If[Q >=0,MC,ME];
                PcPlus  =  P +(Q/M)^2 /P ;
                Print["The latest value of OCR =", P0s/PcPlus];
  ];
 check1=  {{"\[Nu]=", "\[Kappa]=", "\[Lambda]0=", "\[CurlyPhi]=", "rr=", "\[Beta]=",  "ks=", "Pref=", "\[Kappa]s=", "sucRef=", "eporRef="}, Mat } // Transpose// MatrixForm ; 
 check2= {{"P="  ,"Q=",  "P0s=", "\[Epsilon]P=", "\[Epsilon]Q=",   "epor=", "suc=",  "P0=",   "\[Epsilon]pP=","\[Epsilon]pQ="},ICond } // Transpose// MatrixForm ;
 Print[check1,check2];
];


controlPanelBBM[] := DynamicModule[{ Loading },
(*Default possibly inconsistent values for initial visualization of graphics*)
 Manipulate[
{g1,g2,g3,g4,g5,g6,g7,g8,g9} = getGraphics[path, Mat];
Show[GraphicsGrid[{{g1,g3},{g4,g5},{g8,g9}},ImageSize -> Scaled[.9]]],
 Delimiter,
"Number of increments per step:",
 {{ntime, 50,"ntime"}},
 Delimiter,
 "Increments (in [-]or kPa):",
 {{d\[Epsilon]P, xd\[Epsilon]P,"deP"}},
 {{d\[Epsilon]Q, xd\[Epsilon]Q,"deQ"}}  ,
 {{dP, 1,"dP"}}  ,
 {{dQ, 1,"dQ"}}  ,
 {{dsuc, 0,"dsuc"}},
Delimiter,
 Button["Check   input",
  inputCheck[Mat, ICond, {ntime, {d\[Epsilon]P, d\[Epsilon]Q}, dsuc, {dP, dQ}} ]
 ],
   Button["Calculate step",
  Loading={ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }] ,dsuc, N[ {dP, dQ}]};
  dpath =  RunBbmTest[Mat,Last[path],Loading]  ;
  path = Join[ path,  Delete[ dpath, 1] ] ;
   If[Count[Evaluate[symbolQ[#] & /@ {d\[Epsilon]P, d\[Epsilon]Q, dP, dQ }], True] != 2,
  Print[Style[ "Error: system of equations cannot be solved. Exactly two unknowns are needed.", Red, 20] ]; Abort[];
  ] (* Controls if two unknowns are sought *)
 ],
Delimiter,
Button["Undo recent  increments",
  If[Length[path] >= 2, ntimeDel = Min[ntime, Length[path] - 1];  path = Drop[path, -ntimeDel];,
   Print[Style["No more increments can be undone.", Blue, 20] ]; ];
 ],
Button["Print the final state",
  Print["Final {P,Q}= ", path[[-1,1 ;; 2]]," ,  {\[Epsilon]P,\[Epsilon]Q}= ", path[[-1,4 ;; 5]], " , P0s= ", path[[-1,3]], " , epor= ",path[[-1,6]] ,
      ", suc=", path[[-1,7]] ,", P0 (with suction)= ", path[[-1,8]] ,", {\[Epsilon]pP,\[Epsilon]pQ}= ", path[[-1,9 ;; 10]]
  ];
 ],
Button["Reset all ",
  path = {path[[1]]};  ntime = xntime;  d\[Epsilon]Pp = xdeP ; d\[Epsilon]Qp = xdeQ; dP = xdP; dQ = xdQ;
  FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]];
  FrontEndExecute[FrontEndToken["controlPanelBBM[]"]];
 ]
 ]
];
(**)


End[]
EndPackage[ ];
(* $Context="PQ`BBM2015`";*)

Print[" ******** BBM2015.m ****** activated:", DateString[],"  
 provides  COMPUTATIONAL MODULES:
completeState[ state , Mat]     RunBbmTest[  Mat,ICond,{ntime,deps,dT}]
getElasticIsomorph[Mat,state]      controlPanelBBM[ ]
inputCheck[Mat_, Icond_, Loading_]     symbolQ[t_]      inRange[x_, range_]
 by A. Niemunis 2011,2016
  "];
