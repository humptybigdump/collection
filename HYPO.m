(* ::Package:: *)

(*    Mathematica  tools for element tests with Hypoplasticity in isomorphic PQ space  by A.Niemunis

This  notebook produces stress/strain path according to the Von Wolffersdorf's hypoplastic model .
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (= HYPO.m) to the new directory.
4) Begin a Mathematica session with:    Needs["PQ`HYPO`"]
*)


BeginPackage["PQ`HYPO`"]
Off[General::spell,General::spell1,Solve::ifun];
(*---------------------------------------------usages--------------------------------------------------------------------*)
 RunHPTest::usage = "RunHPTest[  Mat,ICond,{ntime,deps,dT}] \
 \n  RunHPTest returns the  sequence of states {P, Q, \[Epsilon]P, \[Epsilon]Q, epor}
 \n  which are incrementally calculated according to HP model within the module \
 \n  using the material parameters Mat = {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a} \
 \n  the initial conditions Icond = {P,Q,\[Epsilon]P,\[Epsilon]Q,epor } \
 \n  and the loading programme  {ntime,deps,dT}.
 \n  The loading programme  consists of : \
 \n  ntime = the number of repetitions,  \
 \n  deps = {depsP, depsQ} = strain increment, \
 \n  dT = {dP,dQ} = stress increment. \
 \n  Two of the four incremental values: depsP, depsQ, dP, dQ  must be prescribed (given as numbers) \
 \n  and the remaining two must be calculated and thus they should be specified as symbols.   \
 \n  " ;

 PQPlot::usage = "PQPlot[Path , Mat ] or  PQPlot[{Path1,Path2,.. }, Mat ]  plots the stress path in the isomorphic P-Q-space.   \
\n Moreover for the first and the last state PQPlot shows the yield surfaces of the HP-model. \
\n If several Paths are used like {Path1,Path2,.. } the yield surfaces for the end-states of each path are also plotted. \
 ";

 epsQQPlot::usage = "epsQQ[Path , Mat ] or  epsQQ[{Path1,Path2,.. }, Mat ]  produces an isomorphic epsQ-Q diagramm.   \
 ";

 oedometricPlot::usage = "oedometricPlot[Path, Mat] or  oedometricPlot[{Path1,Path2,... } , Mat] plots the compression curve  in   P-e-space.   \
\n The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled  \
  ";

 controlPanelHP::usage = "controlPanelHP[ ] loads a graphic interface to perform element tests using HP.   \
\n The initial conditions must be saved in the first row of the global variable path. \
\n Two stress or strain or mixed increments must be prescribed. Push the button calculate to see the results after the increment.  \
\n The number of increments is ntime, but can be modified writing a new number in the cell ntime. \
\n If you want to start with a brand new simulation, click on Reset. \
\n If you wish to undo the last step, please click on the button Undo. \
  ";

ExampleOedometricTest::usage = "ExampleOedometricTest[] calculates an oedometric compression with first loading, \
\n unloading and reloading.  The strain is prescribed for the whole test. \
\n Remember that for oedometric conditions {deps1,deps2}={-1,0}, i.e. depsP=1/sqrt[3], depsQ = Sqrt[2/3] \
\n\n sfi = 0.50; ed0 = 0.61; ec0 = 0.98; ei0 = 1.10; hs = 2600000; nexpo = 0.27; alpha = 0.18; beta = 1; a = Sqrt[3] (3 - sfi)/(2 Sqrt[2] sfi); \
\n Mat1 = {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a};                     (* Material Parameters *) \
\n ICond1 = {10, 0, 0, 0, 0.70};                                            (* Initial conditions {P0,Q0,epsP0,epsQ0,e0} *) \
\n Loading = {500, {0.0001/Sqrt[3], 0.0001 Sqrt[2./3.]}, {xdT1, xdT2}};     (* Loading path, strain controlled *) \
\n ULoading = {100, {-0.0001/Sqrt[3], -0.0001 Sqrt[2./3.]}, {xdT1, xdT2}};  (* Unloading path, strain controlled *) \
\n RLoading = {200, {0.0001/Sqrt[3], 0.0001 Sqrt[2./3.]}, {xdT1, xdT2}};    (* Reloading path, strain controlled *) \
";  

ExampleIsobaricShearing::usage = "ExampleIsobaricShearing[] calculates a shearing test with constant pressure. \
\n Mixed control is prescribed. Final deviatoric strain epsQ=50%; dP = 0 kPa. \
\n Two samples are tested: loose sample with e=ei0, and dense sample with e=ed0. Both have the same initial pressure P=100 kPa. \
\n\n sfi = 0.50; ed0 = 0.61; ec0 = 0.98; ei0 = 1.10; hs = 2600000; nexpo = 0.27; alpha = 0.18; beta = 1; a = Sqrt[3] (3 - sfi)/(2 Sqrt[2] sfi);\
\n Mat1 = {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a};                   (* Material Parameters *)\
\n ICond1 = {100., 0, 0, 0, 0.95*ei0};                                       (* Initial conditions for loose soil {P0,Q0,epsP0,epsQ0,e0} *) \
\n ICond2 = {100., 0, 0, 0, ed0};                                            (* Initial conditions for dense soil {P0,Q0,epsP0,epsQ0,e0} *) \
\n Loading = {500, {xdeP, 0.001}, {0, xdQ}};                                 (* Mixed control. dP=0, depsQ=0.001 *)  \
";  

ExampleIsochoricShearing::usage = "ExampleIsochoricShearing[] calculates an undrained (constant volume) triaxial compression. \
\n  Strain is prescribed and controlled. Final deviatoric strain epsQ=3%, depsP = 0.0. \
\n Two samples are tested: loose sample with e=ei0, and dense sample with e=ed0. Both have the same initial confining pressure P=100 kPa. \
\n\n sfi = 0.50; ed0 = 0.61; ec0 = 0.98; ei0 = 1.10; hs = 2600000; nexpo = 0.27; alpha = 0.18; beta = 1; a = Sqrt[3] (3 - sfi)/(2 Sqrt[2] sfi); \
\n Mat1 = {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a}; \
\n ICond1 = {100, 0, 0, 0, ed0};                 (* Initial conditions for dense sample {P0,Q0,epsP0,epsQ0,e0} *) \
\n ICond2 = {100, 0, 0, 0, ei0};                 (* Initial conditions for loose sample {P0,Q0,epsP0,epsQ0,e0} *) \
\n Loading1 = {300, {0, 0.0001`}, {xdP, xdQ}};   (* Loading path, strain controlled with constant volume. *) \
";  
(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];
inRange[x_, range_] := IntervalMemberQ[Interval[range], x];

getHypoStiff[P_, Q_, epor_, Mat_] := Module[{sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a, ed, ec, ei, 
    fe, fb, fd, eta, F, LL = {{0, 0}, {0, 0}}, NN = {0, 0}},
   {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a} = Mat;
   If[P <= 0, Print[Style["error: P < 0",Red, 20]]; Goto[exodus] ];
   {ed, ec, ei} = {ed0, ec0, ei0}*Exp[-(P*Sqrt[3]/hs)^nexpo];
   fe = (ec/epor)^beta; 
   fd = (Max[(epor - ed), 0]/(ec - ed))^alpha;  
   fb = (ei0/ec0)^beta*(hs/nexpo)*((1 + ei)/ei) (P*Sqrt[3]/hs)^(1 - nexpo)*(3 + a^2 - 
        a Sqrt[3] ((ei0 - ed0)/(ec0 - ed0))^alpha)^-1; 
   eta = Q/P; If[eta > 0, F = 1, F = 1 + eta/Sqrt[2]];
   LL = fb*fe/(1/3*(1 + eta^2)) {{F^2 + a^2/3, a^2 eta/3}, {a^2 eta/3, 
       F^2 + a^2 eta^2/3}};
   NN = fb*fe*fd/(1/3*(1 + eta^2)) a*F/Sqrt[3]*{-1, -2*eta};
   Label[exodus];
   {LL, NN}
   ];
 
 RunHPTest[Mat_, ICond_, Loading_] := Module[{ ntime, state, sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a, P, Q, 
          epsP, epsQ, epor, itime, err0 , err1,depsPT, depsQT, dPT, dQT, epor1T, P1T, Q1T, P1, Q1, epor1, LLPP, LLPQ, LLQQ, NNP, NNQ, 
          F, fb, fe, fd, x, ec, ed, ei, xdepsP, xdepsQ, xdP, xdQ, eqctr1, eqctr2, eqctr3, eqctr4, deps, eqdP, 
          eqdQ, eqepor1, eqP1, eqQ1, eqSystem, unknowns, predictors, unknowns1approxi, solution, exit },
   Off[Set::shape];
   Off[FindRoot]; 
   Off[ReplaceAll::reps]; (* *)
   
   ntime = Loading[[1]]; (* number of increments *)
   state = Table[{0, 0, 0, 0, 0}, {i, 1, ntime + 1}];(* place holder for the step-path *)
   {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a} = N[ Mat ];

   {P, Q, epsP, epsQ, epor} = N[ ICond ];
   state[[ 1 ]] = N[ ICond ];
   
   For[itime = 2, itime <= ntime + 1,
    {err0,err1} = {False,False};
    {P, Q, epsP, epsQ, epor} = 
    state[[itime - 1]];(* Take the last increment as the initial condition *)

   (* Predictor (= presumed values at the end of the increment) of seven unknowns: the prescribed increments = predicted increments.
      the unknown increments = predicted with 0.0 *)
    depsPT = If[symbolQ[Loading[[2, 1]] ], 0, Loading[[2, 1]] ];
    depsQT = If[symbolQ[Loading[[2, 2]] ], 0, Loading[[2, 2]] ];
    dPT = If[symbolQ[Loading[[3, 1]] ], 0, Loading[[3, 1]] ];
    dQT = If[symbolQ[Loading[[3, 2]] ], 0, Loading[[3, 2]] ];
    epor1T = epor;(* void ratio predicted unchanged *)
    P1T = P + dPT;(* predicted final stress P *)
    Q1T = Q + dQT;(* predicted final stress Q  *)

    (* HP abbreviations *)
    ec = ec0*Exp[-(P1*Sqrt[3]/hs)^nexpo];
    ed = ed0*Exp[-(P1*Sqrt[3]/hs)^nexpo];
    ei = ei0*Exp[-(P1*Sqrt[3]/hs)^nexpo];
    fb = (ei0/ec0)^beta*(hs/nexpo)*((1 + ei)/ei) (P1*Sqrt[3]/hs)^(1 - nexpo)*(3 + a^2 - 
          a Sqrt[3] ((ei0 - ed0)/(ec0 - ed0))^alpha)^-1;
    fe = (ec/epor1)^beta;
    fd = (Max[(epor1 - ed), 0]/(ec - ed))^alpha; (* *)

    LLPP = (3 (a^2/3 + F^2) fb fe)/(1 + (Q1/P1)^2);
    LLPQ = (a^2 (Q1/P1) fb fe)/(1 + (Q1/P1)^2);   (* LLPQ = LLQP *)
    LLQQ = (3 ((a^2 (Q1/P1)^2)/3 + F^2) fb fe)/(1 + (Q1/P1)^2);
    NNP = -((Sqrt[3] a F fb fd fe)/(1 + (Q1/P1)^2));
    NNQ = -((2 Sqrt[3] a (Q1/P1) F fb fd fe)/(1 + (Q1/P1)^2));
    F = 1 + 0.5/Sqrt[2] x (1 + Erf[-100 x]) /. {x ->  Q1/P1};  (* This is the approximation of If[(Q1/P1)>0,1,1+(Q1/P1)/Sqrt[2]]; *)

    (* 7 HP implicit equations (for the updated state) but calculate nothing *)
      If[NumberQ[Loading[[2, 1]] ], eqctr1 = xdepsP == Loading[[2, 1]], eqctr1 = 0 == 0];
      If[NumberQ[Loading[[2, 2]] ], eqctr2 = xdepsQ == Loading[[2, 2]], eqctr2 = 0 == 0];
      If[NumberQ[Loading[[3, 1]] ], eqctr3 = xdP == Loading[[3, 1]], eqctr3 = 0 == 0];
      If[NumberQ[Loading[[3, 2]] ], eqctr4 = xdQ == Loading[[3, 2]], eqctr4 = 0 == 0];
      (* Only 2 of the above four equations are meaningful. The other 2 will be disregarded by Complement[{eqxxx,...},True] *)
      
      deps = {xdepsP, xdepsQ};
      eqdP = xdP == {LLPP, LLPQ}.deps + NNP*Norm[deps];(*increment of P*)
      eqdQ = xdQ == {LLPQ, LLQQ}.deps + NNQ*Norm[deps];(*increment of Q*)

      
      eqepor1 = epor1 == (1. + epor) Exp[-xdepsP Sqrt[3]] - 1.0; (* update voids ratio *)
      eqP1 = P1 == P + xdP;  (*updated P*)
      eqQ1 = Q1 == Q + xdQ; (*updated Q*)
      eqSystem = Complement[{eqepor1, eqctr1, eqctr2, eqctr3, eqctr4, eqdP, eqdQ, eqP1, eqQ1}, {True}]; (* remove trivial equations *)
      
      unknowns = {xdepsP, xdepsQ, xdP, xdQ, epor1, P1, Q1}; (*collect all unknowns (two of which depend on test control) for 
                                                             the whole set of scalar equations eqXX *)
      predictors = {depsPT, depsQT, dPT, dQT, epor1T, P1T, Q1T};
      unknowns1approxi = Transpose[{unknowns, predictors}];(* unknowns paired with their predicted solutions,  as required by FindRoot[] *)
      (*-------------------------------------------------------------------------------*)
      solution = Check[FindRoot[eqSystem, unknowns1approxi, AccuracyGoal -> 5],err0=True,FindRoot::lstol ];  (* ; Essential NL solution of the incremental problem =  RMI , AccuracyGoal -> 5,MaxIterations -> 75,WorkingPrecision -> 30 *)
      (*-------------------------------------------------------------------------------*)
      state[[itime]] = Check[{P1, Q1, state[[itime - 1, 3]] + xdepsP, state[[itime - 1, 4]] + xdepsQ, epor1} /. solution, err1 = True,
         Part::partw]; (* save solution for next increment and for output *)

      If[err0, Print["FindRoot accuracy violation. Plots interrupted after ",itime," increments"]; state= Part[state,1;;itime-1]; Goto[exit] ];  (* Prescribed increment could not be solved *)
      If[err1, Print["FindRoot: solution could not be found. Plots interrupted after ",itime," increments. Solution =",solution]; state= Part[state,1;;itime-1]; Goto[exit] ];  (* Solution produced an empty list *)
      If[state[[itime, 1]] < 0.0, Print[Style["error:  P < 0 ", Red, 20]]; state= Part[state,1;;itime-1];  Goto[exit] ];
      itime++;] ;
      Label[exit];  
    state
  ];
 
 inputCheck[Mat_, Icond_, Loading_] := 
  Module[ {P, Q, sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, 
    a, \[Epsilon]P, \[Epsilon]Q, epor, ntime, deP, deQ, dP, dQ, dT, 
    PrescribedStressPath, ed, checkOK  },
   {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a} = Mat;
   {P, Q, \[Epsilon]P, \[Epsilon]Q, epor} = Icond;
   { ntime, {deP, deQ}, {dP, dQ} } = { First[Loading],N[ Loading[[2]] ], N[ Loading[[3]] ] }    ;
   ed = ed0*Exp[-(P*Sqrt[3]/hs)^nexpo];
   checkOK=True;
   Column[{
   If[symbolQ[ntime],Print[ "number of increments  ntime =  ", ntime, " undefined \n" ]; checkOK=False;] ; ,
   If[ ntime < 10 , Print[ "suspiciously small number of increments  ntime =  ",ntime ]; checkOK=False;] ; ,
   If[ deP > 0.005 || deQ > 0.005,Print[ "Strain increments seem to be large {deP, deQ} = ", {deP,deQ}, " Keep them at  0.1% level \n" ]; checkOK=False;]; ,
   If[ dP > 10 || dQ > 10 ,Print[ "Stress increments seem to be large {dP, dQ} = ", {dP,dQ}  , " Keep them below 10 kPa "]; checkOK=False;]  ; ,
   If[epor  ~inRange~ {0.01, 2} , "",Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 2} "  ]; checkOK=False;]; ,
   If[ epor < ed0,Print[ "Warning: epor < ed0 causes a complex fd. Use a larger initial epor" ]; checkOK=False; ]; ,
   If[ed0 ~inRange~ {0.3, 1.0} , "" , Print[ "Suspicious ed0 =  ", ed0  ]; checkOK=False;]; ,
   If[ ec0 ~inRange~ {0.4, 1.0} , "", Print[ "Suspicious ec0 =  ", ec0  ]; checkOK=False;]; ,
   If[ei0~inRange~ {0.7 , 2.0} , "", Print[ "Suspicious ei0 =  ", ei0 ]; checkOK=False; ]; ,
   If[ sfi ~inRange~ {Sin[5 \[Degree]], Sin[50 \[Degree]]} , "", Print[ "Suspicious Sin[\[Phi]]=  ", sfi  ]; checkOK=False;]; ,
   If[P < 0 , Print[ "Warning: negative initial  P =  ", P   ]; checkOK=False;];
     }];
   dT = {dP, dQ};
   If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True] > 2,  
    Print["Error: Too many unknowns in Loading = ", Loading ]; 
    Beep[]; checkOK=False; Abort[]; ];
   If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True]  < 2,  
    Print["Error: Too few unknowns in Loading = ", Loading ]; Beep[]; checkOK=False;
    Abort[]; ];
   If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, True, False, True},  
    Print["Error: You cannot prescribe deP and dP in Loading simultaneously"]; Beep[]; checkOK=False; Abort[]; ];
   If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == {True, False, True, False},  
    Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously"]; Beep[]; checkOK=False; Abort[]; ];

   If[checkOK,Print["Everything seems to be OK."] ]; 
   ];
 
controlPanelHP[] := Module[{},
    If[Length[path] == 0, path = Array[0 &, {1, 6}]; 
   path = {ICond0};]; (* Initialisation of global variable path *)
    Manipulate[
         Switch[ Test, "Oedom", {d\[Epsilon]Pp, d\[Epsilon]Qp, dP, dQ, ntime} = {0.01/Sqrt[3.], 0.01*Sqrt[2./3], xdP, xdQ, 100};, 
                   "Und.Triax", {d\[Epsilon]Pp, d\[Epsilon]Qp, dP, dQ, ntime} = {0, 0.01, xdP, xdQ, 100};,
                   "Drain.Triax", {d\[Epsilon]Pp, d\[Epsilon]Qp, dP, dQ, ntime} = {xdeP, xdeQ, 1, 1*Sqrt[2.], 100};,
                   "Manual", {d\[Epsilon]Pp, d\[Epsilon]Qp, dP, dQ, ntime} = {d\[Epsilon]Pp, d\[Epsilon]Qp, dP, dQ, ntime};
            ];

g1 = Show[{PQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ], PointSize[Medium], 
                        Orange, Point[path[[-1, 1 ;; 2]] ]}] } ];
g2 = Show[{epsQQPlot[path], Graphics[ {PointSize[Large], Red, Point[{ path[[1, 4]] , path[[1, 2]] } ], 
             PointSize[Medium], Orange, Point[{ path[[-1, 4]], path[[-1, 2]] } ] } ] } ] ; 
g3=  Show[{oedometricPlot[path, Mat ], Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 5]] }], PointSize[Medium], 
          Orange, Point[{path[[-1, 1]], path[[-1, 5]] } ] }] } ];
g4 = Show[{epsPepsQPlot[path ], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 3]], path[[1, 4]]} ], PointSize[Medium], 
          Orange, Point[ { path[[-1, 3]], path[[-1, 4]]} ]} ] } ];
 GraphicsGrid[{ {g1, g2 } , { g3 , g4 } }],

    "Loading step consists of:",
    {{ntime, xntime}},

    "increments consisting of",
    {{d\[Epsilon]Pp, xdeP}},
    {{d\[Epsilon]Qp, xdeQ}},
    {{dP, xdP}}  ,
    {{dQ, xdQ}}  ,

    " (in % or kPa) each.",
 
    Delimiter,
 
    Button["Check the input",
    
       d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
       d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100,  d\[Epsilon]Qp ]  ;
      inputCheck[Mat, ICond0, {ntime, {d\[Epsilon]P, d\[Epsilon]Q}, {dP, dQ}} ]
     ],
  
    Button["Calculate step",
      d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ] ;
      d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100,  d\[Epsilon]Qp ] ;
        
      loading = { ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }] , N[ {dP, dQ}] };
      dpath =  RunHPTest[Mat, Last[path], loading]  ;
      path = Join[ path,  Delete[ dpath, 1] ] ;
    
     If[Count[
       Evaluate[
        symbolQ[#] & /@ {d\[Epsilon]P, d\[Epsilon]Q, dP, dQ}], True] != 2,
       Print[ Style[ "Error: system of equations cannot be solved. Exactly two unknowns are needed.", Red, 20] ]; Abort[]; Beep[]; ];  (* Controls if two unknowns are sought *)
     ],
   
   Button["Undo the last ntime increments",
      If[Length[path] >= 2, path = Drop[path, -ntime];,
         Print[ Style["No more increments can be undone.", Blue, 20] ]; ];
     ],
   
   Button["Print the final state",
      Print["Final {P,Q}= ", path[[-1, 1 ;; 2]], " ,  {\[Epsilon]P,\[Epsilon]Q}= ", path[[-1, 3 ;; 4]]," , epor= ", path[[-1, 5]] ];
     ],
   
   {{Test,"Manual"},{"Oedom", "Und.Triax", "Drain.Triax", "Manual"},ControlType-> RadioButtonBar },
 
   Button["Reset all steps, graphics and delete cell",
      path = {ICond0}; ntime = xntime;  d\[Epsilon]Pp = xdeP ; 
    d\[Epsilon]Qp = xdeQ; dP = xdP; dQ = xdQ; 
    FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]];
     ]
    ]
  ];
  
  PQPlot[Paths_, Mat_] := Module[{nPaths , PQs, gPQs, gfirstSurface  , glastSurface, 
    outputPlot111, currentPath , iPath, McPlot, MePlot, Pmax,Pmin, Qmax,Qmin    },
           
   If[Depth[Paths] == 4,  nPaths = First[Dimensions[Paths] ], nPaths = 1 ];
   PQs = Array[0 &, nPaths];
   gfirstSurface =  Array[0 &, nPaths];
   glastSurface =  Array[0 &, nPaths];
   gPQs =  Array[0 &, nPaths];
   McPlot = (Sqrt[2] /3)*(6 Mat[[1]] )/(3 - Mat[[1]] ); (* 6 Sin/(3-Sin)* sqrt2 / 3*)
   MePlot = (-Sqrt[2] /3)*(6 Mat[[1]] )/(3 + Mat[[1]] );(* 6 Sin/(3+Sin)* sqrt2 / 3*)
   If[nPaths == 1, 
    PQs[[1]] = {#[[1]], #[[2]] }  &  /@  Paths;
    {Pmin,Pmax}= {Min[ Paths[[All, 1]] ], Max[ Paths[[All, 1]] ] };
    {Qmin,Qmax}= {Min[ Paths[[All, 2]] ], Max[ Paths[[All, 2]] ] };
    gPQs[[1]] = ListPlot[PQs[[1]], PlotStyle -> PointSize[0.014`],AspectRatio -> Automatic, PlotRegion -> {{0, 1}, {0, 1}}, 
      PlotRange -> {{0, 1.05*Pmax}, {0.95*Qmin, 1.05*Qmax} }, AxesLabel -> {"P", "Q"}, DisplayFunction -> Identity];
    outputPlot  = { gPQs[[1]], Graphics[{ Red, Line[{{0, 0}, {Max[Paths[[All, 1]]],Max[Paths[[All, 1]]]*McPlot }}],
      Line[{{0, 0}, {Max[Paths[[All, 1]]], Max[Paths[[All, 1]]]*MePlot }}] } ]  } ;
     ] ;
    If[nPaths > 1,
      {Pmin,Pmax}= {Min[ Paths[[All, All, 1]] ], Max[ Paths[[All, All, 1]] ] };
      {Qmin,Qmax}= {Min[ Paths[[All, All, 2]] ], Max[ Paths[[All, All, 2]] ] };
      For[iPath = 1, iPath <= nPaths,
          currentPath =   Paths[[iPath]] ;
          PQs[[iPath]] = {#[[1]], #[[2]] }  &  /@  currentPath;
          gPQs[[iPath]] = ListPlot[PQs[[iPath]], PlotStyle -> PointSize[0.014`], 
          PlotRegion -> {{0, 1}, {0, 1}}, AspectRatio -> Automatic, 
          PlotRange -> {{0, 1.05*Pmax}, {0.95*Qmin, 1.05*Qmax} }, AxesLabel -> {"P", "Q"}, 
          DisplayFunction -> Identity];
          iPath++;
         ] ;
      outputPlot = Flatten[{ gPQs , Graphics[{Red, Line[{{0, 0}, {Max[Paths[[All, All, 1]]],
            Max[ Paths[[All, All, 1]] ]* McPlot}}], Line[{{0, 0}, {Max[Paths[[All, All, 1]]],
            Max[ Paths[[All, All, 1]] ]*MePlot}}] } ]  } , 1];
      ];
    Show[outputPlot, DisplayFunction -> $DisplayFunction]      ];

(*
oedometricPlot[Path_, Mat_] := Module[{  Pes,  gPes, csl,  outputPlot, currentPath , iPath, 
                                        sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, Pmin, Pmax, emin, emax,  erange ,a, sq3 =Sqrt[3] },
   {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, a} = Mat;   
      Pes = {#[[1]], #[[5]] }  &  /@  Path;
      {Pmin,Pmax}= {Min[ Pes[[  All,1]] ], Max[ Pes[[All, 1]] ] };
      {emin,emax}= {Min[ Pes[[All, 2]] ], Max[ Pes[[All, 2]] ] };
      gPes = ListLogLinearPlot[Pes, PlotStyle -> PointSize[0.014`], PlotRegion -> {{0, 1}, {0, 1}}, 
               PlotRange -> { Full, {0.95*emin, 1.05*emax} }, AxesLabel -> {"P", "e"},    DisplayFunction -> Identity];
      csl = LogLinearPlot[ ec0*Exp[-(P*sq3/hs)^nexpo] , {P, 1, 1.1*Pmax}, PlotStyle -> Directive[Red, Thick]    ];
      erange = LogLinearPlot[{ei0 Exp[-(P*sq3/hs)^nexpo] ,ed0 *Exp[-(P*sq3/hs)^nexpo] }, {P, 1, 1.1*Pmax}, 
            PlotStyle -> Directive[Yellow, Thick], Filling->{1->{2}} ];
      outputPlot = {erange, gPes, csl };  
   Show[outputPlot, DisplayFunction -> $DisplayFunction]
    ];

*)

 oedometricPlot[Paths_, Mat_] := Module[{nPaths = 1, Pes, gPes,  outputPlot, currentPath , iPath, 
                                        sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta, Pmin, Pmax, emin, emax, csl, erange ,a},
   {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta,a} = Mat;
   If[Depth[Paths] == 4,  nPaths = First[Dimensions[Paths] ], nPaths = 1 ];
   Pes = Array[0 &, nPaths];
   gPes =  Array[0 &, nPaths];
   If[nPaths == 1,
      Pes[[1]] = {#[[1]], #[[5]] }  &  /@  Paths;
      {Pmin,Pmax}= {Min[ Paths[[All, 1]] ], Max[ Paths[[All, 1]] ] };
      {emin,emax}= {Min[ Paths[[All, 5]] ], Max[ Paths[[All, 5]] ] };
      gPes[[1]] = ListLogLinearPlot[Pes[[1]], PlotStyle -> PointSize[0.014`], PlotRegion -> {{0, 1}, {0, 1}}, 
      PlotRange -> { Full, {0.95*emin, 1.05*emax} }, AxesLabel -> {"P", "e"}, 
      DisplayFunction -> Identity];
      csl = LogLinearPlot[ ec0*Exp[-(P*Sqrt[3]/hs)^nexpo] , {P, 1, 1.1*Pmax}, PlotStyle -> Directive[Red, Thick]    ];
      erange = LogLinearPlot[{ei0 Exp[-(P*Sqrt[3]/hs)^nexpo] ,ed0 *Exp[-(P*Sqrt[3]/hs)^nexpo] }, {P, 1, 1.1*Pmax}, 
            PlotStyle -> Directive[Red, Thick], Filling->{1->{2}} ];
      outputPlot = {erange, gPes[[1]], csl };
     ] ;
   If[nPaths > 1,
      {Pmin,Pmax}= {Min[ Paths[[All, All, 1]] ], Max[ Paths[[All, All, 1]] ] };
      {emin,emax}= {Min[ Paths[[All, All, 5]] ], Max[ Paths[[All, All, 5]] ] };
      For[iPath = 1, iPath <= nPaths,
       currentPath =   Paths[[iPath]] ;
       Pes[[iPath]] = {#[[1]], #[[5]] }  &  /@  currentPath;
       gPes[[iPath]] = ListLogLinearPlot[Pes[[iPath]], PlotStyle -> PointSize[0.014`], PlotRegion -> {{0, 1}, {0, 1}}, 
       PlotRange -> {  Full , { 0.95*emin, 1.05*emax}  }, AxesLabel -> {"P", "e"}, 
       DisplayFunction -> Identity];
       iPath++; ];
       csl = LogLinearPlot[ec0*Exp[-(P*Sqrt[3]/hs)^nexpo], {P, 1, 1.1*Pmax}, PlotStyle -> Directive[Red, Thick]];
      outputPlot = Flatten[{ gPes, csl } , 1];
     ] ;
   Show[outputPlot, DisplayFunction -> $DisplayFunction]
    ];






 epsQQPlot[Paths_] := Module[{nPaths = 1, epsQQ, gepsQQ,  outputPlot, currentPath , iPath },
   If[Depth[Paths] == 4,  nPaths = First[Dimensions[Paths] ], nPaths = 1 ];
   epsQQ = Array[0 &, nPaths];
   gepsQQ =  Array[0 &, nPaths];
   If[nPaths == 1,
     epsQQ[[1]] = {#[[4]], #[[2]] }  &  /@  Paths;
     gepsQQ[[1]] = ListPlot[epsQQ[[1]], PlotStyle -> PointSize[0.014`], PlotRegion -> {{0, 1}, {0, 1}}, PlotRange -> {Full, Full }, 
      AxesLabel -> {"\[Epsilon]Q", "Q"}, DisplayFunction -> Identity];
     outputPlot  = { gepsQQ[[1]] } ;
     ] ;
   If[nPaths > 1,
     For[iPath = 1, iPath <= nPaths,
       currentPath =   Paths[[iPath]] ;
       epsQQ[[iPath]] = {#[[4]], #[[2]] }  &  /@  currentPath;
       gepsQQ[[iPath]] = ListPlot[epsQQ[[iPath]], PlotStyle -> PointSize[0.014`], PlotRegion -> {{0, 1}, {0, 1}}, 
        PlotRange -> {Full, {Min[ Paths[[All, All, 2]] ], Max[ Paths[[All, All, 2]] ]} } , AxesLabel -> {"\[Epsilon]Q", "Q"}, 
       DisplayFunction -> Identity];
       iPath++; ];
     outputPlot  = Flatten[{ gepsQQ  } , 1];
      ]  ;
   Show[outputPlot, DisplayFunction -> $DisplayFunction]
    ];

epsPepsQPlot[Paths_] := Module[{nPaths = 1, epsPepsQ, gepsPepsQ,  outputPlot, currentPath , iPath },
   If[Depth[Paths] == 4,  nPaths = First[Dimensions[Paths] ], nPaths = 1 ];
   epsPepsQ = Array[0 &, nPaths];
   gepsPepsQ =  Array[0 &, nPaths];
   If[nPaths == 1,
     epsPepsQ[[1]] = {#[[3]], #[[4]] }  &  /@  Paths;
     gepsPepsQ[[1]] = ListPlot[epsPepsQ[[1]], PlotStyle -> PointSize[0.014`], PlotRegion -> {{0, 1}, {0, 1}}, PlotRange -> All, 
      AxesLabel -> {"\[Epsilon]P", "\[Epsilon]Q"}, DisplayFunction -> Identity];
     outputPlot  = { gepsPepsQ[[1]] } ;
     ] ;
   If[nPaths > 1,
     For[iPath = 1, iPath <= nPaths,
       currentPath =   Paths[[iPath]] ;
       epsPepsQ[[iPath]] = {#[[3]], #[[4]] }  &  /@  currentPath;
       gepsPepsQ[[iPath]] = ListPlot[epsPepsQ[[iPath]], PlotStyle -> PointSize[0.014`], PlotRegion -> {{0, 1}, {0, 1}}, PlotRange -> All, 
       AxesLabel -> {"\[Epsilon]P", "\[Epsilon]Q"}, DisplayFunction -> Identity];
     iPath++;] ;
     outputPlot  = Flatten[{ gepsPepsQ  } , 1];
     ]  ;
   Show[outputPlot, DisplayFunction -> $DisplayFunction]
    ];
 
 ExampleOedometricTest[] := Module[{xdT1, xdT2, Mat1, ICond1, Loading, ULoading, RLoading, 
    Path1, Path2, Path3},
   Print[" Oedometric test, strain controlled. First loading followed by an unloading and finally a reloading. \
    \n Enter ?ExampleOedometricTest to read Initial conditions , mat. constants, and loading steps."];
   sfi = 0.50; ed0 = 0.61; ec0 = 0.98; ei0 = 1.10; hs = 2600000; nexpo = 0.27; alpha = 0.18; beta = 1;
   a = Sqrt[3] (3 - sfi)/(2 Sqrt[2] sfi);
   Mat1 = {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta,a};
   ICond1 = {10, 0, 0, 0, 0.70};  (* {P0,Q0,epsP0,epsQ0,e0} *)
   Loading = {350, {0.0001/Sqrt[3], 0.0001 Sqrt[2./3.]}, {xdT1, xdT2}};
   ULoading = {50, {-0.0001/Sqrt[3], -0.0001 Sqrt[2./3.]}, {xdT1, xdT2}};
   RLoading = {100, {0.0001/Sqrt[3], 0.0001 Sqrt[2./3.]}, {xdT1, xdT2}};
   Path1 = RunHPTest[Mat1, ICond1, Loading];
   Path2 = RunHPTest[Mat1, Path1[[-1]], ULoading];
   Path3 = RunHPTest[Mat1, Path2[[-1]], RLoading];
   GraphicsRow[{oedometricPlot[{Path1, Path2, Path3},Mat1], 
     PQPlot[{Path1, Path2, Path3}, Mat1], 
     epsQQPlot[{Path1, Path2, Path3} ]  }, ImageSize -> 1000]
   ];
 
 ExampleIsochoricShearing[] := Module[{xdP, xdQ, Mat1, ICond1, Path1, Loading1, Path2, ICond2, 
    Path3, Loading2, Path4 },
   Print[" Undrained Triaxial - Influence of pyknotropy \
    \n Enter ?ExampleIsochoricShearing to read  Initial conditions , mat. constants, loading path."];
   sfi = 0.50; ed0 = 0.61; ec0 = 0.98; ei0 = 1.10; hs = 2600000;
   nexpo = 0.27; alpha = 0.18; beta = 1; a = Sqrt[3] (3 - sfi)/(2 Sqrt[2] sfi);
   Mat1 = {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta,a};
   ICond1 = {100, 0, 0, 0, ed0};(* Initial conditions for dense soil {P0,Q0,epsP0,epsQ0,e0}*)
   ICond2 = {100, 0, 0, 0, 0.95*ei0};(* Initial conditions for loose soil {P0,Q0,epsP0,epsQ0,e0}*)
   Loading1 = {300, {0, 0.0001}, {xdP, xdQ}};
   Path1 = RunHPTest[Mat1, ICond1, Loading1];
   Path2 = RunHPTest[Mat1, ICond2, Loading1];
   GraphicsRow[{oedometricPlot[{Path1, Path2},Mat1], PQPlot[{Path1, Path2}, Mat1], epsQQPlot[{Path1, Path2}]}, ImageSize -> 1000]
   ];
 
 ExampleIsobaricShearing[] := Module[{xdeP, xdQ, Mat1, ICond1, ICond2, Loading, ULoading, 
    RLoading, Path1, Path2, Path3},
   Print[" Isobaric shearing - Influence of pyknotropy \
    \n Enter ?ExampleIsobaricShearing to read Initial conditions , mat. constants, loading path."];
   sfi = 0.50; ed0 = 0.61; ec0 = 0.98; ei0 = 1.10; hs = 2600000; 
   nexpo = 0.27; alpha = 0.18; beta = 1; a = Sqrt[3] (3 - sfi)/(2 Sqrt[2] sfi);
   Mat1 = {sfi, ed0, ec0, ei0, hs, nexpo, alpha, beta,a};
   ICond1 = {100., 0, 0, 0, 0.95*ei0};(* Initial conditions for loose soil {P0,Q0,epsP0,epsQ0,e0} *)
   ICond2 = {100., 0, 0, 0, ed0};(* Initial conditions for dense soil {P0,Q0,epsP0,epsQ0,e0} *)
   Loading = {500, {xdeP, 0.001}, {0, xdQ}};
   Path1 = RunHPTest[Mat1, ICond1, Loading];
   Path2 = RunHPTest[Mat1, ICond2, Loading];
   GraphicsRow[{oedometricPlot[{Path1, Path2},Mat1], 
   PQPlot[{Path1, Path2}, Mat1],  
   epsQQPlot[{Path1, Path2}]   }, ImageSize -> 1000]
   ];
   
 EndPackage[ ]
$Context = "PQ`HYPO`";
Print[ "The vW-Hypoplastic (HP) Model for isomorphic P-Q space with implicit integration; by A. Niemunis 2010-2016 \
\n  You are in the context PQ`HYPO` which provides functions: \
\n    RunHPTest, PQPlot, oedometricPlot, epsQQPlot, controlHP \
\n    and some ready to use examples:  \
\n    ExampleOedometricTest[], ExampleIsochoricShearing[], ExampleIsobaricShearing[]\
\n  *********************************************************************************************************** \
\n    ??ExampleOedometricTest  gives you the source code of the example procedure   \
\n    ?RunHPTest gives user-oriented  information to the procedure and to the input variables \
\n    ??RunHPTest gives   additionally the source code of  RunHPTest (for the programmers) \
\n  *********************************************************************************************************** \
\n    For detailed information on the programming see the notebook HP-impli.nb or read the PDF documentation.\
"];
