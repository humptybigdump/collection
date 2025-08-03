(* ::Package:: *)

(*    Mathematica  tools for element tests with  VHP in isomorphic PQ space  by A.Niemunis

This  notebook produces stress/strain path according to the modified Cam clay .
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (=  VHP.m) to the new directory.
4) Begin a Mathematica session with:    Needs["PQ`VHP`"]
*)


BeginPackage["PQ`VHP`"]
Off[General::spell,General::spell1,Solve::ifun];
(*---------------------------------------------usages--------------------------------------------------------------------*)
 RunVHPTest::usage = "RunVHPTest[  Mat,ICond,{ntime,deps,dT}] \
 \n  RunVHPTest returns the  sequence of states {P, Q, Pe, \[Epsilon]P, \[Epsilon]Q, epor}
 \n  which are incrementally calculated according to VHP model within the module \
 \n  using the material parameters Mat = {\[Kappa]B,\[Lambda]B,\[Phi],Iv,ee0,Pe0,evisPref } \
 \n  the initial conditions Icond = {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor } \
 \n  and the loading programme  {ntime,deps,dT, dt}.
 \n  The loading programme  consists of : \
 \n  ntime = the number of repetitions,  \
 \n  deps = {depsP, depsQ} = strain increment, \
 \n  dT = {dP,dQ} = stress increment, \
 \n  dt = time increment. \
 \n  The values for ntime and dt  must be provided by the user (usually via contolPanelVHP[...]).
 \n  Moreover two of the four increments: depsP, depsQ, dP, dQ  must be prescribed (given as numbers) \
 \n  and the remaining two must be calculated and thus they should be specified as symbols.   \
 \n  " ;
plotVHPsurface::usage = "plotVHPsurface[Pe,\[Phi]]  plots the yield surface  of the VHP-model in the isomorphic P-Q-space.   \
\n The yield surface consists of two half-elipses  with the common diameter Pe along the P-axis and different half-diameters \
\n along the Q-axis.  \
 ";
getElasticIsomorph::usage = "getElasticIsomorph[P_,Q_,extendedMat_]  returns the isotropic hypoelastic stiffness for the isomorphic P-Q-space    \
\n    ( barotropic ).  \
";
PQPlot::usage = "PQPlot[Path , Mat ] or  PQPlot[{Path1,Path2,.. }, Mat ]  plots the stress path in the isomorphic P-Q-space.   \
\n Moreover for the first and the last state PQPlot shows the yield surfaces of the VHP-model. \
\n If several Paths are used like {Path1,Path2,.. } the yield surfaces for the end-states of each path are also plotted. \
 ";
epsQQPlot::usage = "epsQQ[Path , Mat ] or  epsQQ[{Path1,Path2,.. }, Mat ]  produces an isomorphic epsQ-Q diagramm.   \
 ";
oedometricPlot::usage = "oedometricPlot[Path, Mat] or  oedometricPlot[{Path1,Path2,... } , Mat] plots the compression curve  in   P-e-space.   \
\n The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled  \
  ";
controlPanelVHP::usage = "controlVHP[ ] loads a graphic interface to perform element tests using VHP.   \
\n The initial conditions must be saved in the first row of the global variable path. \
\n Two stress or strain or mixed increments must be prescribed. Push the button calculate to see the results after the increment.  \
\n The number of increments is ntime, but can be modified writing a new number in the cell ntime. \
\n If you want to start with a brand new simulation, click on Reset. \
\n If you wish to undo the last step, please click on the button Undo. \
  ";

cycle::usage = "Syntax:  cycle[Mat,  loading1 , loading2  ,ncyc_] is a loop performing ncyc cyles calling two step[ ] procedures per cycle.
See also step[] ";
step::usage = "Syntax:  step[ Mat,  loading ]  \n \
 calls RunVHPTest[Mat, Last[path],  Loading ]. The number of increments in cointained in Loading. 
 A piece of path  returned by  RunVHPTest  is Joined  with the existing path. ";
(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];
inRange[x_, range_] := IntervalMemberQ[Interval[range], x];

getElasticIsomorph[P_,Q_,extendedMat_] := Module[{\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b,\[Eta],fb,F},
 {\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b}  = extendedMat;
   \[Eta] = Q/P;
  fb = \[Beta]b*P*Sqrt[3.0];
  F = If[Q>= 0, 1, 1+ (Q/P)/Sqrt[2.0] ];
  fb*( F*F* {{1,0},{0,1}} + a*a/3 * {{1,\[Eta]},{\[Eta],\[Eta]*\[Eta]}}  ) //Simplify
];

getCreepRate[P_, Q_, Pe_, Elast_, extendedMat_] := Module[{\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b,M,    PePlus , OCR, mb},
{\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b} = extendedMat;
  M = If[Q >=  0, MC,  ME] ;  \[Eta] = Q/P;
  PePlus = P* (1 + \[Eta]*\[Eta]);
  OCR = Pe/PePlus;
  mb = Normalize[  Inverse[Elast] . {P, 2*Q } ] ;
  evisPref * mb* OCR^-(1/Iv)
];

plotVHPsurface[Pe_,\[Phi]_] := Module[{MC,ME,P,Q,gC,gE},
   MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));
   ME=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));
  gC=ContourPlot[P^2+(Q/MC)^2-P Pe==0,{P,0.001`,Pe},{Q,0,(Pe MC)/2},ContourStyle->Hue[Pe/100.`],DisplayFunction->Identity,AspectRatio -> Automatic];
  gE=ContourPlot[P^2+(Q/ME)^2-P Pe==0,{P,0.001`,Pe},{Q,1/2 (-Pe) ME,0},ContourStyle->Hue[Pe/100.`],DisplayFunction->Identity,AspectRatio -> Automatic];
  {gC,gE}
];

step[ Mat_,  Loading_  ] := Module[{ },  path = Join[ path,   Delete[ RunVHPTest[Mat ,Last[path] ,Loading] ,1 ]  ] ];
cycle[ Mat_,  Loading1_  ,Loading2_ , ncyc_ ]  :=  Do[ step[Mat , Loading1  ] ; step[Mat , Loading2 ]  ,{icyc,1,ncyc}];
 
 
RunVHPTest[Mat_,ICond_,Loading_]:=Module[  { ntime, d\[Epsilon], dT, dt, \[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref, MC,ME,a, \[Beta]b, extendedMat,
       P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor, state, itime, elastStiff, unknowns, solution, d\[Epsilon]Trial, dTTrial, PTrial,QTrial,  d\[Epsilon]vis, P1, Q1, Pe1,
        unknowns1, approxTF, approx, approxi, unknowns1approxi, solution1,dT2,d\[Epsilon]2,eq1a,eq1b,eq2a,eq2b,eq4},
   {ntime, d\[Epsilon], dT, dt}   = { Loading[[1]], N[ Loading[[2]] ], N[Loading[[3]] ],  N[Loading[[4]] ] };
	 If[NumberQ[ntime] == False, ntime = 0;  Print["Warning: number of increments must be prescribed and not symbolic, the default value is ntime = 0 "] ];
   If[NumberQ[dt]== False , dt = 0;  Print["Warning: time increment must be prescribed and not symbolic, the default value is dt = 0 "] ];
	 If[Length[ Select[ Join[ d\[Epsilon], dT] , NumberQ[#]& ] ] != 2 , Print[ "Error: You must prescribe numerical values of exactly two components" ] ; Abort[]   ];
	 If[NumberQ[  d\[Epsilon][[1]] ]   ==  NumberQ[  dT[[1]]  ] ,  Print[ "Error: dP  and  depsP cannot be both prescribed" ] ; Abort[]   ] ;
	 If[NumberQ[  d\[Epsilon][[2]] ]   ==  NumberQ[  dT[[2]]  ] ,  Print[ "Error: dQ  and  depsQ cannot be both prescribed" ] ; Abort[]   ] ;
   state = { ICond };   (* to be  appended *)
   {\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref}= N[ Mat ];
    MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));  (*isomorphic M*)
    a = Sqrt[3.0] (3 - Sin[\[Phi]] ) /( 2 Sqrt[2.0] Sin[\[Phi]] );
    \[Beta]b = 1/((1 + a*a/3)*\[Kappa]B)  ;
    extendedMat =  Join[ Mat ,{MC,ME,a,\[Beta]b}] ;
   {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor}=N[ ICond ];
   state[[ 1 ]]={P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor} ;
  For[itime=2,itime<=ntime+1,
       elastStiff=getElasticIsomorph[P,Q, extendedMat];
       unknowns=Select[Flatten[{d\[Epsilon],dT}],symbolQ];  (* unknown increments depend on the choice of the test control *)
       solution=Solve[dT==elastStiff.d\[Epsilon],unknowns];
       {d\[Epsilon]Trial,dTTrial}={d\[Epsilon],dT}/.solution[[1]]; (* elastic trial solution (explicit predictor) *)
       { PTrial,QTrial} = {P,Q} + dTTrial;
       elastStiff = getElasticIsomorph[P1, Q1, extendedMat];
       d\[Epsilon]vis = getCreepRate[P1 ,Q1 ,Pe1,elastStiff,extendedMat];

       eq1a=dT[[1]]==(  elastStiff.(d\[Epsilon]- d\[Epsilon]vis*dt)   )[[1]];  (* increment of P *)
       eq1b=dT[[2]]==(  elastStiff. (d\[Epsilon]- d\[Epsilon]vis*dt)  )[[2]];  (* increment of Q *)
       eq2a=P1==P+dT[[1]];            (* updated P *)
       eq2b=Q1==Q+dT[[2]];            (* updated Q *)
       eq4=Pe1== Pe * Exp[d\[Epsilon][[1]]/ \[Lambda]B  ]   ;

       unknowns1=Flatten[{unknowns,Pe1,P1,Q1}];  (* collect 5 unknowns (two of which depend on test control) for  5  scalar equations eq1a..eq4 *)
       approxTF=(symbolQ[#1]&)/@Flatten[{d\[Epsilon],dT}];  (* a vector like {True,False,False,True} showing symbols among the increments {d\[Epsilon],dT} // Flatten *)
       approx=Pick[Flatten[{d\[Epsilon]Trial,dTTrial}],approxTF]; (* trial solutions corresponding to symbolic values of increments  *)
       approxi=Flatten[{approx,Pe,PTrial,QTrial}];   (* take the elastic predictor as the first guess *)
       unknowns1approxi=Transpose[{unknowns1,approxi}]; (* unknowns paired with their approximations *)
       (*-------------------------------------------------------------------------------*)
       solution1=FindRoot[{eq1a,eq1b,eq2a,eq2b,eq4},unknowns1approxi,AccuracyGoal->5]; (* Essential solution of the invremental problem via RMI *)
       (*-------------------------------------------------------------------------------*)

       {dT2,d\[Epsilon]2,Pe,P,Q}={dT,d\[Epsilon],Pe1,P1,Q1}/.solution1;  (* after solution  assign values to 6 unknowns *)

       {\[Epsilon]P,\[Epsilon]Q}+=d\[Epsilon]2;         (* update strain *)
       epor = (1.+epor) Exp[ -d\[Epsilon]2[[1]] Sqrt[3] ] - 1.0;
       AppendTo[state, {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor }];  (* write down the updated state *)
    itime++;];
  state
 ];

 PQPlot[Paths_, Mat_] := Module[{nPaths ,PQs, gPQs, gfirstSurface  , glastSurface, outputPlot, currentPath , iPath    },
        If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
        PQs = Array[0&, nPaths];
        gfirstSurface =  Array[0&, nPaths];
        glastSurface =  Array[0&, nPaths];
        gPQs =  Array[0&, nPaths];
        If[nPaths==1,
           PQs[[1]] = {#[[1]], #[[2]] }  &  /@  Paths;
           gPQs[[1]] = ListPlot[PQs[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},AxesOrigin->{0,0}, DisplayFunction->Identity];
           gfirstSurface[[1]] = plotVHPsurface[Paths[[1,3]],Mat[[3]]];
           glastSurface[[1]] =  plotVHPsurface[Paths[[-1,3]],Mat[[3]]];
           outputPlot  = {  gfirstSurface[[1]], glastSurface[[1]],  gPQs[[1]],
                           Graphics[{ Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (2Sqrt[2] Sin[ Mat[[3]] ])/(3-Sin[ Mat[[3]] ])}}],
                           Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (-2 Sqrt[2] Sin[ Mat[[3]] ])/(3+Sin[ Mat[[3]] ])}}] } ]  } ;
         ] ;
        If[nPaths > 1,
        For[iPath=1, iPath<=nPaths,
          currentPath =   Paths[[iPath]] ;
          PQs[[iPath]] = {#[[1]], #[[2]] }  &  /@  currentPath;
          gPQs[[iPath]] = ListPlot[PQs[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
          gfirstSurface[[iPath]] = plotVHPsurface[currentPath[[1,3]],Mat[[3]]];
           glastSurface[[iPath]] =  plotVHPsurface[currentPath[[-1,3]],Mat[[3]]];
        iPath++;] ;
        outputPlot  = Flatten[{   gfirstSurface , glastSurface,  gPQs } ,1];
         ];
        Show[outputPlot,DisplayFunction->$DisplayFunction, PlotRange -> All]
 ];
 oedometricPlot[Paths_, Mat_] := Module[{nPaths=1,Pes, gPes,  outputPlot, currentPath , iPath  },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          Pes = Array[0&, nPaths];
          gPes =  Array[0&, nPaths];
          If[nPaths==1,
             Pes[[1]] = {#[[1]], #[[6]] }  &  /@  Paths;
             gPes[[1]] = ListPlot[Pes[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,e},DisplayFunction->Identity];
             outputPlot  = { gPes[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            Pes[[iPath]] = {#[[1]], #[[6]] }  &  /@  currentPath;
            gPes[[iPath]] = ListPlot[Pes[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,e},DisplayFunction->Identity];
           iPath++;] ;
           outputPlot  = Flatten[{ gPes  } ,1];
           ]  ;
         Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];
 epsQQPlot[Paths_, Mat_] := Module[{nPaths=1,epsQQ, gepsQQ,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          epsQQ = Array[0&, nPaths];
          gepsQQ =  Array[0&, nPaths];
          If[nPaths==1,
             epsQQ[[1]] = {#[[5]], #[[2]] }  &  /@  Paths;
             gepsQQ[[1]] = ListPlot[epsQQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]Q,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
             outputPlot  = { gepsQQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsQQ[[iPath]] = {#[[5]], #[[2]] }  &  /@  currentPath;
            gepsQQ[[iPath]] = ListPlot[epsQQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]Q,Q},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsQQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];
epsPepsQPlot[Paths_, Mat_] := Module[{nPaths=1,epsPepsQ, gepsPepsQ,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          epsPepsQ = Array[0&, nPaths];
          gepsPepsQ =  Array[0&, nPaths];
          If[nPaths==1,
             epsPepsQ[[1]] = {#[[4]], #[[5]] }  &  /@  Paths;
             gepsPepsQ[[1]] = ListPlot[epsPepsQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]P,\[Epsilon]Q},AxesOrigin->{0,0},DisplayFunction->Identity];
             outputPlot  = { gepsPepsQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsPepsQ[[iPath]] = {#[[4]], #[[5]] }  &  /@  currentPath;
            gepsPepsQ[[iPath]] = ListPlot[epsPepsQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]P,\[Epsilon]Q},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsPepsQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];
 inputCheck[Mat_, Icond_, Loading_]:= Module[ {P, Q, \[Kappa]B, \[Lambda]B, \[Phi], Iv, Pe, PePlus,ee0,Pe0,evisPref, OCR,
                                               \[Epsilon]P, \[Epsilon]Q, epor, ntime, deP, deQ, dT, dP, dQ,dt,MC,ME,M },
{\[Kappa]B,\[Lambda]B,\[Phi],Iv,ee0,Pe0,evisPref}   =Mat;
 {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor }= Icond;
 { ntime, {deP,deQ}, {dP,dQ}, dt } = { First[Loading], N[ Loading[[2]] ], N[ Loading[[3]] ], N[ Loading[[4]] ] }    ;

  If[symbolQ[ntime] , Print[ "number of increments  ntime =  ", ntime," undefined"   ]]  ;
  If[symbolQ[dt] , Print[ "time increment  dt =  ", dt," undefined"   ]]  ;
  If[ ntime < 10 , Print[ "suspiciously small number of increments  ntime =  ", ntime   ]]  ;
  If[ deP > 0.005 || deQ > 0.005 , Print[ "Strain increments seem to be large {deP, deQ} = ", {deP, deQ}  , " Keep them at  0.1% level"   ]]  ;
  If[ dP > 10 || dQ > 10 , Print[ "Stress increments seem to be large {dP, dQ} = ", {dP, dQ}  , " Keep them below  10 kPa "   ]]  ;
  If[ Pe *( 1+ epor )^(1/ \[Lambda]B ) < 5000, Print[ "Warning: epor=0 can be reached already at about Pe =  ",  Pe *( 1+ epor )^(1/ \[Lambda]B ) ,
        "  Try to reduce \[Lambda]B = ",   \[Lambda]B ] ]   ;

  If[ \[Kappa]B ~inRange~ {0, 0.2} , "", Print[ "Suspicious \[Kappa]B =  ",\[Kappa]B  ]]  ;
  If[\[Lambda]B ~inRange~ {  \[Kappa]B , 20  \[Kappa]B } , "", Print[ "Suspicious \[Kappa]B/\[Lambda]B =  ",\[Kappa]B / \[Lambda]B ]]  ;
  If[ \[Phi] ~inRange~ {5\[Degree], 50\[Degree]} , "", Print[ "Suspicious \[Phi]=  ", \[Phi], "rad"  ]]  ;
  If[ Iv ~inRange~ {0.02, 0.10} , "", Print[ "Suspicious Iv=  ", Iv, "[-]"  ]]  ;

  If[P < 0 , Print[ "Warning: negative initial  P =  ", P   ]]      ;
  If[Pe < P , Print[ "Warning: initial underconsolidation Pe =  ", Pe , "<", P, " =P" ]]  ;
  MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));
  ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));
  M=If[Q >=0,MC,ME];
  If[ P^2+(Q/M)^2-P*Pe >  Pe*Pe *10^-6, Print[ "Warning: initial OCR<1 means underconsolidation:  P^2+(Q/M)^2-P*Pe  = ", P^2+(Q/M)^2-P*Pe  , "> 0" ] ]  ;
    PePlus  =  P +(Q/M)^2 /P ;
    OCR = Pe/PePlus;
  If[ OCR ~inRange~ {1.2,2},  Print[ "Initial  OCR =", OCR, ", soil was lightly overconsolidated " ] ];
  If[ OCR ~inRange~ {1.0,1.01},  Print[ "Initial OCR =", OCR, ", soil was  normally consolidated " ] ];
  If[ OCR ~inRange~ {1.01,1.1999},  Print[ "Initial OCR =", OCR, ", soil was  almost  normally consolidated " ] ];
  If[ OCR ~inRange~ {2,4},  Print[ "Initial OCR =", OCR, ", soil was  overconsolidated " ] ];
  If[ OCR ~inRange~ {4, Infinity},  Print[ "Initial OCR =", OCR, ", soil was heavily overconsolidated " ] ];



  If[epor  ~inRange~ {0.01, 3} , "", Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 3} "  ]];

 dT = {dP,dQ};
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True] > 2,  Print["Error: Too many unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True]  < 2,  Print["Error: Too few unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, True, False, True},  Print["Error: You cannot prescribe deP and dP in Loading simultaneously"]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == {True, False, True,False},  Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously"]; Beep[]; Abort[]; ];
 PrescribedStressPath  =  Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, False, True, True} ;
 If[ PrescribedStressPath  ,
      P +=  ntime *dP;      Q += ntime*dQ;
      M=If[Q >=0,MC,ME];
      If[ P^2+(Q/M)^2-P*Pe >  Pe*Pe*10^-6, Print[ "Warning: the stress path ends above CSL; strain or mixed control recommended " ] ]  ;
 ];
   If[Length[path] > 1,
              { P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor}= Last[path];
                M=If[Q >=0,MC,ME];
                PePlus  =  P +(Q/M)^2 /P ;
                Print["The latest value of OCR =", Pe/PePlus];
  ];

];

 controlPanelVHP[] := Module[{Loading,nincr},
  If[Length[path] == 0, path = Array[0 &, {1, 6}]; path = {ICond0};]; (* Initialisation of global variable path *)
  Manipulate[
 GraphicsRow[{ Show[{PQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ],
  Show[{epsQQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[{ path[[1, 5]] , path[[1, 2]] } ],PointSize[Medium], Orange, Point[{ path[[-1, 5]] , path[[-1, 2]] } ] } ] } ],
  Show[{oedometricPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 6]] }],PointSize[Medium], Orange, Point[{path[[-1, 1]], path[[-1, 6]] } ] }] } ],
  Show[{epsPepsQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 4]], path[[1, 5]]} ],PointSize[Medium], Orange, Point[ { path[[-1, 4]], path[[-1, 5]]} ]} ] } ]  },
 ImageSize -> Full]  ,
 "Loading step consists of:",
 {{ntime, xntime}},
 "increments consisting of",
 {{d\[Epsilon]Pp, xdeP}},
 {{d\[Epsilon]Qp, xdeQ}}  ,
 {{dP, xdP}}  ,
 {{dQ, xdQ}}  ,
 {{dt, xdt}}  ,
 " (in [%],[kPa] or [h]).",
 Delimiter,
 Button["Check the input",

   d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
   d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
  inputCheck[Mat, ICond0, {ntime, {d\[Epsilon]P, d\[Epsilon]Q}, {dP, dQ}, dt} ]
 ],
   Button["Calculate step",

  d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
  d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;


  Loading = { ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}], N[dt] };
  dpath =  RunVHPTest[Mat,Last[path],Loading]  ;
  path = Join[ path,  Delete[ dpath, 1] ] ;

 If[Count[Evaluate[symbolQ[#] & /@ {d\[Epsilon]P, d\[Epsilon]Q, dP, dQ}], True] != 2,
  Print[Style[ "Error: system of equations cannot be solved. Exactly two unknowns are needed.", Red, 20] ]; Abort[]; Beep[];
  ];  (* Controls if two unknowns are sought *)
 ],

Button["Undo the last ntime increments",
  If[Length[path] >= ntime+1, path = Drop[path, -ntime] , path = {path[[1]]} ;
   Print[Style["No more increments can be undone.", Blue, 20] ]; ];
 ],

Button["Print the final state",
  Print["Final {P,Q}= ", path[[-1,1 ;; 2]]," ,  {\[Epsilon]P,\[Epsilon]Q}= ", path[[-1,4 ;; 5]], " , Pe= ", path[[-1,3]], " , epor= ",path[[-1,6]] ,
      ", {\[Epsilon]Ppl,\[Epsilon]Qpl}= ", path[[-1,7 ;; 8]]
  ];
 ],
Button["Reset all steps, graphics and delete cell",
  path = {ICond0}; ntime = xntime;  d\[Epsilon]Pp = xdeP ; d\[Epsilon]Qp = xdeQ; dP = xdP; dQ = xdQ; FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]];
 ],
(*
step[ Mat_,  Loading_  ] :=   Join[ path,   Drop[ RunVHPTest[Mat ,Last[path] ,Loading] ,1 ]  ];
cycle [Mat_,  Loading1_  ,Loading2_ , ncyc ]  := Do[ step[Mat , Loading1  ] ; step[Mat , Loading2  ] ,{icyc,1,ncyc}];
*)
  Button["10 strain cycles (default eQ +/- 10 incr.  0.04%) ",
  Clear[xdP, xdQ ] ;  ncyc=10; 
  If[  symbolQ[ ntime ],  ntime= 10 ] ;   If[ symbolQ[dt] , dt = 0 ] ;   
   If[  symbolQ[ d\[Epsilon]Q ],  d\[Epsilon]Q  = 0.0004 ]; 
   If[  symbolQ[ d\[Epsilon]P ],  d\[Epsilon]P = 0 ];  
  step[Mat,  {ntime, {d\[Epsilon]P, d\[Epsilon]Q},{ xdP, xdQ }, dt} ]  ;
  cycle[Mat,   {ntime, {-d\[Epsilon]P, -d\[Epsilon]Q},{ xdP, xdQ}, dt} ,  {ntime,  {d\[Epsilon]P, d\[Epsilon]Q},{ xdP, xdQ}, dt}, ncyc] ;    
  step[Mat,   {ntime,  {-d\[Epsilon]P, -d\[Epsilon]Q},{ xdP, xdQ}, dt}]  ;
  ]
 ]
];



ExampleIsochoricShearing[] := Module[{xdP,xdQ,Mat1, ICond1, Path1, Loading1, Path2, ICond2, Path3, Loading2, Path4 },
Print[" Undrained : active/passive  shearing ;  normally consolidated/overconsolidated.\
\n Note that the initial  preconsolidation Pe=101 may slightly change!  \
\n Enter ??ExampleIsochoricShearing  to get read  IC, mat. constants and loading steps."];
Mat1= {0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001} ;
ICond1={100,0,120,0,0, 1.945799133685738 } ;
Loading1={100,{0,0.001`},{xdP,xdQ},1};
ICond2={10,0,120,0,0, 1.945799133685738 } ;
Loading2={100,{0,-0.001`},{xdP,xdQ},1};
 Path1=RunVHPTest[Mat1,ICond1,Loading1];
 Path2=RunVHPTest[Mat1,ICond2,Loading1];
 Path3=RunVHPTest[Mat1,ICond1,Loading2];
 Path4=RunVHPTest[Mat1,ICond2,Loading2];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1]  , epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];
ExampleOedometricTest[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print[" Oedometric loading , short unloading, reloading \
\n Enter ??ExampleOedometricTest  to get read  IC, mat. constants and loading steps."];
Mat1= {0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001} ;
ICond1={65,0,120,0,0, 1.945799133685738 } ;
Loading={90,{0.001/ Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2},1};
ULoading={10,{-0.001/ Sqrt[3],-0.001 Sqrt[2./3.]},{xdT1,xdT2},1};
RLoading={60,{0.001/Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2},1};
Path1=RunVHPTest[Mat1,ICond1,Loading];
Path2=RunVHPTest[Mat1,Path1[[-1]],ULoading];
Path3=RunVHPTest[Mat1,Path2[[-1]],RLoading];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1], epsQQPlot[{Path1,Path2,Path3}, Mat1]  }, ImageSize-> Large]
];
ExampleOedometricTest2[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print[" Oedometric loading , long unloading, reloading \
\n Enter ??ExampleOedometricTest2  to get read  IC, mat. constants and loading steps."];
Mat1= {0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001} ;
ICond1={65,0,120,0,0, 1.945799133685738 } ;
 Loading={80,{0.001` / Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1};
 ULoading={45,{-0.001` /Sqrt[3.`],-0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1};
 RLoading={70,{0.001`/Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1};
  Path1=RunVHPTest[Mat1,ICond1,Loading];
 Path2=RunVHPTest[Mat1,Path1[[-1]],ULoading];
 Path3=RunVHPTest[Mat1,Path2[[-1]],RLoading];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1],  epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];
ExampleIsobaricShearing[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print[" Isobaric shearing,  slightly/strongly  overconsolidated starting from Pe=100 \
\n Enter ??ExampleIsobaricShearing  to get read  IC, mat. constants and loading steps."];
Mat1= {0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001};
 ICond1={100,0,120,0,0, 1.945799133685738 } ;
 ICond2={20,0,120,0,0, 1.945799133685738 } ;
 Loading={50,{xdeP,0.0025`},{0,xdQ},1};
 Path1= RunVHPTest[Mat1,ICond1,Loading];
 Path2 = RunVHPTest[Mat1,ICond2,Loading];
 GraphicsRow[{oedometricPlot[{Path1,Path2}, Mat1], PQPlot[{Path1,Path2}, Mat1],  epsQQPlot[{Path1,Path2}, Mat1]   }, ImageSize-> Large]
];
EndPackage[ ]
$Context = "PQ`VHP`"   ;
Print[ "The Visco-Hypoplastic (VHP) Model for isomorphic P-Q space; by A. Niemunis 2013 \
\n  You are in the context PQ`VHP` which provides functions: \
\n    RunVHPTest, PQPlot, oedometricPlot, epsQQPlot, controlVHP \
\n    and some ready to use examples:  \
\n    ExampleOedometricTest[], ExampleIsochoricShearing[], ExampleOedometricTest2[], ExampleIsobaricShearing[]\
\n  *********************************************************************************************************** \
\n    ??ExampleOedometricTest  gives you the source code of the example procedure   \
\n    ?RunVHPTest gives user-oriented  information to the procedure and to the input variables \
\n    ??RunVHPTest gives   additionally the source code of  RunVHPTest (for the programmers) \
\n  *********************************************************************************************************** \
\n    For detailed information on the programming see the notebook VHP-impli.nb or read the PDF documentation.\
"];
(*------------- Initialization-----------(to be overridden by tru values in notebook, here they prevent uncontrolled animation)---*)
Mat = {0.01, 0.1, Pi/6 , 0.1, 2.0, 100.0,  0.001} ;   (*  {kappaB,lambdaB,Phi,Iv,ee0,Pe0,evisPref} *)
ICond0 = {100, 0, 120, 0, 0,   1.945799133685738 } ;   (* {P,Q,Pe,epsP,epsQ,epor=(1+ ee0)*(120/Pe0)^-lambdaB -1} *)
path = {ICond0};
loading = {1,{0,0},{xdP,xdQ},0};
