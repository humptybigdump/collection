(* ::Package:: *)

(*    Mathematica  tools for element tests with MCC in isomorphic PQ space  by A.Niemunis

This  notebook produces stress/strain path according to the modified Cam clay .
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (= MCC.m) to the new directory.
4) Begin a Mathematica session with:    Needs["PQ`MCC`"]
*)


BeginPackage["PQ`MCC`"]
Off[General::spell,General::spell1,Solve::ifun];
(*---------------------------------------------usages--------------------------------------------------------------------*)
 RunMccTest::usage = "RunMccTest[  Mat,ICond,{ntime,deps,dT}] \
 \n  RunMccTest returns the  sequence of states {P, Q, Pc, \[Epsilon]P, \[Epsilon]Q, epor}
 \n  which are incrementally calculated according to MCC model within the module \
 \n  using the material parameters Mat = {\[Nu],\[Kappa]B,\[Lambda]B,\[Phi]} \
 \n  the initial conditions Icond = {P,Q,Pc,\[Epsilon]P,\[Epsilon]Q,epor, \[Epsilon]Ppl,\[Epsilon]Qpl } \
 \n  and the loading programme  {ntime,deps,dT}.
 \n  The loading programme  consists of : \
 \n  ntime = the number of repetitions,  \
 \n  deps = {depsP, depsQ} = strain increment, \
 \n  dT = {dP,dQ} = stress increment. \
 \n  Two of the four incremental values: depsP, depsQ, dP, dQ  must be prescribed (given as numbers) \
 \n  and the remaining two must be calculated and thus they should be specified as symbols.   \
 \n  " ;
plotMCCsurface::usage = "plotMCCsurface[Pc,\[Phi]]  plots the yield surface  of the MCC-model in the isomorphic P-Q-space.   \
\n The yield surface consists of two half-elipses  with the common diameter Pc along the P-axis and different half-diameters \
\n along the Q-axis.  \
 ";
getElasticIsomorph::usage = "getElasticIsomorph[P_,\[Nu]_,\[Kappa]B_]  returns the isotropic hypoelastic stiffness for the isomorphic P-Q-space    \
\n  in the form {{3K,0},{0,2G}} with the bulk modulus K and shear modulus G, both barotropic.  \
";
PQPlot::usage = "PQPlot[Path , Mat ] or  PQPlot[{Path1,Path2,.. }, Mat ]  plots the stress path in the isomorphic P-Q-space.   \
\n Moreover for the first and the last state PQPlot shows the yield surfaces of the MCC-model. \
\n If several Paths are used like {Path1,Path2,.. } the yield surfaces for the end-states of each path are also plotted. \
 ";
epsQQPlot::usage = "epsQQ[Path , Mat ] or  epsQQ[{Path1,Path2,.. }, Mat ]  produces an isomorphic epsQ-Q diagramm.   \
 ";
oedometricPlot::usage = "oedometricPlot[Path, Mat] or  oedometricPlot[{Path1,Path2,... } , Mat] plots the compression curve  in   P-e-space.   \
\n The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled  \
  ";
controlPanelMCC::usage = "controlMCC[ ] loads a graphic interface to perform element tests using MCC.   \
\n The initial conditions must be saved in the first row of the global variable path. \
\n Two stress or strain or mixed increments must be prescribed. Push the button calculate to see the results after the increment.  \
\n The number of increments is ntime, but can be modified writing a new number in the cell ntime. \
\n If you want to start with a brand new simulation, click on Reset. \
\n If you wish to undo the last step, please click on the button Undo. \
  ";
(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];
inRange[x_, range_] := IntervalMemberQ[Interval[range], x];
getElasticIsomorph[P_,\[Nu]_,\[Kappa]B_] := Module[{elastG,elastK},elastK=P/ ( 3* \[Kappa]B ); elastG=(elastK (3 -6  \[Nu]))/(2+2 \[Nu]);
  Simplify[{{3 elastK,0},{0,2 elastG}}]
];

plotMCCsurface[Pc_,\[Phi]_] := Module[{MC,ME,P,Q,gC,gE},MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));ME=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));
  gC=ContourPlot[P^2+(Q/MC)^2-P Pc==0,{P,0.001`,Pc},{Q,0,(Pc MC)/2},ContourStyle->Hue[Pc/100.`],DisplayFunction->Identity];
  gE=ContourPlot[P^2+(Q/ME)^2-P Pc==0,{P,0.001`,Pc},{Q,1/2 (-Pc) ME,0},ContourStyle->Hue[Pc/100.`],DisplayFunction->Identity];
  {gC,gE}
];

RunMccTest[Mat_,ICond_,Loading_]:=Module[
  {state,\[Nu],\[Kappa]B,\[Lambda]B,\[Phi],MC,ME,M,P,Q,Pc,\[Epsilon]P,\[Epsilon]Q,epor,itime,elastStiff,unknowns,solution,d\[Epsilon]Trial, 
   dTTrial,PTrial,QTrial,yield,loadingQ,\[Lambda],P1,Q1,Pc1,eq1a,eq1b,eq2a,eq2b,eq3,eq4,eq5,   eq6,   
   unknowns1,approxTF,approx,approxi,unknowns1approxi, 
   solution1,dT2,d\[Epsilon]2,\[Lambda]2, d\[Epsilon],dT, dt, \[Epsilon]Ppl,\[Epsilon]Qpl, directions, dirTrial , oV ,ntime },
   
       oV = False;  (* option Verbose *)
    {ntime, d\[Epsilon], dT, dt, directions }   =    N[ Loading ]  ;
   
	 If[NumberQ[ntime] == False, ntime = 0;  Print["Warning: number of increments must not be symbolic, I set ntime = 0 "] ];
     If[NumberQ[dt]== False , dt = 0;  Print["Warning: time increment is not numeric, I set  dt = 0 "] ]; 
    If[Length[ Select[ Join[ d\[Epsilon], dT, directions ] , NumberQ[#]& ] ] != 2 , Print[ "Error: definition of Loading needs exactly two numerical values  " ] ; Abort[]   ];
    
    state=Table[{0,0,0,0,0,0,0,0},{i,1,ntime+1}]; 
   {\[Nu],\[Kappa]B,\[Lambda]B,\[Phi]}=N[ Mat ];
   \[Kappa]B/=Sqrt[3];\[Lambda]B/=Sqrt[3];
   MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));
     
  {P,Q,Pc,\[Epsilon]P,\[Epsilon]Q,epor,\[Epsilon]Ppl,\[Epsilon]Qpl }=N[ ICond ];

   state[[ 1 ]]=N[ ICond ];
   For[itime=2,itime<=ntime+1,
       elastStiff=getElasticIsomorph[P,\[Nu],\[Kappa]B];
      
     (* AN 2022.07  directions are never used as in unknown because the equations are eliminated *) 
     If[ NumberQ[directions[[1]] ] ,  eq5 = d\[Epsilon][[2]] == directions[[1]]  *d\[Epsilon][[1]] , eq5 = True  ]; 
     If[ NumberQ[directions[[2]] ] ,  eq6 = dT[[2]] == directions[[2]]  *dT[[1]] , eq6 = True  ]; 
     unknowns=Select[Flatten[{d\[Epsilon],dT }],symbolQ];  (* unknown increments depend on the choice of the test control *)
     If[oV, Print[" eq5: ", eq5, "    eq6: ", eq6] ];  
     elasticEquations = DeleteCases[{ dT[[1]] ==(elastStiff . d\[Epsilon])[[1]],  dT[[2]] ==(elastStiff . d\[Epsilon])[[2]] ,  eq5, eq6 } , True ] ; 
	 solution=NSolve[ elasticEquations ,unknowns];
	 
	 If[oV, Print["eq.System= ",  {dT[[1]] ==(elastStiff . d\[Epsilon])[[1]],  dT[[2]] ==(elastStiff . d\[Epsilon])[[2]] ,  eq5, eq6 }   ]];  
	 If[oV, Print["unknowns= ",  unknowns    ]];  
	 If[oV, Print["solution=", solution  ]]; 
	 
       {d\[Epsilon]Trial,dTTrial}={d\[Epsilon],dT }/.solution[[1]]; (* elastic trial solution (explicit) *)
       M=If[Q+dTTrial[[2]]>=0,MC,ME];
       {PTrial,QTrial}={P,Q}+dTTrial;
	   yield=PTrial^2+(QTrial/M)^2-PTrial Pc;
       If[yield>-0.00001 * PTrial^2,loadingQ=True,loadingQ=False];
       (* ELSTIC CASE :::::::::::: *)
       If[!loadingQ,
	       {\[Epsilon]P,\[Epsilon]Q}+=d\[Epsilon]Trial;  epor+=-(1+epor) d\[Epsilon]Trial[[1]] Sqrt[3];  
     	   {P,Q}+=dTTrial; 
     	   Goto[nextInc]
		   ];
		   
       (* PLASTIC CASE ::::::::: build formal implicit equations (for the updated state) but calculate nothing *)
       eq1a=dT[[1]]==(  elastStiff . (d\[Epsilon]-\[Lambda] {2 P1-Pc1,(2 Q1)/M^2})   )[[1]];  (* increment of P *)
       eq1b=dT[[2]]==(  elastStiff . (d\[Epsilon]-\[Lambda] {2 P1-Pc1,(2 Q1)/M^2})   )[[2]];  (* increment of Q *)
       eq2a=P1==P+dT[[1]];            (* updated P *)
       eq2b=Q1==Q+dT[[2]];            (* updated Q *)
       eq3=P1^2+(Q1/M)^2-P1*Pc1==0;    (* yield function *) 
       eq4=Pc1== Pc * Exp[  \[Lambda] (2 P1-Pc1) / (\[Lambda]B-\[Kappa]B)    ]   ;  (* updated Pc *)
      

       unknowns1=Flatten[{unknowns,\[Lambda],Pc1,P1,Q1}];  (* collect 6  unknowns (two of which depend on test control) for  6 scalar equations eq1a..eq4 *)
	   approxTF=(symbolQ[#1]&)/@Flatten[{d\[Epsilon],dT }];  (* a vector like {True,False,False,True} showing symbols among the increments {d\[Epsilon],dT} // Flatten *)
       
	   approx=Pick[Flatten[{d\[Epsilon]Trial,dTTrial }],approxTF]; (* trial solutions corresponding to symbolic values of increments  *)
       approxi=Flatten[{approx,0,Pc,PTrial,QTrial}];   (* take the elastic predictor as the first guess *)
       
	   unknowns1approxi=Transpose[{unknowns1,approxi}]; (* unknowns paired with their approximations *)
     
     If[oV, Print["Plast. eq.System= ", MatrixForm[{eq1a,eq1b,eq2a,eq2b,eq3,eq4,eq5Plast,eq6Plast}  ]   ] ];  
	 If[oV, Print["unknowns1approxi= ", MatrixForm[ unknowns1approxi  ] ]];  
	  
	 plasticEquations = DeleteCases[{ eq1a,eq1b,eq2a,eq2b,eq3,eq4,   eq5, eq6 } , True ] ; 
	 solution=NSolve[ elasticEquations ,unknowns];
	  
	   (*-------------------------------------------------------------------------------*)
       solution1=FindRoot[ plasticEquations,unknowns1approxi,AccuracyGoal->5]; (* Essential solution of the invremental problem via RMI *)
       (*-------------------------------------------------------------------------------*)
       
       If[oV, Print["solution1=", solution1 ]]; 
       
       If[ (\[Lambda] /. solution1 ) <0, 
            Print[Style["ERROR: \[Lambda]<0, usually when stress increments surpass the (over)critical yield surface ",Red,16, Bold] ]; Abort[] 
            ]; (* Extra control to avoid negative lambda *)
       {dT2,d\[Epsilon]2,\[Lambda]2,Pc,P,Q}={dT,d\[Epsilon],\[Lambda],Pc1,P1,Q1}/.solution1;  (* after solution  assign values to 6 unknowns *)
       {\[Epsilon]P,\[Epsilon]Q}+=d\[Epsilon]2;         (* update strain *)
       {\[Epsilon]Ppl,\[Epsilon]Qpl}+=\[Lambda]2*{  (2 P-Pc), 2 Q /M^2 };   (* update plastic strain *)

       epor = (1.+epor) Exp[ -d\[Epsilon]2[[1]] Sqrt[3] ] - 1.0;
       Label[nextInc];
       state[[itime]]={P,Q,Pc,\[Epsilon]P,\[Epsilon]Q,epor, \[Epsilon]Ppl,\[Epsilon]Qpl };  (* write down the updated state (common for elastic and plastic case)*)
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
           gPQs[[1]] = ListPlot[PQs[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
           gfirstSurface[[1]] = plotMCCsurface[Paths[[1,3]],Mat[[4]]];
           glastSurface[[1]] =  plotMCCsurface[Paths[[-1,3]],Mat[[4]]];
           outputPlot  = { gPQs[[1]], gfirstSurface[[1]], glastSurface[[1]],
                           Graphics[{ Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (2Sqrt[2] Sin[ Mat[[4]] ])/(3-Sin[ Mat[[4]] ])}}],
                           Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (-2 Sqrt[2] Sin[ Mat[[4]] ])/(3+Sin[ Mat[[4]] ])}}] } ]  } ;
         ] ;
        If[nPaths > 1,
        For[iPath=1, iPath<=nPaths,
          currentPath =   Paths[[iPath]] ;
          PQs[[iPath]] = {#[[1]], #[[2]] }  &  /@  currentPath;
          gPQs[[iPath]] = ListPlot[PQs[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
          gfirstSurface[[iPath]] = plotMCCsurface[currentPath[[1,3]],Mat[[4]]];
           glastSurface[[iPath]] =  plotMCCsurface[currentPath[[-1,3]],Mat[[4]]];
        iPath++;] ;
        outputPlot  = Flatten[{ gPQs , gfirstSurface , glastSurface } ,1];
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
            gepsQQ[[iPath]] = ListPlot[epsQQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]Q,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
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
            gepsPepsQ[[iPath]] = ListPlot[epsPepsQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]P,\[Epsilon]Q},AxesOrigin->{0,0},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsPepsQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];
 inputCheck[Mat_, Icond_, Loading_]:= Module[ {P, Q,  \[Nu], \[Kappa]B, \[Lambda]B, \[Phi], Pc, PcPlus, OCR, \[Epsilon]P, \[Epsilon]Q, epor, ntime, dT, deP, deQ, dP, dQ , MC,ME,M,  \[Epsilon]Ppl,\[Epsilon]Qpl, dt, directions },
 {\[Nu],\[Kappa]B,\[Lambda]B,\[Phi]}=Mat;
 {P,Q,Pc,\[Epsilon]P,\[Epsilon]Q,epor, \[Epsilon]Ppl,\[Epsilon]Qpl}= Icond;
 { ntime, {deP,deQ}, {dP,dQ}, dt, directions } = N[  Loading   ]   ;

  If[symbolQ[ntime] , Print[ "number of increments  ntime =  ", ntime," undefined"   ]]  ;
  If[ ntime < 10 , Print[ "suspiciously small number of increments  ntime =  ", ntime   ]]  ;
  If[ deP > 0.005 || deQ > 0.005 , Print[ "Strain increments seem to be large {deP, deQ} = ", {deP, deQ}  , " Keep them at  0.1% level"   ]]  ;
  If[ dP > 10 || dQ > 10 , Print[ "Stress increments seem to be large {dP, dQ} = ", {dP, dQ}  , " Keep them below  10 kPa "   ]]  ;
  If[ Pc *( 1+ epor )^(1/ \[Lambda]B ) < 5000, Print[ "Warning: epor=0 can be reached already at about Pc =  ",  Pc *( 1+ epor )^(1/ \[Lambda]B ) ,
        "  Try to reduce \[Lambda]B = ",   \[Lambda]B ] ]   ;

  If[\[Nu] ~inRange~ {0, 0.5} ,"" , Print[ "Suspicious \[Nu] =  ",\[Nu]   ]]  ;
  If[ \[Kappa]B ~inRange~ {0, 0.2} , "", Print[ "Suspicious \[Kappa]B =  ",\[Kappa]B  ]]  ;
  If[\[Lambda]B ~inRange~ {  \[Kappa]B , 20  \[Kappa]B } , "", Print[ "Suspicious \[Kappa]B/\[Lambda]B =  ",\[Kappa]B / \[Lambda]B ]]  ;
  If[ \[Phi] ~inRange~ {5\[Degree], 50\[Degree]} , "", Print[ "Suspicious \[Phi]=  ", \[Phi], "rad"  ]]  ;

  If[P < 0 , Print[ "Warning: negative initial  P =  ", P   ]]      ;
  If[Pc < P , Print[ "Warning:  initial  Pc =  ", Pc , "<", P, " =P" ]]  ;
  MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));
  ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));
  M=If[Q >=0,MC,ME];
  If[ P^2+(Q/M)^2-P*Pc >  Pc*Pc *10^-6, Print[ "Warning: initial OCR<1 means underconsolidation:  P^2+(Q/M)^2-P*Pc  = ", P^2+(Q/M)^2-P*Pc  , "> 0" ] ]  ;
    PcPlus  =  P +(Q/M)^2 /P ;
    OCR = Pc/PcPlus;
  If[ OCR ~inRange~ {1.2,2},  Print[ "Initial  OCR =", OCR, ", soil was lightly overconsolidated " ] ];
  If[ OCR ~inRange~ {1.0,1.01},  Print[ "Initial OCR =", OCR, ", soil was  normally consolidated " ] ];
  If[ OCR ~inRange~ {1.01,1.2},  Print[ "Initial OCR =", OCR, ", soil was  almost  normally consolidated " ] ];
  If[ OCR ~inRange~ {2,4},  Print[ "Initial OCR =", OCR, ", soil was  overconsolidated " ] ];
  If[ OCR ~inRange~ {4, Infinity},  Print[ "Initial OCR =", OCR, ", soil was heavily overconsolidated " ] ];



  If[epor  ~inRange~ {0.01, 3} , "", Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 3} "  ]];

 dT = {dP,dQ};
  
  If[Count[ Evaluate[ symbolQ[#] & /@ ({deP, deQ, dP, dQ }~Join~ directions  )  ], True] > 4,  Print["Error: Too many unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Count[ Evaluate[ symbolQ[#] & /@ ({deP, deQ, dP, dQ }~Join~ directions  ) ], True]  < 4,  Print["Error: Too few unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, True, False, True},  Print["Error: You cannot prescribe deP and dP in Loading simultaneously"]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == {True, False, True,False},  Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously"]; Beep[]; Abort[]; ];
 PrescribedStressPath  =  Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, False, True, True} ;
 If[ PrescribedStressPath  ,
      P +=  ntime *dP;      Q += ntime*dQ;
      M=If[Q >=0,MC,ME];
      If[ P^2+(Q/M)^2-P*Pc >  Pc*Pc *10^-6, Print[ "Warning: the stress path ends above CSL; strain or mixed control recommended " ] ]  ;
 ];
   If[Length[path] > 1,
              { P,Q,Pc,\[Epsilon]P,\[Epsilon]Q,epor, \[Epsilon]Ppl,\[Epsilon]Qpl}= Last[path];
                M=If[Q >=0,MC,ME];
                PcPlus  =  P +(Q/M)^2 /P ;
                Print["The latest value of OCR =", Pc/PcPlus];
  ];

];

 controlPanelMCC[] := Module[{Loading},
  If[Length[path] == 0, path = Array[0 &, {1, 6}]; path = {ICond0};]; (* Initialisation of global variable Path *)
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
 {{d\[Epsilon]Qd\[Epsilon]P, xdeQdeP}}  , 
 {{dP, xdP}}  ,
 {{dQ, xdQ}}  ,
 {{dQdP, xdQdP}}  ,
 {{dt, xdt}}  ,
 " (in % [] or kPa) each.",
 Delimiter,
 Button["Check the input",

   d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
   d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
   Loading = { ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}] ,  dt , N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }]  }; 
  inputCheck[Mat, ICond0, Loading  ]
 ],
   Button["Calculate step",

  d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
  d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;


  Loading = { ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}] ,  dt , N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }]  };
  dpath =  RunMccTest[Mat,Last[path],Loading]  ;
  path = Join[ path,  Delete[ dpath, 1] ] ;

 If[Count[Evaluate[symbolQ[#] & /@ ({d\[Epsilon]P, d\[Epsilon]Q, dP, dQ} ~Join~ N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }] ) ], True] != 4,
  Print[Style[ "Error: system of equations could not be solved. Exactly  four unknowns are needed.", Red, 20] ]; Abort[]; Beep[];
  ];  (* Controls if two unknowns are sought *)
 ],

Button["Undo the last ntime increments",
 If[Length[path] >= ntime+1, path = Drop[path, -ntime] , path = {path[[1]]} ;
   Print[Style["No more increments can be undone.", Blue, 20] ]; ];
  ],

Button["Print the final state",
  Print["Final {P,Q}= ", path[[-1,1 ;; 2]]," ,  {\[Epsilon]P,\[Epsilon]Q}= ", path[[-1,4 ;; 5]], " , Pc= ", path[[-1,3]], " , epor= ",path[[-1,6]] ,
      ", {\[Epsilon]Ppl,\[Epsilon]Qpl}= ", path[[-1,7 ;; 8]]
  ];
 ],

Button["Reset all steps, graphics and delete cell",
  path = {ICond0}; ntime = xntime;  d\[Epsilon]Pp = xdeP ; d\[Epsilon]Qp = xdeQ; dP = xdP; dQ = xdQ; FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]];
 ]
 ]
];



ExampleIsochoricShearing[] := Module[{xdP,xdQ,Mat1, ICond1, Path1, Loading1, Path2, ICond2, Path3, Loading2, Path4 },
Print["Undrained : active/passive  shearing ;  normally consolidated/overconsolidated.\
\n Note that the initial  preconsolidation Pc=101 may slightly change! "];
Mat1={0.2`,0.03`,0.2`,\[Pi]/6};
ICond1={100.`,0.`,101.`,0.`,0.`,1.`,0, 0};
Loading1={100,{0,0.001`},{xdP,xdQ},1, {xdeQdeP,  xdQdP }};
ICond2={10.`,0.`,101.`,0.`,0.`,1.`,0, 0};
Loading2={100,{0,-0.001`},{xdP,xdQ},1, {xdeQdeP,  xdQdP }};
 Path1=RunMccTest[Mat1,ICond1,Loading1];
 Path2=RunMccTest[Mat1,ICond2,Loading1];
 Path3=RunMccTest[Mat1,ICond1,Loading2];
 Path4=RunMccTest[Mat1,ICond2,Loading2];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1]  , epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];
ExampleOedometricTest[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print["Oedometric loading , short unloading, reloading "];
Mat1={0.2,0.03,0.2,\[Pi]/6};
ICond1={65,0.,100.,0,0,1,0, 0};
Loading={90,{0.001/ Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
ULoading={10,{-0.001/ Sqrt[3],-0.001 Sqrt[2./3.]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
RLoading={60,{0.001/Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
Path1=RunMccTest[Mat1,ICond1,Loading];
Path2=RunMccTest[Mat1,Path1[[-1]],ULoading];
Path3=RunMccTest[Mat1,Path2[[-1]],RLoading];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1], epsQQPlot[{Path1,Path2,Path3}, Mat1]  }, ImageSize-> Large]
];
ExampleOedometricTest2[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print["Oedometric loading , long unloading, reloading "];
Mat1={0.2`,0.03`,0.2`,\[Pi]/6};
ICond1={65.`,0.`,100.`,0.`,0.`,1.`,0, 0};
 Loading={80,{0.001` / Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
 ULoading={45,{-0.001` /Sqrt[3.`],-0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
 RLoading={70,{0.001`/Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
  Path1=RunMccTest[Mat1,ICond1,Loading];
 Path2=RunMccTest[Mat1,Path1[[-1]],ULoading];
 Path3=RunMccTest[Mat1,Path2[[-1]],RLoading];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1],  epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];
ExampleIsobaricShearing[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print["Isobaric shearing,  slightly/strongly  overconsolidated starting from Pc=100 "];
Mat1={0.2`,0.03`,0.2`,\[Pi]/5};
 ICond1={80.`,0.`,100.`,0.`,0.`,1.`,0, 0};
 ICond2={20.`,0.`,100.`,0.`,0.`,1.`,0, 0};
 Loading={50,{xdeP,0.0025`},{0,xdQ},1, {xdeQdeP,  xdQdP }};
 Path1= RunMccTest[Mat1,ICond1,Loading];
 Path2 = RunMccTest[Mat1,ICond2,Loading];
 GraphicsRow[{oedometricPlot[{Path1,Path2}, Mat1], PQPlot[{Path1,Path2}, Mat1],  epsQQPlot[{Path1,Path2}, Mat1]   }, ImageSize-> Large]
];
EndPackage[ ]
$Context = "PQ`MCC`"   ;
Print[ "The Modified Cam Clay (MCC) Model for isomorphic P-Q space; by A. Niemunis 2007-2022 \
\n  You are in the context PQ`MCC` which provides functions: \
\n    RunMccTest, PQPlot, oedometricPlot, epsQQPlot, controlMCC \
\n    and some ready to use examples:  \
\n    ExampleOedometricTest[], ExampleIsochoricShearing[], ExampleOedometricTest2[], ExampleIsobaricShearing[]\
\n  *********************************************************************************************************** \
\n    For detailed information on the programming see the notebook MCC-impli.nb or read the PDF documentation.\
"];
