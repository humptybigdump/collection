(* ::Package:: *)

(*    Mathematica  tools for element tests with  VHP in isomorphic PQ space  by A.Niemunis

This  notebook produces stress/strain path according to the modified Cam clay .
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (=  VHP.m) to the new directory.
4) Begin a Mathematica session with:    Needs["PQ`VHP`"]
*)


BeginPackage["PQ`VHP`"]
Off[General::spell,General::spell1,Solve::ifun];
(*---------------------------------------------usages--------------------------------------------------------------------*)

  RunVHPTest::usage = "RunVHPTest[ Mat_,ICond_,Loading_ ]  
   INPUT: Mat   = {\[Nu], \[Kappa]B, \[Lambda]B, \[Phi], Iv, ee0, Pe0, evisPref} 
          ICond   =  Last[path] =  {P, Q, Pe, \[Epsilon]P, \[Epsilon]Q, epor = (1 + ee0)*(120/Pe0)^-\[Lambda]B - 1}
		  Loading = { ntime,  {d\[Epsilon]P, d\[Epsilon]Q } {dP, dQ} ,   dt ,  { d\[Epsilon]Qd\[Epsilon]P , dQdP }] }   some components are symbolic   
   OUTPUT: dpath  = a portion of ntime states to be appended to path 
   OPTIONS: verbose -> False 
            isotropic -> True  chooses isotropic elasticity and flow rule from the MCC
    
   The loading programme  consists of :  
   ntime = the number of increments,   
   deps = {depsP, depsQ} = strain increment,  
   dT = {dP,dQ} = stress increment,  
   dt = time increment,    
   directions = { depsQ/depsP, dQ/dP }.  
   The values for ntime and dt  must be provided by the user (usually via contolPanelVHP[...]).
   Two comonents in Loading must be numerical others should be specified as symbols.    
   " ;
   
plotVHPsurface::usage = "  {gC,gE} =  plotVHPsurface[Pe,\[Phi]]  
   INPUT: Pe = preconsolidation pressure 
          \[Phi]  = resudual friction angle 
   OUTPUT:  gC = graphics with Coulomb condition 
            gE = graphics with ellipse 
   OPTIONS: 
  plots the yield surface  of the VHP-model in the isomorphic P-Q-space.    
  The yield surface consists of two half-elipses  with the common diameter Pe along the P-axis and different half-diameters  
  along the Q-axis.   
 ";
getElasticIsomorph::usage = "getElasticIsomorph[P_,Q_,extendedMat_]  
   INPUT:  P  = isometric effective stress component 
           Q  = isometric effective stress component 
           extendedMat =  {\[Nu],\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b} 
   OUTPUT:  Elastic stiffness as a 2 x 2 Matrix if isotropic {dP,dQ } = {{3K, 0},{0, 2G}}. {deP , deQ}
   OPTIONS:  verbose -> False  
             isotropic -> oIso      
             if oIso == True  return isotropic hypoelastic   barotropic   stiffness 
             if oIso == False   return orthotropic  hypoelastic   barotropic   stiffness (the linear part of Hypoplasticity)    
  
";

getCreepRate::usage="getCreepRate[P_, Q_, Pe_, Elast_, extendedMat_, verbose ->   ] 
    INPUT:  P  = isometric effective stress component 
            Q  = isometric effective stress component 
             Elast = isometric elastic stiffness (used if the hypoplastic (and not MCC) flow rule is used)
           extendedMat =  {\[Nu],\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b} 
   OUTPUT:  creep rate in isometric P-Q-components  
   OPTIONS:  verbose ->  oIso   
             if oIso == True   uses PePlus and mb from the MCC model 
             if oIso == False  hypoelastic PePlus and the hypoplastic flow rule    
";



PQPlot::usage = "PQPlot[Path , Mat ] or  PQPlot[{Path1,Path2,.. }, Mat ]  
   INPUT:  Mat   = {\[Nu], \[Kappa]B, \[Lambda]B, \[Phi], Iv, ee0, Pe0, evisPref} 
           path = list of states 
   OUTPUT:  
   OPTIONS: 
plots the stress path in the isomorphic P-Q-space.   
 Moreover for the first and the last state PQPlot shows the yield surfaces of the VHP-model.  
 If several Paths are used like {Path1,Path2,.. } the yield surfaces for the end-states of each path are also plotted.  
 ";
  

epsQQPlot::usage = "epsQQ[Path , Mat ] or  epsQQ[{Path1,Path2,.. }, Mat ]  
   INPUT:   Mat   = {\[Nu], \[Kappa]B, \[Lambda]B, \[Phi], Iv, ee0, Pe0, evisPref} 
            path = list of states 
   OUTPUT:  
   OPTIONS: 
produces an isomorphic epsQ-Q diagramm.    
 ";
 

oedometricPlot::usage = "oedometricPlot[Path, Mat] or  oedometricPlot[{Path1,Path2,... } , Mat] 
   INPUT:   Mat   = {\[Nu], \[Kappa]B, \[Lambda]B, \[Phi], Iv, ee0, Pe0, evisPref} 
            path  = list of states 
   OUTPUT:  
   OPTIONS: 
 plots the compression curve  in   P-e-space.    
 The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled  
  ";
  

controlPanelVHP::usage = "controlVHP[ ] 
   INPUT: via global variable 
      Mat =  {\[Nu], \[Kappa]B, \[Lambda]B, \[Phi], Iv, ee0, Pe0, evisPref};
	  ICond0 =  =  {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor=(1+ ee0)*(120/Pe0)^-\[Lambda]B -1}  
	  path = {ICond0} 
   OUTPUT:    a graphic interface to perform element tests using VHP 
   OPTIONS: 
   If you want to start with a brand new simulation, click on Reset.  
   If you wish to undo the last step, please click on the button Undo.  
  ";
  
 
cycle::usage = "Syntax:  cycle[Mat,  loading1 , loading2  ,ncyc_] 
   INPUT:  Mat   = {\[Nu], \[Kappa]B, \[Lambda]B, \[Phi], Iv, ee0, Pe0, evisPref} 
		   Loading1/2 = { ntime,  {d\[Epsilon]P, d\[Epsilon]Q } {dP, dQ} ,   dt ,  { d\[Epsilon]Qd\[Epsilon]P , dQdP }] }   some components are symbolic 
           ncyc = number of cycles consisting of loading1 and loading2
		   The initial state is assumed Last[path]
   OUTPUT:  updated path
   OPTIONS: 
is a loop performing ncyc cyles calling two step[ ] procedures per cycle.
See also step[] ";

step::usage = "Syntax:  step[ Mat,  loading ]   
   INPUT:    Mat   = {\[Nu],\[Kappa]B, \[Lambda]B, \[Phi], Iv, ee0, Pe0, evisPref} 
		     Loading = { ntime,  {d\[Epsilon]P, d\[Epsilon]Q } {dP, dQ} ,   dt ,  { d\[Epsilon]Qd\[Epsilon]P , dQdP }] }   some components are symbolic 
             The initial state is assumed Last[path]
   OUTPUT:  updated path
   OPTIONS: 
 calls RunVHPTest[Mat, Last[path],  Loading ]. The number of increments in cointained in Loading. 
 A piece of path  returned by  RunVHPTest  is Joined  with the existing path. 
";
 




(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];

inRange[x_, range_] := IntervalMemberQ[Interval[range], x];

Options[ getElasticIsomorph ] = {verbose -> False , isotropic -> True }
getElasticIsomorph[P_,Q_,extendedMat_ ,OptionsPattern[ ]] := Module[
{\[Nu],\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b,\[Eta],fb,F, Estiff,oIso, elastK, elastG },
 (*  oIso = OptionValue[ isotropic ]; *)
 oIso = OptionValue[isotropic]; 
  If[Not[ oIso], 
  {\[Nu],\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b}  = extendedMat;
  \[Eta] = Q/P;
  fb = \[Beta]b*P*Sqrt[3.0];
  F = If[Q>= 0, 1, 1+ (Q/P)/Sqrt[2.0] ];
  Estiff=  fb*( F*F* {{1,0},{0,1}} + a*a/3 * {{1,\[Eta]},{\[Eta],\[Eta]*\[Eta]}}  ) //Simplify
  ];
  If[ oIso, 
  {\[Nu], \[Kappa]B} =  extendedMat[[1;;2]];
  elastK=P/ ( 3* \[Kappa]B ); 
  elastG=(elastK (3 -6  \[Nu]))/(2+2 \[Nu]);
  Estiff = Simplify[{{3*elastK,0},{0,2*elastG}}] ];
  Estiff
];

(* d\[Epsilon]vis = getCreepRate[P1 ,Q1 ,Pe1,elastStiff,extendedMat];*)
Options[ getCreepRate ] = {verbose -> False , isotropic -> True }
getCreepRate[P_, Q_, Pe_, Elast_, extendedMat_, OptionsPattern[ ] ] := Module[
{\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b,M, oIso  ,  PePlus , OCR, mb},
{\[Nu],\[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref,MC,ME,a,\[Beta]b} = extendedMat;
  (* oIso = OptionValue[ isotropic ];  *) 
  oIso = OptionValue[isotropic];  
  M= If[Q >=  0,  MC,   ME] ;  
   \[Eta] = Q/P;  
   If[ Not[oIso],  PePlus = P* (1 + \[Eta]*\[Eta]);   mb = Normalize[  Inverse[Elast] . {P, 2*Q } ] ;  ]; 
   If[ oIso,  PePlus = P*( 1+  \[Eta]*\[Eta] /M^2 ) ;  mb = {2P - PePlus, 2 Q / M^2 } //Normalize;     ];      
  OCR = Pe/PePlus; 
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
 


 (* new d\[Epsilon]dir and dTdir in Loading *)
Options[ RunVHPTest ] = {verbose -> False , isotropic -> True } 
RunVHPTest[Mat_,ICond_,Loading_, OptionsPattern[ ]]:=Module[  { oV, oIso, 
        ntime, d\[Epsilon], dT, dt,  directions , \[Nu], \[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref, MC,ME,a, \[Beta]b, extendedMat,
       P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor, state, itime, (* elastStiff,*)   unknowns, solution, d\[Epsilon]Trial, dTTrial,   PTrial,QTrial,  d\[Epsilon]vis, P1, Q1, Pe1,
        unknowns1, approxTF, approx, approxi, unknowns1approxi, solution1,dT2,d\[Epsilon]2,eq1a,eq1b,eq2a,eq2b,eq4,eq5,eq6},
   
     oV = False;  (* option Verbose *)   
     oIso = OptionValue[isotropic] ;  
   {ntime, d\[Epsilon], dT, dt, directions }   =  N[ Loading ];
	 If[NumberQ[ntime] == False, ntime = 0;  Print["Warning: number of increments must be prescribed and not symbolic, the default value is ntime = 0 "] ];
   If[NumberQ[dt]== False , dt = 0;  Print["Warning: time increment must be prescribed and not symbolic, the default value is dt = 0 "] ]; 
	 
	 
	 If[NumberQ[  d\[Epsilon][[1]] ]  &&  NumberQ[  dT[[1]]  ] ,  Print[ "Error: dP  and  depsP cannot be   both prescribed" ] ; Abort[]   ] ;
	 If[NumberQ[  d\[Epsilon][[2]] ]  &&  NumberQ[  dT[[2]]  ] ,  Print[ "Error: dQ  and  depsQ cannot be   both prescribed" ] ; Abort[]   ] ;
	  
   state = { ICond };   (* to be  appended *)
   {\[Nu], \[Kappa]B,\[Lambda]B,\[Phi],Iv,ec0,pc0,evisPref}= N[ Mat ];
    MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));
    ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));  (*isometric M*)
    a = Sqrt[3.0] (3 - Sin[\[Phi]] ) /( 2 Sqrt[2.0] Sin[\[Phi]] );
    \[Beta]b = 1/((1 + a*a/3)*\[Kappa]B)  ;
    extendedMat =  Join[ Mat ,{MC,ME,a,\[Beta]b}] ;
   {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor}=N[ ICond ];
   state[[ 1 ]]={P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor} ;
   
  For[itime=2,itime<=ntime+1,
       elastStiff=getElasticIsomorph[P,Q, extendedMat, isotropic -> oIso ];
       If[oV, Print["explicit elastStiff =", elastStiff]]; 
       (* AN 2022.07  directions are never used as in unknown because the equations are eliminated *) 
      If[ NumberQ[directions[[1]] ] ,  eq5 = d\[Epsilon][[2]] == directions[[1]]  *d\[Epsilon][[1]] , eq5 = True  ]; 
      If[ NumberQ[directions[[2]] ] ,  eq6 = dT[[2]] == directions[[2]]  *dT[[1]] , eq6 = True  ]; 
       
       unknowns=Select[Join[ d\[Epsilon],dT  ],symbolQ];  (* unknown increments depend on the choice of the test control *)   
       elasticEquations = DeleteCases[{ dT[[1]] ==(elastStiff . d\[Epsilon])[[1]],  dT[[2]] ==(elastStiff . d\[Epsilon])[[2]] ,  eq5, eq6 } , True ] ; 
 	  solution=NSolve[ elasticEquations ,unknowns];  
 	  
 	   If[oV, Print["elasticEquations= ",   elasticEquations    ]];  
	    If[oV, Print["unknowns= ",  unknowns    ]];  
	    If[oV, Print["solution=", solution  ]]; 
 	  
       {d\[Epsilon]Trial,dTTrial }={d\[Epsilon],dT }/.solution[[1]]; (* elastic trial solution (explicit predictor) *) 
       
       { PTrial,QTrial} = {P,Q} + dTTrial;
       elastStiff = getElasticIsomorph[P1, Q1, extendedMat, isotropic -> oIso];
       d\[Epsilon]vis = getCreepRate[P1 ,Q1 ,Pe1,elastStiff,extendedMat,  isotropic -> oIso];
       If[oV, Print["d\[Epsilon]vis=",  d\[Epsilon]vis ]];  

       eq1a=dT[[1]]==(  elastStiff . (d\[Epsilon]- d\[Epsilon]vis*dt)   )[[1]];  (* increment of P *)
       eq1b=dT[[2]]==(  elastStiff . (d\[Epsilon]- d\[Epsilon]vis*dt)  )[[2]];  (* increment of Q *)
       eq2a=P1==P+dT[[1]];            (* updated P *)
       eq2b=Q1==Q+dT[[2]];            (* updated Q *)
       eq4=Pe1== Pe * Exp[d\[Epsilon][[1]]/ \[Lambda]B  ] ; 

       If[oV , Print[ "visco problem: \n eq1a,eq1b,eq2a,eq2b,eq4,eq5,eq6 :", MatrixForm[ {eq1a,eq1b,eq2a,eq2b,eq4,eq5,eq6 } ] ]]; 

       unknowns1=Flatten[{unknowns,Pe1,P1,Q1}];  (* collect 5 unknowns (two of which depend on test control) for  5  scalar equations eq1a..eq4 *)
       approxTF=(symbolQ[#1]&)/@Flatten[{d\[Epsilon],dT }];  (* a vector like {True,False,False,True} showing symbols among the increments {d\[Epsilon],dT} // Flatten *)
       approx=Pick[Flatten[{d\[Epsilon]Trial,dTTrial }],approxTF]; (* trial solutions corresponding to symbolic values of increments  *)
      
       approxi=Flatten[{approx, Pe,PTrial,QTrial}];   (* take the elastic predictor as the first guess *)
       unknowns1approxi=Transpose[{unknowns1,approxi}]; (* unknowns paired with their approximations *)
       (*-------------------------------------------------------------------------------*)
       viscoplasticEquations = DeleteCases[{ eq1a,eq1b,eq2a,eq2b,eq4,eq5,eq6 } , True ] ; 
       solution1=FindRoot[viscoplasticEquations ,unknowns1approxi,AccuracyGoal->5]; (* Essential solution of the incremental problem via RMI *)
      {dT2,d\[Epsilon]2,Pe,P,Q}={dT,d\[Epsilon],Pe1,P1,Q1}/.solution1;  (* after solution  assign values to 6 unknowns *)
       (*-------------------------------------------------------------------------------*)
       
       {\[Epsilon]P,\[Epsilon]Q}+=d\[Epsilon]2;         (* update strain *)
       epor = (1.+epor) Exp[ -d\[Epsilon]2[[1]] Sqrt[3] ] - 1.0;
       AppendTo[state, {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor }];  (* write down the updated state *)
    itime++;];
  state
 ];


 PQPlot[Paths_, Mat_] := Module[{nPaths ,PQs, gPQs, gfirstSurface  , glastSurface, outputPlot, currentPath , iPath  ,\[Nu],\[Kappa]B,\[Lambda]B,\[Phi]  },
        {\[Nu],\[Kappa]B,\[Lambda]B,\[Phi]}=N[ Mat[[1;;4]] ];
         If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
        PQs = Array[0&, nPaths];
        gfirstSurface =  Array[0&, nPaths];
        glastSurface =  Array[0&, nPaths];
        gPQs =  Array[0&, nPaths];
        If[nPaths==1,
           PQs[[1]] = {#[[1]], #[[2]] }  &  /@  Paths;
           gPQs[[1]] = ListPlot[PQs[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},AxesOrigin->{0,0}, DisplayFunction->Identity];
           gfirstSurface[[1]] = plotVHPsurface[Paths[[1,3]],\[Phi]];
           glastSurface[[1]] =  plotVHPsurface[Paths[[-1,3]],\[Phi]];
           outputPlot  = {  gfirstSurface[[1]], glastSurface[[1]],  gPQs[[1]],
                           Graphics[{ Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (2Sqrt[2] Sin[ \[Phi] ])/(3-Sin[ \[Phi] ])}}],
                           Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (-2 Sqrt[2] Sin[\[Phi] ])/(3+Sin[\[Phi] ])}}] } ]  } ;
         ] ;
        If[nPaths > 1,
        For[iPath=1, iPath<=nPaths,
          currentPath =   Paths[[iPath]] ;
          PQs[[iPath]] = {#[[1]], #[[2]] }  &  /@  currentPath;
          gPQs[[iPath]] = ListPlot[PQs[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
          gfirstSurface[[iPath]] = plotVHPsurface[currentPath[[1,3]],\[Phi]];
           glastSurface[[iPath]] =  plotVHPsurface[currentPath[[-1,3]],\[Phi]];
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

 


   inputCheck[Mat_, Icond_, Loading_]:= Module[ {P, Q, \[Nu],\[Kappa]B, \[Lambda]B, \[Phi], Iv, Pe, PePlus,ee0,Pe0,evisPref, OCR,
                                               \[Epsilon]P, \[Epsilon]Q, epor, ntime, deP, deQ, dT, dP, dQ, dirEps, dirT,  dt,MC,ME,M },
{\[Nu],\[Kappa]B,\[Lambda]B,\[Phi],Iv,ee0,Pe0,evisPref}   =Mat;
 {P,Q,Pe,\[Epsilon]P,\[Epsilon]Q,epor }= Icond;
 { ntime, {deP,deQ}, {dP,dQ}, dt , {dirEps, dirT} } = N[ Loading ] ;

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
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ, dirEps, dirT} ], True] > 4,  Print["Error: Too many unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ , dirEps, dirT} ], True]  < 4,  Print["Error: Too few unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
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
 GraphicsGrid[{{ Show[{PQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ],
  Show[{epsQQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[{ path[[1, 5]] , path[[1, 2]] } ],PointSize[Medium], Orange, Point[{ path[[-1, 5]] , path[[-1, 2]] } ] } ] } ]},
  {Show[{oedometricPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 6]] }],PointSize[Medium], Orange, Point[{path[[-1, 1]], path[[-1, 6]] } ] }] } ],
   Show[{epsPepsQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 4]], path[[1, 5]]} ],PointSize[Medium], Orange, Point[ { path[[-1, 4]], path[[-1, 5]]} ]} ] } ]  }},
 ImageSize -> Full]  ,
 {{oIso, True, "Model:  " },  {True ->  "Eiso+mbMCC ", False -> "LHP+mbHP" }, ControlType->RadioButton  },  
 Delimiter, 
 "\n Loading step consists of:",
 {{ntime, xntime}},
 "increments consisting of",
 {{d\[Epsilon]Pp, xdeP}},
 {{d\[Epsilon]Qp, xdeQ}},
 {{d\[Epsilon]Qd\[Epsilon]P, xdeQdeP}}  , 
 {{dP, xdP}}  ,
 {{dQ, xdQ}}  ,
 {{dQdP, xdQdP}}  ,
 {{dt, xdt}}  ,
 " (in [%],[],[kPa], [-] or [h]).",
 Delimiter,
 Button["Check the input",
   loading = { ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}],   N[dt],  N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }] };
   d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
   d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
  inputCheck[Mat, ICond0, loading ];
 ],
   Button["Calculate step",
  d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
  d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
  loading = { ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}],   N[dt],  N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }] };
  dpath =  RunVHPTest[Mat,Last[path], loading, isotropic -> oIso ]  ;
  path = Join[ path,  Delete[ dpath, 1] ] ;
  
 If[Count[Evaluate[symbolQ[#] & /@ {d\[Epsilon]P, d\[Epsilon]Q, dP, dQ, d\[Epsilon]Qd\[Epsilon]P, dQdP}], True] != 4,
  Print[Style[ "Error: system of equations cannot be solved. Exactly  4 unknowns are needed.", Red, 20] ]; Abort[]; Beep[];
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
  Button["10 strain cycles (default  +/-10*deQ=0.1% dt=1 ) ",
  Clear[xdP, xdQ ] ;  ncyc=10; 
  If[  symbolQ[ ntime ],  ntime= 10 ] ; 
  If[  symbolQ[dt] , dt = 1 ] ;   
  d\[Epsilon]P=  If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100,   0  ]  ;
  d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] ,  d\[Epsilon]Qp /100, 0.001 ]  ;  
  Loading0 = { ntime/2 , N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}],   N[dt],  N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }] };
  Loading1 = { ntime, N[  -{d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}],   N[dt],  N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }] };
  Loading2 = { ntime, N[   {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}],   N[dt],  N[{d\[Epsilon]Qd\[Epsilon]P, dQdP }] }; 
  dpath =  RunVHPTest[Mat,Last[path], Loading0, isotropic -> oIso ]  ;   path = Join[ path,  Delete[ dpath, 1] ] ;
  Do[ 
  dpath =  RunVHPTest[Mat, Last[path], Loading1 ,isotropic -> oIso];  path = Join[ path,  Delete[ dpath, 1] ] ;
  dpath =  RunVHPTest[Mat, Last[path], Loading2, isotropic -> oIso];  path = Join[ path,  Delete[ dpath, 1] ] ;  
  ,{icyc, 1,ncyc }
  ];
  ]
 ]
];



ExampleIsochoricShearing[] := Module[{xdP,xdQ,Mat1, ICond1, Path1, Loading1, Path2, ICond2, Path3, Loading2, Path4 , oIso = True },
Print[" Undrained : active/passive  shearing ;  normally consolidated/overconsolidated.\
\n Note that the initial  preconsolidation Pe=101 may slightly change!  \
\n Enter ??ExampleIsochoricShearing  to get read  IC, mat. constants and loading steps."];
Mat1= {0.2, 0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001} ;
ICond1={100,0,120,0,0, 1.945799133685738 } ;
Loading1={100,{0,0.001`},{xdP,xdQ},1, {xdeQdeP,  xdQdP }};
ICond2={10,0,120,0,0, 1.945799133685738 } ;
Loading2={100,{0,-0.001`},{xdP,xdQ},1, {xdeQdeP,  xdQdP }};
 Path1=RunVHPTest[Mat1,ICond1,Loading1,  isotropic -> oIso];
 Path2=RunVHPTest[Mat1,ICond2,Loading1, isotropic -> oIso];
 Path3=RunVHPTest[Mat1,ICond1,Loading2, isotropic -> oIso];
 Path4=RunVHPTest[Mat1,ICond2,Loading2, isotropic -> oIso];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1]  , epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];

ExampleOedometricTest[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3, oIso = True},
Print[" Oedometric loading , short unloading, reloading \
\n Enter ??ExampleOedometricTest  to get read  IC, mat. constants and loading steps."];
Mat1= {0.2,0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001} ;
ICond1={65,0,120,0,0, 1.945799133685738 } ;
Loading={90,{0.001/ Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2},1,  {xdeQdeP,  xdQdP }};
ULoading={10,{-0.001/ Sqrt[3],-0.001 Sqrt[2./3.]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
RLoading={60,{0.001/Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
Path1=RunVHPTest[Mat1,ICond1,Loading, isotropic -> oIso];
Path2=RunVHPTest[Mat1,Path1[[-1]],ULoading, isotropic -> oIso];
Path3=RunVHPTest[Mat1,Path2[[-1]],RLoading, isotropic -> oIso];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1], epsQQPlot[{Path1,Path2,Path3}, Mat1]  }, ImageSize-> Large]
];

ExampleOedometricTest2[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3,oIso = True},
Print[" Oedometric loading , long unloading, reloading \
\n Enter ??ExampleOedometricTest2  to get read  IC, mat. constants and loading steps."];
Mat1= {0.2, 0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001} ;
ICond1={65,0,120,0,0, 1.945799133685738 } ;
 Loading={80,{0.001` / Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
 ULoading={45,{-0.001` /Sqrt[3.`],-0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
 RLoading={70,{0.001`/Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2},1, {xdeQdeP,  xdQdP }};
  Path1=RunVHPTest[Mat1,ICond1,Loading, isotropic -> oIso];
 Path2=RunVHPTest[Mat1,Path1[[-1]],ULoading, isotropic -> oIso];
 Path3=RunVHPTest[Mat1,Path2[[-1]],RLoading, isotropic -> oIso];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1],  epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];

ExampleIsobaricShearing[] := Module[{xdT1,xdT2,Mat1,ICond1,ICond2,Loading,ULoading,RLoading,Path1,Path2,Path3,oIso = True},
Print[" Isobaric shearing,  slightly/strongly  overconsolidated starting from Pe=100 \
\n Enter ??ExampleIsobaricShearing  to get read  IC, mat. constants and loading steps."];
Mat1= {0.2, 0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001};
 ICond1={100,0,120,0,0, 1.945799133685738 } ;
 ICond2={20,0,120,0,0, 1.945799133685738 } ;
 Loading={50,{xdeP,0.0025`},{0,xdQ},1, {xdeQdeP,  xdQdP }};
 Path1= RunVHPTest[Mat1,ICond1,Loading, isotropic -> oIso];
 Path2 = RunVHPTest[Mat1,ICond2,Loading, isotropic -> oIso];
 GraphicsRow[{oedometricPlot[{Path1,Path2}, Mat1], PQPlot[{Path1,Path2}, Mat1],  epsQQPlot[{Path1,Path2}, Mat1]   }, ImageSize-> Large]
];


EndPackage[ ]
$Context = "PQ`VHP`"   ;
Print[ "The Visco-Hypoplastic (VHP) Model for isomorphic P-Q space; by A. Niemunis 2013-2022.07 \
\n    all routines and variables are public (up to 2022) \
\n    You are in the context PQ`VHP` which provides functions: \
\n    RunVHPTest, PQPlot, oedometricPlot, epsQQPlot, controlVHP \
\n    and some ready to use examples:  \
\n    ExampleOedometricTest[], ExampleIsochoricShearing[], ExampleOedometricTest2[], ExampleIsobaricShearing[]\
\n  *********************************************************************************************************** \
\n    ??ExampleOedometricTest  gives you the source code of the example procedure   \
\n    ?RunVHPTest gives user-oriented  information to the procedure and to the input variables \
\n  *********************************************************************************************************** \
\n    For detailed information on the programming see the notebook VHP-impli.nb or read the PDF documentation.\
\n    2021-2022:  two directions = { depsQ/depsP, dQ/dP } may be prescibed apart from  increments of stress/strain components  (enables modeling of oedometric creep) \ 	 
"];
(*------------- Initialization-----------(to be overridden by tru values in notebook, here they prevent uncontrolled animation)---*)
Mat = {0.2, 0.01, 0.1, Pi/6 , 0.1, 2.0, 100.0,  0.001} ;   (*  {kappaB,lambdaB,Phi,Iv,ee0,Pe0,evisPref} *)
ICond0 = {100, 0, 120, 0, 0,   1.945799133685738 } ;   (* {P,Q,Pe,epsP,epsQ,epor=(1+ ee0)*(120/Pe0)^-lambdaB -1} *)
path = {ICond0};
loading = {1,{0,0},{xdP,xdQ},0,{xdeQdeP,  xdQdP }};
