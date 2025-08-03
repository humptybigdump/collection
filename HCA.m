(* ::Package:: *)

(*    Mathematica  tools for element tests with  HCA in isomorphic PQ space  by A.Niemunis 2014

This  notebook produces stress/strain path according to the high cycle accumulation model (HCA) 
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (=  HCA.m) to the new directory.
4) Begin a new Mathematica session with:    Needs["PQ`HCA`"]
*)


BeginPackage["PQ`HCA`"]
Off[General::spell,General::spell1,Solve::ifun];
(*---------------------------------------------usages--------------------------------------------------------------------*)
 RunHCATest::usage = "RunHCATest[ Mat,ICond, Loading ] \
 \n  RunVHPTest returns the  sequence of states {P, Q, \[Epsilon]P, \[Epsilon]Q, epor, gA, gB}
 \n  which are incrementally calculated according to HCA model within the module \
 \n  using the material parameters Mat =  {\[Kappa]B,\[Nu],\[Phi],nampl,\[Epsilon]amplRef,Cp,Patm,CY,CN1,CN2,CN3,Ce,eRef}  \
 \n  the initial conditions Icond = {P,Q,\[Epsilon]P,\[Epsilon]Q,epor,gA,gB} \
 \n  and the loading programme  {ntime,deps,dT, dN, \[Epsilon]ampl}.
 \n  The loading programme  consists of : \
 \n  ntime = the number of repetitions,  \
 \n  deps = {depsP, depsQ} = strain increment, \
 \n  dT = {dP,dQ} = stress increment, \
 \n  dN = number of cycles per increment. \
 \n  \[Epsilon]ampl = the amplitde of strain cycles  \
 \n  The values for ntime and dN  must be provided by the user (usually via contolPanelHCA[...]).
 \n  Moreover two of the four increments: depsP, depsQ, dP, dQ  must be prescribed (given as numbers) \
 \n  and the remaining two must be calculated and thus they should be specified as symbols.   \
 \n  " ;

getElasticIsomorph::usage = "getElasticIsomorph[{P,Q},extendedMat]  returns the isotropic hypoelastic stiffness for the isomorphic P-Q-space.    \
\n   The stiffness is barotropic i.e. K = p/\[Kappa]B .  \
";
PQPlot::usage = "PQPlot[path , Mat ] or  PQPlot[{path1,path2,.. }, Mat ]  plots the stress path in the isomorphic P-Q-space.  ";
epsQQPlot::usage = "epsQQ[path , Mat ] or  epsQQ[{path1,path2,.. }, Mat ]  produces an isomorphic epsQ-Q diagramm.   ";
oedometricPlot::usage = "oedometricPlot[path, Mat] or  oedometricPlot[{path1,path2,... } , Mat] plots the compression curve  in   P-e-space.   \
\n The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled  \
  ";

controlPanelHCA::usage = "controlHCA[ ] loads a graphic interface to perform element tests using HCA.   \
\n The initial conditions must be saved in the first row of the global variable path. \
\n Two stress or strain or mixed increments must be prescribed. Push the button Calculate to see the results after the increment.  \
\n The number of increments is ntime and should be modified writing a new number in the cell ntime. \
\n The number of cycles per increment should be given as dN. The scalar amplitude should be given as \[Epsilon]ampl. \
\n If you want to start with a brand new simulation, click on Reset or  set path = {ICond0} in a separate Mma Cell.  \
\n The button Undo removes the last ntime increments from the path.   \
\n New users are strongly advised to use the button  Check  before the first launch of Calculate.   \
  ";
(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];
inRange[x_, range_] := IntervalMemberQ[Interval[range], x];

getElasticIsomorph[ Tb_, extendedMat_] := Module[{P,Q,\[Kappa]B,\[Nu],K,G,EE},
 {P,Q} = Tb; 
 {\[Kappa]B,\[Nu]}  = extendedMat[[1;;2]]; 
 K = P/(Sqrt[3] \[Kappa]B); G = 3 K (1 - 2 \[Nu] )/ (2 + 2 \[Nu]);
 EE = {{3 K, 0},{0, 2G }}   ; 
 EE 
];

getCreepIncrement[ state_, extendedMat_, Loading_ ] := Module[
{\[Kappa]B,MC,ME,M,PePlus,mb,nampl,\[Epsilon]amplRef,fampl,Cp,Patm,CY,CN1,CN2,CN3,Ce,eRef,epor,P,
Q,gA,gB,gA1,gB1,\[Epsilon]P,\[Epsilon]Q,\[Epsilon]ampl,Y,fp,fY,fe, dN, Ne,dfN, d\[Epsilon]acc,\[Phi],\[Psi] },
{\[Kappa]B,\[Nu],\[Phi],nampl,\[Epsilon]amplRef,Cp,Patm,CY,CN1,CN2,CN3,Ce,eRef,\[Psi], MC,ME}  = N @ extendedMat; 
{P,Q,\[Epsilon]P,\[Epsilon]Q,epor,gA,gB } =  state; 
{dN , \[Epsilon]ampl}= Loading[[4;;5]]; 
  M = If[Q >=  0, MC,  ME] ;  
 If[P>0,
         Y= Min[ Abs[ Q]/(M P) , 1];
                 PePlus = P  +  Q^2 /(P M^2 ); 
        mb = Normalize[ {2P - PePlus, 2Q / M^2 }   ]  ,
         Y= 1;    mb = { -1,0}    ;
];   
fampl = Min[  (\[Epsilon]ampl / \[Epsilon]amplRef)^nampl, 100] ; 
  fp = Exp[- Cp*( P/ Sqrt[3] / Patm  - 1) ];
  fY = Exp[ CY*Y ]; 
  fe = (Ce - epor)^2 / (1+epor) * (1+eRef)/(Ce  - eRef)^2;   
 If[ fampl < 10^-5, 
               gA1 =  gA,  
               gA1 =CN1 fampl Log[ Exp[ gA / (CN1 fampl)] + CN2* dN ]; 
] ;(*evolution of gA*)  
  gB1 = gB + dN*CN1*CN3*fampl; 
  d\[Epsilon]acc = mb * (gA1+gB1-gA-gB) * fp*fY*fe ;  (* no fPi  Delta g instead of dfN fampl *)
  {d\[Epsilon]acc, gA1,gB1}  
];



RunHCATest[Mat_,ICond_,Loading_]:=Module[  {   ntime, d\[Epsilon], dT, dN, \[Kappa]B,\[Phi], MC,ME, extendedMat,\[Epsilon]ampl,gA,gB,gA1,gB1, 
       P,Q,\[Epsilon]P,\[Epsilon]Q,epor, state, itime, elastStiff, unknowns, solution, d\[Epsilon]Trial, dTTrial, PTrial,QTrial,  d\[Epsilon]acc, P1, Q1,  
        unknowns1, approxTF, approx, approxi, unknowns1approxi, solution1,dT2,d\[Epsilon]2,eq1a,eq1b,eq2a,eq2b,eq4,
        nsub1,nsub2,nsub3,nsub,isub,subLoading, elPredictor, aux1,aux2 },
  
    
   \[Phi] = N[ Mat[[3]] ];    MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));  (*isomorphic M*)
    extendedMat =  Join[ Mat ,{MC,ME}] ;
   {P,Q,\[Epsilon]P,\[Epsilon]Q,epor,gA,gB} =N[ ICond ];  
   state= {{P,Q,\[Epsilon]P,\[Epsilon]Q,epor,gA,gB }};  (* state will be a sequence of states  to be  joined with the global variable path *)  
  ntime = Loading[[1]];
  For[itime=2,itime<=ntime+1,
     { d\[Epsilon], dT, dN, \[Epsilon]ampl }   =    N[#]& /@ Loading[[2;;5]];      
     {d\[Epsilon]acc, aux1, aux2} =  getCreepIncrement[ state[[ -1]], extendedMat, Loading ];      (* to check if subincrementing is necessary *)
     nsub1 =  Ceiling[d\[Epsilon]acc/10^-3]; 
     nsub2 =  Ceiling@ ( Max[ Abs[ Select[ d\[Epsilon] , NumericQ]]~Join~ {0}   ]/10^-3 );
     nsub3 =  Ceiling@ ( Max[ Abs[ Select[ dT , NumericQ]]~Join~ {0}   ] / (1+ P/30)); 
     nsub = Max[nsub1,nsub2,nsub3];  
     If[ itime==2 && nsub > 1 , Print["nsub=", nsub, "  automatically generated subincrements " ] ]; 
     subLoading = Partition[(If[NumericQ[#], #/nsub, # ]& /@  Flatten[{d\[Epsilon],dT}] ),2] ~Join~ {dN/nsub,\[Epsilon]ampl};  PrependTo[subLoading,ntime]; 
  For[isub=1,isub<=nsub,  (* automatic subincrementing  *)   (* IMPLICIT epor IS SILL MISSING MISSING *) 
      {P,Q,\[Epsilon]P,\[Epsilon]Q,epor,gA,gB } = state[[-1]];  
       elPredictor = getImplicitPredictor[ extendedMat, state[[-1]], subLoading] ; 
      {dT2,d\[Epsilon]2,P,Q,gA1,gB1} =  elasticImplicitSolution[ extendedMat, state[[-1]], subLoading, elPredictor];   
      {\[Epsilon]P,\[Epsilon]Q}+=d\[Epsilon]2;         (* update strain *)  
       epor = (1.+epor) Exp[ -d\[Epsilon]2[[1]] Sqrt[3]   ] - 1.0; 
      newState = {P,Q,\[Epsilon]P,\[Epsilon]Q,epor,gA1,gB1 };  
      If[ P  <= 0 , Print["error: P=",P," cannot be negative"]; Abort[] ] ;
       If[ !( Q ~inRange~  {-ME *P, MC *P } ) && ( Length@ Select[dT, NumericQ] )  ==2 ,
           Print["error: stress-controlled ",{P,Q} ," outside the yield surface "]; Abort[]  ];        
       If[ !( Q ~inRange~  {-ME *P, MC *P } ) && ( Length@ Select[dT, NumericQ] )  < 2 ,
        newState =  returnMapping[ extendedMat, newState, subLoading ];   
       ] ;
      (* epor = (1.+epor) Exp[ -d\[Epsilon]2[[1]] Sqrt[3]   ] - 1.0;     must consider correction of this in  returnMapping *)
       
       AppendTo[state, newState ];  (* write down the updated state *)
    isub++; ];  
    itime++;]; 
 state
 ];

getImplicitPredictor[ extendedMat_, fromState_, subLoading_ ]  := Module[{d\[Epsilon],dT,P,Q,unknowns,solution,d\[Epsilon]Trial,dTTrial,PTrial,QTrial}, 
       {d\[Epsilon], dT} = subLoading[[2;;3]];
       {P,Q} = fromState[[1;;2]]; 
       elastStiff=getElasticIsomorph[{P,Q}+dT, extendedMat];
       unknowns=Select[Flatten[ {d\[Epsilon],dT} ],symbolQ];  (* unknown increments depend on the choice of the test control *)
       solution=Solve[dT==elastStiff.d\[Epsilon],unknowns];  
       {d\[Epsilon]Trial,dTTrial}= {d\[Epsilon],dT} /.solution[[1]];      (* elastic trial solution (explicit predictor) *)
       { PTrial,QTrial} = {P,Q} + dTTrial;   
       {d\[Epsilon]Trial,dTTrial,{PTrial,QTrial}}  
 ]

 elasticImplicitSolution[ extendedMat_, fromState_, subLoading_, elPredictor_ ] := Module[
       {P,Q,P1,Q1,d\[Epsilon],dT, d\[Epsilon]Trial,dTTrial,PTrial,QTrial, d\[Epsilon]2,dT2, eq1a,eq1b,eq2a,eq2b,eq4, approx, approxTF,
        unknowns, unknowns1,approxi, unknowns1approxi, solution1,  elastStiff,  d\[Epsilon]acc,gA1,gB1  }, 
       {P,Q} =  fromState[[1;;2]] ;  
       elastStiff = getElasticIsomorph[{P1, Q1}, extendedMat];    (* with algebraic  P1, Q1 *)
       { d\[Epsilon], dT  } = subLoading[[2;;3]];
       {d\[Epsilon]Trial,dTTrial,{PTrial,QTrial}} = elPredictor; 
        unknowns=Select[Flatten[ {d\[Epsilon],dT} ],symbolQ]; 
       {d\[Epsilon]acc, gA1, gB1} =  getCreepIncrement[ fromState, extendedMat, subLoading];   (* no implicit  accumulation as yet *)
       eq1a=dT[[1]]==(  elastStiff.(d\[Epsilon]- d\[Epsilon]acc)   )[[1]];  (* increment of P *)
       eq1b=dT[[2]]==(  elastStiff. (d\[Epsilon]- d\[Epsilon]acc)  )[[2]];  (* increment of Q *)
       eq2a=P1==P+dT[[1]];            (* updated P *)
       eq2b=Q1==Q+dT[[2]];            (* updated Q *)      
       unknowns1=Flatten[{unknowns,P1,Q1}];  (* collect 5 unknowns (two of which depend on test control) for  5  scalar equations eq1a..eq4 *)
       approxTF=(symbolQ[#1]&)/@Flatten[{d\[Epsilon],dT}];  (* a vector like {True,False,False,True} showing symbols among the increments {d\[Epsilon],dT} // Flatten *)
       approx=Pick[Flatten[{d\[Epsilon]Trial,dTTrial}],approxTF]; (* trial solutions corresponding to symbolic values of increments  *)
       approxi=Flatten[{approx,PTrial,QTrial}];   (* take the elastic predictor as the first guess *)
       unknowns1approxi=Transpose[{unknowns1,approxi}]; (* unknowns paired with their approximations *)
       (*-------------------------------------------------------------------------------*)
      solution1=  Quiet@ FindRoot[{eq1a,eq1b,eq2a,eq2b},unknowns1approxi,AccuracyGoal->4];      (* Essential solution of the invremental problem via RMI *)
       (*-------------------------------------------------------------------------------*)    
        {dT2,d\[Epsilon]2,P,Q}={dT,d\[Epsilon],P1,Q1}/.solution1;   (* after solution  assign values to 6 unknowns *) 
       {dT2,d\[Epsilon]2,P,Q,gA1,gB1}    
]  

 returnMapping[ extendedMat_, outsideState_, subLoading_ ] := Module[
        {correctedState ,eP,eQ, P,Q,MC,ME,mb,Qred, M,elastStiff,increment,eq1,eq2,eq3,eq4,eq5,solution,
         unknowns, corrections, ceP,ceQ,cP,cQ, rP,rQ, reP,reQ,\[Psi] ,MME,MMC,epor  },
        MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]])); 

         correctedState =  outsideState; 
        {eP,eQ} =  outsideState[[3;;4]] ;  
        {P,Q} =  outsideState[[1;;2]] ;  
         epor =  outsideState[[5]]; 
        {\[Psi],MC,ME} = extendedMat[[ 14;;16 ]] //N ; 
         MMC=(Sqrt[2] (6 Sin[\[Psi]]))/(3 (3-Sin[\[Psi]]));    MME=(Sqrt[2] (6  Sin[\[Psi]]))/(3 (3+Sin[\[Psi]])); (* dilatancy on the yield surface only *)
        If[ Q > 0,  mb = {-MMC,1}; Qred=MC*P; M=MC ,   mb = {-MME,-1}; Qred=-ME*P, M=-ME ];   
        elastStiff = getElasticIsomorph[{P, Qred}, extendedMat];     
        increment  = Flatten@ subLoading[[2;;3]]; 
        Clear[ceP,ceQ,cP,cQ,L ] ; corrections = ( If[NumericQ[#],0,1] & /@ increment )*{ceP,ceQ,cP,cQ }; 
        {ceP,ceQ,cP,cQ } = corrections;
        unknowns = Select[ corrections, symbolQ ];   AppendTo[unknowns,L];  
        {reP,reQ,rP,rQ}  = {eP,eQ,P,Q} - corrections  ;   (* state after correction *)
         eq1 = rQ == M*rP; 
         eq2 =  cP == (elastStiff . {ceP + L * mb[[1]] ,ceQ + L * mb[[2]] } )[[1]];
         eq3 =  cQ == (elastStiff . {ceP + L * mb[[1]] ,ceQ + L * mb[[2]] } )[[2]];
         solution = Solve[{eq1,eq2,eq3 }, unknowns ][[1]]; 
         correctedState[[ 1;;4 ]] = ({ rP, rQ , reP,reQ } /. solution) ;  
         correctedState[[ 5 ]] = (1.+ epor) Exp[  -(-ceP /. solution) Sqrt[3]   ] - 1.0;  
         correctedState     
]  

 PQPlot[Paths_, Mat_] := Module[{nPaths ,PQs, gPQs,  outputPlot, currentPath , iPath, \[Phi], MC, ME , maxP  },
        \[Phi] = N[ Mat[[3]] ];    MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));      
        If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];   (* moze kilka sciezek do porownan, tutaj jedna  *)
        PQs = Array[0&, nPaths];
        gPQs =  Array[0&, nPaths];
        If[nPaths==1,
           PQs[[1]] = {#[[1]], #[[2]] }  &  /@  Paths; 
           maxP =      Max[ Paths[[All,1]] ];     
           gPQs[[1]] = ListPlot[PQs[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All, AxesLabel->{P,Q}, AxesOrigin->{0,0}, DisplayFunction->Identity];
           outputPlot  = { gPQs[[1]],  Graphics[     Line[{ {1, MC}* maxP, {0,0}, {1,-ME}*maxP}  ]     ]  } ;
         ] ;
        If[nPaths > 1,
        For[iPath=1, iPath<=nPaths,
          currentPath =   Paths[[iPath]] ;
          PQs[[iPath]] = {#[[1]], #[[2]] }  &  /@  currentPath; 
          gPQs[[iPath]] = ListPlot[PQs[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},
                                   PlotRange->All,AxesLabel->{P,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
        iPath++;] ;
        outputPlot  =  gPQs ;
         ];
        Show[outputPlot, DisplayFunction->$DisplayFunction, PlotRange->All, AxesLabel->{"P","Q"}, AxesOrigin->{0,0} ]
 ];

 oedometricPlot[Paths_, Mat_] := Module[{nPaths=1,Pes, gPes,  outputPlot, currentPath , iPath  },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          Pes = Array[0&, nPaths];
          gPes =  Array[0&, nPaths];
          If[nPaths==1,
             Pes[[1]] = {#[[1]], #[[5]] }  &  /@  Paths;
             gPes[[1]] = ListPlot[Pes[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,e},DisplayFunction->Identity];
             outputPlot  = { gPes[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            Pes[[iPath]] = {#[[1]], #[[5]] }  &  /@  currentPath;
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
             epsQQ[[1]] = {#[[4]], #[[2]] }  &  /@  Paths;
             gepsQQ[[1]] = ListPlot[epsQQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,
             AxesLabel->{\[Epsilon]Q,Q},AxesOrigin->{0,0},DisplayFunction->Identity];
             outputPlot  = { gepsQQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsQQ[[iPath]] = {#[[4]], #[[2]] }  &  /@  currentPath;
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
             epsPepsQ[[1]] = {#[[3]], #[[4]] }  &  /@  Paths;
             gepsPepsQ[[1]] = ListPlot[epsPepsQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]P,\[Epsilon]Q},AxesOrigin->{0,0},DisplayFunction->Identity];
             outputPlot  = { gepsPepsQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsPepsQ[[iPath]] = {#[[3]], #[[4]] }  &  /@  currentPath;
            gepsPepsQ[[iPath]] = ListPlot[epsPepsQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]P,\[Epsilon]Q},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsPepsQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];
 inputCheck[Mat_, Icond_, Loading_ ]:= Module[ {P, Q, \[Kappa]B, \[Phi],\[Psi], nampl,\[Epsilon]amplRef,Cp,Patm,CY,CN1,CN2,CN3,Ce,eRef, d\[Epsilon], deP, deQ, dT, dP, dQ,dN,MC,ME,M ,ntime, \[Epsilon]ampl, \[Epsilon]P,\[Epsilon]Q,epor,gA,gB },
  {\[Kappa]B,\[Nu],\[Phi],nampl,\[Epsilon]amplRef,Cp,Patm,CY,CN1,CN2,CN3,Ce,eRef,\[Psi]}    = Mat ;
 {P,Q,\[Epsilon]P,\[Epsilon]Q,epor,gA,gB }= Icond;
  ntime = Loading[[1]]; { d\[Epsilon], dT, dN, \[Epsilon]ampl }   =    N[ # ]& /@ Loading[[2;;5]];
   {dP,dQ} = dT; {deP, deQ} = d\[Epsilon]; 
  If[symbolQ[ntime] , Print[ "number of increments  ntime =  ", ntime," undefined"   ]]  ;
  If[symbolQ[ Loading[[4]] ] , Print[ "time increment  dN =  ", Loading[[4]]  ," undefined"   ]]  ;
  If[ dN < 0.01  , Print[ "number of cycles per increment dN =  ", dN ," is very small "   ]]  ;
  If[ ntime < 1 , Print[ "suspiciously small number of increments  ntime =  ", ntime   ]]  ;
  If[ deP > 0.005 || deQ > 0.005 , Print[ "Strain increments seem to be large {deP, deQ} = ", {deP, deQ}  , " Keep them at  0.1% level"   ]]  ;
  If[ dP > 10 || dQ > 10 , Print[ "Stress increments seem to be large {dP, dQ} = ", {dP, dQ}  , " Keep them below  10 kPa "   ]]  ;
  If[ \[Kappa]B ~inRange~ {0, 0.2} , "", Print[ "Suspicious \[Kappa]B =  ",\[Kappa]B  ]]  ;
  If[ \[Phi] ~inRange~ {5\[Degree], 50\[Degree]} , "", Print[ "Suspicious \[Phi]=  ", \[Phi], "rad"  ]]  ; 
  If[ \[Psi] ~inRange~ {-30\[Degree], 40\[Degree]} , "", Print[ "Suspicious \[Psi]=  ", \[Psi], "rad"  ]]  ; 
  If[ Loading[[5]]  ~inRange~ {0, 0.001} , "", Print[ "Suspicious  \[Epsilon]ampl=  ", Loading[[5]]  ]]  ;
  If[P < 0 , Print[ "Warning: negative initial  P =  ", P   ]]      ;
  MC=(Sqrt[2] (6 Sin[\[Phi]]))/(3 (3-Sin[\[Phi]]));
  ME=(Sqrt[2] (6  Sin[\[Phi]]))/(3 (3+Sin[\[Phi]]));
  If[ (Q/P) ~inRange~ {-ME, MC}   , "" , Print[ "  initial obliquity  Q/P =  ", Q/P , " is very large"    ]]      ;
  If[epor  ~inRange~ {0.01, 3} , "", Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 3} "  ]];
  If[Count[ Evaluate[ symbolQ[#] & /@ (Flatten @ Loading) ], True] > 2,  Print["Error: Too many unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Count[ Evaluate[ symbolQ[#] & /@ (Flatten @ Loading) ], True]  < 2,  Print["Error: Too few unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, True, False, True},  Print["Error: You cannot prescribe deP and dP in Loading simultaneously"]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == {True, False, True,False},  Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously"]; Beep[]; Abort[]; ];
  Print[" No further issues "];  
];

 controlPanelHCA[] := Module[{  },
  If[Length[path] == 0, path = Array[0 &, {1, 6}]; path = {ICond0};]; (* Initialisation of global variable Path *)
  Manipulate[
 GraphicsRow[
{ Show[{PQPlot[path, Mat],         Graphics[ { PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ],
  Show[{epsQQPlot[path, Mat],      Graphics[ {PointSize[Large], Red, Point[{path[[1, 4]], path[[1, 2]] }],PointSize[Medium], Orange, Point[{path[[-1, 4]], path[[-1, 2]]}  ]} ] } ],
  Show[{oedometricPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 5]] }],PointSize[Medium], Orange, Point[{path[[-1, 1]], path[[-1, 5]]}  ]} ] } ],
  Show[{epsPepsQPlot[path, Mat],   Graphics[ {PointSize[Large], Red, Point[{path[[1, 3]], path[[1, 4]]} ],PointSize[Medium], Orange, Point[{path[[-1, 3]], path[[-1, 4]]}  ]} ] } ] },
 ImageSize -> Full]  ,
 "Loading step consists of:",
 {{ntime, xntime}},
 "increments consisting of",
 {{d\[Epsilon]Pp, xdeP}},
 {{d\[Epsilon]Qp, xdeQ}}  ,
 {{dP, xdP}}  ,
 {{dQ, xdQ}}  ,
 {{dN, xdN}}  ,
 {{\[Epsilon]amplp,x\[Epsilon]ampl}},
 " (in [%],[kPa] or [-] for dN).",
 Delimiter,
 Button["Check the input",
   d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
   d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
  \[Epsilon]ampl = If[NumberQ[\[Epsilon]amplp ] , \[Epsilon]amplp /100.0, Print[ "\[Epsilon]ampl =" , \[Epsilon]ampl , "but it must be a number "  ]; Abort[] ]  ;
  inputCheck[Mat, ICond0, {ntime, {d\[Epsilon]P, d\[Epsilon]Q}, {dP, dQ}, dN, \[Epsilon]ampl} ];
 ],
   Button["Calculate step",
  d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
  d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
  \[Epsilon]ampl = If[NumberQ[\[Epsilon]amplp ] , \[Epsilon]amplp /100.0, Print[ "\[Epsilon]ampl =" , \[Epsilon]ampl , "but it must be a number "  ]; Abort[] ]  ;
  Loading = { ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}], N[dN], \[Epsilon]ampl };
  dpath =  RunHCATest[Mat,Last[path],Loading]  ;
  path = Join[ path,  Delete[ dpath, 1] ] ;

 If[Count[Evaluate[symbolQ[#] & /@ {d\[Epsilon]P, d\[Epsilon]Q, dP, dQ}], True] != 2,
  Print[Style[ "Error: system of equations cannot be solved. Exactly two unknowns are needed.", Red, 20] ]; Abort[]; Beep[];
  ];  (* Controls if two unknowns are sought *)
 ],

Button["Undo the last ntime increments",
  If[Length[path] >= 2, path = Drop[path, -ntime];,
   Print[Style["No more increments can be undone.", Blue, 20] ]; ];
 ],

Button["Print the final state",
  Print["Final {P,Q}= ", path[[-1,1 ;; 2]]," ,  {\[Epsilon]P,\[Epsilon]Q}= ", path[[-1,3;; 4]], " , epor= ",path[[-1,5]] ,
      ", {gA,\[Epsilon]gB}= ", path[[-1,6 ;; 7]]
  ];
 ],

Button["Reset all steps, graphics ",
  path = {ICond0}; ntime = xntime; d\[Epsilon]Pp = xdeP ; d\[Epsilon]Qp = xdeQ; dP = xdP; dQ = xdQ; \[Epsilon]amplp = x\[Epsilon]ampl;   FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]];
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
 Path1=RunHCATest[Mat1,ICond1,Loading1];
 Path2=RunHCATest[Mat1,ICond2,Loading1];
 Path3=RunHCATest[Mat1,ICond1,Loading2];
 Path4=RunHCATest[Mat1,ICond2,Loading2];
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
Path1=RunHCATest[Mat1,ICond1,Loading];
Path2=RunHCATest[Mat1,Path1[[-1]],ULoading];
Path3=RunHCATest[Mat1,Path2[[-1]],RLoading];
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
  Path1=RunHCATest[Mat1,ICond1,Loading];
 Path2=RunHCATest[Mat1,Path1[[-1]],ULoading];
 Path3=RunHCATest[Mat1,Path2[[-1]],RLoading];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1],  epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];
ExampleIsobaricShearing[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print[" Isobaric shearing,  slightly/strongly  overconsolidated starting from Pe=100 \
\n Enter ??ExampleIsobaricShearing  to get read  IC, mat. constants and loading steps."];
Mat1= {0.01, 0.1, 30\[Degree] , 0.05, 2.0, 100.0, 0.001};
 ICond1={100,0,120,0,0, 1.945799133685738 } ;
 ICond2={20,0,120,0,0, 1.945799133685738 } ;
 Loading={50,{xdeP,0.0025`},{0,xdQ},1};
 Path1= RunHCATest[Mat1,ICond1,Loading];
 Path2 = RunHCATest[Mat1,ICond2,Loading];
 GraphicsRow[{oedometricPlot[{Path1,Path2}, Mat1], PQPlot[{Path1,Path2}, Mat1],  epsQQPlot[{Path1,Path2}, Mat1]   }, ImageSize-> Large]
];
EndPackage[ ]
$Context = "PQ`HCA`"   ;
Print[ "The high cycle accumulation (HCA) Model for isomorphic P-Q space; by A. Niemunis 2014 \
\n   This implementation is based on the paper Niemunis + Wichtmann + Triantafyllidis 2005 but without fPi. \ 
\n   You are in the context PQ`HCA` which provides functions: \
\n    RunHCATest, PQPlot, oedometricPlot, epsQQPlot, controlHCA \
\n    and some (not ready as yet) examples:  \
\n    ExampleOedometricTest[], ExampleIsochoricShearing[], ExampleOedometricTest2[], ExampleIsobaricShearing[]  (not ready as yet)  \
\n  *********************************************************************************************************** \
\n    ??ExampleOedometricTest  gives you   the source code of the example procedure   (not ready as yet)   \
\n    ?RunHCATest gives user-oriented  information to the procedure and to the input variables \
\n    ??RunHCATest gives   additionally the source code of  RunHCATest (for the programmers) \
\n  *********************************************************************************************************** \
\n    For detailed information on the programming see the notebook HCA-impli.nb or read the (non-existent ) PDF documentation.\
"];
