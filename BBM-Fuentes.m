(* ::Package:: *)

(*    Mathematica  tools for element tests with BBM in isomorphic PQ space  by A.Niemunis (2011)

This  notebook produces stress/strain path according to the Barcelona Basic model.
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (= BBM.m) to the new directory.
4) Begin a Mathematica session with:    Needs["PQ`BBM`"]
*)
BeginPackage["PQ`BBM`"]
Off[General::spell,General::spell1,Solve::ifun];
(*---------------------------------------------usages--------------------------------------------------------------------*)
 RunBbmTest::usage = "RunBbmTest[  Mat,ICond,{ntime,deps,dT}] \
 \n  RunBbmTest returns the sequence of states {P, Q, Pc, \[Epsilon]P, \[Epsilon]Q, epor, suc, P0}
 \n  which are incrementally calculated according to BBM model within the module \
 \n  using the material parameters Mat = {\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,kss, Pat}
 \n  and the loading programme  {ntime,deps,dT, ds}.
 \n  The loading programme  consists of : \
 \n  ntime = the number of repetitions,  \
 \n  deps = {depsP, depsQ} = strain increment, \
 \n  dT = {dP,dQ} = stress increment. \
 \n  ds = suction increment. \
 \n  Two of the four incremental values: depsP, depsQ, dP, dQ  must be prescribed (given as numbers) \
 \n  The suction increment ds must be always prescribed \
 \n  and the remaining two must be calculated and thus they should be specified as symbols.   \
 \n  " ;
plotBBMsurface::usage = "plotBBMsurface[Pc,\[CurlyPhi]]  plots the saturated yield surface  of the BBM-model in the isomorphic P-Q-space.  \
\n The saturated yield surface consists of two half-elipses  with the common diameter P0 along the P-axis and different half-diameters \
\n along the Q-axis. 
";
plotBBMsurfaceSuction::usage = "plotBBMsurfaceSuction[ P0 , Mat,suc]   plots the yield surface of the BBM-model with suction in the isomorphic P-Q-space.   \
\n The yield surface with suction consists of two half-elipses  with the common diameter (Pc+Ps) along the P-axis and different half-diameters \
\n along the Q-axis.  \
";
 NClinePlot::usage = "NClinePlot[P0 , Mat,suc,evoidini]   plots the normal consolidation line for saturated conditions on the e-vs ln(P) space.  \
";

NClinePlotSuction::usage = "NClinePlotSuction[P0 , Mat,suc,evoidini]    plots the normal consolidation line for non-saturated conditions on the e-vs ln(P) space.  \
";

getElasticIsomorph::usage = "getElasticIsomorph[P,\[Nu],\[Kappa], epor]  returns the isotropic hypoelastic stiffness for the isomorphic P-Q-space    \
\n  in the form {{3K,0},{0,2G}} with the bulk modulus K and shear modulus G, both barotropic.  \
";
PQPlot::usage = "PQPlot[Path , Mat ] or  PQPlot[{Path1,Path2,.. }, Mat ]  plots the stress path in the isomorphic P-Q-space.   \
\n Moreover for the first and the last state PQPlot shows the yield surfaces of the BBM-model. \
\n If several Paths are used like {Path1,Path2,.. } the yield surfaces for the end-states of each path are also plotted. \
 ";
epsQQPlot::usage = "epsQQ[Path , Mat ] or  epsQQ[{Path1,Path2,.. }, Mat ]  produces an isomorphic epsQ-Q diagramm.   \
 ";
oedometricPlot::usage = "oedometricPlot[Path, Mat] or  oedometricPlot[{Path1,Path2,... } , Mat] plots the compression curve  in   P-e-space.   \
\n The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled  \
  ";
controlPanelBBM::usage = "controlBBM[ ] loads a graphic interface to perform element tests using BBM.   \
\n The initial conditions and parameters must be saved. \
\n Two stress or strain or mixed increments must be prescribed. The suction increment must be prescribed. Push the button calculate to see the results after the increment.  \
\n The number of increments is ntime, but can be modified writing a new number in the cell ntime. \
\n If you want to start with a brand new simulation, click on Reset. \
\n If you wish to undo the last step, please click on the button Undo. \
  ";


ICond0={150,0,200,0.9,0,0,0,0,0,0};
path={ICond0}; 
Mat={0.2,0.02,0.14,\[Pi]/6,0.75,0.0125,0.6,100,0.008, 100};
(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];
inRange[x_, range_] := IntervalMemberQ[Interval[range], x];
getElasticIsomorph[P_,\[Nu]_,\[Kappa]_] := Module[{elastG,elastK}, 
             elastK=P/(\[Kappa]*Sqrt[3]);       elastG=elastK (3.0-6.0 \[Nu])/(2.0+2.0 \[Nu]);
            Simplify[{{3 elastK,0},{0,2 elastG}}]   ];  
RunBbmTest[Mat_,ICond_,Loading_]:=Module[{state,\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s ,sucRef,MC,ME,M, P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor,suc,P0,itime,elastStiff,
               unknowns,solution,  d\[Epsilon]Trial ,dTTrial,PTrial,QTrial ,yield, mflow,
                loadingQ , \[Lambda], P1,Q1,Pc1, eq1a, eq1b, eq2a ,eq2b, eq3, eq4, eq5,unknowns1, 
               approxTF, approx,approxi, unknowns1approxi ,solution1, dT2 ,d\[Epsilon]2,  \[Lambda]2,s1,s2,\[Lambda]s, Pss,P01,alpha, \[Epsilon]pP, \[Epsilon]pQ,P0s1},
{ntime, d\[Epsilon], ds, dT}={ Loading[[1]], N[ Loading[[2]] ], N[Loading[[3]] ] , N[Loading[[4]] ]};
state=Table[{0,0,0,0,0,0,0,0,0,0},{i,1,ntime}];
(*Material constants*)
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s,sucRef}=Mat;
(*alpha=MC*(MC-9)*(MC-3)/(9*(6-MC));*) (*scalar factor to adjust flow rule to K0 conditions with Jackys equation*)
alpha=1;
(*Critical state slope for extension and compression*)
MC = Sqrt[2]/3*(6.0* Sin[\[CurlyPhi]])/ (3-Sin[\[CurlyPhi]]) ;   
ME = Sqrt[2]/3*(6.0 *Sin[\[CurlyPhi]] )/ (3+Sin[\[CurlyPhi]]) ; 
(*Initial conditions*)
{P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P0,\[Epsilon]pP,\[Epsilon]pQ}=ICond;
For[itime=2, itime <= ntime,
state[[1]]=ICond;
suc+=ds; 
If[suc<0,Print["suction<0, step terminated"];Abort[];];
(*Volumetric strain diminished by suction*)
d\[Epsilon]T=d\[Epsilon]-{\[Kappa]s*ds/(suc+sucRef)/Sqrt[3],0};
elastStiff=getElasticIsomorph[P,\[Nu],\[Kappa]]; (*Elastic matrix*)
unknowns=Select[Flatten[{d\[Epsilon],dT}],symbolQ];  
solution = Solve[ dT==elastStiff.d\[Epsilon]T ,unknowns]; (*Trial elastic stress increment*)
{d\[Epsilon]Trial ,dTTrial}= {d\[Epsilon],dT}  /. solution[[1]];  (*Trial step solution*)
M=If[Q+ dTTrial[[2]]>= 0,MC,ME]; (*Critical state slope for compression and extension*)
(*Trial step solution for stress*)
{PTrial,QTrial} = {P,Q} + dTTrial; 
(*Lambda depending on suction*)
\[Lambda]s=\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr);
If[\[Lambda]s<0,Print["\[Lambda]s<0, step terminated"];Abort[]];
P0=Pref*(P0s/Pref)^((\[Lambda]0-\[Kappa])/(\[Lambda]s-\[Kappa])); (*Preconsolidation pressure with suction*)
Pss=kk*suc; (*Tensile strength*)
yield=(QTrial/M)^2-(PTrial+Pss)*(P0-PTrial); (*Yield surface*)
(*Elastic check*)
If[yield>0.0 PTrial^2,   loadingQ = True, loadingQ=False] ;
(*Elastic step*)
If[\[Not]loadingQ , {\[Epsilon]P,\[Epsilon]Q}+=  d\[Epsilon]Trial ;  epor+=-(1.0+epor)*d\[Epsilon]Trial[[1]]*Sqrt[3]; {P,Q}+= dTTrial;   Goto[nextInc] ] ;
(*Plastic step*)
(*Evaluating variables depending on suction*)
(*Lambda depending on suction*)
\[Lambda]s=\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr); 
If[\[Lambda]s<0,Print["\[Lambda]s<0, step terminated"];Abort[]];
(*Implicit preconsolidation stress with suction*)
P01=Pref*(P0s1/Pref)^((\[Lambda]0-\[Kappa])/(\[Lambda]s-\[Kappa])); (*P0s1 is the implicit saturated preconsolidation pressure*)
(*Set of equations*)
mflow={2*P1+Pss-P01,(2*Q1)/M^2*alpha};
eq1a = dT[[1]]  == ( elastStiff. (d\[Epsilon]T - \[Lambda] * mflow) )[[1]];  (*P implicit*)
eq1b = dT[[2]]  == ( elastStiff. (d\[Epsilon]T - \[Lambda] * mflow) )[[2]]; (*Q implicit*)
eq2a = P1  == P  + dT[[1]] ;  (*P implicit*)
eq2b = Q1  == Q  + dT[[2]]; (*Q implicit*)
eq3 = (Q1/M)^2-(P1+Pss)*(P01-P1) ==0; (*Yield surface*)
(*eq4 =P0s1 == Exp[Sqrt[3]*\[Lambda] *mflow[[1]]*(1+epor)/(( \[Lambda]0-\[Kappa]))]*P0s *); (*Integrated in each step*)
eq4=P0s1==P0s*(1+Sqrt[3]*\[Lambda] * mflow[[1]]/(\[Lambda]0-\[Kappa]));
unknowns1 =  {unknowns, \[Lambda], P0s1, P1,Q1 } //Flatten; 
approxTF = symbolQ[#]& /@  (Flatten[{d\[Epsilon], dT}]); 
approx = Pick[ Flatten[ {d\[Epsilon]Trial, dTTrial}], approxTF]; 
approxi  = {approx,0, P0s, PTrial,QTrial} //Flatten ;
unknowns1approxi = Transpose[{unknowns1, approxi}];
solution1 = FindRoot[{eq1a,eq1b, eq2a,eq2b, eq3,eq4}, unknowns1approxi,AccuracyGoal-> 5 ] ;                
{dT2 ,d\[Epsilon]2,   \[Lambda]2 , P0s  ,P  ,  Q}={ dT ,  d\[Epsilon] ,  \[Lambda] , P0s1,  P1, Q1}/. solution1; 
{\[Epsilon]P,\[Epsilon]Q} += d\[Epsilon]2;
{\[Epsilon]pP,\[Epsilon]pQ}+=\[Lambda] * {2*P+Pss-P01,(2*Q)/M^2*alpha}/. solution1; 
epor+=-(1.0+epor)*d\[Epsilon]2[[1]]*Sqrt[3];
Label[nextInc];
state[[itime]]={P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P0,\[Epsilon]pP,\[Epsilon]pQ};

itime++; 
];
state
]  ;
plotBBMsurface[P0s_,\[CurlyPhi]_] := Module[{MC,ME,P,wQ,gC,gE},
MC=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3-Sin[\[CurlyPhi]]));ME=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3+Sin[\[CurlyPhi]]));
  gC=ContourPlot[P^2+(Q/MC)^2-P*P0s==0,{P,0.001`,3*P0s},{Q,0,(P0s MC)/2},ContourStyle->{Red},DisplayFunction->Identity];
  gE=ContourPlot[P^2+(Q/ME)^2-P*P0s==0,{P,0.001`,3*P0s},{Q,1/2 (-P0s) ME,0},ContourStyle->{Red},DisplayFunction->Identity];
  {gC,gE}
];
plotBBMsurfaceSuction[ P0_ , Mat_,suc_] := Module[ { MC,ME, P1,Q1,gC1,gE1,\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, \[Lambda]s,Pss,sucRef,P01},   
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, sucRef}=Mat;
MC = Sqrt[2]/3*(6.0* Sin[\[CurlyPhi]])/( 3-Sin[\[CurlyPhi]] );   ME = Sqrt[2]/3*(6.0 *Sin[\[CurlyPhi]] )/( 3+Sin[\[CurlyPhi]] ); 
\[Lambda]s=\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr);
Pss=kk*suc;
P01=If[P0==0,1,P0];
     gC1= ContourPlot[(Q1  /MC)^2-(P1+Pss)*(P01-P1)==0 , {P1, -Pss-0.1,3*P01}, {Q1,0,(P01+Pss-0.1)*MC/2} ,ContourStyle->{Purple, Dashed},DisplayFunction->Identity ];
      gE1= ContourPlot[(Q1  /ME)^2-(P1+Pss)*(P01-P1)==0 ,{P1,-Pss-0.1,3*P01},{Q1,-(P01+Pss-0.1)*ME/2,0} ,ContourStyle->{Purple, Dashed},DisplayFunction->Identity ];    {gC1, gE1}
    ];
 PQPlot[Paths_, Mat_] := Module[{nPaths ,PQs, gPQs, gfirstSurface  , glastSurface, outputPlot, currentPath , iPath ,gsaturatedsurface   },
        If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
        PQs = Array[0&, nPaths];
        gfirstSurface =  Array[0&, nPaths];
        glastSurface =  Array[0&, nPaths];
        gsaturatedsurface= glastSurface =  Array[0&, nPaths];
        gPQs =  Array[0&, nPaths];
        If[nPaths==1,
           PQs[[1]] = {#[[1]], #[[2]] }  &  /@  Paths;
           gPQs[[1]] = ListPlot[PQs[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},DisplayFunction->Identity];

           gfirstSurface[[1]] = plotBBMsurfaceSuction[Paths[[1,8]],Mat,Paths[[1,7]] ];
           glastSurface[[1]] =  plotBBMsurfaceSuction[Paths[[-1,8]],Mat,Paths[[-1,7]]];
           gsaturatedsurface[[1]] =  plotBBMsurface[Paths[[-1,3]],Mat[[4]]];
           outputPlot  = { gPQs[[1]], glastSurface[[1]],gsaturatedsurface,
                           Graphics[{ Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (2Sqrt[2] Sin[ Mat[[4]] ])/(3-Sin[ Mat[[4]] ])}}],
                           Line[{{0,0},{Max[Paths[[All,3]]]/2,
                           Max[Paths[[All,3]]]/2  (-2 Sqrt[2] Sin[ Mat[[4]] ])/(3+Sin[ Mat[[4]] ])}}] } ]  } ;
         ] ;
        If[nPaths > 1,
        For[iPath=1, iPath<=nPaths,
          currentPath =   Paths[[iPath]] ;
          PQs[[iPath]] = {#[[1]], #[[2]] }  &  /@  currentPath;
          gPQs[[iPath]] = ListPlot[PQs[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,Q},DisplayFunction->Identity];
          (*gfirstSurface[[iPath]] = plotBBMsurface[currentPath[[1,3]],Mat[[4]]];
           glastSurface[[iPath]] =  plotBBMsurface[currentPath[[-1,3]],Mat[[4]]];*)
         gsaturatedsurface[[1]] =  plotBBMsurface[Paths[[-1,3]],Mat[[4]]];
          gfirstSurface[[1]] = plotBBMsurfaceSuction[Paths[[1,8]],Mat,Paths[[1,7]] ];
           glastSurface[[1]] =  plotBBMsurfaceSuction[Paths[[-1,8]],Mat,Paths[[-1,7]]];
        iPath++;] ;
        outputPlot  = Flatten[{ gPQs , gfirstSurface , glastSurface,gsaturatedsurface } ];
         ];
        Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];

NClinePlot[Mat_,path_,ICond01_] := Module[ { suc, MC,ME, P1,Q1,gNCL,evoid,\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, Pat, Pini,P0sini,sucRef,Nini},  
evoidini=N[ICond01[[6]]];
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, sucRef}=N[Mat];
P0sini=N[ICond01[[3]]];
Pini=N[ICond01[[1]]];
suc=N[ICond01[[5]]];
suc=path[[-1,7]];
Nini=Log[evoidini+1];
Nini=Nini-\[Kappa]*Log[Pref/Pini]+\[Kappa]s*Log[sucRef/(suc+sucRef)]-\[Kappa]*Log[P0sini/Pref];

     gNCL= ContourPlot[Log[evoid+1]==-\[Lambda]0*Log[P1/P0sini]+Nini, {P1, 0.001,500}, {evoid,0.1,evoidini},ContourStyle->{Red}]
    ];

NClinePlotSuction[Mat_,path_,ICond01_] := Module[ {evoidini,MC,ME, P1,Q1,gNCL,evoid,P0s,\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,sucRef,\[Kappa]s,\[Lambda]s,Nini,gNCLs, Pat, Pini,Pcini,suc},   
evoidini=N[ICond01[[6]]];
Pini=N[ICond01[[1]]];
Pcini=N[ICond01[[3]]];

suc=path[[-1,7]];
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, sucRef}=N[Mat];
\[Lambda]s=\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr);
P0s=Pref*(Pcini/Pref)^((\[Lambda]0-\[Kappa])/(\[Lambda]s-\[Kappa]));
(*P0s=path[[-1,8]];*)
Nini=Log[evoidini+1];
Nini=Nini-\[Kappa]*Log[P0s/Pini];

     gNCLs= ContourPlot[Log[evoid+1]==-\[Lambda]s*Log[P1/P0s]+Nini, {P1, 0.001,3.0*Pcini}, {evoid,0.1,evoidini},ContourStyle->{Purple, Dashed} ]
    ];

 oedometricPlot[Paths_, Mat_,ICond01_] := Module[{nPaths=1,Pes, gPes,NCline,  outputPlot, currentPath , iPath ,NClinesuction,NClinesuctionlast},
          
           If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          Pes = Array[0&, nPaths];
          gPes =  Array[0&, nPaths];
          NCline=  Array[0&, nPaths];
          NClinesuction=  Array[0&, nPaths];
          NClinesuctionlast=  Array[0&, nPaths];
          If[nPaths==1,
             Pes[[1]] = {#[[1]], #[[6]] }  &  /@  Paths;
             gPes[[1]] = ListPlot[Pes[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,e},DisplayFunction->Identity];
              NCline[[1]] = NClinePlot[Mat,Paths,ICond01 ];    
           NClinesuction[[1]] = NClinePlotSuction[Mat,Paths,ICond01];    
             NClinesuctionlast[[1]] = NClinePlotSuction[Mat,Paths,ICond01];     
             outputPlot  = { gPes[[1]],NCline[[1]] ,(*NClinesuction[[1]]*)NClinesuctionlast[[1]]} ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            Pes[[iPath]] = {#[[1]], #[[6]] }  &  /@  currentPath;
            gPes[[iPath]] = ListPlot[Pes[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,e},DisplayFunction->Identity]; 
            NCline[[1]] = NClinePlot[Mat,Paths,ICond01];       
        NClinesuction[[1]] = NClinePlotSuction[Mat,Paths,ICond01];             
             NClinesuctionlast[[1]] = NClinePlotSuction[Mat,Paths,ICond01];       
           iPath++;] ;
           outputPlot  = { Flatten[gPes,1],NClinesuctionlast,NCline} ;
           ]  ;
         Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];


 epsQQPlot[Paths_, Mat_] := Module[{nPaths=1,epsQQ, gepsQQ,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          epsQQ = Array[0&, nPaths];
          gepsQQ =  Array[0&, nPaths];
          If[nPaths==1,
             epsQQ[[1]] = {#[[5]], #[[2]] }  &  /@  Paths;
             gepsQQ[[1]] = ListPlot[epsQQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]Q,Q},DisplayFunction->Identity];
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
 epPepQPlot[Paths_, Mat_] := Module[{nPaths=1,epsPepsQ, gepsPepsQ,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          epsPepsQ = Array[0&, nPaths];
          gepsPepsQ =  Array[0&, nPaths];
          If[nPaths==1,
             epsPepsQ[[1]] = {#[[9]], #[[10]] }  &  /@  Paths;
             gepsPepsQ[[1]] = ListPlot[epsPepsQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]pP,\[Epsilon]pQ},DisplayFunction->Identity];
             outputPlot  = { gepsPepsQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsPepsQ[[iPath]] = {#[[9]], #[[10]] }  &  /@  currentPath;
            gepsPepsQ[[iPath]] = ListPlot[epsPepsQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]pP,\[Epsilon]pQ},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsPepsQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];


voidsucPlot[Paths_, Mat_] := Module[{nPaths=1,epsPepsQ, gepsPepsQ,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          epsPepsQ = Array[0&, nPaths];
          gepsPepsQ =  Array[0&, nPaths];
          If[nPaths==1,
             epsPepsQ[[1]] = {#[[7]], #[[6]] }  &  /@  Paths;
             gepsPepsQ[[1]] = ListPlot[epsPepsQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{suction,e},DisplayFunction->Identity];
             outputPlot  = { gepsPepsQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsPepsQ[[iPath]] = {#[[7]], #[[6]] }  &  /@  currentPath;
            gepsPepsQ[[iPath]] = ListPlot[epsPepsQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{suction,e},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsPepsQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];


epsPepsQPlot[Paths_, Mat_] := Module[{nPaths=1,epsPepsQ, gepsPepsQ,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          epsPepsQ = Array[0&, nPaths];
          gepsPepsQ =  Array[0&, nPaths];
          If[nPaths==1,
             epsPepsQ[[1]] = {#[[4]], #[[5]] }  &  /@  Paths;
             gepsPepsQ[[1]] = ListPlot[epsPepsQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{\[Epsilon]P,\[Epsilon]Q},DisplayFunction->Identity];
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
suctionPlot[Paths_, Mat_] := Module[{nPaths=1,sucpl,gsucpl,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          sucpl = Array[0&, nPaths];
          gsucpl =  Array[0&, nPaths];
          If[nPaths==1,
             sucpl[[1]] = {#[[1]], #[[7]] }  &  /@  Paths;
             gsucpl[[1]] = ListPlot[sucpl[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,suction},DisplayFunction->Identity];
             outputPlot  = { gsucpl[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            sucpl[[iPath]] = {#[[1]], #[[7]] }  &  /@  currentPath;
            gsucpl[[iPath]] = ListPlot[sucpl[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{P,suction},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gsucpl  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];


PQs3DPlot[Paths_, Mat_,ICond01_] := Module[{nPaths=1,PQs3D, gPQs3D,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          PQs3D = Array[0&, nPaths];
          gPQs3D =  Array[0&, nPaths];
          If[nPaths==1,
             PQs3D[[1]] = {#[[1]], #[[7]], #[[2]] }  &  /@  Paths;
             gPQs3D[[1]] = ListPointPlot3D[PQs3D[[1]],AxesLabel->{P,s,Q},PlotRange->All,PlotStyle->PointSize[0.014`],DisplayFunction->Identity,BoxRatios->1.0];
             outputPlot  = { gPQs3D[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            PQs3D[[iPath]] = {#[[1]], #[[7]], #[[2]]}  &  /@  currentPath;
            gPQs3D[[iPath]] = ListPointPlot3D[PQs3D[[iPath]],AxesLabel->{P,s,Q},PlotRange->All,PlotStyle->PointSize[0.014`],DisplayFunction->Identity,BoxRatios->1.0];
           iPath++;] ;
            outputPlot  = Flatten[{ gPQs3D  } ,1];
           ]  ;
          Show[outputPlot]
 ];
Pse3DPlot[Paths_, Mat_,ICond01_] := Module[{nPaths=1,Pse3D, gPse3D,  outputPlot, currentPath , iPath },
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          Pse3D = Array[0&, nPaths];
          gPse3D =  Array[0&, nPaths];
          If[nPaths==1,
             Pse3D[[1]] = {#[[1]], #[[6]], #[[7]] }  &  /@  Paths;
             gPse3D[[1]] = ListPointPlot3D[Pse3D[[1]],AxesLabel->{P,e,s},PlotRange->All,PlotStyle->PointSize[0.014`],DisplayFunction->Identity,BoxRatios->1.0];
             outputPlot  = { gPse3D[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            Pse3D[[iPath]] = {#[[1]], #[[6]], #[[7]]}  &  /@  currentPath;
            gPse3D[[iPath]] = ListPointPlot3D[Pse3D[[iPath]],AxesLabel->{P,e,s},PlotRange->All,PlotStyle->PointSize[0.014`],DisplayFunction->Identity,BoxRatios->1.0];
           iPath++;] ;
            outputPlot  = Flatten[{ gPse3D  } ,1];
           ]  ;
          Show[outputPlot]
 ];
 inputCheck[Mat_, Icond_, Loading_]:= Module[ {P, Q, \[Nu], \[Kappa], \[Lambda]0, \[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, Pat, P0s, \[Epsilon]P, \[Epsilon]Q, epor, ntime, deP, deQ, dP, dQ, suc, P0,\[Epsilon]pP,\[Epsilon]pQ },
 
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, Pat}=Mat;
(*Initial conditions*)
{P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P0,\[Epsilon]pP,\[Epsilon]pQ}=Icond;

 { ntime, {deP,deQ}, {dP,dQ} } = { First[Loading], N[ Loading[[2]] ], N[ Loading[[3]] ] }    ;

  If[symbolQ[ntime] , Print[ "number of increments  ntime =  ", ntime," undefined"   ]]  ;
  If[ ntime < 10 , Print[ "suspiciously small number of increments  ntime =  ", ntime   ]]  ;
  If[ deP > 0.005 || deQ > 0.005 , Print[ "Strain increments seem to be large {deP, deQ} = ", {deP, deQ}  , " Keep them at  0.1% level"   ]]  ;
  If[ dP > 10 || dQ > 10 , Print[ "Stress increments seem to be large {dP, dQ} = ", {dP, dQ}  , " Keep them below  10 kPa "   ]]  ;
  If[ P0s *( 1+ epor )^(1/ \[Lambda]0 ) < 5000, Print[ "Warning: epor=0 can be reached already at about P0s =  ",  P0s *( 1+ epor )^(1/ \[Lambda]0 ) ,
        "  Try to reduce \[Lambda]0 = ",   \[Lambda]0 ] ]   ;

  If[\[Nu] ~inRange~ {0, 0.5} ,"" , Print[ "Suspicious \[Nu] =  ",\[Nu]   ]]  ;
  If[ \[Kappa] ~inRange~ {0, 0.2} , "", Print[ "Suspicious \[Kappa] =  ",\[Kappa]  ]]  ;
  If[\[Lambda]0 ~inRange~ {  \[Kappa] , 20  \[Kappa] } , "", Print[ "Suspicious \[Kappa]/\[Lambda]0 =  ",\[Kappa] / \[Lambda]0 ]]  ;
  If[ \[CurlyPhi] ~inRange~ {5\[Degree], 50\[Degree]} , "", Print[ "Suspicious \[CurlyPhi]=  ", \[CurlyPhi], "rad"  ]]  ;
  If[ rr ~inRange~ {0, 1} , "", Print[ "Suspicious rr=  ", rr  ]]  ;
  If[ \[Beta] ~inRange~ {0, 10000000} , "", Print[ "Suspicious \!\(\*
StyleBox[\"\[Beta]\",\nFontWeight->\"Bold\"]\)=  ", \[Beta] ]]  ;
  If[ kk ~inRange~ {0, 1} , "", Print[ "Suspicious \!\(\*
StyleBox[\"kk\",\nFontWeight->\"Bold\"]\)=  ", kk]]  ;
  If[ Pref ~inRange~ {0, 10000} , "", Print[ "Suspicious \!\(\*
StyleBox[\"Pref\",\nFontWeight->\"Bold\"]\)=  ", Pref]]  ;
  If[ \[Kappa]s ~inRange~ {0, 1000} , "", Print[ "Suspicious \!\(\*
StyleBox[\"\[Kappa]s\",\nFontWeight->\"Bold\"]\)=  ", \[Kappa]s]]  ;
  If[ Pat ~inRange~ {0, 100000} , "", Print[ "Suspicious \!\(\*
StyleBox[\"Pat\",\nFontWeight->\"Bold\"]\)=  ", Pat]]  ;

  If[P < 0 , Print[ "Warning: negative initial  P =  ", P   ]]      ;
  If[P0s < P , Print[ "Warning:  initial  P0s =  ", P0s , "<", P, " =P" ]]  ;
  If[P0 < P , Print[ "Warning:  initial  P0s =  ", P0s , "<", P, " =P" ]]  ;
  MC=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3-Sin[\[CurlyPhi]]));
  ME=(Sqrt[2] (6  Sin[\[CurlyPhi]]))/(3 (3+Sin[\[CurlyPhi]]));
  M=If[Q >=0,MC,ME];
  If[ P^2+(Q/M)^2-P*P0s >  P0s*P0s *10^-6, Print[ "Warning: initial OCR<1 means underconsolidation:  P^2+(Q/M)^2-P*P0s  = ", P^2+(Q/M)^2-P*P0s  , "> 0" ] ]  ;
    PcPlus  =  P +(Q/M)^2 /P ;
    OCR = P0s/PcPlus;
  If[ OCR ~inRange~ {1.2,2},  Print[ "Initial  OCR =", OCR, ", soil was lightly overconsolidated " ] ];
  If[ OCR ~inRange~ {1.0,1.01},  Print[ "Initial OCR =", OCR, ", soil was  normally consolidated " ] ];
  If[ OCR ~inRange~ {1.01,1.2},  Print[ "Initial OCR =", OCR, ", soil was  almost  normally consolidated " ] ];
  If[ OCR ~inRange~ {2,4},  Print[ "Initial OCR =", OCR, ", soil was  overconsolidated " ] ];
  If[ OCR ~inRange~ {4, Infinity},  Print[ "Initial OCR =", OCR, ", soil was heavily overconsolidated " ] ];



  If[epor  ~inRange~ {0.01, 3} , "", Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 3} "  ]];

 dT = {dP,dQ};
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True] > 2,  Print["Error: Too many unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True]  < 2,  Print["Error: Too few unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, True, False, True},  Print["Error: You cannot prescribe deP and dP in Loading simultaneously"]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == {True, False, True,False},  Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously"]; Beep[]; Abort[]; ];
 PrescribedStressPath  =  Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, False, Ttue, True} ;
 If[ PrescribedStressPath  ,
      P +=  ntime *dP;      Q += ntime*dQ;
      M=If[Q >=0,MC,ME];
      If[ P^2+(Q/M)^2-P*P0s >  P0s*P0s *10^-6, Print[ "Warning: the stress path ends above CSL; strain or mixed control recommended " ] ]  ;
 ];
   If[Length[path] > 1,
              { P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, \[Epsilon]Ppl,\[Epsilon]Qpl}= Last[path];
                M=If[Q >=0,MC,ME];
                PcPlus  =  P +(Q/M)^2 /P ;
                Print["The latest value of OCR =", P0s/PcPlus];
  ];

];

 controlPanelBBM[] := Module[{Loading},
(*Default values for initial visualization of graphics*)
ICond0={150,0,200,0.9,0,0,0,0,0,0};
path={ICond0}; 
Mat={0.2,0.02,0.14,\[Pi]/6,0.75,0.0125,0.6,100,0.008, 100};
\[Lambda]s0=Mat[[3]]*((1-Mat[[5]])*Exp[-Mat[[6]]*ICond0[[7]]]+Mat[[5]]);
P0=Mat[[8]]*(ICond0[[3]]/Mat[[8]])^((Mat[[3]]-Mat[[2]])/(\[Lambda]s0-Mat[[2]]));
ICond0[[8]]=P0;
 Manipulate[
Switch[ Left, "none",g1x="empty" ;,"e-s", g1x=g2;,"\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)",g1x=g6;, "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)", g1x=g7;, "P-Q-s", g1x=g8;, "P-s-e", g1x=g9;];
Switch[ Right, "none",g2x="empty";,"e-s", g2x=g2;,"\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)",g2x=g6;, "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)", g2x=g7;, "P-Q-s", g2x=g8;, "P-s-e", g2x=g9;];
g1=Show[{PQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ];
g2=Show[{voidsucPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 7]], path[[1, 6]]} ],PointSize[Medium], Orange,  Point[ { path[[-1, 7]], path[[-1,6]] } ]}] } ];
g3=Show[{epsQQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 4]], path[[1, 2]]} ],PointSize[Medium], Orange, Point[ { path[[-1, 4]], path[[-1, 5]]} ]} ] } ] ;
g4=Show[{oedometricPlot[path, Mat,ICond0], Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 6]] }],PointSize[Medium], Orange, Point[{path[[-1, 1]], path[[-1, 6]] } ] }] } ];
g5=Show[{suctionPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ];
g6=Show[{epsPepsQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 4]], path[[1, 5]]} ],PointSize[Medium], Orange, Point[ { path[[-1, 4]], path[[-1, 5]] } ]} ] } ] ;
g7=Show[{epPepQPlot[path,Mat],Graphics[{PointSize[Large],Red,Point[{path[[1,9]],path[[1,10]]}],PointSize[Medium],Orange,Point[{path[[-1,9]],path[[-1,10]]}]}]}];
g8=Show[{PQs3DPlot[path,Mat,ICond0],Graphics3D[{PointSize[Large],Red,Point[{path[[1,1]],path[[1,7]],path[[1,2]]}],PointSize[Medium],Orange,Point[{path[[-1,1]],path[[-1,7]],path[[-1,2]]}]}]}];
g9=Show[{Pse3DPlot[path,Mat,ICond0],Graphics3D[{PointSize[Large],Red,Point[{path[[1,1]],path[[1,6]],path[[1,7]]}],PointSize[Medium],Orange,Point[{path[[-1,1]],path[[-1,6]],path[[-1,7]]}]}]}];
Show[GraphicsGrid[{{g1,g3},{g4,g5},{If[Left=="none"," ",g1x],If[Right=="none"," ",g2x]}},ImageSize -> Scaled[.5]]],
 Delimiter, 
"MMC Parameters",
 {{\[Nu], 0.2}}  ,
 {{\[Kappa], 0.02}}  ,
 {{\[Lambda]0, 0.14}}  ,
 {{\[CurlyPhi], \[Pi]/6}}  ,
"BBM Parameters",
 {{rr, 0.75}}  ,
 {{\[Beta], 0.0125}}  ,
 {{kk, 0.6}}  ,
 {{Pref, 100}}  ,
 {{\[Kappa]s, 0.008}}  ,
 {{sucRef, 100}}  ,
 Delimiter,
"Initial conditions:",
 {{P, 150}}  ,
 {{Q, 0.0}}  ,
 {{P0s, 200}}  ,
 {{epor, 0.9}}  ,
 {{suc, 200}}  ,
(*{P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P0}=ICond*)
 Delimiter,
"Number of steps:",
 {{ntime, 50}},
 Delimiter,
 "Preescribed components (in % or kPa):",
 {{d\[Epsilon]P, xd\[Epsilon]P}},
 {{d\[Epsilon]Q, xd\[Epsilon]Q}}  ,
 {{dP, 0}}  ,
 {{dQ, 0}}  ,
 {{ds, 0}},
Delimiter,
 Button["Check the input",
Mat={\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, sucRef};
ICond0={P,Q,P0s,0,0,epor, suc,P0,0, 0};
   d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
   d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
  inputCheck[Mat, ICond0, {ntime, {d\[Epsilon]P, d\[Epsilon]Q}, {dP, dQ}} ]
 ],
   Button["Calculate step",
  Mat={\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],kk,Pref,\[Kappa]s, sucRef};
  ICond0={P,Q,P0s,0,0,epor, suc,P0,0, 0};
 If[Length[path] == 1, path = Array[0 &, {1, 8}]; path = {ICond0};]; (* Initialisation of global variable Path *)
 (* d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
  d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
*)
Loading={ntime,N[ {d\[Epsilon]P, d\[Epsilon]Q }] ,ds, N[ {dP, dQ}]};  
(*Loading = { ntime,-ICond0[[7]]/ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}] };*)
  dpath =  RunBbmTest[Mat,Last[path],Loading]  ;
  path = Join[ path,  Delete[ dpath, 1] ] ;

 If[Count[Evaluate[symbolQ[#] & /@ {d\[Epsilon]P, d\[Epsilon]Q, dP, dQ}], True] != 2,
  Print[Style[ "Error: system of equations cannot be solved. Exactly two unknowns are needed.", Red, 20] ]; Abort[]; Beep[];
  ] (* Controls if two unknowns are sought *)
 ],
Delimiter,
"Additional Graphs:",
{{Left,"none"},{"none","e-s", "\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)", "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)", "P-Q-s", "P-s-e"},ControlType-> RadioButtonBar },
 {{Right,"none"},{"none","e-s", "\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)", "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)","P-Q-s", "P-s-e"},ControlType-> RadioButtonBar },
Delimiter,
Button["Undo the last ntime increments",
  If[Length[path] >= 2, path = Drop[path, -ntime];,
   Print[Style["No more increments can be undone.", Blue, 20] ]; ];
 ],

Button["Print the final state",
  Print["Final {P,Q}= ", path[[-1,1 ;; 2]]," ,  {\[Epsilon]P,\[Epsilon]Q}= ", path[[-1,4 ;; 5]], " , P0s= ", path[[-1,3]], " , epor= ",path[[-1,6]] ,
      ", suc=", path[[-1,7]] ,", P0s (with suction)= ", path[[-1,8]] ,", {\[Epsilon]pP,\[Epsilon]pQ}= ", path[[-1,9 ;; 10]]
  ];
 ],

Button["Reset all steps, graphics and delete cell",
  path = {ICond0}; ntime = xntime;  d\[Epsilon]Pp = xdeP ; d\[Epsilon]Qp = xdeQ; dP = xdP; dQ = xdQ; FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]];
 ]
 ]
];



(*ExampleIsochoricShearing[] := Module[{xdP,xdQ,Mat1, ICond1, Path1, Loading1, Path2, ICond2, Path3, Loading2, Path4 },
Print[" Undrained : active/passive  shearing ;  normally consolidated/overconsolidated.\
\n Note that the initial  preconsolidation P0s=101 may slightly change!  \
\n Enter ??ExampleIsochoricShearing  to get read  IC, mat. constants and loading steps."];
Mat1={0.2`,0.03`,0.2`,\[Pi]/6};
ICond1={100.`,0.`,101.`,0.`,0.`,1.`,0, 0};
Loading1={100,{0,0.001`},{xdP,xdQ}};
ICond2={10.`,0.`,101.`,0.`,0.`,1.`,0, 0};
Loading2={100,{0,-0.001`},{xdP,xdQ}};
 Path1=RunBbmTest[Mat1,ICond1,Loading1];
 Path2=RunBbmTest[Mat1,ICond2,Loading1];
 Path3=RunBbmTest[Mat1,ICond1,Loading2];
 Path4=RunBbmTest[Mat1,ICond2,Loading2];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1]  , epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];
ExampleOedometricTest[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print[" Oedometric loading , short unloading, reloading \
\n Enter ??ExampleOedometricTest  to get read  IC, mat. constants and loading steps."];
Mat1={0.2,0.03,0.2,\[Pi]/6};
ICond1={65,0.,100.,0,0,1,0, 0};
Loading={90,{0.001/ Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2}};
ULoading={10,{-0.001/ Sqrt[3],-0.001 Sqrt[2./3.]},{xdT1,xdT2}};
RLoading={60,{0.001/Sqrt[3],0.001 Sqrt[2./3.]},{xdT1,xdT2}};
Path1=RunBbmTest[Mat1,ICond1,Loading];
Path2=RunBbmTest[Mat1,Path1[[-1]],ULoading];
Path3=RunBbmTest[Mat1,Path2[[-1]],RLoading];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1], epsQQPlot[{Path1,Path2,Path3}, Mat1]  }, ImageSize-> Large]
];
ExampleOedometricTest2[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print[" Oedometric loading , long unloading, reloading \
\n Enter ??ExampleOedometricTest2  to get read  IC, mat. constants and loading steps."];
Mat1={0.2`,0.03`,0.2`,\[Pi]/6};
ICond1={65.`,0.`,100.`,0.`,0.`,1.`,0, 0};
 Loading={80,{0.001` / Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2}};
 ULoading={45,{-0.001` /Sqrt[3.`],-0.001 Sqrt[2.`/3.`]},{xdT1,xdT2}};
 RLoading={70,{0.001`/Sqrt[3.`],0.001 Sqrt[2.`/3.`]},{xdT1,xdT2}};
  Path1=RunBbmTest[Mat1,ICond1,Loading];
 Path2=RunBbmTest[Mat1,Path1[[-1]],ULoading];
 Path3=RunBbmTest[Mat1,Path2[[-1]],RLoading];
GraphicsRow[{oedometricPlot[{Path1,Path2,Path3}, Mat1], PQPlot[{Path1,Path2,Path3}, Mat1],  epsQQPlot[{Path1,Path2,Path3}, Mat1]   }, ImageSize-> Large]
];
ExampleIsobaricShearing[] := Module[{xdT1,xdT2,Mat1,ICond1,Loading,ULoading,RLoading,Path1,Path2,Path3},
Print[" Isobaric shearing,  slightly/strongly  overconsolidated starting from P0s=100 \
\n Enter ??ExampleIsobaricShearing  to get read  IC, mat. constants and loading steps."];
Mat1={0.2`,0.03`,0.2`,\[Pi]/5};
 ICond1={80.`,0.`,100.`,0.`,0.`,1.`,0, 0};
 ICond2={20.`,0.`,100.`,0.`,0.`,1.`,0, 0};
 Loading={50,{xdeP,0.0025`},{0,xdQ}};
 Path1= RunBbmTest[Mat1,ICond1,Loading];
 Path2 = RunBbmTest[Mat1,ICond2,Loading];
 GraphicsRow[{oedometricPlot[{Path1,Path2}, Mat1], PQPlot[{Path1,Path2}, Mat1],  epsQQPlot[{Path1,Path2}, Mat1]   }, ImageSize-> Large]
];
*)

EndPackage[ ]
$Context="PQ`BBM`";
Print["The Barcelona Basic Model (BBM) Model for isomorphic P-Q space; by A. Niemunis 2011   \n 
 You are in the context PQ`BBM` which provides functions: \n   
 RunBbmTest, PQPlot, oedometricPlot, epsQQPlot, controlBBM  \n 
and some ready to use examples:   \n  
  ExampleOedometricTest[], ExampleIsochoricShearing[], ExampleOedometricTest2[], ExampleIsobaricShearing[]  \n  ***********************************************************************************************************   \n    ??ExampleOedometricTest  gives you the source code of the example procedure     \n    ?RunBbmTest gives user-oriented  information to the procedure and to the input variables   \n    ??RunBbmTest gives   additionally the source code of  RunBbmTest (for the programmers)   \n  ***********************************************************************************************************   \n    For detailed information on the programming see the notebook BBM-impli.nb or read the PDF documentation.  "];
