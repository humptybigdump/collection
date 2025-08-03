(* ::Package:: *)

BeginPackage["PQ`BBMgraphics`"]

NCsurface::usage = "NCsurface[Mat_,state_] creates a 3D plot of lambda surface  in s,P,epor  space " ;
NCsurfacePsQ::usage= "NCsurfacePsQ[Mat_, state_] creates a 3D plot of  yield surface in P-s-Q space  " ;
epPepQPlot::usaage = "epPepQPlot[path_, Mat_]   yet another plot ";
voidsucPlot::usaage = " voidsucPlot[path_, Mat_]   yet another plot ";
epsPepsQPlot::usaage = "  epsPepsQPlot[path_, Mat_]  yet another plot ";
suctionPlot::usaage = "  suctionPlot[path_, Mat_]   yet another plot ";
PQs3DPlot::usaage = " PQs3DPlot[path_, Mat_ ]    yet another plot ";
Pse3DPlot::usaage = " Pse3DPlot[path_, Mat_]   yet another plot ";
epsQQPlot::usage = "epsQQ[path , Mat ]    produces an isomorphic epsQ-Q diagramm. ";

plotBBMellipseSuction::usage = "plotBBMsurfaceSuction[ state , Mat ]   plots the yield surface of the BBM-model with suction in the isomorphic P-Q-space.   \
\n The yield surface with suction consists of two half-elipses  with the common diameter (Pc+Ps) along the P-axis and different half-diameters \
\n along the Q-axis.  \
";

NClineSuction::usage = "NClineSuction[state, Mat ]    plots the normal consolidation line for non-saturated conditions using  {PO, epor} from states
 and for saturated conditions using {POs, epor} from states. The plot is  on the e-P plane.  \
";

PQPlot::usage = "PQPlot[path , Mat ]  plots the stress path in the isomorphic P-Q-space.   \
\n Moreover for the first and the last state PQPlot shows the yield surfaces of the BBM-model. \
 ";

isoCompressionPlot::usage = "isoCompressionPlot[path, Mat] plots the compression curve  in   P-e-space.   \
\n The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled  \
  ";

getGraphics::usage=" {g1,g2,... g9} = getGraphics[path, Mat] invokes all graphics "


Begin["`Private`"]

getGraphics[path_, Mat_] := Module[ {},
If[ Length[Mat] > 1 && Length[ path[[1]]] > 1,
g1= PQPlot[path, Mat] ;   (* does not attempt to plot anything unless Mat and path[[]1] are initialized *)
g2= voidsucPlot[path, Mat] ;
g3= epsQQPlot[path, Mat] ;
g4= isoCompressionPlot[path, Mat] ;
g5= suctionPlot[path, Mat] ;
g6= epsPepsQPlot[path, Mat] ;
g7= epPepQPlot[path,Mat] ;
g8= PQs3DPlot[path,Mat] ;
g9= Pse3DPlot[path,Mat] ;
,{g1,g2,g3,g4,g5,g6,g7,g8,g9} = Array[ Graphics[Circle[{0, 0}, 1]]&, 9],
 {g1,g2,g3,g4,g5,g6,g7,g8,g9} = Array[ Graphics[Point[{0,0}]]&, 9]
 ];
  {g1,g2,g3,g4,g5,g6,g7,g8,g9}
 ];

plotBBMellipseSuction[ state_ , Mat_ ] := Module[ { MC,ME, P1,Q1,gE,gC,gC1,gE1,\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, \[Lambda]s,Pss,sucRef,P01,eporRef,suc,P0,P0s},
{suc,P0} = state[[7;;8]] ; P0s = state[[3]];
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef, eporRef}=Mat;
MC = Sqrt[2]/3*(6.0* Sin[\[CurlyPhi]])/( 3-Sin[\[CurlyPhi]] );   ME = Sqrt[2]/3*(6.0 *Sin[\[CurlyPhi]] )/( 3+Sin[\[CurlyPhi]] );
\[Lambda]s=\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr);
Pss=ks*suc;
P01=If[P0==0,1,P0];   
gC= Plot[ Sqrt[ -MC^2 P*(  P-P0s)],{P,0.0,P0s}, AspectRatio -> 1,AxesOrigin->{0,0} ,PlotStyle->{Red}, PlotRange->{-  ME/2,  MC/2}*(P0+Pss) ];gE= Plot[ -Sqrt[ -ME^2 P*(  P-P0s)],{P,0.0,P0s}, AspectRatio -> 1,AxesOrigin->{0,0}, PlotStyle->{Red},PlotRange->{-  ME/2,  MC/2}*(P0+Pss)];
gC1= Plot[ Sqrt[ -MC^2 (P+Pss)*(  P-P0)],{P,-Pss,P0}, AspectRatio -> 1,AxesOrigin->{0,0} , PlotStyle->{Purple, Dashed},PlotRange->{-  ME/2,  MC/2}*(P0+Pss)];gE1= Plot[ -Sqrt[ -ME^2 (P+Pss)*(  P-P0)],{P,-Pss,P0}, AspectRatio -> 1,AxesOrigin->{0,0}, PlotStyle->{Purple, Dashed},PlotRange->{-  ME/2,  MC/2}*(P0+Pss)];
    Show[ {gC1, gE1, gC, gE} ]
  ];

PQPlot[path_, Mat_] := Module[{  gPQ, gfirstSurface  , glastSurface, gsaturatedsurface,\[CurlyPhi], MC,ME,PcMax, P0Max, gCSL,gPoints },
         \[CurlyPhi] = Mat[[ 4 ]];  MC=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3-Sin[\[CurlyPhi]]));ME=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3+Sin[\[CurlyPhi]]));
         PcMax=Max[path[[All,3]]]; P0Max= Max[path[[All,8]]];
           gPQ = ListPlot[path[[All,1;;2]]    ,PlotStyle->PointSize[0.014`],PlotRange->All, AspectRatio -> 1, AxesLabel->{"P","Q"},  AxesOrigin->{0,0} ];
           gfirstSurface  = plotBBMellipseSuction[path[[1]],  Mat ];
           glastSurface  =  plotBBMellipseSuction[path[[-1]], Mat ];
           gCSL =  Graphics[  Line[{{P0Max /2, MC*P0Max /2 },{0,0},{P0Max/2,   -ME*P0Max/2}}] ];
           gPoints =  Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] ;
        Show[{ glastSurface, gPQ,  gCSL, gPoints   } ]
 ];


APQPlot[path_, Mat_] := Module[{  gPQ, gfirstSurface  , glastSurface, gsaturatedsurface,\[CurlyPhi], MC,ME,PcMax, P0Max, gCSL,gPoints },
         \[CurlyPhi] = Mat[[ 4 ]];  MC=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3-Sin[\[CurlyPhi]]));ME=(Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3+Sin[\[CurlyPhi]]));
         PcMax=Max[path[[All,3]]]; P0Max= Max[path[[All,8]]];
           gPQ = ListPlot[path[[All,1;;2]]    ,PlotStyle->PointSize[0.014`],PlotRange->All,AxesLabel->{"P","Q"},  AxesOrigin->{0,0} ];
           gfirstSurface  = plotBBMellipseSuction[path[[1]],  Mat ];
           glastSurface  =  plotBBMellipseSuction[path[[-1]], Mat ];
           gCSL =  Graphics[  Line[{{P0Max /2, MC*P0Max /2 },{0,0},{P0Max/2,   -ME*P0Max/2}}] ];
           gPoints =  Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] ;
        Show[{ gPQ, glastSurface, gCSL,gPoints   } ]
 ];

 NClineSuction[Mat_,state_] := Module[ {P0,suc,\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef ,eporRef,eporRefsuc },
   {\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef ,eporRef }=Mat;
   {suc,P0} = state[[7;;8]];
   eporRefsuc = (1+eporRef)((suc + sucRef)/sucRef)^(-\[Kappa]s) - 1;
   Plot[{(1+eporRefsuc)(P/Pref)^(-\[Lambda]0*((1-rr)*Exp[-\[Beta]*suc]+rr) )-1, (1+eporRef)(P/Pref)^(-\[Lambda]0)-1}, {P, 0.3*P0, 1.1*P0}, Filling -> {2} ]
    ];

NCsurface[Mat_,state_] := Module[ {P0,suc,  \[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef,eporRef },
{\[Nu],\[Kappa],\[Lambda]0,\[CurlyPhi],rr,\[Beta],ks,Pref,\[Kappa]s, sucRef,eporRef }=Mat;
{suc,P0} = state[[7;;8]];
suc = Max[suc,1] ; P0 = Max[P0,1];
  Plot3D[(1+eporRef) ((suc + sucRef)/sucRef)^(-\[Kappa]s) (P/Pref)^(-\[Lambda]0*((1-rr)*Exp[-\[Beta]*s]+rr) ) - 1 , {s, 0.1*suc, 1.1*suc } , {P, 0.3*P0,  1.1*P0}, Filling -> Bottom ,
          FillingStyle -> Opacity[0.2], MeshStyle -> {{Gray, Opacity[0.5] }, {Gray, Opacity[0]}},  ViewPoint ->  {2*suc, 0.5 P0, 0.5 P0}     ]
    ];

 NCsurfacePsQ[Mat_, state_] :=  Module[{P0, suc, \[Nu], \[Kappa], \[Lambda]0, \[CurlyPhi], rr, \[Beta], ks, Pref, \[Kappa]s, sucRef, eporRef, sq3 = 1.7320508075688772, P0s, ME,MC,P,s, gQplus,gQminus},
{\[Nu], \[Kappa], \[Lambda]0, \[CurlyPhi], rr, \[Beta], ks,      Pref, \[Kappa]s, sucRef, eporRef} = Mat // N;
   MC = (Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3 - Sin[\[CurlyPhi]]));   ME = (Sqrt[2] (6 Sin[\[CurlyPhi]]))/(3 (3 + Sin[\[CurlyPhi]]));
   P0s = state[[3]]; {suc, P0} = state[[7 ;; 8]];
   suc = Max[suc, 1]; P0 = Max[P0, 1];
   gQplus = Plot3D[ Sqrt[ -  MC^2 (P + sq3*ks*s) (P -  Pref (P0s/ Pref)^((\[Lambda]0 - \[Kappa])/(\[Lambda]0*((1 - rr)* Exp[-\[Beta]*s] + rr) - \[Kappa]))) ],
                   {P, -ks*P0,P0}, {s, -ks*suc, suc}, AxesLabel -> {"P", "suc", "Q"},  Mesh -> None, PlotRange ->{-P0*ME,P0*MC },
                    AspectRatio -> 1, PlotStyle-> {Yellow,Opacity[0.2] } ];
   gQminus = Plot3D[-Sqrt[- ME^2 (P + sq3*ks*s) (P -  Pref (P0s/ Pref)^((\[Lambda]0 - \[Kappa])/(\[Lambda]0*((1 - rr)* Exp[-\[Beta]*s] + rr) - \[Kappa])))],
                    {P, -ks*P0,P0}, {s,-ks*suc, suc}, AxesLabel -> {"P", "suc", "Q"},  Mesh -> None, PlotRange -> {-P0*ME,P0*MC},
                     AspectRatio -> 1, PlotStyle ->{ Yellow, Opacity[0.2] } ];
    Show[{ gQplus, gQminus} ,ViewPoint -> {0.7*P0, 4*suc, 0.2 P0} ]
   ];
isoCompressionPlot[path_, Mat_ ] := Module[{ Pes, gPes,P1,P2,gPoints, gNClineSuction },
     Pes = {#[[1]], #[[6]] }  &  /@  path;    P1 = { path[[1,1]]  , path[[1,6]] }; P2 = { path[[-1,1]]  , path[[-1,6]] };
     gPes = ListPlot[Pes,PlotStyle->PointSize[0.014`], PlotRange->All,AxesLabel->{"P","e"} ];
      gPoints =  Graphics[ {PointSize[Large], Red, Point[P1 ],PointSize[Medium], Orange, Point[P2]}] ;
      gNClineSuction  = NClineSuction[Mat,path[[-1]] ];
    Show[{ gPes,gPoints, gNClineSuction  }]
 ];
 epsQQPlot[path_, Mat_] := Module[{epsQQ, gepsQQ,P1,P2, gPoints },
             epsQQ =  {#[[5]], #[[2]] }  &  /@  path;   P1 = {path[[1,5]], path[[1,2]] }; P2 =  {path[[-1,5]], path[[-1,2]] };
             gepsQQ = ListPlot[epsQQ,PlotStyle->PointSize[0.014`],PlotRange->All,AxesLabel->{"\[Epsilon]Q","Q"} ];
             gPoints =  Graphics[ {PointSize[Large], Red, Point[ P1 ],PointSize[Medium], Orange, Point[ P2 ]}] ;
         Show[{  gepsQQ ,gPoints   } ]
 ];

 epPepQPlot[path_, Mat_] := Module[{epsPepsQ, gepsPepsQ,P1,P2,gPoints},
             epsPepsQ = {#[[9]], #[[10]] }  &  /@  path;  P1 = {path[[1,9]], path[[1,10]] }; P2 =  {path[[-1,9]], path[[-1,10]] };
             gepsPepsQ = ListPlot[epsPepsQ,PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{"\[Epsilon]pP","\[Epsilon]pQ"},DisplayFunction->Identity];
             gPoints =  Graphics[ {PointSize[Large], Red, Point[ P1 ],PointSize[Medium], Orange, Point[ P2 ]}] ;
              Show[{  gepsPepsQ ,gPoints   } ]
 ];
voidsucPlot[path_, Mat_] := Module[{epsPepsQ, gepsPepsQ,P1,P2,gPoints },
           epsPepsQ  = {#[[7]], #[[6]] }  &  /@  path;  P1 = {path[[1,7]], path[[1,6]] }; P2 =  {path[[-1,7]], path[[-1,6]] };
             gepsPepsQ  = ListPlot[epsPepsQ ,PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{"suction","e"},DisplayFunction->Identity];
             gPoints =  Graphics[ {PointSize[Large], Red, Point[ P1 ],PointSize[Medium], Orange, Point[ P2 ]}] ;
              Show[{   gepsPepsQ ,gPoints   } ]
 ];

epsPepsQPlot[path_, Mat_] := Module[{ epsPepsQ, gepsPepsQ,P1,P2,gPoints },
             epsPepsQ = {#[[4]], #[[5]] }  &  /@  path;   P1 = {path[[1,4]], path[[1,5]] }; P2 =  {path[[-1,4]], path[[-1,5]] };
             gepsPepsQ  = ListPlot[epsPepsQ,PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{"\[Epsilon]P","\[Epsilon]Q"},DisplayFunction->Identity];
             gPoints =  Graphics[ {PointSize[Large], Red, Point[ P1 ],PointSize[Medium], Orange, Point[ P2 ]}] ;
             Show[{   gepsPepsQ,gPoints   } ]
 ];

suctionPlot[path_, Mat_] := Module[{sucpl,gsucpl,P1,P2,gPoints},
             sucpl = {#[[1]], #[[7]]} & /@  path;   P1 = {path[[1,1]], path[[1,7]] }; P2 =  {path[[-1,1]], path[[-1,7]] };
             gsucpl = ListPlot[sucpl,PlotStyle->PointSize[0.014`],PlotRange->All,AxesLabel->{"P","suction"}];
             gPoints =  Graphics[ {PointSize[Large], Red, Point[ P1 ],PointSize[Medium], Orange, Point[ P2 ]}] ;
         Show[ { gsucpl , gPoints   } ]
 ];

PQs3DPlot[path_, Mat_ ] := Module[{ PQs3D, gPQs3D,P1,P2,gPoints,gSurface },
             PQs3D = {#[[1]], #[[7]], #[[2]] }  &  /@  path;  P1 = {path[[1,1]],path[[1,7]], path[[1,2]] }; P2 =  {path[[-1,1]],path[[-1,7]], path[[-1,2]] };
             gPQs3D  = ListPointPlot3D[PQs3D,AxesLabel->{"P","suc","Q"},PlotRange->All,PlotStyle->PointSize[0.014`],DisplayFunction->Identity,BoxRatios->1.0];
             gPoints =  Graphics3D[ {PointSize[Large], Red, Point[ P1 ],PointSize[Medium], Orange, Point[ P2 ]}] ;
             gSurface = NCsurfacePsQ[Mat, path[[-1]]];
               Show[{  gSurface,gPQs3D,gPoints } ]
 ];

Pse3DPlot[path_, Mat_] := Module[{ Pse3D, gPse3D ,P1,P2,gPoints, gNCsurface },
             Pse3D  = { #[[7]],#[[1]], #[[6]] }  &  /@  path; P1 = {path[[1,7]], path[[1,1]],path[[1,6]] }; P2 =  {path[[-1,7]], path[[-1,1]],path[[-1,6]] };
             gPse3D = ListPointPlot3D[Pse3D,AxesLabel->{"suc","P","epor"},PlotRange->All,PlotStyle->PointSize[0.014`],BoxRatios->1.0];
             gPoints =  Graphics3D[ {PointSize[Large], Red, Point[ P1 ],PointSize[Medium], Orange, Point[ P2 ]}] ;
             gNCsurface = NCsurface[Mat,path[[-1]] ];
              Show[{ gPse3D  , gNCsurface, gPoints   } ]
 ];




End[]

EndPackage[ ];

Print[ "****** BBMgraphics.m****** activated:", DateString[],"  
provides GRAPHIC MODULES:
plotBBMellipseSuction[ state_ , Mat_ ]  NClineSuction[Mat_,state_]
PQPlot[path_, Mat_]  epsQQPlot[path_, Mat_]
NCsurface[Mat_,state_]   NCsurfacePsQ[Mat_, state_]
epPepQPlot[path_, Mat_]    voidsucPlot[path_, Mat_]   
epsPepsQPlot[path_, Mat_]  suctionPlot[path_, Mat_]  
PQs3DPlot[path_, Mat_ ]   Pse3DPlot[path_, Mat_]   "
];
