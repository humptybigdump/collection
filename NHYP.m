(* ::Package:: *)

(*    Mathematica  tools for element tests with NHYP in the triaxial space  by W. Fuentes (2011)
This  notebook produces stress/strain path according to the New Hypoplastic model.
*)
BeginPackage["PQ`NHYP`"]
Off[General::spell,General::spell1,Solve::ifun];



Mat1={1.17,0.13,6,0.93,60000,0.75,31.2,2.9,1.8, 1.5*10^9,20,4.5};
ICond01={500,0,0.8, 500, 0,0,0};
Mat=Mat1;
ICond0=ICond0;
path={ICond0}; 
(*---------------------------------------------calculation + graphic--------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];
inRange[x_, range_] := IntervalMemberQ[Interval[range], x];
getElastic[p_,epor_,Mat_]:= Module[{elastG,elastK,ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne,r,ec,fd,fq,a,Yi, pmax,rK, Tbound, fs}, 
{ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne}=Mat;
r=(2.0+2.0*\[Nu])/(3.0-6.0*\[Nu]);  
\[CurlyPhi]=\[CurlyPhi]*\[Pi]/180;
ec=ec0*Exp[-(3*p/hs)^n];
fd=Exp[nd*(epor-ec)];
fq=Exp[-nq*(epor-ec)];
a=Sqrt[3]*(3-Sin[\[CurlyPhi]])/(2*Sqrt[2]*Sin[\[CurlyPhi]]);
Yi=(3*a*r)/(3+a^2);
pmax=(hs*(-Log[epor/ei0])^(1.0/n))/3.0;
rK=1+p/pmax*rKmax;
Tbound=fq/a;
elastK=rK*hs*(Log[(ei0/epor)])^(1/n-1)*(1+epor)/(3*n*epor)*(1+(3*Yi*fd*Sqrt[3])/(a*(3*Sqrt[3]*Tbound*(fq-fd)+2/Tbound/r)))^(-1);   
elastG=elastK/r;
Simplify[{{elastK,0},{0,3*elastG}}] ];  
RunNHYPTest[Mat_,ICond_,Loading_]:=Module[{state,ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne,MC,ME,M, p,q,epor,pc,itime,elastStiff,
                r,ec,fd,fq,a,Yi,ei, Fw,\[Eta],hTd,normhTd, Tcrit, Tdil, Tbound,c, Y, Dd, d\[Epsilon]p,d\[Epsilon]v, d\[Epsilon]q, \[Epsilon]v,\[Epsilon]q, \[Alpha],
               unknowns,solution,  d\[Epsilon]Trial ,dTTrial,PTrial,QTrial ,yield, mflow, epor1, TrialSolution,
                loadingQ , \[Lambda], p1,q1,pc1, eq1a, eq1b, eq2a ,eq2b, eq3, eq4, eq5,unknowns1, 
               approxTF, approx,approxi, unknowns1approxi ,solution1, dT2 ,d\[Epsilon]2,  \[Lambda]2,s1,s2,\[Lambda]s, Pss,P01,alpha, \[Epsilon]pP, \[Epsilon]pQ,P0s1,fs
               ,dT2tr ,d\[Epsilon]2tr,   ptr , qtr  ,eportr,seq1,seq2,seq3,seq4,seq5,seq6,seq7,seq8, \[Alpha]1,fs1,elastK,rK, pmax,\[Lambda]1,ErrorVars
               ,ErrorVars2},
{ntime, d\[Epsilon], dT}={ Loading[[1]], N[ Loading[[2]] ], N[Loading[[3]] ] };
state=Table[{0,0,0,0,0,0,0},{i,1,ntime}];
(*Material constants*)
{ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne}=Mat;
\[CurlyPhi]=\[CurlyPhi]*\[Pi]/180;
(*Initial conditions*)
{p,q,epor, pc, \[Alpha],\[Epsilon]v,\[Epsilon]q}=ICond;
If[p>pc && q==0, 
MessageDialog[{"p=",p, "> ", " pc=",pc ,". PROGRAM TERMINATED"}];
Abort[];];
For[itime=2, itime <= ntime,
state[[1]]=ICond;
(*Stress variables*)
\[Eta]=q/p;
hTd=\[Eta]*Sqrt[2/3]/3;
normhTd=Abs[\[Eta]]*Sqrt[2/3]/3;
(*Characteristic void ratios*)
ei=ei0*Exp[-(3*p/hs)^n];
ec=ec0*Exp[-(3*p/hs)^n];
(*Dilatancy and bounding factor*)
fd=Exp[nd*(epor-ec)];
fq=Exp[-nq*(epor-ec)];
(*Additional factors*)
a=Sqrt[3]*(3-Sin[\[CurlyPhi]])/(2*Sqrt[2]*Sin[\[CurlyPhi]]);
r=(2.0+2.0*\[Nu])/(3.0-6.0*\[Nu]);  
Yi=(3*a*r)/(3+a^2);
(*Elastic stiffness*)
elastStiff=getElastic[p, epor, Mat]; (*Elastic matrix*)
(*Image stress at critical, bounding and dilatancy surface*)
Fw=If[\[Eta]>0,1,1+q/(3*p)];
Tcrit=Fw/a;
Tbound=Tcrit*fq;
Tdil=Tcrit*fd;
(*Flow rule*)
mflow=({{3/Sqrt[3]*(Tdil-normhTd)},{(hTd/Tbound)*Sqrt[2/3]}});
(*Degree of non-linearity*)
c=(1-Yi)/Yi;
Y=(normhTd+Tbound)/(c*(Tbound-normhTd)+(normhTd+Tbound));
(*Scalar factor D*)
Dd=(-3*d\[Epsilon][[1]]+2*Sqrt[3/2]Abs[d\[Epsilon][[2]]]/Tbound/r)/(3*Sqrt[3]*Tbound*(fq-fd)+2/Tbound/r);
d\[Epsilon]p=Y*mflow*Dd;
unknowns=Select[Flatten[{d\[Epsilon],dT}],symbolQ];  
TrialSolution = Solve[ dT==elastStiff.d\[Epsilon] ,unknowns]; (*Trial elastic stress increment*)
{d\[Epsilon]Trial ,dTTrial}= {d\[Epsilon],dT}  /. TrialSolution[[1]];  (*Trial step solution*)
(*Set of equations*)
eq1a = dT[[1]]  == ( elastStiff. (d\[Epsilon] - d\[Epsilon]p) )[[1]]; 
eq1b = dT[[2]]  == ( elastStiff. (d\[Epsilon] - d\[Epsilon]p) )[[2]]; 
eq2a = p1  == p  + dT[[1]] ;  (*P implicit*)
eq2b = q1  == q  + dT[[2]]; (*Q implicit*)
eq3=epor1==epor-d\[Epsilon][[1]]*(1+epor);  (*epor implicit*)
unknowns1 =  {unknowns, p1,q1,epor1} //Flatten; 
approxTF = symbolQ[#]& /@  (Flatten[{d\[Epsilon], dT}]); 
approx = Pick[ Flatten[ {d\[Epsilon]Trial, dTTrial}], approxTF]; 
approxi  = {approx,p,q,epor} //Flatten ;
unknowns1approxi = Transpose[{unknowns1, approxi}];
solution1 = FindRoot[{eq1a,eq1b, eq2a,eq2b, eq3}, unknowns1approxi,AccuracyGoal-> 4 ] ;        
{dT2 ,d\[Epsilon]2,   ptr , qtr  ,eportr}={ dT ,  d\[Epsilon] ,  p1 , q1,  epor1}/. solution1;  
ErrorVars=Select[Flatten[{dT2 ,d\[Epsilon]2,   ptr , qtr  ,eportr}],symbolQ];    
If [Length[ErrorVars]>1, MessageDialog["Error 1"];Abort[];];
MC =(6.0* Sin[\[CurlyPhi]])/ (3-Sin[\[CurlyPhi]]) ;
(*Evaluation of the surface*)
fs=(qtr-ptr*\[Alpha])^2-(MC*Fw)^2*ptr^2*(1-(ptr/pc)^(1/2));
If[fs>0.0 ,   loadingQ = True, loadingQ=False] ;
(*Elastic step*)
If[loadingQ==False ,
p=ptr;
q=qtr;
epor=eportr;
pc=pc;
\[Alpha]=\[Alpha];
Goto[nextInc] ] ;
pmax=(hs*(-Log[epor/ei0])^(1.0/n))/3.0;
rK=1+p1/pmax*rKmax;
elastK=rK*hs*(Log[(ei0/epor1)])^(1/n-1)*(1+epor1)/(3*n*epor1)*(1+(3*Yi*fd*Sqrt[3])/(a*(3*Sqrt[3]*Tbound*(fq-fd)+2/Tbound/r)))^(-1); 
seq1=dT[[1]]==( elastStiff. (d\[Epsilon] - d\[Epsilon]p-\[Lambda]1*mflow))[[1]]; 
seq2=dT[[2]]==( elastStiff. (d\[Epsilon] - d\[Epsilon]p-\[Lambda]1*mflow))[[2]]; 
seq3 = p1  == p  + dT[[1]] ;  
seq4 = q1  == q  + dT[[2]]; 
seq5=0==(q1-p1*\[Alpha]1)^2-(MC*Fw)^2*p1^2*(1-(p1/pc1)^(1/2));
seq6=\[Alpha]1==\[Alpha]+\[Lambda]1*hk*(3*hTd-\[Alpha]);
seq7=pc1==pc+\[Lambda]1*hi*(ei-epor1)*(Abs[hTd]/Tbound)*(1-epor1/ei0)^ne+elastK/(rK-1)*\[Lambda]1*3/Sqrt[3]*(Tdil-normhTd);
seq8=epor1==epor-d\[Epsilon][[1]]*(1+epor);  (*epor implicit*)
unknowns1 =  {unknowns, p1,q1,\[Lambda]1, \[Alpha]1,pc1, epor1} //Flatten; 
approxTF = symbolQ[#]& /@  (Flatten[{d\[Epsilon], dT}]); 
approx = Pick[ Flatten[ {d\[Epsilon]2, dT2}], approxTF]; 
approxi  = {approx,p,q,0,\[Alpha],pc,epor} //Flatten ;
unknowns1approxi = Transpose[{unknowns1, approxi}];
solution1 = FindRoot[{seq1,seq2,seq3,seq4,seq5,seq6,seq7,seq8}, unknowns1approxi,AccuracyGoal-> 4 ] ;     
{dT2 ,d\[Epsilon]2,   p , q  ,epor,\[Lambda],pc,\[Alpha]}={ dT ,  d\[Epsilon] ,  p1 , q1,  epor1,\[Lambda]1,pc1,\[Alpha]1}/. solution1;   
ErrorVars2=Select[Flatten[{dT2 ,d\[Epsilon]2,   p , q  ,epor,\[Lambda],pc,\[Alpha]}],symbolQ];     
If [Length[ErrorVars]>1, MessageDialog["Error 2"];Abort[];];   
Goto[nextInc]; 
(*-----------------------------------*)
(*-----------------------------------*)
       
(*-----------------------------------*)
(*-----------------------------------*)

Label[nextInc];
{\[Epsilon]v,\[Epsilon]q} += d\[Epsilon]2;
state[[itime]]={p,q,epor,pc,\[Alpha],\[Epsilon]v,\[Epsilon]q};
(*Print[state];*)
itime++; 
];
state
]  ;


 PQPlot[Paths_, Mat_] := Module[{pointpcrit2,pointpcrit,pcrit,ec2,gsupFs3,gsupFs2,gsupFs4,q,p,pc,gsupFs,Fw,\[Alpha],supp,infp,inf,sup,epor,ec,gboundline,gdilline,fd,fq,Mc,Me,x,nPaths ,PQs, gPQs, gfirstSurface  , glastSurface, outputPlot, currentPath , iPath ,gsaturatedsurface ,ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq,hi,hk,ne ,M1,gCritLine,gCritLine2 },
{ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq,hi,hk,ne}=Mat;  
\[CurlyPhi]=\[CurlyPhi]*\[Pi]/180;

(*M1= (6.0* Sin[\[CurlyPhi]])/ (3-Sin[\[CurlyPhi]]) *)
Mc=(6*Sin[\[CurlyPhi]])/(3-Sin[\[CurlyPhi]]);
Me=-(6*Sin[\[CurlyPhi]])/(3+Sin[\[CurlyPhi]]);

infp=If[Min[Paths[[All,1]]]>0,0,0.95*Min[Paths[[All,1]]]];
supp=1.1*Max[Paths[[All,1]]];
(*Characteristic void ratios*)
ec=ec0*Exp[-(3*x/hs)^n];
(*Dilatancy and bounding factor*)
epor=Paths[[-1,3]];
fd=Exp[nd*(epor-ec)];
fq=Exp[-nq*(epor-ec)];
ec2=ec0*Exp[-(3*supp/hs)^n];
inf=1.1*Me*Exp[-nq*(epor-ec2)]*supp;
sup=1.1*Mc*Exp[-nq*(epor-ec2)]*supp;
gCritLine= Plot[Mc*x, {x, 0.001,supp},PlotStyle->{Red}];
gCritLine2= Plot[Me*x, {x, 0.001,supp},PlotStyle->{Red}];
gdilline=Plot[{fd*Mc*x,fd*Me*x}, {x, 0.001,supp},PlotStyle->{Green}];
gboundline=Plot[{fq*Mc*x,fq*Me*x}, {x, 0.001,supp},PlotStyle->{{Green, Thick},{Green, Thick}}];
If[epor>ec0,pcrit=0;,pcrit=(hs*(-Log[epor/ec0])^(1.0/n))/3.0;];
(*pointpcrit=Graphics[Circle[{pcrit,Mc*pcrit}]];*)
pointpcrit=Graphics[{Darker[Green],PointSize[0.03],Point[{pcrit,Mc*pcrit}],Text["SS",{pcrit,1.13*Mc*pcrit}]}];
pointpcrit2=Graphics[{Darker[Green],PointSize[0.03],Point[{pcrit,Me*pcrit}],Text["SS",{pcrit,1.18*Me*pcrit}]}];
pc=Paths[[-1,4]];
\[Alpha]=Paths[[-1,5]];
Fw=If[q/p>0,1,1+q/(3*p)];
gsupFs=ContourPlot[(q-p*\[Alpha])^2-(Mc*Fw)^2*p^2*(1-(p/pc)^(1/2))==0,{p,infp,supp},{q,inf,sup},ContourStyle->{Purple},Frame->True,FrameLabel->{"p [kPa]","q [kPa]"}];
gsupFs2=ContourPlot[q==p*\[Alpha],{p,infp,supp},{q,inf,sup},ContourStyle->{Purple, Dashed},AxesLabel->{"p [kPa]","q [kPa]"}];
gsupFs3=RegionPlot[(q-p*\[Alpha])^2-(Mc*Fw)^2*p^2*(1-(p/pc)^(1/2))<0,{p,infp,supp},{q,inf,sup},Frame->False, Axes->True,AxesLabel->{"p [kPa]","q [kPa]"}, PlotStyle->LightPurple];
gsupFs4=RegionPlot[{q>fq*Mc*x,q<fq*Me*x},{x,infp,supp},{q,inf,sup},Frame->False, PlotStyle->LightGreen,BoundaryStyle->None];
     If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
        PQs = Array[0&, nPaths];
        gPQs =  Array[0&, nPaths];
        If[nPaths==1,
           PQs[[1]] = {#[[1]], #[[2]] }  &  /@  Paths;
           gPQs[[1]] = ListPlot[PQs[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->{{infp,supp},{inf,sup}},AxesLabel->{"p [kPa]","q [kPa]"},DisplayFunction->Identity];

          
           outputPlot  = {gsupFs3,gsupFs4,gsupFs,gsupFs2, gCritLine,gCritLine2,gdilline,gboundline, gPQs[[1]],pointpcrit,pointpcrit2} ;
         ] ;
        If[nPaths > 1,
        For[iPath=1, iPath<=nPaths,
          currentPath =   Paths[[iPath]] ;
          PQs[[iPath]] = {#[[1]], #[[2]] }  &  /@  currentPath;
          gPQs[[iPath]] = ListPlot[PQs[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->{{infp,supp},{inf,sup}},AxesLabel->{"p [kPa]","q [kPa]"},DisplayFunction->Identity];
          (*gfirstSurface[[iPath]] = plotNHYPsurface[currentPath[[1,3]],Mat[[4]]];
           glastSurface[[iPath]] =  plotNHYPsurface[currentPath[[-1,3]],Mat[[4]]];*)

        iPath++;] ;
        outputPlot  = Flatten[{ gPQs} ];
         ];
        Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];




 oedometricPlot[Paths_, Mat_,ICond01_] := Module[{gei,supe,infe,supp,infp,e,nPaths=1,Pes, gPes, outputPlot, currentPath , iPath, elinesPlot,ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq,hi,hk,ne,p,eiPlot,ecPlot},
 {ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq,hi,hk,ne}=Mat;         
           [Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          Pes = Array[0&, nPaths];
          gPes =  Array[0&, nPaths];

infp=If[Min[Paths[[All,1]]]>0,0,0.95*Min[Paths[[All,1]]]];
supp=1.1*Max[Paths[[All,1]]];
infe=0.95*Min[Paths[[All,3]]];
supe=1.17*Max[Paths[[All,3]]];
eiPlot= Plot[ei0*Exp[-(3*p/hs)^n], {p, infp,supp},PlotStyle->{Green, Thick}];
ecPlot= Plot[ec0*Exp[-(3*p/hs)^n], {p, infp,supp},PlotStyle->{Red}];
gei=RegionPlot[{epor>ei0*Exp[-(3*p/hs)^n]},{p,infp,supp},{epor,infe,supe},Frame->False, PlotStyle->LightGreen,BoundaryStyle->None];
          If[nPaths==1,
             Pes[[1]] = {#[[1]], #[[3]] }  &  /@  Paths;
             gPes[[1]] = ListPlot[Pes[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->{{infp,supp},{infe,supe}},AxesLabel->{"p [kPa]", "e [-]"},DisplayFunction->Identity];           
               outputPlot  = {gPes[[1]],ecPlot,eiPlot,gei} ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            Pes[[iPath]] = {#[[1]], #[[3]] }  &  /@  currentPath;
            gPes[[iPath]] = ListPlot[Pes[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,AxesLabel->{"p [kPa]", "e [-]"},DisplayFunction->Identity];          
           iPath++;] ;
           outputPlot  = { Flatten[gPes,1],eiPlot,ecPlot} ;
           ]  ;
         Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];


 epsQQPlot[Paths_, Mat_] := Module[{ec2,infp2,infp,supp,ec,epor,fd,fq,nPaths=1,epsQQ, gepsQQ,  outputPlot, currentPath , iPath,ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq,hi,hk,ne,Mc,Me,inf,sup,infeps,supeps},
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
{ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq,hi,hk,ne}=Mat;  
\[CurlyPhi]=\[CurlyPhi]*\[Pi]/180;

(*M1= (6.0* Sin[\[CurlyPhi]])/ (3-Sin[\[CurlyPhi]]) *)
Mc=(6*Sin[\[CurlyPhi]])/(3-Sin[\[CurlyPhi]]);
Me=-(6*Sin[\[CurlyPhi]])/(3+Sin[\[CurlyPhi]]);
(*inf=0.95*Me*Max[Paths[[All,1]]];
sup=1.2*Mc*Max[Paths[[All,1]]];*)

infp=If[Min[Paths[[All,1]]]>0,0,0.95*Min[Paths[[All,1]]]];
supp=1.1*Max[Paths[[All,1]]];
(*Characteristic void ratios*)
ec2=ec0*Exp[-(3*supp/hs)^n];
(*Dilatancy and bounding factor*)
epor=Paths[[-1,3]];
fd=Exp[nd*(epor-ec)];
fq=Exp[-nq*(epor-ec)];
inf=1.1*Me*Exp[-nq*(epor-ec2)]*supp;
sup=1.1*Mc*Exp[-nq*(epor-ec2)]*supp;


infeps=1.1*Min[Paths[[All,7]]];
supeps=1.1*Max[Paths[[All,7]]];
          epsQQ = Array[0&, nPaths];
          gepsQQ =  Array[0&, nPaths];
          If[nPaths==1,
             epsQQ[[1]] = {#[[7]], #[[2]] }  &  /@  Paths;
             gepsQQ[[1]] = ListPlot[epsQQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->{{infeps,supeps},{inf,sup}},AxesLabel->{"\[Epsilon]q [-]","q [kPa]"},DisplayFunction->Identity, AspectRatio->1];
             outputPlot  = { gepsQQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsQQ[[iPath]] = {#[[7]], #[[2]] }  &  /@  currentPath;
            gepsQQ[[iPath]] = ListPlot[epsQQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->{{infeps,supeps},{inf,sup}},PlotRange->All,AxesLabel->{"\[Epsilon]q [-]","q [kPa]"},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsQQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];






epsvepsqPlot[Paths_, Mat_] := Module[{nPaths=1,epsPepsQ, gepsPepsQ,  outputPlot, currentPath , iPath ,infeps,supeps,inf,sup},
          If[Depth[Paths]==4,  nPaths=First[Dimensions[Paths] ], nPaths=1 ];
          epsPepsQ = Array[0&, nPaths];
          gepsPepsQ =  Array[0&, nPaths];
infeps=1.1*Min[Paths[[All,7]]];
supeps=1.1*Max[Paths[[All,7]]];
inf=0.95*Min[Paths[[All,6]]];
sup=1.1*Max[Paths[[All,6]]];
          If[nPaths==1,
             epsPepsQ[[1]] = {#[[7]], #[[6]] }  &  /@  Paths;
             gepsPepsQ[[1]] = ListPlot[epsPepsQ[[1]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->{{infeps,supeps},{inf,sup}},AxesLabel->{"\[Epsilon]q [-]","\[Epsilon]v [-]"},DisplayFunction->Identity];
             outputPlot  = { gepsPepsQ[[1]] } ;
           ] ;
          If[nPaths > 1,
          For[iPath=1, iPath<=nPaths,
            currentPath =   Paths[[iPath]] ;
            epsPepsQ[[iPath]] = {#[[7]], #[[6]] }  &  /@  currentPath;
            gepsPepsQ[[iPath]] = ListPlot[epsPepsQ[[iPath]],PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->{{infeps,supeps},{inf,sup}},AxesLabel->{"\[Epsilon]q [-]","\[Epsilon]v [-]"},DisplayFunction->Identity];
           iPath++;] ;
            outputPlot  = Flatten[{ gepsPepsQ  } ,1];
           ]  ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];




 inputCheck[Mat_, Icond_, Loading_]:= Module[ { dT,p,q,epor, pc, \[Alpha],\[Epsilon]v,\[Epsilon]q, ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne, ntime, deP, deQ, dP, dQ, suc, P0,\[Epsilon]pP,\[Epsilon]pQ },

{ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne}=Mat;
\[CurlyPhi]=\[CurlyPhi]*\[Pi]/180;
{p,q,epor, pc, \[Alpha],\[Epsilon]v,\[Epsilon]q}=Icond;

 { ntime, {deP,deQ}, {dP,dQ} } = { First[Loading], N[ Loading[[2]] ], N[ Loading[[3]] ] }    ;

  If[symbolQ[ntime] , Print[ "number of increments  ntime =  ", ntime," undefined"   ]]  ;
  If[ ntime < 10 , Print[ "suspiciously small number of increments  ntime =  ", ntime   ]]  ;
  If[ deP > 0.005 || deQ > 0.005 , Print[ "Strain increments seem to be large {deP, deQ} = ", {deP, deQ}  , " Keep them at  0.1% level"   ]]  ;
  If[ dP > 10 || dQ > 10 , Print[ "Stress increments seem to be large {dP, dQ} = ", {dP, dQ}  , " Keep them below  10 kPa "   ]]  ;
  If[ P0s *( 1+ epor )^(1/ \[Lambda]0 ) < 5000, Print[ "Warning: epor=0 can be reached already at about P0s =  ",  P0s *( 1+ epor )^(1/ \[Lambda]0 ) ,
        "  Try to reduce \[Lambda]0 = ",   \[Lambda]0 ] ]   ;

  If[ei0 ~inRange~ {0.5, 2.0} ,"" , Print[ "Suspicious ei0 =  ",ei0   ]]  ;
  If[ \[Nu] ~inRange~ {0., 0.5} , "", Print[ "Suspicious \[Nu] =  ",\[Nu]  ]]  ;
  If[rKmax ~inRange~ {  0 , 15 } , "", Print[ "Suspicious rKmax =  ",rKmax ]]  ;

 If[ec0 ~inRange~ {  0.4 , 2.0 } , "", Print[ "Suspicious ec0=  ",ec0]]  ;
 If[hs ~inRange~ {  0 , 100000000 } , "", Print[ "Suspicious hs =  ",hs]]  ;
 If[n ~inRange~ {  0 , 1 } , "", Print[ "Suspicious n =  ",n ]]  ;
  If[ nd ~inRange~ {0, 5} , "", Print[ "Suspicious nd=  ", nd  ]]  ;
If[ nq ~inRange~ {0, 5} , "", Print[ "Suspicious nq=  ", nq  ]]  ;
If[ hi ~inRange~ {0, 1000000000000000000000000} , "", Print[ "Suspicious hi=  ", hi  ]]  ;
If[ hk ~inRange~ {0, 10000} , "", Print[ "Suspicious hk=  ", hk  ]]  ;
If[ ne ~inRange~ {0, 10} , "", Print[ "Suspicious ne=  ", ne  ]]  ;
  
  If[p < 0 , Print[ "Warning: negative initial  p =  ", p   ]]      ;
  If[pc < p , Print[ "Warning:  initial  pc =  ", pc , "<", p, " =p" ]]  ;
  If[epor  ~inRange~ {0.01, 3} , "", Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 3} "  ]];

  (* dT = {dP,dQ}
If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True] > 2,  Print["Error: Too many unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Count[ Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ], True]  < 2,  Print["Error: Too few unknowns in Loading = ", Loading ]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, True, False, True},  Print["Error: You cannot prescribe deP and dP in Loading simultaneously"]; Beep[]; Abort[]; ];
  If[Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == {True, False, True,False},  Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously"]; Beep[]; Abort[]; ];
 PrescribedStressPath  =  Evaluate[ symbolQ[#] & /@ {deP, deQ, dP, dQ} ] == { False, False, True, True} ;*)


];

 controlPanelNHYP[] := Module[{Loading},
(*Default values for initial visualization of graphics*)
Mat=Mat1;
ICond0=ICond01;
path={ICond0}; 
Manipulate[
         Switch[ TEST, "Iso", {d\[Epsilon]v, d\[Epsilon]q, dp, dq, ntime} = {0.00001, 0, xdp, xdq, 100};, 
                   "TriaxU", {d\[Epsilon]v, d\[Epsilon]q, dp, dq, ntime} = {0, 0.00005, xdp, xdq, 100};,
                   "TriaxD", {d\[Epsilon]v, d\[Epsilon]q, dp, dq, ntime} = {xdev, xdeq, 1, 3, 100}; ,
                   "Manual", {d\[Epsilon]v, d\[Epsilon]q, dp, dq, ntime} = {d\[Epsilon]v, d\[Epsilon]q, dp, dq, ntime};
                     "Oed", {d\[Epsilon]v, d\[Epsilon]q, dp, dq, ntime} = {0.0001, 0.000066667, xdp, xdq, 100};
                     "pConstant", {d\[Epsilon]v, d\[Epsilon]q, dp, dq, ntime} = {xdev, xdeq, 0, 1, 100};
            ];
Switch[ Left, "none",g1x="empty" ;,"e-s", g1x=g2;,"\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)",g1x=g6;, "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)", g1x=g7;, "P-Q-s", g1x=g8;, "P-s-e", g1x=g9;];
Switch[ Right, "none",g2x="empty";,"e-s", g2x=g2;,"\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)",g2x=g6;, "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)", g2x=g7;, "P-Q-s", g2x=g8;, "P-s-e", g2x=g9;];
(*g1=Show[{PQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ];
g2=Show[{voidsucPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 7]], path[[1, 6]]} ],PointSize[Medium], Orange,  Point[ { path[[-1, 7]], path[[-1,6]] } ]}] } ];
g3=Show[{epsQQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 4]], path[[1, 2]]} ],PointSize[Medium], Orange, Point[ { path[[-1, 4]], path[[-1, 5]]} ]} ] } ] ;
g4=Show[{oedometricPlot[path, Mat,ICond0], Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 6]] }],PointSize[Medium], Orange, Point[{path[[-1, 1]], path[[-1, 6]] } ] }] } ];
g5=Show[{suctionPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Medium], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ];
g6=Show[{epsPepsQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 4]], path[[1, 5]]} ],PointSize[Medium], Orange, Point[ { path[[-1, 4]], path[[-1, 5]] } ]} ] } ] ;
g7=Show[{epPepQPlot[path,Mat],Graphics[{PointSize[Large],Red,Point[{path[[1,9]],path[[1,10]]}],PointSize[Medium],Orange,Point[{path[[-1,9]],path[[-1,10]]}]}]}];
g8=Show[{PQs3DPlot[path,Mat,ICond0],Graphics3D[{PointSize[Large],Red,Point[{path[[1,1]],path[[1,7]],path[[1,2]]}],PointSize[Medium],Orange,Point[{path[[-1,1]],path[[-1,7]],path[[-1,2]]}]}]}];
g9=Show[{Pse3DPlot[path,Mat,ICond0],Graphics3D[{PointSize[Large],Red,Point[{path[[1,1]],path[[1,6]],path[[1,7]]}],PointSize[Medium],Orange,Point[{path[[-1,1]],path[[-1,6]],path[[-1,7]]}]}]}];
(*Show[GraphicsGrid[{{g1,g3},{g4,g5},{If[Left=="none"," ",g1x],If[Right=="none"," ",g2x]}},ImageSize -> Scaled[.5]]],*)
*)

g1=Show[{PQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[path[[1, 1 ;; 2]] ],PointSize[Large], Orange, Point[path[[-1, 1 ;; 2]] ]}] } ];
g3=Show[{epsQQPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 7]], path[[1, 2]]} ],PointSize[Large], Orange, Point[ { path[[-1, 7]], path[[-1, 2]]} ]} ] } ] ;
g4=Show[{oedometricPlot[path, Mat,ICond0], Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 3]] }],PointSize[Large], Orange, Point[{path[[-1, 1]], path[[-1, 3]] } ] }] } ];
g6=Show[{epsvepsqPlot[path, Mat], Graphics[ {PointSize[Large], Red, Point[ { path[[1, 7]], path[[1, 6]]} ],PointSize[Large], Orange, Point[ { path[[-1, 7]], path[[-1, 6]] } ]} ] } ] ;

Show[GraphicsGrid[{{g3,g1},{g6,g4}},ImageSize -> Scaled[.7]]],
 Delimiter, 
"Reference model",
 {{ei0, 1.17}}  ,
 {{\[Nu], 0.13}}  ,
 {{rKmax, 6.0}}  ,
 {{ec0, 0.93}}  ,
 {{hs, 60000}}  ,
 {{n, 0.75}}  ,
 {{\[CurlyPhi], 31.2}}  ,
 {{nd, 2.9}}  ,
 {{nq, 1.8}}  ,
"Surface parameters",
 {{hi,1.5*10^9}}  ,
 {{hk, 20}}  ,
 {{ne, 4.5}}  ,
 Delimiter,
"Initial conditions:",
 {{p, 500}}  ,
 {{q, 0.0}}  ,
 {{epor, 0.8}}  ,
 {{pc, 500}}  ,
 {{\[Alpha], 0}}  ,
(*{P,Q,P0s,\[Epsilon]P,\[Epsilon]Q,epor, suc, P0}=ICond*)
 Delimiter,
"Number of steps:",
 {{ntime, 50}},
 Delimiter,
 "Test control (stress in kPa)",
{{TEST ,"Manual"},{"Manual","TriaxU", "TriaxD", "Iso", "Oed", "pConstant"},ControlType-> PopupMenu },
(*{{TEST ,"Manual"},{"Manual","TriaxU", "TriaxD", "Iso"},ControlType-> RadioButtonBar },*)
 {{d\[Epsilon]v, 0.0001}},
 {{d\[Epsilon]q, 0}}  ,
 {{dp, xdp}}  ,
 {{dq, xdq}}  ,
Delimiter,
 Button["Check the input",
Mat={ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne};
ICond0={p,q,epor, pc, \[Alpha],0,0};
   d\[Epsilon]P = If[NumberQ[d\[Epsilon]Pp ] ,  d\[Epsilon]Pp /100, d\[Epsilon]Pp ]  ;
   d\[Epsilon]Q = If[NumberQ[d\[Epsilon]Qp ] , d\[Epsilon]Qp /100, d\[Epsilon]Qp ]  ;
  inputCheck[Mat, ICond0, {ntime, {d\[Epsilon]P, d\[Epsilon]Q}, {dP, dQ}} ]
 ,ImageSize->250, Background->LightGreen],
   Button["Calculate step",
Mat={ei0,\[Nu],rKmax,ec0,hs,n,\[CurlyPhi],nd,nq, hi,hk,ne};
ICond0={p,q,epor, pc, \[Alpha],0,0};
If[Length[path] == 1, path = Array[0 &, {1, 7}]; path = {ICond0}; lastrow=1;]; (* Initialisation of global variable Path *)
If[path[[lastrow,All]] != ICond0, path = Join[ path,  {ICond0} ];lastrow=Length[path];MessageDialog["The initial conditions have been changed. A new test will be perform."];];
Loading={ntime,N[ {d\[Epsilon]v, d\[Epsilon]q }] , N[ {dp, dq}]};  
(*Loading = { ntime,-ICond0[[7]]/ntime, N[ {d\[Epsilon]P, d\[Epsilon]Q }]  ,N[ {dP, dQ}] };*)
dpath =  RunNHYPTest[Mat,Last[path],Loading]  ;
path = Join[ path,  Delete[ dpath, 1]] ;
 If[Count[Evaluate[symbolQ[#] & /@ {d\[Epsilon]v, d\[Epsilon]q, dp, dq}], True] != 2,
  MessageDialog["Error: system of equations cannot be solved. Exactly two unknowns are needed." ];  path = Drop[path, -ntime]; Abort[]; 
  ] 
 ,ImageSize->250, Background->LightGreen],
(*Delimiter,
"Additional Graphs:",
{{Left,"none"},{"none","e-s", "\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)", "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)", "P-Q-s", "P-s-e"},ControlType-> RadioButtonBar },
 {{Right,"none"},{"none","e-s", "\!\(\*SubscriptBox[\"\[Epsilon]\", \"P\"]\)-\!\(\*SubscriptBox[\"\[Epsilon]\", \"Q\"]\)", "\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"P\"]\)-\!\(\*SubscriptBox[SuperscriptBox[\"\[Epsilon]\", \"P\"], \"Q\"]\)","P-Q-s", "P-s-e"},ControlType-> RadioButtonBar },
*)Delimiter,
Button["Undo the last ntime increments",
  If[Length[path] >= ntime, path = Drop[path, -ntime];,MessageDialog["No more increments can be deleted." ] ];
 ,ImageSize->250, Background->LightGreen],

Button["Print the final state",
MessageDialog[{"FINAL STATE :                                                                               
"  ,"  p=" ,path[[-1,1]] ,"   q=", path[[-1,2]] ,"   epor=", path[[-1,3]] ,"   pc=", path[[-1,4]] ,"   \[Alpha]=" ,path[[-1,5]]}]; 
(* Print["(FINAL STATE) {p,q}= ", path[[-1,1 ;; 2]]," ,  epor= ", path[[-1,3]], " ,  pc= ", path[[-1,4]], " , \[Alpha]= ", path[[-1,5]]
  ];*)
 ,ImageSize->250, Background->LightGreen],

Button["Reset all steps, graphics and delete cell",
  path = {ICond0}; ntime = ntime;  d\[Epsilon]v = d\[Epsilon]v ; d\[Epsilon]q = d\[Epsilon]q; dp = xdp; dq = xdq; ;
 ,ImageSize->250, Background->LightGreen]
 ]
];




EndPackage[ ]
$Context="PQ`NHYP`";
