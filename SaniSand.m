(* ::Package:: *)

(*  Mathematica  tools for element tests with  SaniSand in isomorphic PQ space  by A.Niemunis 2015
    according to the paper by Dafalias and Manzari 2004
    This notebook is for didactic purposes  only.  Use it at your own risk, I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory. It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory constitutive there
3) copy this file (=  SaniSand.m) to the new directory.
4) Begin a new Mathematica session with:    Needs["PQ`SaniSand`"]
*)


BeginPackage["PQ`SaniSand`"]
(*  Off[General::spell,General::spell1,Solve::ifun];   *)
(*---------------------------------------------usages--------------------------------------------------------------------*)
states::usage=" e.g. Last[states] takes the current value from th global list of states consisting of records
{eP, eQ, P, Q, epor, alpha, alphain, zet} each.  \
Note that states is an identical global variable as path but it is produced in a single step[ ] or cycle[ ].
path will be updated in controlPanelSaniSand[ ]. After a step is executed its states are joined to path. ";

step::usage = "Syntax:  step[ mat,  loading, ninc]  or   step[ mat,  loading, {Hold[criterion]} ]      \n \
is a loop calling  increment[mat,  loading ] and writing the updated states to states \n \
it is assumed that the constitutive routine increment[mat,  loading ]  reads the current state using   {...} = Last[ states ] \n\
In the form with criterion, e.g.   step[ mat,{0, 0.001 ,xdP, xdQ }, { Hold[ states[[-1,4]] < 15] } loading is continued until \n
as long as the 4th component of states is below  15.  See also cycle[] and increment[].
";

cycle::usage = "Syntax:  cycle[mat_,  loading1_ ,ninc1_,loading2_ ,ninc2_,ncyc_] or  \n \
cycle[ mat,loading1, { Hold[ criterion1 ] }, loading2 , { Hold[criterion2]  }, 18 ];   \n
is a loop calling  two step[ ] procedures per cycle.
It is assumed that the constitutive routine increment[mat,  loading ]  reads the current state using   {...} = Last[ states ] \n
In the form with criterion, e.g.
cycle[ mat,{0, -0.001 ,xdP, xdQ }, { Hold[ states[[-1,4]] > -15] }, {0, 0.001 ,xdP, xdQ } , { Hold[states[[-1,4]]  < 27]  } ,10 ];
ten undrained cycles with strain control are performed  between Q=-15 and Q=27.
See also step[] and increment[]
";

pickListPlot::usage = "Syntax: pickListPlot[ alist, {#[[1]] ,  #[[2]] + #[[3]]  } ] from a 2D alist  plots a combination of its columns    \
              according to the pattern {x,y}. Usually pickListPlot  plots states = {{e1,e2,s1,s2,....}, {e1,e2,s1,s2, ...}, {}, }    \
               Alternative sytnax is:    pickListPlot[ list,  1, 2 ]  to plot the 2nd column  over 1st column. In both version     \
               you may add y2 for comparison,  e.g.  {#[[1]] ,  #[[1]] + #[[2]] , #[[3]] - #[[2]] } Options for Graphics    \
               can be set via variable  gOptions={.....} , per default: gOptions = {PlotRange \[Rule] All}    ";

symbolQ::usage = "Syntax: symbolQ[t_]:=\[Not]NumberQ[t] ";

numberQ::usage=" Syntax numberQ[ x ] similar to NumberQ but works also with  lists and arrays. True means all components are numerical ";

realQ::usage=" Syntax realQ[ x ]  works also with  lists and arrays. True means all components are numerical and real ";

getHypoelasticE::usage = "Syntax: getHypoelasticE[state_,mat_]. Given the current state,   the barotropic + pycnotropic hypoelastic  stiffness is returned ";

getNM::usage = "Syntax:  {nb,mb} = getNM[state_?numberQ, mat_?numberQ] returns two unit directions: loading direction and flow rule  " ;

explicitIncrement::usage="Syntax: updated = explicitIncrement[ mat_?numberQ, {dePin_,deQin_,dPin_,dQin_}  ,deMax_:0.004 ]
                          takes the state as the last record from the global variable states and applies the increment
                          (two of its components must be analytical)  "

increment::usage = "Syntax: increment[ mat_, loading_ ,deMax_:0.002 ] reads global Last[states] returns the updated state.    \
     Neohypoplastic model is used calling getHyperelasticE  getY  getm. Two increments in loading are symbolic and the     \
     other two are numeric (mixed or stress control possible). Explicit time integration  is used. The global variable states is not updated. ";

path::usage="path is a global variable consisting of chronological list of states from all steps. \
               Each state consists  of the following state variables:  \
             \!\(\*
StyleBox[\"{\", \"Input\"]\)eP,eQ,P,Q,epor,alpha,alphaIn,zet}. The first state is called ICond0  and consists of the initial conditions\!\(\*
StyleBox[\" \", \"Input\"]\) \
              further states are joined to path in controlPanelSaniSand[].
               states is also an identical list of states  but it   will be updated by step[...] or by cycle[...] only."
mat::usage="mat is a list of three sublists mat[[1]]  ,mat[[2]]  ,mat[[3]] containing the material constants: \
             mat[[1]] = {patm=100, Patm=173, Gel0=125, nu=0.05}
             mat[[2]]  =  { Mc = 1.25,     m=0.01,       mbar = iso*m ,enb=1.1,end=3.5,  xi=0.7,lambdac=0.019,ec0=0.934,cLode=0.712,Ad0=0.704 } \
             mat[[3]]  =  {h0=7.05, ch=0.968, cz = 600.0, zmax= 4.0}  \
"


(*  ------------ below old usages --------------  *)


PQPlot::usage = "PQPlot[path , mat ] or  PQPlot[{path1,path2,.. }, mat ]  plots the stress path in the isomorphic P-Q-space.  ";

epsQQPlot::usage = "epsQQ[path , mat ] or  epsQQ[{path1,path2,.. }, mat ]  produces an isomorphic epsQ-Q diagramm.   ";

oedometricPlot::usage = "oedometricPlot[path, mat] or  oedometricPlot[{path1,path2,... } , mat] plots the compression curve  in   P-e-space.   \
\n The isomorphic P is used and the P-axis is not logarithmic.  The void ratio e is also linearly scaled ";

controlPanelSaniSand::usage = "controlSaniSand[ ] loads a graphic interface to perform element tests using SaniSand.   \
\n The initial conditions must be saved in the first row of the global variable path. \
\n Two stress or strain or mixed increments must be prescribed. Push the button Calculate to see the results after the increment.  \
\n The number of increments is ntime and should be modified writing a new number in the cell ntime. \
\n If you want to start with a brand new simulation, click on Reset or  set path = {ICond0} in a separate Mma Cell.  \
\n The button Undo removes the last ntime increments from the path.   \
\n New users are strongly advised to use the button  Check  before the first launch of Calculate. ";

(*--------------------------------------------- small routines --------------------------------------------------------*)
symbolQ[t_]:=!NumberQ[t];

inRange[x_, range_] := IntervalMemberQ[Interval[range], x];

numberQ[ x_] := Module[{n, q, xflat},
                xflat = x; If[Length[xflat] > 0 , xflat =  Flatten[xflat]; ];
                n= Length[xflat];
               If[ n==0, q = NumberQ[xflat] ];
               If[ n>0,  q = (n ==Length[ Select[xflat,NumberQ]] )];
               q   ];

realQ[ x_] := Module[{n, q, xflat},
               If[ Not[ numberQ[x] ], q= False ; Goto[exitealQ] ]   ;
                xflat = x; If[Length[xflat] > 0 , xflat =  Flatten[xflat]; ];
                n= Length[xflat];
               If[ n==0, q = Element[xflat, Reals ]];
               If[ n>0,  q =  And @@( Element[#,Reals]&  /@  xflat )    ];
               Label[exitealQ];
               q   ];

step[ mat_,  loading_ , ninc_] := Do[  AppendTo[ states, increment[mat,  loading ] ]; If[debug, Print[Length[states] ]; ];,   {iinc ,1,ninc}    ];
step[ mat_,  loading_ , { crit_ } ] := While[ ReleaseHold[crit],  AppendTo[ states, increment[mat,  loading ] ]; If[debug, Print[Length[states] ]; ];   ];
cycle [mat_,  loading1_ ,ninc1_,loading2_ ,ninc2_,ncyc_]  :=Do[ step[mat , loading1 ,ninc1] ;step[mat , loading2 ,ninc2 ] ,{icyc,1,ncyc}];
cycle [mat_,  loading1_ , {critL_},  loading2_,  {critUL_} ,ncyc_]  :=Do[ step[mat , loading1 ,{critL } ] ; step[mat , loading2 ,{critUL} ] ,{icyc,1,ncyc}];
increment[ mat_, {deP_,deQ_,dP_,dQ_,dQdP_} ,deMax_:0.002 ]:= Module[{ updated, implicit = False }, (* implicit not implemented as yet *)
If[ implicit  , updated = implicitIncrement[ mat, {deP,deQ,dP,dQ,dQdP} ,deMax ],
                updated = explicitIncrement[ mat, {deP,deQ,dP,dQ,dQdP} ,deMax ],
                updated = explicitIncrement[ mat, {deP,deQ,dP,dQ,dQdP} ,deMax ]
 ];
updated
];

increment[ mat_, {deP_,deQ_,dP_,dQ_} ,deMax_:0.002 ]:= Module[{ updated,implicit = False  },   (* implicit not implemented as yet *)
If[ implicit  , updated = implicitIncrement[ mat, {deP,deQ,dP,dQ} ,deMax ],
                updated = explicitIncrement[ mat, {deP,deQ,dP,dQ} ,deMax ],
                updated = explicitIncrement[ mat, {deP,deQ,dP,dQ} ,deMax ]
 ];
updated
] ;


(*--------------------------------------------- main calculation--------------------------------------------------------*)

getHypoelasticE[state_,mat_]:=Module[{P,Q,epor,patm,Patm,Gel,Kel, Gel0, nu},
{P,Q,epor}=state[[3;;5]];  If[P <=0, Print["error getHypoelasticE: P=",P];  P=0.1; Q=0.0; debug=True; ]   ;
{patm, Patm, Gel0, nu}=mat[[1, 1;;4]];  (* if patm = 100 then Patm = 173 *)
Gel = Gel0* patm*(2.97 - epor)^2 / (1+epor) *  Sqrt[ P/Patm ];
Kel = Gel * (2 + 2 nu)/(3 - 6 nu );
{{3*Kel, 0 },{0, 2*Gel}}
];

getNM[state_?numberQ, mat_?numberQ]:=Module[{P,Q,epor,alpha,zet,Patm,Mc,m,mbar, enb,end,cLode, sq2d3=Sqrt[2.0]/3, sq3=Sqrt[3.0],
                                              nQ,  lambdac,ec0 ,xi, psi,nb, mb, Md, Bdilat,Dilat, alphadilat, alphain, g, Ad, Ad0 },
{P,Q,epor,alpha,alphain,zet}= state[[3;;8]] ;
Patm = mat[[1, 2]];
{ Mc,m,mbar,enb,end,xi,lambdac,ec0,cLode,Ad0 } = mat[[2, 1;;10 ]];
nQ  =  Sign[ Q -  alpha *P ]; If[ nQ == 0, nQ = 1] ;
nb = {-alpha nQ - mbar   ,  nQ } // Normalize ;
If[ nQ > 0, g= 1   ,   g=cLode ];
If[ nQ > 0, Bdilat = 1+1.5*(1 - cLode)/cLode   ,   Bdilat=1 - 1.5*(1-cLode) ];
psi = epor - (ec0 - lambdac * (P/Patm)^xi );
Md = Mc * Exp[ end*psi];
alphadilat = nQ*sq2d3*( g*Md - m ) ;
If[ zet*nQ > 0,  Ad = Ad0 (1+zet*nQ), Ad=Ad0];
Dilat = -Ad* (alphadilat - alpha)*nQ   ;
mb = { -Dilat/sq3, Bdilat*nQ} // Normalize;   (* minus added *)
{nb, mb}
];

explicitIncrement[ mat_?numberQ, {dePin_,deQin_,dPin_,dQin_}  ,deMax_:0.004 ]:= Module[
  {state,stateBeforeYield,updated, eP,eQ,P,Q,epor,alpha,alphaIn,zet, deP,deQ,dP,dQ, unknowns,EE,EEP,soluPre,soluPlast,
   dePe, deQe, dPe,dQe, depore, Pe,Qe, yield,       h1,h2,h,   dePr,deQr,dPr,dQr, nQ,fprime,fgrave,g,  nb,mb,psi,Mb,Mc,
   aa,hard,  alphabar, alphabound, Kmod, dlambda, dalpha, dzet,depor, patm, alphaold,
   m,mbar,cLode,lambdac,xi, ec0,enb,end,Ad0,h0,ch,  Gel0,nu,Patm,  zmax,cz,sq2d3=Sqrt[2.0]/3, sq3=Sqrt[3.0]  },
 state  = Last[states];  (* states is a global variable *)

 {eP, eQ, P,Q,epor,alpha,alphaIn,zet} = state[[1;;8]]; If[P<0|| epor<0.1,Print["error implicitIncrement, P=",P,"  epor=",epor]];

 (* read all material constants *)
 {patm,Patm, Gel0, nu}=mat[[1, 1;;4]];  (* if patm = 100 then Patm = 173 *)
 {Mc,m,mbar,enb,end,xi,lambdac,ec0,cLode,Ad0 } = mat[[2, 1;;10 ]];
 {h0, ch,cz, zmax}  = mat[[3,1;;4]];

(* get elastic predictor *)
{deP,deQ,dP,dQ} = {dePin,deQin,dPin,dQin} ;
unknowns=Select[ {deP,deQ,dP,dQ} , symbolQ ];
EE = getHypoelasticE[state ,mat];
soluPre = Solve[  {dP,dQ}  ==  EE. {deP,deQ} , unknowns ][[1]];
epor = state[[5]];
{dePe, deQe, dPe,dQe, depore} =  {deP, deQ, dP,dQ, (1+epor)*Exp[- deP*Sqrt[3]] -1  - epor } /.  soluPre ;  (* elast. increments *)
{Pe,Qe} = {P,Q} + {dPe,dQe}  ;
yield   =   ( Qe - alpha *Pe )^2  - mbar^2*Pe^2  ;   (* crit of el. predictor *)
If[yield<=0, updated = state + {dePe,deQe,dPe,dQe,depore,0,0,0};

If[debug, AppendTo[debugInfos, {"elastic", Pe,Qe, yield } ]]; Goto[exitIncrement]  ] ;

(* PLAST 1: elastic return mapping, partial update, find remaining increment  *)
h= 0;
If[Qe > alpha Pe,  h= (Q-alpha P-mbar P )/(alpha dPe-dQe+dPe *mbar) ];
If[Qe < alpha Pe,  h= (Q-alpha P+mbar P )/(alpha dPe-dQe-dPe *mbar)  ];
{eP, eQ, P,Q, epor} +=  h *{dePe, deQe, dPe,dQe,depore};
stateBeforeYield = state;  stateBeforeYield[[1;;5]] = {eP, eQ, P,Q, epor};
{dePr,deQr,dPr,dQr} = If[NumberQ[#],# * (1-h), #  ] & /@ {deP,deQ,dP,dQ} ; (* remaining increment after elastic piece *)

(* PLAST 2: preparation for hardening modulus *)
nQ = Sign[Q - alpha P ];
fprime = { -alpha * nQ - mbar,  nQ };
fgrave = - P nQ;
If[ nQ > 0, g= 1, g=cLode ];
nb = {0,0}; mb={0,0};
{nb, mb} = getNM[stateBeforeYield, mat ];

psi = epor - (ec0 - lambdac * (P/Patm)^xi );
Mb = Mc * Exp[ -enb*psi];
alphabound = nQ*sq2d3*( g*Mb - m ) ;

aa = (alpha - alphaIn )*nQ ;
If[aa <  0,
        alphaIn = alpha;  (* new reversal *)
        aa =  0.001 ;
     ];
If[aa < 0.001, aa = 0.001];
hard = Gel0* h0*( 1- ch*epor)/ (P/Patm * aa /sq3 );
alphabar =    (2/3)*(alphabound - alpha) *hard ;
Kmod = - fgrave/ Norm[ fprime ] * alphabar ;
 EEP = EE - Outer[Times, (EE.mb) , (nb.EE) ] / ( Kmod + nb.(EE.mb));
unknowns=Select[ {dePr,deQr,dPr,dQr}  , symbolQ ];
soluPlast = Solve[{dPr,dQr}  ==  EEP. {dePr,deQr} , unknowns ][[1]];
{deP,deQ,dP,dQ} =  {dePr,deQr,dPr,dQr} /. soluPlast ;
dlambda = (nb.EE).{deP,deQ}/ ( Kmod + (nb.EE).mb);
If[dlambda < 0, Print["Error in explicitIncrement:  dlambda =", dlambda, "  (nb.EE).{deP,deQ} = ", (nb.EE).{deP,deQ} ,
               "   Kmod + (nb.EE).mb)=" , Kmod + (nb.EE).mb , "nb=",nb, "{deP,deQ}= ",{deP,deQ}, "EE=", EE ]; Abort[];];

dalpha = dlambda * alphabar ;
If[ mb[[1]] > 0,  dzet = 0, dzet =   -cz *( dlambda *(-mb[[1]] *sq3)  ) (zmax * nQ + zet)  ]; (* only plast dilatancy matters  *)
depor = (1+epor)*Exp[- deP*Sqrt[3]] -1  - epor   ;
If[debug, AppendTo[debugInfos, {"plastic",Pe,Qe, dalpha,dzet, nQ } ]];    (* debugInfos is a  global variable *)
updated = {eP + deP, eQ+deQ, P+dP, Q+dQ, epor+depor, alpha + dalpha, alphaIn, zet+dzet };
 P += dP; Q+=dQ; alpha += dalpha;
(* postplastic return mapping via shifting alpha *)
alphaold = alpha;
If[ Q >  (alpha + 0.99 mbar ) * P  , alpha = ( Q - mbar P) /  Max[P,0.00001]  ] ;
If[ Q <  (alpha - 0.99 mbar ) * P  , alpha = ( Q  + mbar P ) /  Max[P,0.00001]  ] ;
If[  Abs[ alphaold  - alpha] > 0.1 Abs[dalpha] , Print[ "explicitIncrement warning : correction > 10% dalpha =", dalpha, "  alphaold =", alphaold, "  alpha=", alpha ]  ];
updated[[6]] = alpha;
Label[exitIncrement];
updated
];

(* ----------------- version  of explicitIncrement with dQ/dP as a prescribed parameter ---------------------- *)
explicitIncrement[ mat_?numberQ, {dePin_,deQin_,dPin_,dQin_, dQdPin_ }  ,deMax_:0.004 ]:= Module[
  {state,stateBeforeYield,updated, eP,eQ,P,Q,epor,alpha,alphaIn,zet, deP,deQ,dP,dQ, unknowns,EE,EEP,soluPre,soluPlast,
   dePe, deQe, dPe,dQe, depore, Pe,Qe, yield,       h1,h2,h,   dePr,deQr,dPr,dQr, nQ,fprime,fgrave,g,  nb,mb,psi,Mb,Mc,
   aa,hard,  alphabar, alphabound, Kmod, dlambda, dalpha, dzet,depor, patm, alphaold, dQdP,
   m,mbar,cLode,lambdac,xi, ec0,enb,end,Ad0,h0,ch,  Gel0,nu,Patm,  zmax,cz,sq2d3=Sqrt[2.0]/3, sq3=Sqrt[3.0]  },
 state  = Last[states];  (* states is a global variable *)

 {eP, eQ, P,Q,epor,alpha,alphaIn,zet} = state[[1;;8]]; If[P<0|| epor<0.1,Print["error implicitIncrement, P=",P,"  epor=",epor]];

 (* read all material constants *)
 {patm,Patm, Gel0, nu}=mat[[1, 1;;4]];  (* if patm = 100 then Patm = 173 *)
 {Mc,m,mbar,enb,end,xi,lambdac,ec0,cLode,Ad0 } = mat[[2, 1;;10 ]];
 {h0, ch,cz, zmax}  = mat[[3,1;;4]];

(* get elastic predictor *)
{deP,deQ,dP,dQ, dQdP } = {dePin,deQin,dPin,dQin,dQdPin } ;
unknowns=Select[ {deP,deQ,dP,dQ,dQdP} , symbolQ ];
EE = getHypoelasticE[state ,mat];
soluPre = Solve[  {dP  ==  EE[[1,1]] deP + EE[[1,2]]*deQ ,  dQ ==  EE[[2,1]] deP + EE[[2,2]]*deQ ,  dQ == dQdP *  dP}   , unknowns ][[1]];
epor = state[[5]];
{dePe, deQe, dPe,dQe, depore} =  {deP, deQ, dP,dQ, (1+epor)*Exp[- deP*Sqrt[3]] -1  - epor } /.  soluPre ;  (* elast. increments *)
{Pe,Qe} = {P,Q} + {dPe,dQe}  ;
yield   =   ( Qe - alpha *Pe )^2  - mbar^2*Pe^2  ;   (* crit of el. predictor *)
If[yield<=0, updated = state + {dePe,deQe,dPe,dQe,depore,0,0,0};

If[debug, AppendTo[debugInfos, {"elastic", Pe,Qe, yield } ]]; Goto[exitIncrement]  ] ;

(* PLAST 1: elastic return mapping, partial update, find remaining increment  *)
h= 0;
If[Qe > alpha Pe,  h= (Q-alpha P-mbar P )/(alpha dPe-dQe+dPe *mbar) ];
If[Qe < alpha Pe,  h= (Q-alpha P+mbar P )/(alpha dPe-dQe-dPe *mbar)  ];
{eP, eQ, P,Q, epor} +=  h *{dePe, deQe, dPe,dQe,depore};
stateBeforeYield = state;  stateBeforeYield[[1;;5]] = {eP, eQ, P,Q, epor};
{dePr,deQr,dPr,dQr} = If[NumberQ[#],# * (1-h), #  ] & /@ {deP,deQ,dP,dQ} ; (* remaining increment after elastic piece *)

(* PLAST 2: preparation for hardening modulus *)
nQ = Sign[Q - alpha P ];
fprime = { -alpha * nQ - mbar,  nQ };
fgrave = - P nQ;
If[ nQ > 0, g= 1, g=cLode ];
nb = {0,0}; mb={0,0};
{nb, mb} = getNM[stateBeforeYield, mat ];

psi = epor - (ec0 - lambdac * (P/Patm)^xi );
Mb = Mc * Exp[ -enb*psi];
alphabound = nQ*sq2d3*( g*Mb - m ) ;

aa = (alpha - alphaIn )*nQ ;
If[aa <  0,
        alphaIn = alpha;  (* new reversal *)
        aa =  0.001 ;
     ];
If[aa < 0.001, aa = 0.001];
hard = Gel0* h0*( 1- ch*epor)/ (P/Patm * aa /sq3 );
alphabar =    (2/3)*(alphabound - alpha) *hard ;
Kmod = - fgrave/ Norm[ fprime ] * alphabar ;

 EEP = EE - Outer[Times, (EE.mb) , (nb.EE) ] / ( Kmod + nb.(EE.mb));
unknowns=Select[ {dePr,deQr,dPr,dQr,dQdP}  , symbolQ ];
soluPlast = Solve[{ dPr ==  EEP[[1,1;;2]]. {dePr,deQr}, dQr  ==  EEP[[2,1;;2]]. {dePr,deQr},   dQ == dQdP *  dP } ,unknowns ][[1]];
{deP,deQ,dP,dQ} =  {dePr,deQr,dPr,dQr} /. soluPlast ;
dlambda = (nb.EE).{deP,deQ}/ ( Kmod + (nb.EE).mb);
If[dlambda < 0, Print["Error in explicitIncrement:  dlambda =", dlambda, "  (nb.EE).{deP,deQ} = ", (nb.EE).{deP,deQ} ,
               "   Kmod + (nb.EE).mb)=" , Kmod + (nb.EE).mb , "nb=",nb, "{deP,deQ}= ",{deP,deQ}, "EE=", EE ]; Abort[];];

dalpha = dlambda * alphabar ;
If[ mb[[1]] > 0,  dzet = 0, dzet =   -cz *( dlambda *(-mb[[1]] *sq3)  ) (zmax * nQ + zet)  ]; (* only plast dilatancy matters  *)
depor = (1+epor)*Exp[- deP*Sqrt[3]] -1  - epor   ;
If[debug, AppendTo[debugInfos, {"plastic",Pe,Qe, dalpha,dzet, nQ } ]];    (* debugInfos is a  global variable *)
updated = {eP + deP, eQ+deQ, P+dP, Q+dQ, epor+depor, alpha + dalpha, alphaIn, zet+dzet };
 P += dP; Q+=dQ; alpha += dalpha;
(* postplastic return mapping via shifting alpha *)
alphaold = alpha;
If[ Q >  (alpha + 0.99 mbar ) * P  , alpha = ( Q - mbar P) /  Max[P,0.00001]  ] ;
If[ Q <  (alpha - 0.99 mbar ) * P  , alpha = ( Q  + mbar P ) /  Max[P,0.00001]  ] ;
If[  Abs[ alphaold  - alpha] > 0.1 Abs[dalpha] , Print[ "explicitIncrement warning : correction > 10% dalpha =", dalpha, "  alphaold =", alphaold, "  alpha=", alpha ]  ];
updated[[6]] = alpha;
Label[exitIncrement];
updated
];


 inputCheck[mat_, Icond_, Loading_ ,ninc_ ]:= Module[ {P, Q,   deP, deQ, dT, dP, dQ    },

     If[ ninc < 1 || ninc > 1000 , Print[ "Suspicious ninc =  ", ninc ]]  ;
   { {patm, Patm, Gel0, nu} , { Mc ,m, mbar  ,enb,end, xi,lambdac,ec0 ,cLode,Ad0 } ,  {h0 , ch , cz , zmax } } = mat;
    If[ patm ~inRange~ {0, 1000} , "", Print[ "Suspicious patm =  ", patm  ]]  ;
    If[ Gel0 ~inRange~ {50, 500} , "", Print[ "Suspicious Gel0 =  ", Gel0  ]]  ;
    {deP, deQ, dP, dQ }   =     Loading[[1;;4]];
    If[ Abs[ deP ] > 0.005 || Abs[deQ] > 0.005 , Print[ "Strain increments seem to be large {deP, deQ} = ", {deP, deQ}  , " Keep them at  0.1% level"   ]]  ;
    If[ Abs[ dP ] > 10 || Abs[ dQ ] > 10 , Print[ "Stress increment(s) too large {dP, dQ} = ", {dP, dQ}  , " Keep them below  2 kPa "   ]]  ;
    {eP, eQ, P, Q, epor, alpha, alphain, zet}  = Icond;
    If[epor  ~inRange~ {0.01, 3} , "", Print[ "Suspicious initial epor =  ", epor, "out of usual range {0.01, 3} "  ]];

  nUnknowns = Count[   symbolQ[#] & /@  Loading, True] ;   nUnknownsShoulndbe = Length[Loading] - 2;

  If[ nUnknowns !=  nUnknownsShoulndbe,  Print["Found ", nUnknowns ," unknowns instead of ",  nUnknownsShoulndbe , " in Loading = ", Loading ]; Beep[]; Abort[]; ];

  {deP, deQ, dP, dQ} = Loading[[1;;4]];
  If[ numberQ[deP] &&  numberQ[dP],  Print["Error: You cannot prescribe deP and dP in Loading simultaneously in Loading =", Loading ];  Abort[]; ];
  If[ numberQ[deQ] &&  numberQ[dQ],  Print["Error: You cannot prescribe deQ and dQ in Loading simultaneously in Loading =", Loading ];  Abort[]; ];
  Print[" No further issues "];
];


(*--------------------------------------------- graphics--------------------------------------------------------*)
gOptions = {PlotRange -> All} ;

 PQPlot[Path_, Mat_] := Module[{PQs, gPQs,  outputPlot,  MC, ME , maxP ,iso =   Sqrt[2]/3.0   },
        MC = N[ Mat[[2,1]] ];    ME=3 MC / (3+MC);
           PQs = {#[[3]], #[[4]] }  &  /@  Path;
           maxP =      Max[ Path[[All,3]] ];
           gPQs = ListPlot[PQs,PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All, AxesLabel->{P,Q},
                            AxesOrigin->{0,0}, DisplayFunction->Identity, Joined -> True ];
           outputPlot  = { gPQs,  Graphics[     Line[{ {1, iso*MC}* maxP, {0,0}, {1,-iso* ME}*maxP}  ]     ]  } ;
        Show[outputPlot, DisplayFunction->$DisplayFunction, PlotRange->All, AxesLabel->{"P","Q"}, AxesOrigin->{0,0} ]
 ];

 oedometricPlot[Path_, Mat_] := Module[{Pes, gPes, gCLS, outputPlot, currentPath, Patm, Pmax,Pmin, xi, lambdac,ec0},
             Pes = {#[[3]], #[[5]] }  &  /@  Path;
             Pmax= Max[ Pes[[All,1]]  ]+0.1; Pmin= Min[ Pes[[All,1]]  ]-0.1;
             {xi, lambdac,ec0} = Mat[[2 ,6;;8]];      Patm = Mat[[1,2]];
             gCSL = ListPlot[   Table[{P, ec0 - lambdac*(P/Patm)^xi}, {P, Pmin, Pmax, 0.1} ],  Joined -> True , PlotStyle -> Red ] ;
             gPes = ListPlot[Pes ,PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},  PlotRange->All,AxesLabel->{P,e},DisplayFunction->Identity,  Joined -> True ];
             outputPlot  = { gPes , gCSL} ;
         Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];

 epsQQPlot[Path_, Mat_] := Module[{nPaths=1,epsQQ, gepsQQ,  outputPlot, currentPath },
             epsQQ = {#[[2]], #[[4]] }  &  /@  Path;
             gepsQQ = ListPlot[epsQQ,PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,
             AxesLabel->{\[Epsilon]Q,Q},AxesOrigin->{0,0},DisplayFunction->Identity,  Joined -> True ];
             outputPlot  = { gepsQQ } ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];

epsPepsQPlot[Path_, Mat_] := Module[{nPaths=1,epsPepsQ, gepsPepsQ,  outputPlot, currentPath , iPath },

             epsPepsQ = {#[[1]], #[[2]] }  &  /@  Path;
             gepsPepsQ = ListPlot[epsPepsQ,PlotStyle->PointSize[0.014`],PlotRegion->{{0,1},{0,1}},PlotRange->All,
                                      AxesLabel->{\[Epsilon]P,\[Epsilon]Q},AxesOrigin->{0,0},DisplayFunction->Identity, Joined -> True];
             outputPlot  = { gepsPepsQ } ;
          Show[outputPlot,DisplayFunction->$DisplayFunction]
 ];

pickListPlot[list_,ix_,iy_ ] :=  ListLinePlot[  { Transpose[list] [[ ix ]]  ,  Transpose[list] [[ iy]]    } //Transpose,   Evaluate[gOptions] ];
pickListPlot[list_,ix_,iy_ ,iz_] := Module[{g1,g2},
g1 =  ListLinePlot[  { Transpose[list] [[ ix ]]  ,  Transpose[list] [[ iy]]    } //Transpose,   Evaluate[gOptions] ];
g2 =   ListLinePlot[  { Transpose[list] [[ ix ]]  ,  Transpose[list] [[ iz]]    } //Transpose,  Evaluate[gOptions] , PlotStyle -> Red ] ;
Show[g1,g2]
];
pickListPlot[list_,ix_,iy_ ,iz_, ia_] := Module[{g1,g2,g3},
g1 =  ListLinePlot[  { Transpose[list] [[ ix ]]  ,  Transpose[list] [[ iy]]    } //Transpose,   Evaluate[gOptions] ];
g2 =   ListLinePlot[  { Transpose[list] [[ ix ]]  ,  Transpose[list] [[ iz]]    } //Transpose,  Evaluate[gOptions] , PlotStyle -> Red ] ;
g3 =   ListLinePlot[  { Transpose[list] [[ ix ]]  ,  Transpose[list] [[ ia]]    } //Transpose,  Evaluate[gOptions] , PlotStyle -> Blue ] ;
Show[g1,g2,g3]
];
SetAttributes[ pickListPlot,HoldAll];
pickListPlot[list_  , pickPattern_ ] := Module[{ xyp,  g1,g2,g3},
 xyp = (pickPattern &  /@ list );  (* creates combinations. First is x then y1,y2,y3 are possible *)
 n = Dimensions[xyp][[2]];
 If[n >= 2,  g1 =   ListLinePlot[  xyp[[All, 1;;2]],  Evaluate[gOptions]  ]  ];
 If[ n >= 3,   g2 = ListLinePlot[  { Transpose[xyp] [[ 1 ]]  ,  Transpose[xyp] [[3]]    } //Transpose, Evaluate[gOptions] , PlotStyle -> Red  ] ];
 If[ n >= 4,   g3 = ListLinePlot[  { Transpose[xyp] [[ 1 ]]  ,  Transpose[xyp] [[4]]    } //Transpose, Evaluate[gOptions] , PlotStyle -> Blue  ] ];
 Which[ n==2 , Show[g1], n==3 , Show[g1,g2],  n==4 , Show[g1,g2,g3]  ]
];



 controlPanelSaniSand[] := Module[{ (* ninc, deP,deQ,dP,dQ,dQdP *) },
  implicit = False; debug = False;
  path = { ICond0 }; (* Initialisation of global variable path *)
  Manipulate[
 GraphicsGrid[
{ {Show[{PQPlot[path, mat],         Graphics[ { PointSize[Large], Red, Point[path[[1, 3 ;; 4]] ],PointSize[Medium], Orange, Point[path[[-1, 3 ;; 4]] ]}] } ],
  Show[{epsQQPlot[path, mat],      Graphics[ {PointSize[Large], Red, Point[{path[[1, 2]], path[[1, 4]] }],PointSize[Medium], Orange, Point[{path[[-1, 2]], path[[-1, 4]]}  ]} ] } ]
  },
 {Show[{oedometricPlot[path, mat], Graphics[ {PointSize[Large], Red, Point[{path[[1, 3]], path[[1, 5]] }],PointSize[Medium], Orange, Point[{path[[-1, 3]], path[[-1, 5]]}  ]} ] } ],
  Show[{epsPepsQPlot[path, mat],   Graphics[ {PointSize[Large], Red, Point[{path[[1, 1]], path[[1, 2]]} ],PointSize[Medium], Orange, Point[{path[[-1, 1]], path[[-1, 2]]}  ]} ] } ]
 }},
 ImageSize -> Full]  ,
 "Loading step applies :",
 {{ninc, xninc}},
 "increments each consisting of",
 {{deP, xdeP}}  ,
 {{deQ, xdeQ}}  ,
 {{dP, xdP}}  ,
 {{dQ, xdQ}}  ,
 " in [-],[kPa] and possibly ",
 {{dQdP, xdQdP}},
 "  [-].",
 Delimiter,
 Button["Check the input",
  If[ symbolQ[dQdP ] ,
   inputCheck[mat, ICond0,  {deP, deQ, dP, dQ }, ninc  ],
   inputCheck[mat, ICond0,  {deP, deQ, dP, dQ, dQdP  }, ninc  ] ];
 ],
   Button["Calculate step",
  If[ symbolQ[dQdP ],
  Loading = {deP, deQ, dP, dQ },  Loading = {deP, deQ, dP, dQ ,dQdP } ];
  If[ numberQ[deP] &&   numberQ[dP],  Print["Error: You cannot prescribe deP and dP simultaneously Loading =", Loading ];  Abort[]; ];
  If[ numberQ[deQ] &&   numberQ[dQ],   Print["Error: You cannot prescribe deQ and dQ simultaneously in Loading =", Loading ]; Abort[];   ];
  states = { Last[path] };
  step[mat,Loading, ninc]  ;
  path = Join[ path,  Delete[ states, 1] ] ;
 ],

Button["Undo the last ninc increments",
  If[Length[path] -ninc  >= 1, path = Drop[path, -ninc], path = { path[[1]] } ];
 ],

Button["Print the final state",
  Print["Final state = ", path[[-1]] ];
 ],

Button["Reset all steps, graphics ",
  path = {ICond0}; ninc = xninc; deP = xdeP  ; deQ  = xdeQ; dP = xdP; dQ = xdQ;  FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]];
 ],
"Other plots via
 pickListPlot[path, 2, 5 ]
 or
 pickListPlot[path,  {#[[2]] , Log[ (1 + #[[5]])/ (1+ICond0[[5]])  ]  }  ]   ",
  Button["undrained strain cycles eQ +/-  0.4% ",
  Clear[xdP, xdQ ] ;
  states = { Last[path] };
  step[mat,  {0, 0.0004, xdP, xdQ}, 10]  ;
  cycle[mat,  {0, -0.0004, xdP, xdQ}, 10, {0, 0.0004, xdP, xdQ} ,10 , 10] ;
  step[mat,  {0, -0.0004, xdP, xdQ}, 10]  ;
  path = Join[ path,  Delete[ states, 1] ] ;  ninc = Length[states];
 ],
  Button["undrained stress cycles Q +/- 20 kPa ",
  Clear[xdP, xdQ ] ;
  states = { Last[path] };
  currentQ = states[[1,4]];
  upperQ =   currentQ + 20;    lowerQ =   currentQ - 20;
  cycle[ mat, {0, -0.0001 ,xdP, xdQ }, { Hold[ states[[-1,4]] > lowerQ] }, {0, 0.0001 ,xdP, xdQ } , { Hold[states[[-1,4]]  <  upperQ]  } ,10 ] ;
  step[ mat, {0, -0.00001 ,xdP, xdQ }, { Hold[ states[[-1,4]] > currentQ] } ] ;
  path = Join[ path,  Delete[ states, 1] ] ;  ninc = Length[states];
 ]
 ]
];





EndPackage[ ]
$Context = "PQ`SaniSand`"   ;
Print[ " *** SaniSand.m  provides the CONSTITUTIVE MODULES:
 getHypoelasticE[state_,mat_]
 getNM[state_]
 explicitIncrement[ mat_?numberQ, {dePin_,deQin_,dPin_,dQin_}  ,deMax_:0.004 ]
 increment[ mat_, loading_ ]    <--  the essential constitutive routine
SMALL AUXILIARY MODULES:
 step[ mat_,  loading_ , ninc_]
 cycle [mat_,  loading1_ ,ninc1_,loading2_ ,ninc2_,ncyc_]
 symbolQ[t_]
 No  PDF documentation as yet.\
"];
