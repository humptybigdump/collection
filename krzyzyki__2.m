(* ::Package:: *)

getBoussinesq[{x_,y_,z_},F_,ni_]:=Module[{R,AA,BB,CC,DD,sigxx,sigyy,sigzz,sigxy,sigzx,sigzy,T},
R=Sqrt[x^2+y^2+z^2];
AA=1/3*(1-2*ni);
BB=(R^2-R*z-z^2)/(R^3*(R+z));
CC=(2*R+z)/(R^3*(R+z)^2);
DD=3*F/(2*Pi)*z^2/R^5;
sigxx = (3*F)/(2*Pi)*((x^2*z)/R^5+AA*(BB-x^2*CC));
sigyy = (3*F)/(2*Pi)*((y^2*z)/R^5+AA*(BB-y^2*CC));
sigxy = (3*F)/(2*Pi)*((x*y*z)/R^5-AA*CC*x*y);
sigzx = DD*x;
sigzy = DD*y;
sigzz = DD*z;
T={{sigxx,sigxy,sigzx},
   {sigxy,sigyy,sigzy},
    {sigzx,sigzy,sigzz}}//N
];


getPhiMN[T_]:=Module[{phiMN},
                                   evT  = Eigenvalues[T] ; 
                                   If[  Max[evT]*Min[evT] <= 0, phiMN = 90, 
                              phiMN=180/Pi*ArcTan[  Sqrt[  (  Tr[ T ]*Tr[  Inverse[T]  ]-9  )/8  ]  ] ];
                               phiMN
];


getKrzyzyki[xx_, T22list_,scaleFactor_]:=Module[{nP,maxT,x,y,dx,dy,ds,el,T,ev1,ev2,L1,L2,colour1,colour2},
nP  = Length[xx];  
maxT = Max[ Norm[ #,"Frobenius"]&/@ T22list  ]; 
{x,y} = Transpose[xx];  dx = Max[ x ] - Min[x]  ; dy =  Max[ y ] - Min[y] ; ds = Sqrt[  (dx^2 + dy^2)/nP ]; 
el = 0.5 * ds/maxT*scaleFactor; 
gg=Array[  0 &, nP ]; (* prep. of list that will store graphics krzyzyki *)
Do[  
 x= xx[[i]]; 
 T = T22list[[i]] //N;   {ev1,ev2} =  Orthogonalize[Eigenvectors[ T ]] ;   {L1,L2} = Eigenvalues[T]*el;
 If[  L1<0 , colour1= "Blue" , colour1="Red" ]; 
 If[  L2<0 , colour2= "Blue", colour2="Red" ]; 
 gg[[i]] = Graphics[ { ToExpression[colour1] , Line[{x, x+L1*ev1,x-L1*ev1}],   ToExpression[colour2] , Line[{x, x+L2*ev2,x-L2*ev2}]  } ];  
,{i,1,nP}];
gg
];
