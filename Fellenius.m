(* ::Package:: *)

(*  Mathematica simplified Fellenius method  by A. Niemunis 2013-2017

This  notebook  solves  a  kinematic  failure  mechanism  with a single rotation using the simplified  Fellenius method (no water).
Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\\Dokumente und Einstellungen\\xxx\\Anwendungsdaten\\Mathematica\\   or   C:\\Windows\\xxx\\Application Data\\Mathematica\\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory  KEM there
3) copy this file (= Fellenius.m) to the new directory.
4) Begin a new  Mathematica session with:    Needs["KEM`Fellenius`"]

  *)


BeginPackage["KEM`Fellenius`"]
(*---------------------------------------------usages--------------------------------------------------------------------*)

getGraphics::usage = "Syntax:  {g0, g1, g2, g2a, g3, g4, g5}  = getGraphics[] ;  produce a nice picture from global variable  geometria ";

FelleniusTargetFunction::usage = "Syntax   { r0,c0, safetyF} = FelleniusTargetFunction[ r0_, c0_ ] " ;
mat::usage = " Syntax: { cohesion ,tanPhi,specificWeight ,Psi,tanPsi} = mat  "  ;
geomOut::usage = "Syntax  = geomOut  " ;
geomIn::usage = "Syntax  = geomIn  " ;
t::usage = "t is a parameter"  ;
u::usage = "u is a parameter"  ;



(* ------------------------solution functions--------------------------------------------------------------------*)

Begin["`Private`"]


 getGraphics[] := Module[{g0, g1, g2, g2a, g3, g4, g5, kreski, c0, spiral, parametrizedSegments, slopePoints, topPoints, bottomPoints,interSliceXX, interSliceYY,interSliceZZ,segments, nSegments,nSlices},
  {slopePoints, segments, nSegments,  parametrizedSegments, nSlices} = 	geomIn  ;
	 {c0, spiral, topPoints, bottomPoints,interSliceXX, interSliceYY,interSliceZZ} = geomOut;
  g0 = Graphics[Point[ c0 ] ];
  g1 = ParametricPlot[spiral, {t, Pi, 2 Pi }    ];
   g2 = ParametricPlot[ parametrizedSegments   , {u, 0, 1}]  ;
  g2a = Graphics[ Point[slopePoints] ];
  kreski = Line[#] & /@  ({topPoints , bottomPoints } // Transpose);
  g3 = Graphics[ Point[  {interSliceXX, interSliceYY} // Transpose  ]];
  g4 =  Graphics[
    Point[  {interSliceXX, interSliceZZ} // Transpose  ]];
  g5 = Graphics[kreski];
  {g0, g1, g2, g2a, g3, g4, g5}
  ];


 FelleniusTargetFunction[ r0_?NumberQ, {c0x_?NumberQ, c0y_?NumberQ } ] :=
 Module[{segment, intersections, m, rule, solu, tSolutions, heights, widths, tmiddles, weights, dSpiraldt,
   tangentials, normals, resistances, radii, resistantMoment, activeMoment ,  rotate90CCW, t0, u0 , dist,
  cohesion ,tanPhi,specificWeight ,Psi,tanPsi,
   slopePoints, segments, nSegments,  parametrizedSegments, nSlices,
  spiral,  topPoints, bottomPoints,interSliceXX, interSliceYY,interSliceZZ },
	 { cohesion ,tanPhi,specificWeight ,Psi,tanPsi} = mat  ;
	  {slopePoints, segments, nSegments,  parametrizedSegments, nSlices} = 	geomIn  ;
		c0 = {c0x,c0y};
  spiral = c0 +  r0 Exp[  tanPsi *t] {Cos[t], Sin[t]} ;
  intersections = {};
  Do[ segment =    parametrizedSegments[[ i ]] ;
    dist  = (  segment - spiral ).  (  segment - spiral ) // N;
    {m, rule } =    FindMinimum[{dist ,       0 <= u <= 1  &&    2 < t  <  7 }, {{ u, 0.5}, {t, 4.5}}    ] //   N;
     If[  Abs[m] < 10^-5,  AppendTo[ intersections, segment /. rule]    ];
      , {i, nSegments}];
  If[Length[intersections] != 2,   Print[" cannot find two intersections of spiral with slope "];   Return[  {r0, c0, 20}] ];
    interSliceXX =  intersections[[1, 1]]  +   Range[ 0, nSlices ]  (     intersections[[2, 1]]   - intersections[[1, 1]]   ) /nSlices;
  interSliceYY =   intersections[[1, 2]]  +    Range[ 0,      nSlices ]  (       intersections[[2, 2]]   - intersections[[1, 2]]   ) /nSlices;
  Do[  solu =    Solve[      parametrizedSegments[[ iSegment, 1 ]]  ==       interSliceXX[[iSlice]]   , u ][[1]]; u0 = u /. solu;
       If[  0 <= u0 <=  1 ,   interSliceYY[[  iSlice ]] =   (parametrizedSegments[[ iSegment, 2 ]] /.      solu)   ];
   , {iSlice, 2, nSlices }, {iSegment, 1, nSegments} ];
  interSliceZZ =
   intersections[[1, 2]]  +    Range[ 0,      nSlices ]  (       intersections[[2, 2]]   - intersections[[1, 2]]   ) /nSlices;
  tSolutions = {};
  Do[  solu =
    FindRoot[     spiral[[ 1 ]]  == interSliceXX[[iSlice]]   , {t, 4.5 } ];
   t0 = t /. solu;
   If[3  <= t0 <=  6 , interSliceZZ[[ iSlice ]] =   (spiral[[2 ]] /.  solu)   ];
   AppendTo[tSolutions, t0];
   , {iSlice, 1, nSlices + 1 }  ];
  topPoints =  {interSliceXX, interSliceYY} // Transpose ;
  bottomPoints =  {interSliceXX, interSliceZZ} // Transpose ;
   heights = topPoints[[ All, 2 ]] - bottomPoints[[All, 2 ]]  ;
  widths =   Drop[ topPoints[[ All, 1 ]] , 1] -   Drop[ bottomPoints[[All, 1 ]]  , -1];
  weights =   specificWeight*( heights[[ # ]] +   heights[[ # + 1 ]]) *     widths[[#]]/2 &   /@ Range[ nSlices ];
  tmiddles = (Drop[ tSolutions, 1] +  Drop[ tSolutions, -1] )/2;
  dSpiraldt =  D[spiral, t]  // Simplify;
  rotate90CCW = {{0, -1}, {1, 0}} ;
  tangentials  =   Normalize[#] & /@ ( { (dSpiraldt[[1]] /.         t -> tmiddles )  ,  (dSpiraldt[[2]] /. t -> tmiddles )  } //     Transpose );
  normals  = rotate90CCW . # & /@ tangentials;
  resistances  = (     normals[[All, 2]]  *  weights  * tanPhi  +      cohesion * widths/ tangentials[[ All, 1]]   )   * Cos[Psi];
  radii =  r0 Exp[  Tan[Psi] *t]  /. t -> tmiddles;
   resistantMoment = Plus @@ (resistances *radii);
  activeMoment  =   Plus @@ (  (  c0[[1]]  - (spiral[[1]] /.  t -> tmiddles  )  )*      weights );
  If[activeMoment < 0, Return[ {r0, c0, 20}]];
	geomOut= {c0, spiral, topPoints, bottomPoints,interSliceXX, interSliceYY,interSliceZZ}  ;
  {r0, c0,  resistantMoment / activeMoment }
	   ]    ;



End[]

EndPackage[ ]

$Context = "KEM`Fellenius`";

  Print[ " Simplified  Fellenius  slope stability solutionby implemented to Mma by A. Niemunis  2013 - 2017" ]   ;

