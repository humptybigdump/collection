(* ::Package:: *)

(*    Mathematica  tools for solving examples with the kinematic element method (KEM) by A. Niemunis & F.Prada 2007-2020

This  notebook  solves  a  kinematic  failure  mechanism  using the kinematic element method (KEM).   No optimization of geometry is performed.
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory  KEM there
3) copy this file (= KEMTools2016u.m) to the new directory.
4) Begin a new  Mathematica session with:    Needs["KEM`KEMTools2016u`"]
   or study some precalculated examples opening the notebook testKEMTools6.nb
   
 Locally saved  d:\KEM\KEMTools2016u.m  can be invoked  executing  PrependTo[$Path, \"D:\"] followed by the usual Needs[\"KEM`KemTools2016u`\"]

  2013:
      getHodograph[] can find velocity if more than 2 neighbouring velocities are known (the user is responsible that  the third neighbour is kinematically consistent)
      If AFR is everywhere satisfied it is possible to find bring2LimitLoad from the comparison A=D rather than from Equilibrium.
           For this purpose, having solved the hodograph, one may write
           diss = getAFRdissipationRate[edgesProps , edgesT , edgesN , edgesL ,    adjacentE , areasE , v ] ;
           sup = getPowerSupply[externalLoads, seepageForces, selfWeights,   shortPPF, v];
           extras =  Sum[  bring2LimitLoad[[ie]].v[[ie]]  , {ie, 1, Length[ v]  }];
           Solve[extras + sup  == diss, r] [[1, 1]]
           or use these tools:  
                  getAFRdissipationRate[edgesProps_, edgesT, edgesN, edgesL,   adjacentE, areasE, v] 
                  getNAFRdissipationRate[edgesProps_, edgesT, edgesN, edgesL,   adjacentE, areasE, v] 
                   getPowerSupply[externalLoads, seepageForces, selfWeights, shortPPF, v]
 
NEW 2017:  change calls PlotForcePolygon[3, solution, imagesizeFactor -> 0.6] to   plotForcePolygon[3, solution, imageSizeFactor -> 0.6]
NEW 2017:  new meaning of initializeGlobalArrays[] called before selfWeights, .. , 
           initializeGlobalArrays[]  must be called  before bring2LimitLoad = {... };
            bring2LimitLoad = { {0, 0}, {0, -r2}, {0, 0} , {0, 0}  };  can be distributed over all elements 
           selfWeights =   {0, -20}  *# & /@   areasE ;  must be calculated manually  (removed from getHodograph)  
            movementExpectation = { 2, {0, -1}};   a new syntax.  Two elements may obtain independent prescribed velocities 
           unknowns = findAllUnknowns[];    obligatory separate procedure to be called before getHodograph
           {v, Loads} = getHodograph[];  followed by  Loads += externalLoads;  ...
            checkLoads[];    a separate call  isolated from initializeGlobalArrays[] and called before staticEquilibrium
            staticEquilibriumEquations =   Loads[[#]] == {0, 0} & /@ (Range[1, Length[elements] - 1]);  open programming style 
            {solution} = Solve[staticEquilibriumEquations, unknowns] // Chop    ; 
              PrintForces[solution]   ;        only non-zero Forces are printed    
              plotForcePolygon[1, solution , imageSizeFactor -> 1 ] superseded the old   PlotForcePolygon[1, solution , imagesizeFactor -> 1 ]
              plotHodograph[]  with  automatic aspect ratio  
NEW 2020:      Option verbose->True  and geometric tools added:              
 --- geometric functions:   getIntersectionPointBetweenSegments[] and  getOverlapLength[] for future optimization  
 ----repaired plotForcePolygon[] and option legendForce \[Rule] 4000  
 ----q[] is global  
  ----OPTIMIZATION in example[2]:    


*)

BeginPackage["KEM`KEMTools2016u`"]
(*---------------------------------------------usages--------------------------------------------------------------------*)

getArea::usage = "getArea[xx, elements, e] returns the area within a polynom (an element) with vertices \
\n      xx: table with the x, y coordinates \
\n      elements: connectivity table with global element node numbers \
\n      e: specific element whose area will be calculated ";

getIntersectionPointBetweenSegments::usage= " getIntersectionPointBetweenSegments[seg1_, seg2_] returns the intersection point of lines defined by segments seg1 = {{x1,y1},{x2,y2}} and seg2 = {{x3,y3},{x4,y4}} ";

getOverlapLength::usage=" getOverlapLength[seg1,seg2]  returns the length of the overlap of two 1D segments   " ; 

clearGlobals::usage=" undefines global variables (from topology kinematics and statics) which may have been left by the previous run of getHodograph etc. ";


getTopology::usage = "{edgesT,edgesN,edgesL,adjacentE,areasE} = getTopology[elements, edges, xx] returns for  for all edges:   \
\n                                                                        list of directions of tangent vectors=edgesT, \
\n                                                                        list of directions of normal vectors=edgesN, \
\n                                                                        list of lengths=edgesL \
\n                                                                        list of adjacent elements left and right (looking along edgesT)=adjacentE, \
\n                                                     for all elements:list of areas=areasE \
\n The essential  local  variables in getTopology  module are  :  \
\n eSides = a collection (for each element) of  lists  of node pairs that constitute sides of  each element. \
\n                    For example, elements = {{}, {1,2,3,4},...}  would have eSides = {{  } , {{1,2},{2,3},{3,4},{4,1}},... }  \
\n LeftOfE = a list (for all edges) with the number of element  on the left-hand side of each edge  \
\n RightOfE = a list (for all edges) with the number of element on the right-hand side of each edge \
\n adjacentE =   a list (for all edges) with the element pairs on the left- and right-hand side of each edge " ;

plotKEM::usage = "plotKEM[ elements,edges,xx,v,opts]   draws  two meshs with KEM elements: before and after the displacement defined in the list 'v'. \
\n The edges, nodes and elements may be numbered (labelled) if required.\
\n The default options  are  nodeLabels->True , elementLabels->True and edgeLabels->True \
\n Although the arguments  elements,edges,xx   are all global variables their presence is obligatory. \
\n The velocity v must be specified to indicate the position of the 'deformed' mesh and is used for animations: \
\n e.g v=Array[{0,0}&,Length[elements]] ";

plotForcePolygon::usage = "plotForcePolygon[ elementNumber, solution, options] draws the polygon of forces for the body elementNumber \
 \n solution = a list of rules to evaluate the  unknown inter-element forces obtained from  {solution} = Solve[staticEquilibriumEquations,unknowns]//Chop;  \
 The optional parameters opts may contain: \
\n ElementName, quoted symbol of the element \
\n imagesizeFactor  = the length of 1 kN/m Force in pixels, Default=1 ,\
\n printForceVal = whether to label all forces, Default=True,\
\n printElementLabel = whether to print element name, Default=False,\
\n printForceName = whether to label forces, Default=False. \ ";


plotHodograph::usage="plotHodograph[ v , adjacentE, options  ]  plots velocities of  all elements. It needs  the following argumets : \
\n v = list of   velocity vectors  of all elements \
\n adjacentE = a topological array created  by the getTopology routine  ";

showKEMtranslation::usage="showKEMtranslation[elements ,edges, xx,v, nFrames, scaleV]  \
\n creates animated KEM mesh: using nFrames between the original position  and displaced position according to the vector 'v' . \
\n The edges are numbered (labelled).\
\n scaleV  is a scaling factor for the displacements v  \
\n Although the arguments  elements,edges,xx   are all global variables their presence is obligatory. \
\n The velocity v must be specified to indicate the position of the 'deformed' mesh and is used for animations: \
\n e.g v=Array[{0,0}&,Length[elements]] applies zero movement ";



getHodograph::usage = "getHodograph[ verbose->True ]     This module returns the list {v,Loads} containning two lists:
\n 1) v = element velocity vectors,  calculated (created) in  getHodograph \
\n 2)  Loads += contribution to  element forces  due to cohesion  and (symbolically) due to unknown interaction forces between neighbour elements.
\n While entering getHodograph the list Loads contains external load force acting on each element. \
\n This  force is supplemented by  getHodograph by the contributions from   neighbouring elements, i.e.  \
\n  forces in Loads are increased by forces transferred through the edges. These additional forces result from \
\n 1) cohesion (which is known as soon as the relative velocities between blocks are established) \
\n 2) normal+friction reactions.  \
\n    The values of these forces are unknown but their deflection from the contact normal (by the friction angle) \
\n    can be determined from the  relative  velocities of elements. \
\n   \
\n The module getHodograph uses the following local  variables: \
\n ne = number of elements \
\n haveV = indicator True/False whether a velocity in a given element has been calculated \
\n e0, e1,e2, = currently solved element and neighbouring elements with known velocities  \
\n v0 = initially expected velocity of a special element chosen to dictate the velocities of all remaining elements \
\n edge0 = edge between the special element (with prescribed velocity) and the Earth = elements[[ne]] \
\n T1,T2 = unit vector tangential to the edge1 or edge2 \
\n N10,N20  = unit vector normal to the edge1 (or edge2) pointing from the element e1 (or e2) to e0 \
\n tanPhi1, tanPsi1, coh1 = Tan of the friction angle, dilatancy angle and cohesion [kPa]  for the edge1. \
\n                         Analogous  variables are also used for edge2. \
\n signN =  1 (or -1)  depending if the natural  unit vector normal to the edge is pointing from e1 to e0 (or vice versa) \
\n signT = 1 if the edge0 is pointing approximately along the expected movement of the special element (otherwise -1) \
\n v[[e]] = velocity {vx,vy}  of the element e \
\n Qe10,Qe20 = forces  acting  from e1 and e2 on e0. \
\n                           They contain the unknown factors q[iedge] which must be determined from static equilibrium. \
\n cohesion1, cohesion2 = total cohesiom force [kN] from the contact edges  with elements e1 and e2 \
\n myAdjacentE = all components of the list adjacentE which contain the actually solved element e0 \
\n MyNeighbours = a list of elements which contact  the actually solved element e0 \
\n MySolvedNeighbours =   a list of elements  which contact  the actually solved element e0 and the velocities of which are known \
\n l01,l10,l02,l20 = auxiliary variables to determine whether e0 is on the left-hand side of ritht-hand side wrt the edge1  or edge2 \
\n signN1 = 1 (or -1)  depending if the natural  unit vector normal to the edge is pointing from   e1 to e0 (or vice versa). \
\n signN2 == 1 (or -1)  depending if the natural  unit vector normal to the edge is pointing from   e2 to e0 (or vice versa). \
\n {v01,v02} = unknown velocity of the current element e0 \
\n eqs1, eqs2,  solu = kinematic equations resulting from the compatibility of the motion of  element e0, e1, e2 with the flow rules \
\n along edge1 and edge ";

getAFRdissipationRate::usage=" getAFRdissipationRate[edgesProps, edgesT, edgesN, edgesL,   adjacentE, areasE, v] \
\n given props and geometry and solved kinematics (with corrected prescribed velocity) this module returns the total dissipation rate D. \
\n The procedure requires associated flow rule (AFR) on all edges
";

getNAFRdissipationRate::usage="diss = getNAFRdissipationRate[edgesProps, edgesT, edgesN, edgesL, adjacentE, areasE, v, solution]  \
\n given props and geometry and solved kinematics (with corrected prescribed velocity) this module returns the total dissipation rate D. \
\n The procedure does not require the associated flow rule  but it uses the normal forces q[i] on all edges from the solution
";


getPowerSupply::usage="getPowerSupply[externalLoads, seepageForces, selfWeights, shortPPF, v]  \
\n Given prescribed loads and solved kinematics (with corrected prescribed velocity) this module returns the supplied  power A. \
\n The prescribed loads do NOT include cohesion forces, friction forces, normal contact forces on edges and  bring2LimitLoad. \
\n This module is useful together with  the module getAFRdissipationRate[.... ] to find the bring2LimitLoad without solving \
\n the equilibrium of forces.
";

getPorePressureForces::usage= "{ shortPPF, plottingPPF} =  getPorePressureForces[ porePressures ]   \
\n In the case of instationary seepage (for example due to pp distribution after an earthquake) it is convenient to perform the total stress analysis.  \
\n Instead of buoyant specific weight and seepage forces we use the saturated specific weight and pore pressures along  the edges = porePressures     \
\n The list porePressures contains the pore pressures (usually in kPa) for each edge. This pressure is multiplied by the length of the edge and the respective edgesN.  \
\n The routine returns the resulting pp-forces acting on each element. They are given in two forms: \
\n a short one  (= shortPPF) to be directly added to Loads for evaluation of  the equilibrium condition \
\n a long one (= plottingPPF) to be used in graphic output ";

initializeGlobalArrays::usage= " initializeGlobalArrays[ verbose -> True]   \
\n to be called  \
\n  AFTER getTopology[elements,edges,xx]  \
\n BEFORE getHodograph  (Loads due to cohesions  are calculated in  getHodograph. This is not nice and should be moved somewhere else  )   \
\n  BEFORE  (new 2017.06.26)  per-hand initialization of  bring2LimitLoad, movementExpectation
\n Initialises the following working variables with zero-lists or empty-lists: \
\n interElementForces = Array[{ }&,Length[elements]]; \
\n interElementCohesions= Array[{ }&,Length[elements]]; \
\n unknownQ=Array[q,Length[edges]]; \
\n unknowns = {unknownQ, Select[ Flatten[ bring2LimitLoad ], Not[NumericQ[#]]&  ] } //Flatten  ; \
\n seepageForces = Array[{0,0}&,Length[elements]]; \
\n externalLoads = Array[{0,0}&,Length[elements]]; \
\n porePressures=Array[0 &,Length[edges]]; \
\n plottingPPF=Array[{}&,ne];
 ";

 PrintForces::usage= " PrintForces[solution] \
 \n Displays the resulting values of all forces acting on each element.
 ";

elements::usage= "elements: a connectivity table with global node numbers belonging to each element. \
\n Nodes should be listed CCW (counterclockwise) for each element. The Earth should be included as the last element, \
\n respectively. Its nodes should also  be listed CCW wrt its imagined centre.  elements = {{.. , .. , ..}, {.. , .. , ..}, ..} ";

Loads::usage= "Loads: a list with  all known forces {Fx,Fy} acting on each element  (without self weight which are added automatically). \
\n Depending on the number of DOFs of the kinematic system  one or more unknown forces are added to Loads with the purpose of bringing \
\n the system into the LIMIT EQUILIBRIUM. Usually there is just one unknown force needed, but BearingCapacity needs two such forces.  \
\n Loads = {{.. , ..}, {.. , ..}, ..}";


movementExpectation::usage= "movementExpectation={e0, {ve0x,ve0y}} : a special element e0 and its prescribed virtual velocity vector {ve0x,ve0y}. \
\n The movementExpectation dictates the sense of the movement of the whole system. \
\n It is necessary e.g. to distinguish between the active and the passive earth pressure. movementExpectation = {.. , {.. , ..}}\
\n If e0 is aligned to Earth-element the specified velocity {ve0x,ve0y} need not be exact. It will be corrected according to the edge with Earth and to the dilatancy \
\n If e0 is not aligned to the Earth-element then {ve0x,ve0y} must be specified exactly. Problems  with TWO elements with independently prescribed velocities can be also defined. \
In such case you must specify  movementExpectation={e0, {ve0x,ve0y}, e1, {ve1x,ve1y} }  ";

edges::usage= "edges: a list of ordered pairs of points constituting the sliding lines (usually with friction and cohesion) \
\n edges = {{.. , ..}, {.. , ..}, ..}. Ground surface should not be declared as edge";

edgesProps::usage= "edgesProps: a list of triples {Tan[phi], Tan[psi], c} with the friction angle, dilatancy and cohesion for each edge.\
\n  In the current version of LittleKEM the pore pressure along an edge cannot be defined. edgesProps = {{.. , .. , ..}, {.. , .. , ..}, ..}";

getPolygonArea::usage= "getPolygonArea[ polygon ] where polygon is a list of 2D points, e.g. {{x1,y1},{x2,y2}, ... {xn,yn}}   " ;

example::usage=" example[1]   prints a simple example of a notebook with the KEM calculation   " ;


 xx::usage="   xx      is a global variable    " ;
 earth::usage="    earth                         " ;
 
 
 externalLoads::usage="     externalLoads  = { {F1x,F1y}, {F2x,F2y}, ...}  resulting external force in each element             " ;
 bring2LimitLoad::usage="     bring2LimitLoad  = { {r ,0}, {0,0}, ...}  like  external force in each element with a symbolic variable, here r, to be determined.  
                   This list will be added to externalForces  " ;
 selfWeights::usage=" self weights of elements,  e.g. selfWeights=   {0,-20}  *# & /@   areasE ; The list of surfaces areasE is returned by getTopology[ ]  " ;
 interElementForces::usage=" interElementForces are inclined forces Q  computed by getHodograph[]  to be printed or plotted for individual elements          " ;
 interElementCohesions ::usage="    interElementCohesions   are  tangential forces  C computed by getHodograph[]  to be printed or plotted for individual elements    " ;
 seepageForces::usage="     like  externalLoads  = { {F1x,F1y}, {F2x,F2y}, ...}  from seepage in each element  (for graphic output only) they will be added to externalForces     " ;
 
 shortPPF::usage="   ??  shortPPF  is a short list of pore pressures forces on the edges               "  ;
 longPPF ::usage="  ??   longPPF     is a long list of pore pressures forces on the edges                        "  ;
 plottingPPF::usage="  ??   plottingPPF  is a list for plots/print  of the pore pressure forces                "  ;
 
 edgesT::usage="     edgesT    is a global variable , unit vectors   tangential  to each edge    "  ;
 edgesN::usage="     edgesN     is a global variable , unit vectors   normal  to  each edge    "  ;
 edgesL::usage="     edgesL     is a global variable, length od the edge       "  ;
 adjacentE  ::usage="     adjacentE   is a global variable      "  ;
 areasE::usage="     areasE  is a global variable  with areas of all elements       "  ;
 v::usage="     v    is a global variable         "  ;
 q::usage=" q is a global variable with unknown contact forces "
 Loads ::usage="     Loads  is a list with  externalLoads + seepageLoads + ...                      "  ;
 listForces::usage="     listForces  for print                 "  ;
 solution::usage="    solution  is a list of rules for all symbolic variables in the list of loads, usually r from   bring2LimitLoad   and q[i] interaction forces between elements "  ;
checkLoads::usage=" checkLoads[] "  ;
unknowns::usage="unknowns is a global variable with the list of symbolic quanties, usually  r from   bring2LimitLoad   and q[i] interaction forces between elements ";
findAllUnknowns::usage="  findAllUnknowns[] ";
imageSizeFactor::usage=" an option to plotForcePolygon with syntax,  e.g.  plotForcePolygon[3, solution, imageSizeFactor -> 5]  ";

(*---------------------------------------------solution functions--------------------------------------------------------------------*)

Begin["`Private`"]




getArea[xx_,elements_,e_]:=Module[{i,nv,area=0,ele,n,xxe},
    ele=elements[[e]];
    n=Length[ele];xxe=xx[[#]]&/@ele;xxe=Append[xxe,xxe[[1]]];
    For[i=1,i<=n,area+=(xxe[[i,1]]-xxe[[i+1,1]])*(xxe[[i,2]]+xxe[[i+1,2]]);i++;];
    area/2//N
];

getPolygonArea = Compile[{{v, _Real, 2}}, Block[{x, y}, {x, y} = Transpose[v];   (x.RotateLeft[y] - RotateLeft[x].y) /2]] ; (* AN 2016, not used *)


getTopology[elements_,edges_,xx_]:=Module[{eSides,LeftOfE,RightOfE,edgesT,edgesN,adjacentE,areasE ,edgesL, ne,nk,np  },
ne =  Length[elements] ; nk = Length[edges];   np = Length[xx];
If[ AnyTrue[{ne,nk,np}, # <= 0 &],   Print[ Style["Error in getTopology: ",Red], "received elements, edges or xx in wrong format or empty" ;  Goto[koNiec];   ] ;  ];
If[  AllTrue[ Flatten[ elements ]  , !MemberQ[  Range[np], # ] & ] ,   Print[ Style["Error in getTopology: ",Red], "some node numbers in elements  = ", elements , " outside ", Range[np] ] ; Goto[koNiec];  ];
eSides=Transpose[{#,RotateLeft[#]}]&/@elements;                                       (* node pairs for each el.  { {1,2,3,4},{2,3,4,1}}// Transpose => {{ {1,2},{2,3},{3,4},{4,1} }, { } }  *)
edgesT=  Normalize[xx[[ #[[2]] ]] - xx[[ #[[1]]  ]]]  & /@ edges  ;                   (*  unit vectors  tangential to edges *)
edgesN={{0,-1},{1,0}}.#&/@edgesT;                                                     (*  unit vectors  normal  to edges  (tangential rotated 90 deg CCW) *)
edgesL=  Norm[xx[[ #[[2]] ]] - xx[[ #[[1]]  ]]]  & /@ edges  ;
If[AnyTrue[edgesL, # <= 0.000001& ],Print[Style["Check geometry. One edge has length 0",Red,Bold] ]; Goto[koNiec];];
LeftOfE=Position[eSides,#][[1,1]]&/@edges;                   (* two node of an selement coincide with a given edge hence the element lies on the left hand side of this edge *)
RightOfE=Position[eSides,#][[1,1]]&/@(Reverse[#]&/@edges);   (* two node of an selement inversed coincide with a given edge hence the element lies on the right hand side of this edge  *)
If[Length[LeftOfE] != Length[RightOfE],  Print[ Style["Error in getTopology: different lengths",Red], "LeftOfE= ", LeftOfE,  "RightOfE= ", RightOfE ] ;  Goto[koNiec]; ]  ;
adjacentE=Transpose[{LeftOfE,RightOfE}];                      (* = {{eLeft,eRight}, {eLeft, eRight}, ...} list of pairs of el. numbers on the left and right wrt edge 1,2,...*)
areasE=getArea[xx,elements,#]&/@(Range[1,ne-1]);
AppendTo[areasE,0];                                           (* last element is Earth.  It has no area (by definition) *)
If[ AnyTrue[areasE, # < 0 &],   Print[ Style["Error in getTopology: ",Red], "negative values in areasE = " , areasE   ] ;  Goto[koNiec]; ];
If[ AnyTrue[areasE[[1;;-2]], # == 0 &],   Print[ Style["Error in getTopology: ",Red], " zero values in areasE = " , areasE   ] ;  Goto[koNiec]; ];
If[ AnyTrue[ adjacentE,  Length[#] != 2 & ],   Print[ Style["Error in getTopology: ",Red], " suspicious  adjacentE = ", adjacentE  ] ;  Goto[koNiec]; ];
Label[ koNiec ];  {edgesT,edgesN,edgesL,adjacentE,areasE}
];


getIntersectionPointBetweenSegments[seg1_, seg2_] :=  Module[{  xx1, xx2, xx3, xx4, xx, x, y  },
  If[Dimensions[seg1] != {2,2} ||  Dimensions[seg2] != {2,2}, Print[ " getIntersectionPointBetweenSegments: not 2x2 segments: ", seg1, " and ", seg2 ]; Abort[] ];
   {xx1, xx2} = seg1;  { xx3, xx4} = seg2;  xx = {x, y, 1};
    {xx1, xx2, xx3, xx4} = Append[#, 1] & /@ {xx1, xx2, xx3, xx4};
  solu =    Solve[ {Det[ {xx, xx1, xx2}] == 0, Det[ {xx, xx3, xx4}] == 0 }, {x, y} ][[1]];
  {x, y} /. solu    ];
  
 getOverlapLength[seg1_,seg2_ ] := Module[{x1,x2,y1,y2,overlap, l,r},  
{x1 ,x2} = {Min[#], Max[#]}& @ seg1; 
{y1 ,y2} = {Min[#], Max[#]}& @ seg2;  
 If[ x2 >=  y2, r= y2, r= x2 ];
If[x1 >=  y1, l= x1, l=  y1]; 
 If[x2 <= y1 || y2 <=  x1, overlap = 0  , overlap = Abs[ r-l]]; 
overlap
]; 
  
  
  


getPorePressureForces[ edgesPP_ ] := Module[ {ppLeft,ppRight,longPPF,shortPPF,plottingPPF,tmp },
ppLeft = { (edgesPP [[#]] * edgesL[[ # ]]* edgesN[[#]]& /@ Range[Length[edges]]) , Transpose[ adjacentE ][[1]]  } //Transpose ;
ppRight = { (edgesPP [[#]] * edgesL[[ # ]]*(-edgesN[[#]])& /@ Range[Length[edges]]) ,Transpose[ adjacentE ][[2]] } //Transpose ;
shortPPF = Array[{0,0}&, Length[elements]] ;
plottingPPF=Array[{}&,Length[elements]];
shortPPF[[ #[[2]]  ]] += # [[1]] & /@  Join[ppLeft,ppRight] ;
tmp=Transpose[Join[Transpose[ppLeft],{Transpose[ppRight][[2]]} ]  ];
Do[plottingPPF=Insert[plottingPPF,{tmp[[i,1]],tmp[[i,3]]},{tmp[[i,2]],1}  ] ,{i,1,Length[edges]}];
tmp=Transpose[Join[Transpose[ppRight],{Transpose[ppLeft][[2]]} ]  ];
Do[plottingPPF=Insert[plottingPPF,{tmp[[i,1]],tmp[[i,3]]},{tmp[[i,2]],1}  ] ,{i,1,Length[edges]}];
{shortPPF, plottingPPF}
];

Options[clearGlobals]={verbose->True};
clearGlobals[OptionsPattern[]] := Module[{},
   Clear[xx, earth, elements, edges, edgesProps];
   Clear[ movementExpectation, externalLoads,
    bring2LimitLoad , selfWeights, interElementForces,
    interElementCohesions, seepageForces, shortPPF, longPPF,plottingPPF] ;
   Clear[edgesT, edgesN, edgesL, adjacentE, areasE, v, Loads,   listForces, solution];
   If[  OptionValue[verbose] ,
   Print[ "clearGlobals undefines the following variables: 
     xx, earth, elements, edges, edgesProp,
      movementExpectation, externalLoads,
    bring2LimitLoad , selfWeights, interElementForces,
    interElementCohesions, seepageForces, shortPPF, longPPF,plottingPPF,
    edgesT, edgesN, edgesL, adjacentE, areasE, v, Loads,   listForces, solution "] ]; 
   ];


checkLoads[] := Module[{ne,nedges,unknownR, unknownQ, nR},
 ne=Length[elements] ;
nedges = Length[edges];
If[MemberQ[edgesL,0],Print[Style["Check geometry. One edge has length 0",Red,Bold] ]; Abort[];];
If[ ne== 0 || nedges == 0 , Print[Style["Error in checkLoads: ne=0 or nedges=0 encountered ",Red,Bold] ];  Abort[];] ;
If[ Length[selfWeights] != ne  , Print[Style["Error in checkLoads:   suspicious length of selfWeights  ",Red,Bold] ];   Abort[];  ] ;
If[ Length[ bring2LimitLoad ] == 0, Print[ Style["Error in checkLoads:  bring2LimitLoad does not exist or of  Length zero ",Red,Bold]  ];  Abort[];];
 ]  ;

findAllUnknowns[] := Module[{nR, unknownR, unknownQ },
unknownR = Select[ Flatten[ bring2LimitLoad ],  Not[NumericQ[#]]& ] ;
unknownR = If[ Not[AtomQ[ # ]] , Select[  Apply[List, # ],  Not[NumericQ[#]] & ][[1]], #  ]& /@ unknownR ; (* extracts  x  if bring2LimitLoad  contains nonatomic expressions like 3 x   *)   ;
unknownR = Union[unknownR] ;                                                                   (* = DeleteDuplicates[ ]  e.g.  if bring2LimitLoad  contains { -2*x, x}   *)
nR =  Length[unknownR] ;
If[ nR==0,  Print[ Style["error in findAllUnknowns: no symbolic bring2Limit values nR == 0 ",Red,Bold] ];  Goto[koniec] ;] ;
unknownQ=Array[q,Length[edges]];
{ unknownQ,unknownR } //Flatten
 ];
 
 
Options[initializeGlobalArrays]={verbose->True};
initializeGlobalArrays[ OptionsPattern[] ]:=Module[{ne,nedges, nR},    (* initialize basing on elements edges *)
ne=Length[elements] ;
nedges = Length[edges];
bring2LimitLoad = Array[{0,0}&, ne ]   ;
selfWeights = Array[{0,0}&, ne ]  ;
seepageForces = Array[{0,0}&,ne];
Loads = Array[{0,0}&,ne];
porePressures = Array[ 0&,nedges];
{shortPPF,plottingPPF} = getPorePressureForces[ porePressures  ] ;
externalLoads = Array[{0,0}&,ne];
interElementForces = Array[{ }&,ne];     (* to be printed for individual elements *)
interElementCohesions= Array[{ }&,ne];   (* to be printed for individual elements *)
If[  OptionValue[verbose] ,
Print[ "The  global variables are initialized as follows:" ];
Print[ "bring2LimitLoad = ", bring2LimitLoad  ];   Print[ "selfWeights = ", selfWeights];
Print[ "seepageForces = ", seepageForces ];        Print[ "externalLoads = ", externalLoads ];
Print[ "porePressures = ", porePressures ];        Print[ "Loads = ", Loads];
Print[ "After this initialization MUST define bring2LimitLoad and you may modify some others manually" ];
Print[ "If you use non-zero porePressures you MUST execute: {shortPPF,plottingPPF} = getPorePressureForces[ porePressures  ] ;" ];
]; 
];

Options[getHodograph]={verbose->True};
getHodograph[ OptionsPattern[]   ]:=Module[{ ne,haveV,e0,v0,edge0,signN,signT,myAdjacentE,MyNeighbours,MySolvedNeighbours,e1,e2,l01,l10,l02,l20,signN1,signN2,
                         tanPsi1,tanPsi2,tanPhi1,tanPhi2,coh1,coh2,cohesion1,cohesion2,v01,v02,T1,T2,N10,N20,eqs1,eqs2,solu,Qe10,Qe20,v,
                         nonspecial,firstOffEarth,nVerbose}, 
nVerbose =   OptionValue[verbose] ;                        
ne=Length[elements];        (* number of elements *)
haveV=Array[#==ne &,ne];    (* create a logical vector  which elements have known velocity (at first just the Earth) *)
v=Array[{0,0}&,ne];         (* initialize element velocities *)

secondPrescribedMovement = False;  {e0,v0}=movementExpectation[[1;;2]]; (*  AN 2017 element e0  has the prescribed velocity.  e0 is usually adjacent to Earth. Otherwise it needs a special treatment *)
Label[handlePrescribedElement]  ;      (* AN 2017 *)
If[ nVerbose, Print["handle prescribed element e0= ", e0, " with v0= ",v0]];
firstOffEarth  = Not[ MemberQ[adjacentE,{e0,ne}] ||  MemberQ[adjacentE,{ne,e0}] ];  (* true if no edge between element e0 and Earth,  *)
If[ firstOffEarth ,
    v[[e0]]=v0; haveV[[e0]]=True;
		 If[ nVerbose,  Print[" Off-Earth prescribed element ", e0, " has velocity ", v[[e0]]  ]];
     Goto[nonspecial];  (* if movem.expectation refers to element with no contact to Earth then the expected velocity is taken exactly as prescribed *)
];
{edge0}={Position[adjacentE,{e0,ne}],Position[adjacentE,{ne,e0}]} //Flatten;       (* edge0 = is the Nr of the edge between e0 and Earth  *)
{T1,N10,{tanPhi1,tanPsi1,coh1}}={edgesT[[edge0]],edgesN[[edge0]],edgesProps[[edge0]]}; (* props of the special edge  *)
{signN,signT}={If[adjacentE[[edge0,1]]==e0,1,-1],If[T1.v0>0,1,-1]};  (* if prescribed e0 on the left of edge0 do not change the sign of unit vectors N and T   *)
N10*=signN;                    (* from movementExpectation edge points towards e0 *)
v[[e0]]=Normalize[ (signT*T1+N10*tanPsi1)]*Sqrt[v0.v0] ;  (* improved direction of  the prescribed velocity  but the norm exactly as in  movementExpectation  AN 2017 *)
haveV[[e0]]=True;
If[ nVerbose, Print[" Prescribed element ", e0, " has velocity ", v[[e0]]  ]];

 (* relative movement on edge0 has been calculated, hence we may find forces on this edge  *)
Qe10=If[signT>0,N10-tanPhi1*T1,N10+tanPhi1*T1]*unknowns[[edge0]];  (* unknowns[[1;;nedges]] = values of the normal component of the contact force  on each edge   *)
cohesion1=If[signT>0,-T1,T1]*edgesL[[edge0]]* coh1;
Loads[[e0]]+=Qe10+cohesion1;
Loads[[ne]]-=Qe10+cohesion1;
interElementForces = Insert[interElementForces , {Qe10,ne},    {e0,1}];  (* Prepend force Qe10 and element e1 of its origin  to the component e0 of  interElementForces *)
interElementForces = Insert[interElementForces , {-Qe10,e0}, {ne,1}];  (* Prepend force -Qe10 and element e0 of its origin  to the component e1 of  interElementForces *)
 interElementCohesions=   Insert[interElementCohesions , {cohesion1,ne},    {e0,1}];  (* Prepend cohesion1 and el. e1 of its origin  to the component e0 of  interElementCohesions *)
 interElementCohesions=   Insert[interElementCohesions , {-cohesion1,e0},    {ne,1}];

Label[nonspecial];
If[secondPrescribedMovement==False && Length[movementExpectation] == 4, secondPrescribedMovement=True; {e0,v0} =movementExpectation[[3;;4]];  Goto[handlePrescribedElement]    ]  ; (* AN 2017 *)

e0=0;
While[MemberQ[haveV,False],For[e0=1,e0<=ne-1,If[haveV[[e0]],Goto[nexte0]]; (* take next element e0 without computed velocity *)
myAdjacentE=Pick[adjacentE,(MemberQ[#,e0]&/@adjacentE)];  (* find edges around the current element e0 *)
MyNeighbours=Complement[Flatten[myAdjacentE],{e0}];                  (* find  neighbouring elements behind these edges *)
MySolvedNeighbours=Pick[MyNeighbours,(haveV[[#]]&/@MyNeighbours)];    (* find solved neighbouring elements (with computed velocity) *)
If[Length[MySolvedNeighbours]!=2,Goto[nexte0]];        (* element e0 cannot be solved, we need two solved neighbours to proceede with e0 *)
{e1,e2}=MySolvedNeighbours;
If[ nVerbose,  Print["Calculating velocity of element ", e0, " from elements ",e1," and ", e2] ];    (* set velocity eqs system based on edges 01 and 02*)
{l01,l10}=Length/@{Position[adjacentE,{e0,e1}],Position[adjacentE,{e1,e0}]};    (* {1,0} = e0 is on the left of the edge  01 {0,1} = ont the right *)
{l02,l20}=Length/@{Position[adjacentE,{e0,e2}],Position[adjacentE,{e2,e0}]};    (* {1,0} = e0 is on the left of the edge  02 {0,1} = ont the right *)
{{edge1}}=If[l01==1,Position[adjacentE,{e0,e1}],Position[adjacentE,{e1,e0}]];   (* edge1 is set to the global number of the edge *)
{{edge2}}=If[l02==1,Position[adjacentE,{e0,e2}],Position[adjacentE,{e2,e0}]];   (* edge1 is set to the global number of the edge *)
{signN1,signN2}={If[l01==1,1,-1],If[l02==1,1,-1]};
{N10,T1,N20,T2}={signN1*edgesN[[edge1]],edgesT[[edge1]],signN2*edgesN[[edge2]],edgesT[[edge2]]};  (* calculating vel. of e0 Ns should be oriented to the inside of e0*)
{tanPhi1,tanPsi1,coh1}=edgesProps[[edge1]];
{tanPhi2,tanPsi2,coh2}=edgesProps[[edge2]];
eqs1={v01,v02}-v[[e1]]==a*T1+Abs[a]*tanPsi1*N10;   (* kinematic equations to find velocity {v01, v02}  *)
eqs2={v01,v02}-v[[e2]]==b*T2+Abs[b]*tanPsi2*N20;
solu=Solve[{eqs1,eqs2},{a,b,v01,v02}];
If[ Length@solu == 0 , Print[ Style["Error in getHodograph: ",Red], "constraints on el. ", e0,  " from el.  ",e1," and el. ", e2,  " seem  contradictive."  ];  Goto[koniec]; , solu = solu[[1]] ] ;
{alpha,beta}={a,b}/.solu;
If[Length[{solu}]!=1,Print["no or multiple velocity of ",e0,"wrt",e1," and ",e2];Abort[];];
If[alpha*beta==0,Print["no velocity jump of elem=",e0,"wrt",e1," or ",e2]; Abort[];];
v[[e0]]={v01,v02}/.solu;haveV[[e0]]=True;    If[ nVerbose, Print[" element ", e0, " has velocity ", v[[e0]]  ]];
Qe10=If[alpha>0,N10-tanPhi1*T1,N10+tanPhi1*T1]*unknowns[[edge1]];
cohesion1=If[alpha>0,-T1,T1]*edgesL[[edge1]]* coh1;
 Qe20=If[beta>0,N20-tanPhi2*T2,N20+tanPhi2*T2]*unknowns[[edge2]];
cohesion2=If[beta>0,-T2,T2]*edgesL[[edge2]]* coh2 ;
Loads[[e0]]+=Qe10+Qe20+cohesion1+cohesion2;
Loads[[e1]]-=Qe10+cohesion1;
Loads[[e2]]-=Qe20+cohesion2;
interElementForces = Insert[interElementForces , {Qe10,e1},    {e0,1}];             (* Prepend force Qe10 and element e1 of its origin  to the component e0 of  interElementForces *)
interElementForces = Insert[interElementForces , {-Qe10,e0}, {e1,1}];               (* Prepend force -Qe10 and element e0 of its origin  to the component e1 of  interElementForces *)
interElementForces = Insert[interElementForces , {Qe20,e2},    {e0,1}];             (* Prepend force Qe20 and element e2 of its origin  to the component e0 of  interElementForces *)
interElementForces = Insert[interElementForces , {-Qe20,e0}, {e2,1}];                (* Prepend force -Qe20 and element e0 of its origin  to the component e2 of  interElementForces *)
interElementCohesions=   Insert[interElementCohesions , {cohesion1,e1},    {e0,1}];  (* Prepend cohesion1 and el. e1 of its origin  to the component e0 of  interElementCohesions *)
interElementCohesions=   Insert[interElementCohesions , {-cohesion1,e0},    {e1,1}];
interElementCohesions=   Insert[interElementCohesions , {cohesion2,e2},    {e0,1}];
interElementCohesions=   Insert[interElementCohesions , {-cohesion2,e0},    {e2,1}];
Label[nexte0];e0++;];
];
Label[koniec];{v,Loads}
];

getAFRdissipationRate[edgesProps_, edgesT_, edgesN_, edgesL_,   adjacentE_, areasE_, v_] := Module[{nedge, iedge, c, tphi, tpsi, diss, ie, je},
  nedge = Length[edgesL]; diss = 0;
  Do[{ tphi, tpsi, c} = edgesProps[[iedge]];
   If[Abs[tphi - tpsi] > 0.001, Print["AFR violaten on edge ", iedge];     Abort[]];
   ie = adjacentE[[iedge, 1]]; je = adjacentE[[iedge, 2]] ;
   diss +=     c*edgesL[[iedge]]*     Abs[(v[[ie]] - v[[je]]).edgesT[[iedge]]];, {iedge, 1, nedge}];   diss];

getNAFRdissipationRate[edgesProps_, edgesT_, edgesN_, edgesL_, adjacentE_, areasE_, v_, solution_] := Module[{nedge, iedge, c, tphi, tpsi, diss, ie, je}, nedge = Length[edgesL]; diss = 0;
   Do[{tphi, tpsi, c} = edgesProps[[iedge]];
    ie = adjacentE[[iedge, 1]]; je = adjacentE[[iedge, 2]];
    diss += (c*edgesL[[iedge]] + (q[iedge] /. solution ) *(tphi - tpsi)  )*Abs[(v[[ie]] - v[[je]]).edgesT[[iedge]]];
    , {iedge, 1, nedge}];
   diss];

getPowerSupply[externalLoads_, seepageForces_, selfWeights_, shortPPF_, v_] := Module[{A, nelem, ie, Loads}, nelem = Length[v];
  A = 0;  Loads = (externalLoads + seepageForces + selfWeights + shortPPF);
  Do[A += Loads[[ie]].v[[ie]];, {ie, 1, nelem}];     A];






(*---------------------------------------------------plot functions ---------------------------------------------------------------------*)

Options[plotKEM]={nodeLabels->True,elementLabels->True,edgeLabels->True};

plotKEM[elements_,edges_,xx_,v_,  OptionsPattern[] ]:=Module[
{z0,z1,lines, lines0,lines1,e,ne, nne,   eg, nLabel, eLabel, gLabel },

nLabel =  OptionValue[nodeLabels];
eLabel =  OptionValue[elementLabels];
gLabel = OptionValue[edgeLabels];
ne=Length[elements];
ng=Length[edges];
{xmm,ymm}=Transpose[xx];
window={Point[{Min[xmm]-1,Min[ymm]-1}],Point[{Max[xmm]+1,Max[ymm]+1}]};
lines1=Range[1,ne-1];
lines0=Range[1,ne-1];
For[e=1,e<=ne-1,
closedNodes = Flatten[  {elements[[e]],elements[[e,1]]}  ];
z0=(xx[[#1]]&)                /@ closedNodes;     (* original coordinates of element e *)
z1=(xx[[#1]]+v[[e]]&)/@closedNodes;  (* displaced coordinates of element e *)
lines0[[e]]={Hue[0.1 * e],Line[z0]};
lines1[[e]]={Dashing[{0.05`,0.02`}],Hue[e* 0.1],Line[z1]};
e++];
lines=Join[lines0,lines1];
If[nLabel,nodeNames=Table[Style[ Text[i,xx[[i]]] ,FontFamily->"Arial"],{i,1,Length[xx]}];,nodeNames={};];
If[eLabel,
elNames=Table[Style [Text[e,Sum[xx[[elements[[e,i]]]], {i,1,Length[elements[[e]]]}] /Length[elements[[e]]] ],FontFamily->"Arial" ],{e,1,ne-1}];,
elNames={};
];
If[gLabel,egNames=Table[Text[eg, ( xx[[edges[[eg,1]]]]+xx[[edges[[eg,2]]]])/2    ],{eg,1,ng}];,
egNames={};
];
egNames=Prepend[egNames,Hue[0.9`]];elNames=Prepend[elNames,Hue[0.55`]];
conventions=Text[{Style["Elements ",Blue],Style["Nodes ",Black],Style["Edges",Red] } ,{Min[xmm],Min[ymm]-0.5}];
Show[Graphics[{lines,nodeNames,elNames,egNames,White, window,Black, conventions}],BaseStyle->{FontSize->14},AspectRatio->Automatic]
];


Options[plotHodograph]={printValues->False, printElementLabels->True};

plotHodograph[ v_, adjacentE_ , OptionsPattern[] ] := Module[{nv, n1,n2, aux,labels, labeledV , lines, points, g0, g1, g2, g3, printValue ,printElementLabel },

printValue  =  OptionValue[printValues];
printElementLabel  = OptionValue[printElementLabels];
 labeledV =Transpose[ {v,Range[  Length[v]    ] } ] ;   (* for each element: calculated velocity followed by the number of  element e.g. {{{Vex, Vey }, e},  } *)

lines = Line[  {v[[ #[[1]]  ]] , v[[#[[2]] ]] }  ] & /@ adjacentE;
points = Point[v];

g0 = Graphics[{PointSize[Medium], Red, lines },   AspectRatio -> Automatic , Axes-> True, AxesLabel->  {"Vx","Vy"} ]   ;
g1 =  ListPlot[{PointSize[Medium], Red,  Point[v] }   ]   ;
g2 = Null;
If[printElementLabel ,  g2=  {Text[Style["V"<>ToString[ #[[2]] ],FontSize->9,FontFamily->"Arial"] ,1.1*#[[1]] ] }& /@ labeledV ]  ;
If[printValue  && printElementLabel ,
   g2= {Text[Style["V"<>ToString[ #[[2]]  ]<>"="<>ToString[SetPrecision[#[[1]] ,3]],FontSize->9,FontFamily->"Arial"] ,1.1*#[[1]] ] }& /@ labeledV
   ] ;
  Show[g0, g1, Graphics[g2] ]
 ] ;


PrintForces[solution_]:=Module[{EN},
Do[
Print[Style["Non-zero forces acting on element "<>ToString[EN],Purple,Bold,18] ];
If[ (Norm[ bring2LimitLoad[[EN]]//.solution  ] // Chop) > 0,
     Print["bring2LimitLoad: r",EN,"=",bring2LimitLoad[[EN]]//.solution]
   ];
If[ (Norm[ externalLoads[[EN]]//.solution]   // Chop) > 0,
Print["externalLoads: L",EN,"=",externalLoads[[EN]]//.solution];
];
Print["selfWeights: G",EN,"=",selfWeights[[EN]]//.solution];


Print[Style["interElementForces: ",Green] ];
Do[
Print["  Q",EN,interElementForces[[EN,i,2]],"=",interElementForces[[\
EN,i,1]]//.solution, "   ||Q",EN,interElementForces[[EN,i,2]],"||=",Norm[interElementForces[[\
EN,i,1]]//.solution] ];
,{i,1,Length[ interElementForces[[EN]] ] } ];


 If[ Chop[ Sum[ Norm[ interElementCohesions[[EN,i,1]]//.solution ], {i,1,Length[ interElementCohesions[[EN]] ] }   ]] > 0,
 Print[Style["interElementCohesions: ",Orange] ]
 ];
Do[
If[ (Norm[ interElementCohesions[[EN,i,1]]//.solution ]//Chop) > 0,
Print["  C",EN,interElementCohesions[[EN,i,2]],"=",\
interElementCohesions[[EN,i,1]]//.solution, "   ||C",EN,interElementCohesions[[EN,i,2]],"||=",Norm[ interElementCohesions[[EN,i,1]]//.solution ] ]
];
,{i,1,Length[ interElementCohesions[[EN]] ] } ];


If[ ( Norm[ seepageForces[[EN]]//.solution ]   // Chop) > 0 ,
     Print["seepageForces: f",EN,"=",seepageForces[[EN]]//.solution]
];   (* print only if not zero *)

If[  Chop[ Sum[    Norm[ plottingPPF[[EN,i,1]]//.solution], {i,1,Length[ plottingPPF[[EN]] ] }   ]],
Print[Style["pore pressure forces plottingPPF: ",Blue]];
] ;
Do[
If[ (Norm[ plottingPPF[[EN,i,1]]//.solution]//Chop ) > 0,
Print["  U",EN,plottingPPF[[EN,i,2]],"=",plottingPPF[[EN,i,1]]//.\
solution, "   ||U",EN,plottingPPF[[EN,i,2]],"||=",Norm[ plottingPPF[[EN,i,1]]//.solution] ]
];
,{i,1,Length[ plottingPPF[[EN]] ] } ];
Print[Style["--------------------------------------------------------------------------",LightPurple]];
,{EN,1,Length[ elements ]-1}]  ;
]


Options[plotForcePolygon]={imageSizeFactor-> 2.0,printForceVal->False,printElementLabel->True,printForceName->True, legendForce-> "automatic" , verbose-> False };

plotForcePolygon[ EN_, solution_, OptionsPattern[] ] := Module[{Rf, L, G,  Qs , Cs,FF,Fs,Us,n,Ename,listOfPoints,i, f,t,vectors,pFV,
peL,pfN,FN,isF,xmax,ymax,xmin,ymin,middle,imagesize,corners,papersize, allRs,allLs,allGs,  allQs , allCs, allfs, allUs, g01,g02,g0,g3,g41,g42,g43,g44,g4,dx,dy,dd,dLegend,legendF,oV },
 If[ EN < 1 || EN >= Length[elements], Print["element number ",EN, " out of range" ]; Abort[]; ] ;

{pFV,peL,pfN,isF,legendF,oV}= OptionValue[ {printForceVal,printElementLabel,printForceName,imageSizeFactor,legendForce,verbose}];

(* ForceList= closure force, external loads, weight forces, friction forces, cohesion forces, seepage force, pore pressure force *)
ForceList  = {bring2LimitLoad, externalLoads, selfWeights,    interElementForces, interElementCohesions, seepageForces ,    plottingPPF};
{allRs,allLs,allGs,allQs,allCs, allfs, allUs}=ForceList//.solution;
{Rf, L ,G ,  Qs , Cs,Fs, Us }   = Chop[ #[[EN]] ]& /@{allRs, allLs ,allGs ,  allQs  , allCs, allfs, allUs};  (* forces for the current element EN *)

FF={}; FN={};  (* making labels:  FF = values of force, FN = names of forces.   They will be set for nonzero forces only *)
Ename = ToString[EN];
If [  Norm[Rf ]   >0,   AppendTo[FF,Rf]; AppendTo[FN,"R"<>Ename] ];   (* bringToLimit *)
If [  Norm[ L ]   >0,   AppendTo[FF,L];  AppendTo[FN,"L"<>Ename] ];     (* external load *)
If [  Norm[ G ]   >0,   AppendTo[FF,G] ; AppendTo[FN,"G"<>Ename]];      (* weight *)
If [  Norm[ Fs ]  >0,   AppendTo[FF,Fs] ; AppendTo[FN,"Fs"<>Ename]];     (* seepage *)
Do[  
If[  Norm[ Qs[[i,1]] ] > 0,   AppendTo[ FF,Qs[[i,1]] ];    AppendTo[ FN,"Q"<>Ename<>ToString[  Qs[[i,2]]] ];  ]   (* Qs[[i,2]] = number of the neighbour element from which this Q enforces   *)     
,{i,1,Length[Qs]}   ];
Do[  
If[   Norm[ Cs[[i,1]] ] >0,   AppendTo[ FF,Cs[[i,1]] ];  AppendTo[ FN,"C"<>Ename<>ToString[  Cs[[i,2]] ] ]; ]    
 ,{i,1,Length[Cs]} ];
Do[  
If[  Norm[ Us[[i,1]] ] > 0, AppendTo[ FF,Us[[i,1]] ];  AppendTo[ FN,"U"<>ToString[  Us[[i,2]] ] ]; ]     
 ,{i,1,Length[Us]} ];

listOfPoints= {{0,0}};  
 AppendTo[listOfPoints, listOfPoints[[-1]] + # ]& /@  FF ; 
vectors = Partition[listOfPoints,2,1]; 
{xmin, xmax} = MinMax[ listOfPoints[[All,1]]] ; 
{ymin, ymax} = MinMax[ listOfPoints[[All,2]]] ; 
  middle = {xmin+xmax , ymin+ymax}/2;   
 { dx, dy }=   { xmax-xmin,ymax-ymin } ;
   imagesize = Max[ dx, dy]; 
  corners =   { {xmin, ymin}-0.1*imagesize ,  {xmax, ymax}+  0.1*imagesize  };
papersize={{corners[[1,1]],corners[[1,1]]+Max[Differences[corners]]},{corners[[1,2]],corners[[1,2]]+Max[Differences[corners]]}};

If[peL,  Ename=ToString[EN],   Ename=""];   (* print element label option *)
If[!pfN,FN=Array[{""}&,Length[vectors]]//Flatten;];
If[pfN && pFV, FN =#<>"="& /@  FN ];

g01 =  Graphics[Append[
        Apply[{Arrowheads[Medium], Arrow[#1], Style[Text[#2<>ToString[Differences[#1]//Flatten],Mean[#1]] ,FontFamily->"Arial"]}& ,
         Transpose[{vectors , FN}],{1}], {Opacity[0.3], Text[Style[Ename, FontSize->25, FontFamily->"Arial",FontColor-> Red],middle] } ],
         AspectRatio->1,  ImageSize -> imagesize*isF,  PlotRange->papersize] ;

g02 = Graphics[ Append[Apply[{Arrowheads[Medium],Arrow[#1],Text[#2,Mean[#1] ]}& ,
      Transpose[{vectors,FN}],{1}],{Opacity[0.3],Text[Style[Ename, FontSize-> 25,FontFamily->"Arial", FontColor-> Red],middle] } ],
      AspectRatio->1,ImageSize -> imagesize*isF,PlotRange->papersize] ;

g3 =  Graphics[ {White, Point[corners] } ]  ;

If[  NumberQ[ legendF], dLegend=   legendF , 
dd = Max[dx,dy]; 
dLegend = Which[ dd< 5, 1,   5<=dd< 10, 2.5,   
  10<= dd <50, 10,           50<=dd< 100, 25, 
 100<= dd <500, 100,           500<=dd< 1000, 250, 
 1000<= dd <5000, 1000,        5000<=dd< 10000, 2500, 
 10000<= dd <50000, 10000,    50000<=dd , 25000
]  ];
If[ oV, Print["dd=", dd, " legendF=", legendF, " dLegend=", dLegend ] ];  
g4 = Graphics[{Arrowheads[Medium], Blue, Arrow[{ {0,0} + 0.2*dLegend*{1,0} , {0,0} +1.2*dLegend*{1,0}}      ],
      Text[Style[ ToString[dLegend]<> "kN/m",FontFamily->"Arial"],   dLegend*{1,0.2}/2 ] }  ,
       AspectRatio->1,ImageSize -> imagesize*isF,PlotRange->papersize
      ]  ;
If[pFV, g0= g01, g0= g02]; 
Show[  {g0 ,g3, g4} ]
   ];



showKEMtranslation[elements_,edges_, xx_,v_, nFrames_, scaleV_ ] := Module[{ },
Animate[ plotKEM[elements,edges, xx,v*(i/nFrames)*scaleV, edgeLabels-> False, nodeLabels->False,elementLabels->True] ,{i,0,20}   , AnimationRunning->False ]
];


example[ i_:1 ] := Module[ {} , 
If[i==1,   Print[" 
\n     xx = {{0.0 , 3.5}, {4.0 , 3.5}, ..};  \
\n     elements = {{1 , 2 , 3}, {3 , 4 , 5 }, ..}; \
\n     movementExpectation = {2 , {-1 , -1}} ;  \
\n     edges = {{ 1, 2 }, {2 , 3}, ..}; \
\n     {edgesT, edgesN, edgesL, adjacentE, areasE} =   getTopology[elements, edges, xx];   \
\n     edgesProps = {{30\[Degree] , 10\[Degree] , 10.5}, { , , }, ..}; \
\n     bring2LimitLoad  = { {0 ,0 }, { r, 0 }, ..};   (* should contain unknown force r *)       \
\n    {v, Loads} = getHodograph[];   (* determine velocities c and add unknown inter-element forces to Loads *)    \
\n    Loads += selfWeights + externalLoads + bring2LimitLoad +  seepageForces + shortPPF;    \
\n    staticEquilibriumEquations = Loads[[#]] == {0, 0} & /@ (Range[1, Length[elements] - 1]);  \
\n    {solution} = Solve[staticEquilibriumEquations, unknowns]  (* <----- check output if all q-s are positive and what force r is required for limit equilibrium *) \
\n    PrintForces[solution]      plotForcePolygon[elem, listForces, solution]   /. elem -> 2 \
\n    plotHodograph[ v , adjacentE  ]  \
\n    showKEMtranslation[elements, edges, xx, v, 20, 1] \  
"]
]; 

If[i==2,   Print["
KEMTargetFunction[(ab_)?NumberQ, (db_)?NumberQ] := Module[{b, aB, dB}, 
clearGlobals[verbose -> False]; b = 1; aB = ab*b; dB = db*b; 
    xx = {{-b/2, 0}, {0, -0.69}, {b/2, 0}, {1.2*(b/2), -dB}, {b + aB/4, -dB}, {b/2 + aB, 0}, {-1.2*(b/2), -dB}, {-b - aB/4, -dB}, {-b/2 - aB, 0}}; 
    elements = {{9, 8, 1}, {8, 7, 1}, {7, 2, 1}, {2, 3, 1}, {2, 4, 3}, {4, 5, 3}, {5, 6, 3}, {6, 5, 4, 2, 7, 8, 9}}; 
    edges = {{1, 2}, {2, 3}, {2, 4}, {4, 5}, {5, 6}, {3, 4}, {3, 5}, {2, 7}, {7, 8}, {8, 9}, {1, 7}, {1, 8}}; 
    {edgesT, edgesN, edgesL, adjacentE, areasE} = getTopology[elements, edges, xx]; 
    edgesProps = N[Array[{Tan[30*Degree], Tan[0*Degree], 10.} & , Length[edges]]]; 
    initializeGlobalArrays[verbose -> False]; 
    bring2LimitLoad = {{0, 0}, {0, 0}, {0, 0}, {r1, r2}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}; 
    specificWs = Array[{0, -20} & , Length[elements]]; 
     movementExpectation = {4, {0, -1}}; 
    unknowns = findAllUnknowns[]; {v, Loads} = getHodograph[verbose -> False]; 
    selfWeights = ({0, -20}*#1 & ) /@ areasE; 
    Loads += externalLoads; Loads += bring2LimitLoad; 
    Loads += seepageForces; Loads += selfWeights; Loads += shortPPF; checkLoads[]; 
    staticEquilibriumEquations = (Loads[[#1,All]] == {0, 0} & ) /@ Range[1, Length[elements] - 1]; 
    {solution} = Solve[staticEquilibriumEquations, unknowns]; -r2 /. solution];
NMinimize[   {KEMTargetFunction[ab , db  ]  , 2.5<ab<3.5 ,  1 < db<1.5  } ,{ab,db}, 
             Method\[Rule]{\"NelderMead\",\"InitialPoints\"\[Rule]{{2.1, 1.1} , { 2.2,1.0} , {2.0,1.2}}  }, 
              MaxIterations\[Rule]100   ] 
"]
]; 

];





End[]

EndPackage[ ]



$Context = "KEM`KEMTools2016u`"   ;
Off[General::spell,General::spell1,Solve::ifun,ReplaceAll::reps,ReplaceAll::rmix,General::stop];
  Print[ " Mathematica  tools for the kinematic element method (KEM) by A. Niemunis and F.Prada 2010 - 2020
   You are in the context KEM`KEMTools2016u`.
    It provides the following tools:  
    ESSENTIAL:       clearGlobals  getTopology initializeGlobalArrays  getHodograph findAllUnknowns getPorePressureForces
    GEOMETRY:         getIntersectionPointBetweenSegments   getOverlapLength getPolygonArea   getArea
    OUTPUT/PLOTS:       plotKEM,  plotForcePolygon, plotHodograph, showKEMtranslation  PrintForces   
    AUXILIARIES:   checkLoads getPowerSupply   getNAFRdissipationRate   getAFRdissipationRate 
   For an example  of manual input execute example[] or, with optimalization, example[2] 
 
" ];

