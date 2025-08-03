(* ::Package:: *)

(*    Mathematica  tools for solving examples with the kinematic element method (KEM) by A. Niemunis 2007

This  notebook  solves  a  kinematic  failure  mechanism  using the kinematic element method (KEM).   No optimization of geometry is performed.
This notebook is for didactic purposes  only.  Use it at your own risk , I guarantee nothing.

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   or C:\Users\xxx\AppData\Roaming\Mathematica  where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory  KEM there
3) copy this file (= KEMTools2010u.m) to the new directory.
4) Begin a new  Mathematica session with:    Needs["KEM`KEMTools2010u`"]
   or study some precalculated examples opening the notebook testKEMTools6.nb


New in 2013: 
can find velocity if more than 2 neighbouring velocities are known (the user is responsible that  the third neighbout is kinematically consistent)
If AFR is everywhere satisfied it is possible to find Bring2LimitLoad from the comparison A=D rather than from Equilibrium. 
For this purpose, having solved the hodograph, one may write
diss = getAFRdissipationRate[edgesProps , edgesT , edgesN , edgesL ,    adjacentE , areasE , v ] ;
sup = getPowerSupply[externalLoads, seepageForces, selfWeights,   shortPPF, v];
extras =  Sum[  bring2LimitLoad[[ie]].v[[ie]]  , {ie, 1, Length[ v]  }]; 
Solve[extras + sup  == diss, r] [[1, 1]]


using  the new functions 
getAFRdissipationRate[edgesProps_, edgesT, edgesN, edgesL,   adjacentE, areasE, v] 
getPowerSupply[externalLoads, seepageForces, selfWeights, shortPPF, v] 

*)

BeginPackage["KEM`KEMTools2010u`"]    (*No-Air-Element version 2009*)
(*---------------------------------------------usages--------------------------------------------------------------------*)

normalizev::usage = "normalizev[ v] normalizes a vector of 2 components" ;

normv::usage = "normv[v] returns a norm of a vector of 2 components";

dir::usage = "dir[xx ,{i,j}] returns the direction (a unit vector) from the first to the second node (used for edges)" ;

getLength::usage = "getLength[xx ,{i,j}] returns the distance between two nodes (used for edges)" ;

getArea::usage = "getArea[xx, elements, e] returns the area within a polynom (an element) with vertices \
\n      xx: table with the x, y coordinates \
\n      elements: connectivity table with global element node numbers \
\n      e: specific element whose area will be calculated ";

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

plotForcePolygon::usage = "plotForcePolygon[ elementNumber, forceList, solution, opts] draws the polygon of forces for the body elementNumber \
\n forceList={bring2LimitLoad,externalLoads,selfWeights,interElementForces, interElementCohesions,seepageForces } , e.g {{10.0,1.0},{-5.0,6.7},...} contains vectors [kN/m] \
\n to be plotted in polygon preserving their sequence.\
\n nsolution = a list of rules to evaluate the  unknown inter-element forces in the  listForces \
 The optional parameters opts may contain: \
\n ElementName, quoted symbol of the element \
\n imagesizeFactor  = the length of 1 kN/m Force in pixels, Default=1 ,\
\n printForceVal = whether to label all forces, Default=True,\
\n printElementLabel = whether to print element name, Default=False,\
\n printForceName = whether to label forces, Default=False. \ ";


plotHodograph::usage="plotHodograph[ v , adjacentE  ]  plots velocities of  all elements. It needs  the following argumets : \
\n v = list of   velocity vectors  of all elements \
\n adjacentE = a topological array created  by the getTopology routine  ";

showKEMtranslation::usage="showKEMtranslation[elements ,edges, xx,v, nFrames, scaleV]  \
\n creates animated KEM mesh: using nFrames between the original position  and displaced position according to the vector 'v' . \
\n The edges are numbered (labelled).\
\n scaleV  is a scaling factor for the displacements v  \
\n Although the arguments  elements,edges,xx   are all global variables their presence is obligatory. \
\n The velocity v must be specified to indicate the position of the 'deformed' mesh and is used for animations: \
\n e.g v=Array[{0,0}&,Length[elements]] applies zero movement ";



getHodograph::usage = "getHodograph[ verbose ]     This module returns the list {v,Loads} containning two lists:
\n 1) element velocity vectors  v which are calculated in  getHodograph \
\n 2) modified the global Loads which are partly unknown after  getHodograph. \
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
\n The procedure requires assiciated flow rule (AFR) on all edges 
";

getPowerSupply::usage="getPowerSupply[externalLoads, seepageForces, selfWeights, shortPPF, v]  \ 
\n Given prescribed loads and solved kinematics (with corrected prescribed velocity) this module returns the supplied  power A. \
\n The prescribed loads do NOT include cohesion forces, friction forces, normal contact forces on edges and  Bring2LimitLoad. \
\n This module is useful together with  the module getAFRdissipationRate[.... ] to find the Bring2LimitLoad without solving \
\n the equilibrium of forces. 
";
 

getPorePressureForces::usage= "{ shortPPF, plottingPPF} =  getPorePressureForces[ porePressures ]   \
\n In the case of instationary seepage (for example due to pp distribution after an earthquake) it is convenient to perform the total stress analysis.  \
\n Instead of buoyant specific weight and seepage forces we use the saturated specific weight and pore pressures along  the edges = porePressures     \
\n The list porePressures contains the pore pressures (usually in kPa) for each edge. This pressure is multiplied by the length of the edge and the respective edgesN.  \
\n The routine returns the resulting pp-forces acting on each element. They are given in two forms: \
\n a short one  (= shortPPF) to be directly added to Loads for evaluation of  the equilibrium condition \
\n a long one (= plottingPPF) to be used in graphic output ";

initializeGlobalArrays::usage= " initializeGlobalArrays[]   \
\n Initialises the following working variables with zero-lists or empty-lists: \
\n interElementForces = Array[{ }&,Length[elements]]; \
\n interElementCohesions= Array[{ }&,Length[elements]]; \
\n unknownQ=Array[q,Length[edges]]; \
\n unknowns = {unknownQ, Select[ Flatten[ bring2LimitLoad ], symbolQ ]} //Flatten  ; \
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

specificWs::usage= "specificWs: represent a list of specific weight vectors in each element {{gx,gy}, {} ...}. They will be just  multiplied by area of the element and added to Loads. \
\n In the current version of LittleKEM the density is assumed identical in all elements.  specificWs = {{.. , ..}, { .., ..}}";

movementExpectation::usage= "movementExpectation={e0, {ve0x,ve0y}} : a special element e0 and its prescribed virtual velocity vector {ve0x,ve0y}. \
\n The movementExpectation dictates the sense of the movement of the whole system. \
\n It is necessary e.g. to distinguish between the active and the passive earth pressure. movementExpectation = {.. , {.. , ..}}\
\n If e0 is aligned to Earth-element the specified velocity {ve0x,ve0y} need not be exact. It will be corrected according to the edge with Earth and to the dilatancy \
\n If e0 is not aligned to the Earth-element then {ve0x,ve0y} must be specified exactly ";

edges::usage= "edges: a list of ordered pairs of points constituting the sliding lines (usually with friction and cohesion) \
\n edges = {{.. , ..}, {.. , ..}, ..}. Ground surface should not be declared as edge";

edgesProps::usage= "edgesProps: a list of triples {Tan[phi], Tan[psi], c} with the friction angle, dilatancy and cohesion for each edge.\
\n  In the current version of LittleKEM the pore pressure along an edge cannot be defined. edgesProps = {{.. , .. , ..}, {.. , .. , ..}, ..}";


(*---------------------------------------------solution functions--------------------------------------------------------------------*)


symbolQ[t_]:=\[Not]NumberQ[t];
dir[xx_,{i_,j_}]:=Normalize[xx[[j]]-xx[[i]]];
getLength[xx_,{i_,j_}]:=Norm[xx[[j]]-xx[[i]]];
getArea[xx_,elements_,e_]:=Module[{i,nv,area=0,ele,n,xxe},ele=elements[[e]];
                          n=Length[ele];xxe=xx[[#]]&/@ele;xxe=Append[xxe,xxe[[1]]];
                          For[i=1,i<=n,area+=(xxe[[i,1]]-xxe[[i+1,1]])*(xxe[[i,2]]+xxe[[i+1,2]]);i++;];
                          area/2//N];

getTopology[elements_,edges_,xx_]:=Module[{eSides,LeftOfE,RightOfE,edgesT,edgesN,adjacentE,areasE ,edgesL, ne },
ne =  Length[elements]  ;
eSides=Transpose[{#,RotateLeft[#]}]&/@elements;  (* node pairs for each el.  { {1,2,3,4},{2,3,4,1}}// Transpose => {{ {1,2},{2,3},{3,4},{4,1} }, { } }  *)
edgesT=dir[xx,#]&/@edges;                       (*  unit vectors  tangential to edges *)
edgesL=getLength[xx,#]&/@edges;                (*  unit vectors  tangential to edges *)
Print["getTopology:  a manual check is recommended: areasE (should not be negative)  , adjacentE (not empty or double) "];
edgesN={{0,-1},{1,0}}.#&/@edgesT;            (*  unit vectors  normal  to edges  (tangential rotated 90 deg CCW) *)
LeftOfE=Position[eSides,#][[1,1]]&/@edges;   (* if edge==side then the element is on the left hand side of the edge *)
RightOfE=Position[eSides,#][[1,1]]&/@(Reverse[#]&/@edges); (* if edge==Rotated[side] then the element is on the right hand side of the edge *)
adjacentE=Transpose[{LeftOfE,RightOfE}];    (* = {{eLeft,eRight}, {eLeft, eRight}, ...} list of pairs of el. numbers on the left and right wrt edge 1,2,...*)
areasE=getArea[xx,elements,#]&/@(Range[1,ne-1]);
AppendTo[areasE,0];         (* last element is Earth.  It has no area (by definition) *)
{edgesT,edgesN,edgesL,adjacentE,areasE}
];

getPorePressureForces[ edgesPP_ ] := Module[ {ppLeft,ppRight,longPPF,shortPPF,plottingPPF,tmp },
ppLeft = { (edgesPP [[#]] * edgesL[[ # ]]*edgesN[[#]]& /@ Range[Length[edges]]) , Transpose[ adjacentE ][[1]]  } //Transpose ;
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

clearGlobals[] := Module[{},
   Clear[xx, earth, elements, edges, edgesProps];
   Clear[specificWs, movementExpectation, externalLoads,
    bring2LimitLoad , selfWeights, interElementForces,
    interElementCohesions, seepageForces, shortPPF, longPPF,plottingPPF] ;
   Clear[edgesT, edgesN, edgesL, adjacentE, areasE, v, Loads,
    listForces, solution];
   ];

initializeGlobalArrays[]:=Module[{ne,nedges,unknownR, unknownQ, nR},
ne=Length[elements] ;
nedges = Length[edges];
If[MemberQ[edgesL,0],Print[Style["Check geometry. One edge has length 0",Red,Bold] ]; Goto[koniec];];
If[ ne== 0 || nedges == 0 , Print[Style["error in initializeGlobalArrays: ne=0 or nedges=0 encountered ",Red,Bold] ];  Goto[koniec] ;] ;
If[ Length[specificWs] != ne  , Print[Style["Error in initializeGlobalArrays:   suspicious specificWs  ",Red,Bold] ];  Goto[koniec] ;] ;
If[ MemberQ[   (Norm[#]==0& /@ specificWs  ), True] , Print[Style["warning: zero elements in specificWs ",Red,Bold] ];  ] ;
If[ Length[ bring2LimitLoad ] == 0, Print[ Style["Global list bring2LimitLoad does not exist",Red,Bold]  ];  Goto[koniec] ; ];
unknownR = Select[ Flatten[ bring2LimitLoad ], symbolQ ] ;
unknownR = If[ Not[AtomQ[ # ]] , Select[  Apply[List, # ], symbolQ][[1]], #  ]& /@ unknownR ; (* extracts  x  if bring2LimitLoad  contains -3 x   *)   ;
unknownR = Union[unknownR] ;   (* = DeleteDuplicates[ ] i.e.  extracts  x  if bring2LimitLoad  contains a force like  { -2*x, x}   *)
nR =  Length[unknownR] ;
If[ nR==0,  Print[ Style["error in initializeGlobalArrays: nR == 0 ",Red,Bold] ];  Goto[koniec] ;] ;
interElementForces = Array[{ }&,ne];
interElementCohesions= Array[{ }&,ne];
unknownQ=Array[q,nedges];
unknowns = {unknownQ,unknownR } //Flatten   ;
seepageForces = Array[{0,0}&,ne];
Loads = Array[{0,0}&,ne];
porePressures = Array[ 0&,nedges];
{shortPPF,plottingPPF} = getPorePressureForces[ porePressures  ] ;
externalLoads = Array[{0,0}&,ne];
selfWeights = (specificWs *areasE) ;
Print[ "The  global variables were initialized as follows:" ];
Print[ "seepageForces=", seepageForces ];
Print[ "externalLoads=", externalLoads ];
Print[ "porePressures=", porePressures ];
Print[ "selfWeights=", selfWeights];
Print[ "You may modify some or all of them manually" ];
Print[ "Having modified porePressures you MUST execute: {shortPPF,plottingPPF} = getPorePressureForces[ porePressures  ] ;" ];
Label[koniec] ;
];

getHodograph[]:=Module[{ ne,haveV,e0,v0,edge0,signN,signT,myAdjacentE,MyNeighbours,MySolvedNeighbours,e1,e2,l01,l10,l02,l20,signN1,signN2,
                         tanPsi1,tanPsi2,tanPhi1,tanPhi2,coh1,coh2,cohesion1,cohesion2,v01,v02,T1,T2,N10,N20,eqs1,eqs2,solu,Qe10,Qe20,v,
                         nonspecial,firstOffEarth},
ne=Length[elements];   (* number of elements useful for loops , initialization of dummy lists  etc. *)
haveV=Array[#==ne &,ne];  (* indicate that all elements are without known velocity except for Earth*)
v=Array[{0,0}&,ne];  (* placeholder for element velocities *)

{e0,v0}=movementExpectation; (* e0 is usually aligned to Earth otherwise (e.g. bearing capacity problem) special behaviour *)
firstOffEarth  = Not[ MemberQ[adjacentE,{e0,ne}] ||  MemberQ[adjacentE,{ne,e0}] ]; (* true if no edge between presc. vel. El. and Earth, e.g. symm. strip foundation  *)
If[ firstOffEarth ,
    v[[e0]]=v0; haveV[[e0]]=True;  Goto[nonspecial];  (* if movem.expectation refers to element with no contact to Earth  the expected velocity is taken 1:1  *)
];
{edge0}={Position[adjacentE,{e0,ne}],Position[adjacentE,{ne,e0}]}//Flatten;       (* edge0 = is the Nr of the edge between e0 and Earth  *)
{T1,N10,{tanPhi1,tanPsi1,coh1}}={edgesT[[edge0]],edgesN[[edge0]],edgesProps[[edge0]]}; (* props of the special edge  *)
{signN,signT}={If[adjacentE[[edge0,1]]==e0,1,-1],If[T1.v0>0,1,-1]};  (* if prescribed e0 on the left of edge 0 do not change the sign  *)
N10*=signN;                    (* from movementExpectation edge points towards e0 *)
v[[e0]]=signT*T1+N10*tanPsi1;  (* precise velocity of the prescribed element (and not just the approximate movementExpectation) *)
haveV[[e0]]=True;
Qe10=If[signT>0,N10-tanPhi1*T1,N10+tanPhi1*T1]*unknowns[[edge0]];  (* unknowns[[1;;nedges]] = value of the normal component of the contact force   *)
cohesion1=If[signT>0,-T1,T1]*edgesL[[edge0]]* coh1;
Loads[[e0]]+=Qe10+cohesion1;
Loads[[ne]]-=Qe10+cohesion1;
interElementForces = Insert[interElementForces , {Qe10,ne},    {e0,1}];  (* Prepend force Qe10 and element e1 of its origin  to the component e0 of  interElementForces *)
interElementForces = Insert[interElementForces , {-Qe10,e0}, {ne,1}];  (* Prepend force -Qe10 and element e0 of its origin  to the component e1 of  interElementForces *)
 interElementCohesions=   Insert[interElementCohesions , {cohesion1,ne},    {e0,1}];  (* Prepend cohesion1 and el. e1 of its origin  to the component e0 of  interElementCohesions *)
 interElementCohesions=   Insert[interElementCohesions , {-cohesion1,e0},    {ne,1}];
Label[nonspecial]; e0=0;
While[MemberQ[haveV,False],For[e0=1,e0<=ne-1,If[haveV[[e0]],Goto[nexte0]]; (* take next element e0 without computed velocity *)
myAdjacentE=Pick[adjacentE,(MemberQ[#,e0]&/@adjacentE)];  (* find edges around the current element e0 *)
MyNeighbours=Complement[Flatten[myAdjacentE],{e0}];                  (* find  neighbouring elements behind these edges *)
MySolvedNeighbours=Pick[MyNeighbours,(haveV[[#]]&/@MyNeighbours)];    (* find solved neighbouring elements (with computed velocity) *)
If[Length[MySolvedNeighbours]!=2,Goto[nexte0]];        (* element e0 cannot be solved, we need two solved neighbours to proceede with e0 *)
{e1,e2}=MySolvedNeighbours;
Print["Calculating velocity of element ", e0, " from elements ",e1," and ", e2] ;    (* set velocity eqs system based on edges 01 and 02*)
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
{solu}=Solve[{eqs1,eqs2},{a,b,v01,v02}];
{alpha,beta}={a,b}/.solu;
If[Length[{solu}]!=1,Print["no or multiple velocity of ",e0,"wrt",e1," and ",e2];Abort[];];
If[alpha*beta==0,Print["no velocity jump of elem=",e0,"wrt",e1," or ",e2];Abort[];];
v[[e0]]={v01,v02}/.solu;haveV[[e0]]=True;
Qe10=If[alpha>0,N10-tanPhi1*T1,N10+tanPhi1*T1]*unknowns[[edge1]];
cohesion1=If[alpha>0,-T1,T1]*edgesL[[edge1]]* coh1;
 Qe20=If[beta>0,N20-tanPhi2*T2,N20+tanPhi2*T2]*unknowns[[edge2]];
cohesion2=If[beta>0,-T2,T2]*edgesL[[edge2]]* coh2 ;
Loads[[e0]]+=Qe10+Qe20+cohesion1+cohesion2;
Loads[[e1]]-=Qe10+cohesion1;
Loads[[e2]]-=Qe20+cohesion2;
interElementForces = Insert[interElementForces , {Qe10,e1},    {e0,1}];  (* Prepend force Qe10 and element e1 of its origin  to the component e0 of  interElementForces *)
interElementForces = Insert[interElementForces , {-Qe10,e0}, {e1,1}];  (* Prepend force -Qe10 and element e0 of its origin  to the component e1 of  interElementForces *)
interElementForces = Insert[interElementForces , {Qe20,e2},    {e0,1}];  (* Prepend force Qe20 and element e2 of its origin  to the component e0 of  interElementForces *)
interElementForces = Insert[interElementForces , {-Qe20,e0}, {e2,1}];  (* Prepend force -Qe20 and element e0 of its origin  to the component e2 of  interElementForces *)
interElementCohesions=   Insert[interElementCohesions , {cohesion1,e1},    {e0,1}];  (* Prepend cohesion1 and el. e1 of its origin  to the component e0 of  interElementCohesions *)
interElementCohesions=   Insert[interElementCohesions , {-cohesion1,e0},    {e1,1}];
interElementCohesions=   Insert[interElementCohesions , {cohesion2,e2},    {e0,1}];
interElementCohesions=   Insert[interElementCohesions , {-cohesion2,e0},    {e2,1}];
Label[nexte0];e0++;];
];
{v,Loads}
];

getAFRdissipationRate[edgesProps_, edgesT_, edgesN_, edgesL_,   adjacentE_, areasE_, v_] := Module[{nedge, iedge, c, tphi, tpsi, diss, ie, je},
  nedge = Length[edgesL]; diss = 0;
  Do[{ tphi, tpsi, c} = edgesProps[[iedge]];
   If[Abs[tphi - tpsi] > 0.001, Print["AFR violaten on edge ", iedge];     Abort[]];
   ie = adjacentE[[iedge, 1]]; je = adjacentE[[iedge, 2]] ;
   diss +=     c*edgesL[[iedge]]*     Abs[(v[[ie]] - v[[je]]).edgesT[[iedge]]];, {iedge, 1, nedge}];   diss];

getPowerSupply[externalLoads_, seepageForces_, selfWeights_, shortPPF_, v_] := Module[{A, nelem, ie, Loads}, nelem = Length[v];
  A = 0;  Loads = (externalLoads + seepageForces + selfWeights + shortPPF);
  Do[A += Loads[[ie]].v[[ie]];, {ie, 1, nelem}];     A]; 





(*---------------------------------------------------plot functions ---------------------------------------------------------------------*)
Options[plotKEM]={nodeLabels->True,elementLabels->True,edgeLabels->True};
Options[PlotForcePolygon]={imagesizeFactor-> 2.0,printForceVal->False,printElementLabel->True,printForceName->True};

plotKEM[elements_,edges_,xx_,v_,opts___]:=Module[
{z0,z1,lines, lines0,lines1,e,ne, nne,   eg}, {nL,eL,gL}={nodeLabels,elementLabels,edgeLabels}/.{opts}/.Options[plotKEM];
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
If[nL,nodeNames=Table[Style[ Text[i,xx[[i]]] ,FontFamily->"Arial"],{i,1,Length[xx]}];,nodeNames={};];
If[eL,
elNames=Table[Style [Text[e,Sum[xx[[elements[[e,i]]]], {i,1,Length[elements[[e]]]}] /Length[elements[[e]]] ],FontFamily->"Arial" ],{e,1,ne-1}];,
elNames={};
];
If[gL,egNames=Table[Text[eg, ( xx[[edges[[eg,1]]]]+xx[[edges[[eg,2]]]])/2    ],{eg,1,ng}];,
egNames={};
];
egNames=Prepend[egNames,Hue[0.9`]];elNames=Prepend[elNames,Hue[0.55`]];
conventions=Text[{Style["Elements ",Blue],Style["Nodes ",Black],Style["Edges",Red] } ,{Min[xmm],Min[ymm]-0.5}];
Show[Graphics[{lines,nodeNames,elNames,egNames,window,conventions}],BaseStyle->{FontSize->14},AspectRatio->Automatic]
];

plotHodograph[ v_, adjacentE_  ] := Module[{nv, n1,n2, aux,labels, labeledV , lines},
nv = Length[v];
labeledV =Transpose[ {v,Range[nv] } ] ;
lines = Line[ {v[[ #[[1]]  ]] , v[[#[[2]] ]] }  ] & /@ adjacentE;
labels= { Opacity[1.0],Text[Style[ToString[ #[[2]]  ]<>":=\n\
"<>ToString[SetPrecision[#[[1]] ,3]],FontSize->9,FontFamily->"Arial", \
FontColor->RGBColor[0,0,0.5],Background->LightBlue] ,1.1*#[[1]] ] } & \
/@
labeledV; Graphics[{PointSize[Medium],Red,Point[v], labels, lines} , Axes-> True,AxesLabel->  {"Vx","Vy"} ]
] ;


PrintForces[solution_]:=Module[{EN},
Do[
Print[Style["Forces acting on element number "<>ToString[EN],Purple,Italic,Bold,20] ];
Print["bring2LimitLoad: r",EN,"=",bring2LimitLoad[[EN]]//.solution];
Print["externalLoads: L",EN,"=",externalLoads[[EN]]//.solution];
Print["selfWeights: G",EN,"=",selfWeights[[EN]]//.solution];
(* Print["interElementForces: \
Q",EN,"=",interElementForces[[EN]]//.solution," followed by \
neighbouring element numbers"]; *)
Print[Style["interElementForces: ",Green] ];
Do[
Print["  Q",EN,interElementForces[[EN,i,2]],"=",interElementForces[[\
EN,i,1]]//.solution, "   ||Q",EN,interElementForces[[EN,i,2]],"||=",Norm[interElementForces[[\
EN,i,1]]//.solution] ];
,{i,1,Length[ interElementForces[[EN]] ] } ];
(* Print["interElementCohesions = \
",interElementCohesions[[EN]]//.solution," followed by neighbouring \
element numbers"]; *)
Print[Style["interElementCohesions: ",Orange] ];
Do[
Print["  C",EN,interElementCohesions[[EN,i,2]],"=",\
interElementCohesions[[EN,i,1]]//.solution, "   ||C",EN,interElementCohesions[[EN,i,2]],"||=",Norm[ interElementCohesions[[EN,i,1]]//.solution ] ];
,{i,1,Length[ interElementCohesions[[EN]] ] } ];
Print["seepageForces: f",EN,"=",seepageForces[[EN]]//.solution];
(* Print["pore pressure forces : plottingPPF = \
",plottingPPF[[EN]]//.solution," followed by neighbouring element \
numbers"]; *)
Print[Style["pore pressure forces plottingPPF: ",Blue]];
Do[
Print["  U",EN,plottingPPF[[EN,i,2]],"=",plottingPPF[[EN,i,1]]//.\
solution, "   ||U",EN,plottingPPF[[EN,i,2]],"||=",Norm[ plottingPPF[[EN,i,1]]//.solution] ];
,{i,1,Length[ plottingPPF[[EN]] ] } ];
Print[Style[" \
--------------------------------------------------------------------------"\
,LightPurple]];,{EN,1,Length[ elements ]-1}]]


PlotForcePolygon[ EN_,solution_,opts___] := Module[{Rf, L, G,  Qs , Cs,FF,Fs,Us,n,Ename,listOfPoints,i, f,t,vectors,pFV,
peL,pfN,FN,isF,xmax,ymax,xmin,ymin,listOfX,listOfY,middle,imagesize,corners,papersize,g0,allRs,allLs,allGs,  allQs , allCs, allfs, allUs },
{pFV,peL,pfN,isF}={printForceVal,printElementLabel,printForceName,imagesizeFactor}/.{opts}/.Options[PlotForcePolygon];
(* ForceList= stabilising force, external loads, weight forces, friction forces, cohesion forces, seepage force, pore pressure force *)

ForceList  = {bring2LimitLoad, externalLoads, selfWeights,    interElementForces, interElementCohesions, seepageForces ,    plottingPPF};
{allRs,allLs,allGs,allQs,allCs, allfs, allUs}=ForceList//.solution;
{Rf, L ,G ,  Qs , Cs,Fs, Us }   = #[[EN]]& /@{allRs, allLs ,allGs ,  allQs  , allCs, allfs, allUs};  (* choose the forces for the given element EN *)
(* Remove forces equal to zero from the graphic output *)
FF={}; FN={};
If [Chop[ Norm[Rf ]] !=0,AppendTo[FF,Rf];AppendTo[FN,"R"<>ToString[EN]] ];
If [Chop[ Norm[ L ] ] !=0,AppendTo[FF,L];AppendTo[FN,"L"<>ToString[EN]] ]; If [Chop[ Norm[ G ] ] !=0.0,AppendTo[FF,G] ;AppendTo[FN,"G"<>ToString[EN]]];
If [Chop[ Norm[ Fs ] ] !=0,AppendTo[FF,Fs] ;AppendTo[FN,"Fs"<>ToString[EN]]];
Do[  If[  Chop[ Norm[ Qs[[i,1]] ] ] !=0,
AppendTo[ FN,"Q"<>ToString[EN]<>ToString[  Qs[[i,2]]] ]; AppendTo[ FF,Qs[[i,1]] ];  ]
 ,{i,1,Length[Qs]}
];
Do[  If[ Chop[ Norm[ Cs[[i,1]] ] ] !=0,
AppendTo[ FN,"C"<>ToString[EN]<>ToString[  Cs[[i,2]] ] ];  AppendTo[ FF,Cs[[i,1]] ]; ]
,{i,1,Length[Cs]}
];
Do[  If[ Chop[ Norm[ Us[[i,1]] ] ] !=0,
AppendTo[ FN,"U"<>ToString[  Us[[i,2]] ] ];  AppendTo[ FF,Us[[i,1]] ]; ]
,{i,1,Length[Us]}
];
(* Procedure to calculate the coordinates of each vector*)
 n= Length[FF];
Ename=ToString[EN];
listOfPoints= {{0,0}};
Do[ listOfPoints = Append[listOfPoints,  listOfPoints[[ i]] + FF[[i]]  ],
{i,1,n}
];
n= Length[listOfPoints] ;
vectors = { }  ;
Do[  f = listOfPoints [[i]] ;
t= listOfPoints[[i+1]];
vectors=Append[vectors, {f,t}],
{i,1,n-1}
];
 {listOfX, listOfY} =  Transpose[listOfPoints] ;
      {xmax, ymax}=  Max[#]& /@    {listOfX, listOfY}   ;
       {xmin, ymin}=  Min[#]& /@    {listOfX, listOfY}   ;
       middle = {xmin+xmax , ymin+ymax}/2;
       imagesize = Max[ ymax-ymin, xmax-xmin]*  isF;
       corners =   { {xmin, ymin}-0.1*imagesize,  {xmax, ymax}+  0.1*imagesize};
papersize={{corners[[1,1]],corners[[1,1]]+Max[Differences[corners]]},{corners[[1,2]],corners[[1,2]]+Max[Differences[corners]]}};
If[!peL, Ename="";];
If[!pfN,FN=Array[{""}&,Length[vectors]]//Flatten;];
  If[pfN && pFV, FN =#<>"="& /@  FN ];
g0=If[pFV,
Graphics[
Append[
Apply[{Arrowheads[Medium], Arrow[#1],Style[Text[#2<>ToString[Differences[#1]//Flatten],Mean[#1],\
Background->LightRed ] ,FontFamily->"Arial"]}&  , Transpose[{vectors , FN}],{1}
],
{Opacity[0.3],Text[Style[Ename, FontSize->25, FontFamily->"Arial",FontColor-> Red],middle] }
],
AspectRatio->1,  ImageSize -> imagesize,PlotRange->papersize],


Graphics[ Append[Apply[{Arrowheads[Medium],Arrow[#1],Text[#2,Mean[#1],Background->LightBlue ]}&  , Transpose[{vectors,FN}],{1}],{Opacity[0.3],Text[Style[Ename, FontSize-> 25,FontFamily->"Arial", FontColor-> Red],middle] } ],AspectRatio->1,ImageSize -> imagesize,PlotRange->papersize]
];
Show[g0,Graphics[ {Yellow, Point[corners] } ],
If[isF<0.5,
Graphics[{Arrowheads[Medium],Blue,Arrow[{{(corners[[2,1]]+corners[[1,1]])/2-125,corners[[2,2]]-2},{(corners[[2,1]]+corners[[1,1]])/2+125,corners[[2,2]]-2}}],
Text[Style["250 kN / m",FontFamily->"Arial"],{(corners[[2,1]]+corners[[1,1]])/2,corners[[2,2]]-35}] }] ,
If[ isF<= 2,
 Graphics[{Arrowheads[Medium],Blue,Arrow[{{(corners[[2,1]]+corners[[1,1]])/2-50,corners[[2,2]]-2},{(corners[[2,1]]+corners[[1,1]])/2+50,corners[[2,2]]-2}}],
Text[Style["100 kN / m",FontFamily->"Arial"],{(corners[[2,1]]+corners[[1,1]])/2,corners[[2,2]]-12}] }] ,
 Graphics[{Arrowheads[Medium],Blue,Arrow[{{(corners[[2,1]]+corners[[1,1]])/2-10,corners[[2,2]]-2},{(corners[[2,1]]+corners[[1,1]])/2+10,corners[[2,2]]-2}}],
Text[Style["20 kN / m",FontFamily->"Arial"],{(corners[[2,1]]+corners[[1,1]])/2,corners[[2,2]]-10}] }]
]
 ]
 ]
   ];



showKEMtranslation[elements_,edges_, xx_,v_, nFrames_, scaleV_ ] := Module[{i},
Animate[ plotKEM[elements,edges, xx,v*(i/nFrames)*scaleV, nodeLabels->False,elementLabels->False] ,{i,0,20}   , AnimationRunning->False ]
];

EndPackage[ ]



$Context = "KEM`KEMTools2010u`"   ;
Off[General::spell,General::spell1,Solve::ifun,ReplaceAll::reps,ReplaceAll::rmix,General::stop];
  Print[ " Mathematica  tools for the kinematic element method (KEM) by A. Niemunis and F.Prada 2010\
\n  You are in the context KEM`KEMTools`. \
\n  It provides the following basic tools: \
\n     getTopology, getHodograph, plotKEM, \
\n     PlotForcePolygon, plotHodograph, showKEMtranslation:   \
\n  (and some primitives:   dir, getLength, getArea,...)
\n    The input syntax is given in help,  e.g. ?plotKEM  or ?xx \
\n  *********************************************************************************************************** \
\n    ?helpExamples gives you a brief explanation of the procedure used to solve the examples with Mathematica. \
\n    ?helpInput gives you a general explanation  of the global input variables \
\n    ?helpOutput gives you a general  explanation of the output variables   \
\n  *********************************************************************************************************** \
\n     A new task  requires  geometry, soil parameters and load data  (  define your own  ): \
\n     xx = {{0.0 , 3.5}, {4.0 , 3.5}, ..};  \
\n     elements = {{1 , 2 , 3}, {3 , 4 , 5 }, ..}; \
\n     specificWs = {{0 , -20}, {}, ... }; \
\n     movementExpectation = {2 , {-1 , -1}} ;  \
\n     edges = {{ 1, 2 }, {2 , 3}, ..}; \
\n     {edgesT, edgesN, edgesL, adjacentE, areasE} =   getTopology[elements, edges, xx];   \
\n     edgesProps = {{30\[Degree] , 10\[Degree] , 10.5}, { , , }, ..}; \
\n     bring2LimitLoad  = { {0 ,0 }, { r, 0 }, ..};   (* should contain unknown force r *)       \
\n    {v, Loads} = getHodograph[];   (* determine velocities c and add unknown inter-element forces to Loads *)    \
\n    Loads += selfWeights + externalLoads + bring2LimitLoad +  seepageForces + shortPPF;    \
\n    staticEquilibriumEquations = Loads[[#]] == {0, 0} & /@ (Range[1, Length[elements] - 1]);  \
\n    {solution} = Solve[staticEquilibriumEquations, unknowns]   \
\n     (* <----- check output if the q-s are positive and what force r is required for limit equilibrium *) \
\n    PrintForces[solution] \
\n    PlotForcePolygon[elem, listForces, solution]   /. elem -> 2 \
\n    plotHodograph[ v , adjacentE  ]   \
\n    showKEMtranslation[elements, edges, xx, v, 20, 1]
" ];

