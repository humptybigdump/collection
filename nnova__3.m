(* ::Package:: *)

(*    Mathematica  rules to simplify tensorial expressions (including Frechet derivatives) by A. Niemunis
This is a free package and I guarantee nothing. It would be nice if you report bugs to  andrzej.niemunis@kit.edu

Installation:
1) go to the directory shown by the Mathematica's  global variable $UserBaseDirectory.   It should be something like
   C:\Dokumente und Einstellungen\xxx\Anwendungsdaten\Mathematica\   or   C:\Windows\xxx\Application Data\Mathematica\
   where 'xxx' is your user name  under windows
2) go deeper to the subdirectory \Applications\ and make a new directory  Tensor there
3) copy this file (= nnova.m) to the new directory.
4) Begin a Mathematica session with:    Needs["Tensor`nnova`"]



Usage of the rules:
All tensors are of 2.order and  should be first declared with addst[], e.g.  addst[T].
The predefined Kronecker symbol is \[Delta][i,j] and not  delta[i,j].
Cartesian coordinates are used (otherwise see larger packages like Ricci or TensorCalculus3 in Internet).
A tensor must  be typewritten as an indexed variable e.g. x[i,j] or as tr[x] or norm2[x]  and indices should be symbolic (not x[1,2]).


 RECENT CHANGES:
 07.02.2021  manipulation of vector fields v[i] including spatial cartesian derivatives  v[i][j] = d v_i / d x_j using addv, and a few simple delta rules  
 26.04.2019  the direction cosine matrix \[Alpha][i,j] is implemented with rules associated with its orthogonality, with  invariance of \[Delta][i,j] and of ricci[i,j,k]
 22.4.2016 \[Delta]-replacement rules apply to ricci (didn't work because ricci is not a symm tensor)
 22.4.2016  public rules:  i123Q and  numerical. Rewritten transfer99 and transfer33 without local rules.  default indices i,j,k,l rendered global
  14.10.2015 total privatisation, usages completed
 21.01.2013  repaired withTHEitQ   with consequences for some  fD[ ] rules which are using it
             fD[Sqrt[....], T[i,j]]  possible only numerically, see tensorialfD[ ]  in anova.m (Itskov)
 22.02.2012  derivatives of power and zoutSym product (used by Itskov)
 19.01.2010  corrected Delta[]^2 rule     Delta[1,2] Delta[1,2] is now 0 and not 3
 10.07.2009  improved subscriptForm
 5.01.2007  ricci[i,j,k] = permutation symbol almost complete set of rules added
 24.10.2006  powers of tensors T[i,j]^3 = pow[T,3,i,j] or   T[i,j]^4 = norm2[T]^2 are  acceptable but with warning pow[\[Delta],n,i,j] added
 21.09.2006  powers of tensors T[i,j]^-2 must be written as  pow[T,-2,i,j] with many rules for pow
 21.09.2006  with indexed tensor query added
 01.02.2006  improved beautifierT ( _. added )
 9.12.2005  ?iQ is added in most rules to prevent them affecting expressions like \[Delta][1,1] within transfer99, better transfer99 and transfer33
 7.12.2005 added rules:  qubic[\[Delta]] -> 3 ;
                         scalar[x_,\[Delta],\[Delta]] -> tr[x];
                         scalar[x_,y_,\[Delta]] -> scalar[x, y] ;
                         in beautifierT:   qubic[T] - norm2[T]*tr[T] + (2*tr[T]^3)/9     ->  qubic[Td]

 TO DO:



    0) mult (a,b,i,j) improve comutativity     x_ik y_kj  is understood as an abbreviation for   (x_ik y_kj +   y_ik x_kj) /2 ?????
    0) beautifierT nie dziala jesli dewiator jest schowany w ulamku lub pod pierwiastkiem
    1) brak r. Hamiltona
    2) extend qubic for complex expressions   (as already done for deviator tr norm2 invariant1 roscoeP )
    3) diagnosis function  to check syntax of  arguments.
       write functions
           aiisQ[ expr_ ] aiis = all identically  indexed  summands   True/False
           nupi[ expr_ ]  nui = number of unique pairs of indices
           cesiQ[ expr_ ] complex expression with a single pair of indices
    9) need tQ[deviator[T]]  for fD wrt a deviator ? (any special restriction needed ?)


*)

 (*  Needs["Tensor`nnova`"]   *)


BeginPackage["Tensor`nnova`"]
(*---------------------------------------------usages--------------------------------------------------------------------*)
i::usage = "global index" ;
j::usage = "global index" ;
k::usage = "global index" ;
l::usage = "global index" ;
\[Nu]::usage = "global Poisson number" ;
EE::usage = "global Young modulus" ;
\[Alpha]::usage = "\[Alpha][i,j] direction cosine matrix ";
div::usage = " div[v] = v[a,a] = v[b,b] ... useful in simplificaitons "; 
grad::usage = " grad[v^2]/2   = v[a] v[a,b]  ... useful in simplificaitons  "; 
igrad::usage = " igrad[v^2]/2   = v[a] v[b,a]  ... useful in simplificaitons "; 

addst::usage = "Syntax: addst[x]  or addst[x,y,...] \n adds the tensor x or the tensors x,y,... to the list 'alltensors' giving it  \
          the attribute Orderless defining x[i,i] etc. \nsee noLongerTensor[x_]" ;
addv::usage = "Syntax: addv[x]  \n adds the vector x to the list 'allvectors' " ;   
addgv::usage = "Syntax: addgv[x]  \n adds the vector x or the vectors x,y,... to the list 'allgradvectors' giving it  \
          the attribute Orderless defining x[i,i] etc. \nsee noLongerTensor[x_]" ;                    
addst4::usage = "Syntax: addst4[x]  or addst[x,y,...] \nadds the tensor x or the tensors x,y,... to the list 'all4tensors' " ;
allvectors::usage = "allvectors returns the list of all vectors";
allgradvectors::usage = "allgradvectors returns the list of all vectors declared as gradient fields ";
alltensors::usage = "alltensors returns the list of all 2nd rank  tensors";
allorthogonals::usage = "allorthogonals returns the list of all 2nd rank orthogonals";
noLongerOrthogonal::usage = "noLongerOrthogonal[ \[Alpha] ] removes a the orthogonal matrix \[Alpha] from the list allorthogonals.  noLongerOrthogonal cannot remove a list of matrices ";
all4tensors::usage = "all4tensors returns the list of all 4th rank tensors";
 assumeDeviatoric::usage ="assumeDeviatoric[T] \n  activates a new rule  tr[T] := 0   " ;
 assumeUnitary::usage ="assumeUnitary[T] \n  activatates a new rule norm2[T] := 1   " ;
 assumeMajorSymmetric::usage = "assumeMajorSymmetric[A] \n  activatates  the symmetry property  A[i,j,k,l] == A[k,l,i,j]  " ;
 assumeMinorSymmetric::usage ="assumeMinorSymmetric[A] \n  activatates the symmetry properties  A[i,j,k,l] == A[j,i,k,l],  A[i,j,k,l] == A[i,j,l,k], A[i,j,k,l] == A[j,i,l,k]  " ;
 assumeNotDeviatoric::usage = "assumeNotDeviatoric[T] \n  deactivates the rule  tr[T] := 0 if set  " ;
 ssumeNotUnitary::usage = "assumeNotUnitary[T] \n  deactivatates  the rule norm2[T] := 1  if set " ;
beautifierT::usage ="expr /. beautifierT \n  the rule tries to abbreviate  expr replacing deviator of T[] by Td[]  " ;
beautifierT::usage ="expr /. beautifierE \n  the rule tries to abbreviate  expr replacing deviator of e[] by ed[]  " ;
different3Q::usage = "different3Q[i,j,k] this query returns True because all arguments are different. different3Q[k,j,k] returns False" ;
different4Q::usage = "different4Q[i,j,k,l] this query returns True because all arguments are different. different3Q[k,j,k,l] returns False" ;
deviator::usage= "deviator[expr_[i,j]] computes deviatoric part of tensorial expression with obligatory identical i,j arguments in each summand.\n The forms  like  deviator[ x[i,k] y[k,j] ] work too.  " ;
\[Delta]::usage=" \[Delta][i,j] Kronecker tensor (many simplifying rules are automatically used)";

\[Delta]4::usage=" \[Delta]4 beautifier's  abbreviation for identity  tensor. It will not be recognized by other rules ";
\[Delta]4s::usage=" \[Delta]4a beautifier's  abbreviation for symm. identity  tensor. It will not be recognized by other rules ";
Td::usage=" Td beautifier's  abbreviation for deviatoric part of T tensor. It will not be recognized by other rules ";
ed::usage=" ed beautifier's  abbreviation for deviatoric part of e tensor. It will not be recognized by other rules ";
\[Delta]Dev::usage=" \[Delta]Dev beautifier's  abbreviation for deviatorer. It will not be recognized by other rules ";
Dev4::usage=" Dev4 beautifier's  abbreviation for deviatorer. It will not be recognized by other rules ";
T::usage=" T is an input of beautifierT and by this usage is is rendered   global ";
e::usage=" eis an input of beautifierE and by this usage is is rendered   global ";

fD::usage ="fD[expr,x[i,j]] calculates the Fr\[EAcute]chet derivative of expr wrt the tensor x[i,j] " ;
hated::usage ="hated[T[i,j]] \n  returns  T[i,j]/tr[T]   " ;
iC::usage = "iC[i,j,k,l,EE,\[Nu]] or  iC[i,j,k,l]  or  iC[ ]   generates an isotropic compliance tensor ";
iE::usage = "iE[i,j,k,l,EE,\[Nu]] or  iE[i,j,k,l]  or  iE[ ]   generates an isotropic stiffness tensor ";
iQ::usage = "iQ[expr] index query returns True if expr is an atom but not a number or a tensor";
i123Q::usage = "iQ[expr] index query returns True if expr is 1 or 2 or 3";
invariant1::usage="Syntax: invariant1[ x ] this can be use for input only and represents  1.st  invariant of   tensor x.
           This function remains  inactive until useInvariants  is executed.";
invariant2::usage="Syntax: invariant2[ x ] this can be use for input only and represents  2.nd  invariant of   tensor x.
           This function remains inactive until useInvariants  is executed.";
invariant3::usage="Syntax: invariant3[ x ] this can be use for input only and represents  3.rd  invariant of   tensor x.
              This function remains inactive until useInvariants  is executed.";
jvariant2::usage="Syntax: invariant2[ x ] this can be use for input only and represents  2.nd  invariant of  deviator of x.
            This function remains inactive until useInvariants  is executed.";
jvariant3::usage="Syntax: invariant3[ x ] this can be use for input only and represents  3.rd  invariant of  of deviator of x.
           This function remains inactive until useInvariants  is executed.";
mult::usage=" mult[x,y,i,j] ; represents the product   x_ik y_kj  = (x[].y[])_(ij) of two tensors (but not expressions).
        If mult[...]  appears within an expressions it can be simplified  (several rules apply)   The result need not be symmetric ! "  ;
mult11::usage=" NOT READY mult11[x,y,i,j] ; represents the product  x_ki y_kj  = (x[].y[])_ij of two nonsym. tensors (but not expressions). \
               \n If mult11[...] appears within an expressions it can be simplified  (several rulnes apply) "  ;
mult22::usage=" NOT READY mult22[x,y,i,j] ; represents the product  x_ik y_jk  = (x[].y[])_ij   of two nonsym. tensors (but not expressions).\
               \n If mult22[...] appears within an expressions it can be simplified  (several rules Apply) "  ;
noLongerTensor::usage= "noLongerTensor[x] or noLongerTensor[{x,y,...}] remove one or more tensors from the list alltensors";
normalized::usage ="normalized[expr[i,j]] \n  returns expr[i] / Sqrt[norm2[expr] ]   " ;
norm2::usage="norm2[T] or norm2[expr] denotes square of the norm of the argument. \nIf argument is a tensor  norm2 does nothing. \
              \n^  Indexed argument like expr= a T[i,j] + b d[i,j]+... will be simply expanded as (expr)*(expr) so user must provide   \
              \n identical indices at all summands " ;
One::usage="Syntax One[i,j,k,l]   returns (1/2) * (\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k]) if i,j,k,l are all indices  ";
pow::usage ="pow[x, n, i,j ] canonical writing of   x_{ij}^n . You cannot write x[i,j]^2 because it means x[i,j] x[i,j] = norm2[x]" ;
qubiclikeQ::usage =  "qubiclikeQ[i,j,k,l,m,n] check if multiplication  T_ij T_kl T_mn = corresponds to tr(T.T.T) " ;
qubic::usage ="qubic[T] inputs the following invariant of a tensor T_ij T_jk T_ki. qubic[expr] does not work yet ! " ;
ricci::usage = "ricci[i,j,k]  denotes the permutation symbol. Simplifying rules are provided. It is always converted to alphabetic order \
         \n and wherever possible ricci is replaced by Kroneckers \[Delta]    " ;
riccidelta::usage ="Syntax: riccidelta[i,j,k,l]   abbreviation for ( \[Delta][i,k]   \[Delta][j,l]  -   \[Delta][i,l]   \[Delta][j,k] ) ";

roscoeP::usage="Syntax: roscoeP[ T ] this can be use for input only and represents    Roscoe  invariant p of  stress tensor T (tension >0).
        This function remains  inactive until useRoscoeLode is executed."  ;
roscoeQ::usage="Syntax: roscoeQ[ T ] this can be use for input only and represents  q Roscoe  invariant q of  stress tensor T (tension >0).
       This function remains  inactive until useRoscoeLode is executed."  ;
roscoeEP::usage="Syntax: roscoeEP[ e ] this can be use for input only and represents  Roscoe  invariant \[Epsilon]Vol of strain tensor \[Epsilon] (tension >0).
         This function remains  inactive until useRoscoeLode is executed."  ;
roscoeEQ::usage="Syntax: roscoeEQ[ e ] this can be use for input only and represents  Roscoe  invariant \[Epsilon]q of strain tensor \[Epsilon] (tension >0).
         This function remains  inactive until useRoscoeLode is executed." ;
isomorphicP::usage="Syntax: isomorphicP[ x ] this can be use for input only and represents isomorphic Roscoe  invariant P or \[Epsilon]P of   tensor T or \[Epsilon] (tension >0).
         This function remains  inactive until useRoscoeLode is executed." ;
isomorphicQ::usage="Syntax: isomorphicQ[ x ]  this can be use for input only and represents isomorphic Roscoe  invariant Q or \[Epsilon]Q of  tensor T or \[Epsilon] (tension >0).
         This function remains  inactive until useRoscoeLode is executed."  ;
lodeTheta::usage="Syntax: lodeTheta[x]  this can be use for input only and represents Lode angle (in radians) of  tensor x (tension >0).
         This function remains  inactive until useRoscoeLode is executed."  ;
scalar::usage="scalar[x,y,z] or scalar[x,y] denotes the scalar product x_ij y_jk z_ki  or x_ij y_ij (in both versions scalar is Orderless). \
              \n If arguments are tensorial expressions e.g. scalar[expr1,expr2,expr3] or scalar[expr1,expr2] \
              \n the arguments are multiplied by each other  (expr1)*(expr2)*(expr3) to be simplified so user must provide reasonable indices at all summands \
               \n in both  arguments. E.g. in scalar[expr1,expr2] the arguments must have the form like \
               \n expr1= a T[i,j] + b d[i,j]+...  and    expr2= c f[i,j] +  T[i,j]+...  " ;
stQ::usage="stQ[x] or stQ[x[i,j]] tensor query, True if x declared as a symmetric tensor (added to the list alltensors with addst)";
subscriptForm::usage="expr //. subscriptForm  This command displays expression expr superscripting all indices of Tensors or their powers \
        \n  In the expression:  T[2,1] T[j,i] /. subscriptForm   transpositions must be expected " ;
tQ::usage="tQ[x] or tQ[x[i,j]] tensor 2nd rank query, True if x declared as tensor (added to the list alltensors with addst), see also t4Q, oQ";
oQ::usage="oQ[x] orthogonal 2nd rank matrix query, True if x declared  as orthogonal (added to the list allorthogonals), see also tQ, t4Q";
t4Q::usage="t4Q[x] or t4Q[x[i,j]] tensor 4th rank query, True if x declared as tensor (added to the list all4tensors with addst4), see also tQ , oQ ";
tr::usage=" tr[x] trace of x ";
det::usage=" det[x] determinant of x ";
transfer99::usage = "Syntax: transfer99[expr ] or  transfer99[expr, diagList ]    \
                    \n converts a tensorial expression obligatorily indexed with i,j,k,l  to a 9x9 matrix . Tensors must be declared by addst[]  \
                     \n The second argument diagList is an optonal list of tensors \
                     \n which may be assumed to have a diagonal form and hence simplified.   By default diagList = {}  i.e. it is empty.\
                     \n \[Delta] is implemented.  tr[] will be expanded but norm2[] will not. \
                      Ugly \[Delta][1,2] etc. can be removed by  (...) //. numerical.
                       see also transfer99ijkl and transfer33 "  ;
transfer99ijkl::usage = "Syntax: transfer99ijkl[expr, {i,j,k,l}, diagList ]    \
                    \n converts a tensorial expression indexed with variables i,j,k,l to a 9x9 matrix . Tensors must be declared by addst[]  \
                     \n The third argument diagList is an optonal list of tensors \
                     \n which may be assumed to have a diagonal form and hence simplified.   By default diagList = {}  i.e. it is empty.\
                     \n \[Delta] is implemented.  tr[] will be expanded but norm2[] will not. \
                     \n Ugly \[Delta][1,2] etc. can be removed by  (...) //. numerical.
                     \n see also transfer99  and transfer33"   ;
transfer33::usage = "Syntax: transfer33[expr ] or  transfer33[expr, diagList ]    \
                    \n converts a tensorial expression obligatorily indexed with variables i,j   to a 3x3 matrix . Tensors must be declared by addst[] i.e. placed  \
                     \n in 'alltensors' i.e. replying true to tQ[]. The second argument diagList contains  an opitonal list of tensors \
                     \n which are assumed to have a diagonal form.  By default it is empty,  diagList = {}.\
                     \n Numerical values for \[Delta][] and ricci[]  will be set. "  ;
transfer33ij::usage = "Syntax: transfer33ij[expr, {i,j} ] or  transfer33[expr, {a,b}, diagList ]    \
                    \n converts a tensorial expression   indexed with  a,b to a 3x3 matrix . Tensors must be declared by addst[] i.e. placed  \
                     \n in 'alltensors' i.e. replying true to tQ[]. The second argument diagList contains  an opitonal list of tensors \
                     \n which are assumed to have a diagonal form.  By default diagList = {}  i.e. it is empty.\
                     \n Numerical values for \[Delta][] and ricci[]  will be set.    "  ;
useInvariants::usage="useInvariants  \n activates (allows for usage as input) the classical scalar invariants  \
               \n invariant1[T]   invariant2[T]  invariant3[T]  jvariant2[T] jvariant3[T] of a tensor T";
useRoscoeLode::usage="useRoscoeLode  \n activates (allows for usage as input) the geotechnical  invariants \n   roscoeP[T] roscoeQ[T]  lodeTheta[T] of tensor T";
vQ::usage="Syntax vQ[v] checks if v is a member of a global list  allvectors  ";
gvQ::usage="Syntax gvQ[v] checks if v is a member of a global list  allgradvectors, i.e. belongs to gradient fields  ";
withtQ::usage = "withtQ[expr] True if  expr contains any tensor, i.e. any  x  (from the list alltensors, see addst)" ;
withvQ::usage = "withtQ[expr] True if  expr contains any tensor, i.e. any  v  (from the list allvectors, see addv or addgv)" ;
withoQ::usage="withoQ[expr] True if  expr contains any orthogonal matrix from the list allorthogonals"  ;
withANitQ::usage = "withANitQ[expr] True if  expr contains any indexed tensor, i.e. any  x[i,j]  (from the list alltensors, see addst)" ;
withTHEitQ::usage = "withTHEitQ[expr,T] True if  expr contains an indexed tensorial expression  T[i,j] or mult[A,T,i,j] etc. \
                     Expressions like   tr[T] , norm2[T], qubic[T] do not count as indexed " ;
zoutSym::usage="zoutSym[a[i,j] b[k,l]] = ( a[i,k]*b[j,l] + a[i,l] b [j,k] ))/2 abbreviates outer product by Itskov followed by symmetrization";
numerical::usage="expr //. numerical   It is a set of delayed rules  replacing \[Delta][i,j] and ricci[i,j,k] by 0,1, or -1 when appear in expr with \
           numerical  indices, e.g. \[Delta][1,2] /. numerical returns 0. This is useful after transfer99[.. ] ";


(*--------------------------------------------------------------------------------------------------------------------*)




 Begin["Private`"]
(***** primitives *******)
alltensors = {\[Delta]}  ;
allvectors = { }  ;
allgradvectors = { }  ;
allorthogonals={\[Alpha]} ;
all4tensors = {  }  ;
iQ[i_] := AtomQ[i] && !NumberQ[i] && !MemberQ[alltensors,i];   (*index Query*)
            (*  declareSymmetric[x_] := Attributes[x]={Orderless};   *)
tQ[x_[a_?iQ,b_?iQ]] := MemberQ[alltensors,x];
t4Q[x_[a_?iQ,b_?iQ, c_?iQ, d_?iQ]] := MemberQ[all4tensors,x];

tQ[x_] := MemberQ[alltensors,x];
oQ[x_] := MemberQ[allorthogonals,x];
t4Q[x_] := MemberQ[all4tensors,x];

withtQ[expr__] :=
      Module[ {i,answers={ }},   (* checks if an expression contains at least one of alltensors *)
           Do[ answers= Append[answers,  FreeQ[ expr, alltensors[[i]]  ]  ];,
              {i,Length[alltensors]}] ;
            MemberQ[answers,False]
            ] ;
            
withvQ[expr__] :=
      Module[ {i,answers={ }},   (* checks if an expression contains at least one of allvectors *)
           Do[ answers= Append[answers,  FreeQ[ expr, allvectors[[i]]  ]  ];,
              {i,Length[allvectors]}] ;
            MemberQ[answers,False]
            ] ;            
            
            

withoQ[expr__] :=
      Module[ {i,answers={ }},   (* checks if an expression contains at least one of alltensors *)
           Do[ answers= Append[answers,  FreeQ[ expr, allorthogonals[[i]]  ]  ];,
              {i,Length[allorthogonals]}] ;
            MemberQ[answers,False]
            ] ;

withANitQ[expr__] :=
      Module[ {i,answers={ }},   (* checks if an expression contains at least one of alltensors with indices *)
           answers={ FreeQ[ expr, mult[_,_,_,_] ] ,  FreeQ[ expr, pow[_,_,_,_]],   FreeQ[ expr,fD[_,_] ]  } ;
           Do[ answers= Append[answers,  FreeQ[ expr, alltensors[[i]][_,_]  ]  ];,
              {i,Length[alltensors]}] ;
            MemberQ[answers,False]
            ] ;
(* repaired 21.01.2013 *)
withTHEitQ[expr__,T_] :=   !FreeQ[ expr, T[_,_]  ]          ||  !FreeQ[ expr, pow[T,_,_,_]  ]  || !FreeQ[ expr, mult[T,_,_,_]  ]  ||
                            !FreeQ[ expr, mult[_,T,_,_]  ]  ;   (* checks if an expr  contains T with indices *)






vQ[v_[_]] :=  vQ[v];                 gvQ[v_[_]] :=  gvQ[v];
vQ[v_] := MemberQ[allvectors,v];     gvQ[v_] := MemberQ[allgradvectors,v];

(* tQ[deviator[x_[_,_]]] := MemberQ[alltensors,x];  probably redundant but  must be tested yet ****)
stQ[x_] :=  MemberQ[alltensors,x] && MemberQ[Attributes[x], Orderless] ;

stQ[deviator[x_[_,_]]] := MemberQ[alltensors,x]  && MemberQ[Attributes[x], Orderless] ;

different3Q[i_,j_,k_] := !MatchQ[i,j] && !MatchQ[i,k]  && !MatchQ[k,j];
different4Q[i_,j_,k_,l_ ] := !MatchQ[i,j] && !MatchQ[i,k]  && !MatchQ[i,l] &&   !MatchQ[j,k]  && !MatchQ[j,l]   && !MatchQ[k,l];

qubiclikeQ[i_,j_,k_,l_,m_,n_]:=  (!MatchQ[i,j] && !MatchQ[k,l] && !MatchQ[m,n] && Count[{i,j,k,l,m,n},i ] == 2 &&  Count[{i,j,k,l,m,n}, j ]==2  &&
                  Count[{i,j,k,l,m,n}, k ]==2 &&   Count[{i,j,k,l,m,n}, l ] == 2 &&  Count[{i,j,k,l,m,n}, m]==2  &&  Count[{i,j,k,l,m,n},n]==2 );



addst[x_] := Module[{n},   (*add a single (!) tensor*)
                     If[!MemberQ[alltensors,x] && !NumberQ[x],
                        {alltensors=Append[alltensors,x];
                         SetAttributes[x,Orderless];
                         x[i_?iQ,i_?iQ] := tr[x];  (* there will be  nothing  to assign  these  rules  to. (tr[] and norm[])  12.11.99 *)
                         Unprotect[Power];
                            x[i_?iQ,j_?iQ]^(n_?EvenQ)  := norm2[x]^(n/2);
                         Protect[Power];
                           }   ]; (*if*)
                        Print[ "You have ", Length[alltensors]  , " symmetric tensor(s): ",  alltensors ];
                     ];
addv[x_] := Module[{n},   (*add a single (!) vector *)
                     If[!MemberQ[allvectors,x] && !NumberQ[x],
                        {allvectors=Append[allvectors,x];
                         Unprotect[Power];
                            x[i_?iQ]^(n_?EvenQ)  := norm2[x]^(n/2);
                         Protect[Power];
                           }   ]; (*if*)
                             Print[ "You have ", Length[allvectors]  , " vectors(s): ",  allvectors ];
                     ];   
                     
 addgv[x_] := Module[{n},   (*add a single gradient-vector *)
                     If[!MemberQ[allgradvectors,x] && !NumberQ[x],
                        {allgradvectors=Append[allgradvectors,x];
                         SetAttributes[x,Orderless];
                         Unprotect[Power];
                            x[i_?iQ]^(n_?EvenQ)  := norm2[x]^(n/2);
                         Protect[Power];
                           }   ]; (*if*)
                           Print[ "You have ", Length[allgradvectors]  , " grad-vectors(s): ",  allgradvectors ];
                         addv[x];   
                     ];                          
                                                         
                     

 addst4[x_] := Module[{n},   (*add a single (!) 4th order tensor*)
                     If[!MemberQ[all4tensors,x] && !NumberQ[x],
                          AppendTo[all4tensors,x] ];
                        Print[ "You have ", Length[all4tensors]  , " 4th order tensor(s): ",  all4tensors ];
                     ];






addst[x__] := Module[{args,ia},   (* add several tensors *)
                     args = List[x];
                     For[ ia=1, ia<=Length[args],ia++, addst[ args[[ia]] ]  ] ;
                      ];

noLongerTensor[x_] :=   Module[{n},     (*remove one or more tensors from alltensors *)
                             alltensors=Complement[alltensors,Flatten[{x}]];
                             n = Length[alltensors] ;
                    Print[ "You have still ", n, " symmetric tensor(s): ",  alltensors ]
                     ] ;

noLongerOrthogonal[x_] :=   Module[{n},     (*remove one or more tensors from alltensors *)
                             allorthogonals=Complement[allorthogonals, {x} ];
                             n = Length[allorthogonals] ;
                    Print[ "You have ", n, " orthogonal matrix(es): ",  allorthogonals ]
                     ] ;


(**** rules for single argument functions   ****)

(*  deviator rule (s) *)
deviator[z_. x_?tQ[i_?iQ,j_?iQ]] := z*(x[i,j]- tr[x] \[Delta][i,j]/3) ;
deviator/: deviator[ x_ +   y_ ] :=   deviator[x]  +   deviator[y];
deviator/: deviator[ x_ * y_ ] /; ( withANitQ[x] && !withANitQ[y] )      :=   ( deviator[x] ) * y ;
deviator/:  deviator[ k_. * mult[x_?stQ,y_?stQ,i_?iQ,j_?iQ]] :=  k * ( mult[x,y,i,j] - scalar[x,y]  \[Delta][i,j]/3) ;
(*  trace rule (s) *)
tr/:    tr[\[Delta]] := 3  ;
tr/: tr[ x_ +   y_ ] :=   tr[x]  +   tr[y];
tr/: tr[ x_ * y_ ] /; ( withtQ[x] && !withtQ[y] && !withoQ[y] )      :=   tr[x]  * y ;
tr/: tr[z_. * x_?tQ[i_?iQ,j_?iQ]] /; ( tQ[x]   && !withANitQ[z]  && !withoQ[z]  ):= z*  tr[x] ;
tr/: tr[ \[Alpha][a_?iQ,i_?iQ] * \[Alpha][b_?iQ,j_?iQ] * x_?tQ[i_?iQ,j_?iQ]  ] /; ( tQ[x] ):=  tr[x] ;
tr/: tr[ \[Alpha][a_?iQ,i_?iQ] * \[Alpha][b_?iQ,j_?iQ] * x_?tQ[a_?iQ,b_?iQ]  ] /; ( tQ[x] ):=  tr[x] ;
tr/:  tr[ k_. * mult[x_?stQ,y_?stQ,i_?iQ,j_?iQ]] :=  k *  scalar[x,y];
(*  norm2 rule (s) *)
norm2/: norm2[\[Delta]] := 3 ;
norm2/: norm2[x_] /;  (!tQ[x] && withtQ[x]) := Simplify[Expand[(x) (x)]];
norm2/: norm2[x_[i_?iQ,j_?iQ]] /;  tQ[x] := norm2[x] ;             (* bo z indeksami to tez tensor *)
(*  qubic rule (s) *)
Unprotect[Plus,Minus,Divide,Power,Times] ;
x_?tQ[i1_?iQ,i2_?iQ] * z1_.* x_[i3_?iQ,i4_?iQ] *z2_.* x_[i5_,i6_] := qubic[x] z1 z2  /;  qubiclikeQ[i1,i2,i3,i4,i5,i6] ;
Protect[Plus,Minus,Divide,Power,Times] ;
qubic /: qubic[x_[i_?iQ, j_?iQ]] /; tQ[x] := qubic[x];
qubic /: qubic[\[Delta]] := 3;

(**** rules for multiple argument functions   ****)
qubic /: qubic[m_. *x_[i_?iQ, j_?iQ] + n_.* y_[i_?iQ,j_?iQ]] /; (tQ[x]  &&  tQ[y])  :=   qubic[m* x + n* y] ;
qubic /: qubic[m_. x_[i_?iQ, j_?iQ] + n_. y_[i_?iQ,j_?iQ]] /; (tQ[x]  &&  tQ[y]) :=   m^3*qubic[x] + n^3*qubic[y] +3* m^2*n* scalar[x,x,y] +  3* m*n^2*scalar[x,y,y]   ;
qubic /: qubic[a_?iQ * b_?iQ ] /; !withtQ[a]  :=   a qubic[ b ];

(*** ricci rules ***)
riccidelta[i_,j_,k_,l_] := ( \[Delta][i,k]   \[Delta][j,l]  -   \[Delta][i,l]   \[Delta][j,k] ) ;
ricci /: ricci[i_?iQ,j_?iQ,k_?iQ] := 0  /; Length[Union[{i,j,k}]] < 3 ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] /; Ordering[{i,j,k}]== {3,1,2} :=  ricci[k,i,j]     ;   (* always bring ricci to the alphabetic order *)
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] /; Ordering[{i,j,k}]== {2,3,1} :=  ricci[j,k,i]      ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] /; Ordering[{i,j,k}]== {3,2,1} := -ricci[k,j,i]      ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] /; Ordering[{i,j,k}]== {1,3,2} := -ricci[i,k,j]     ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] /; Ordering[{i,j,k}]== {2,1,3} := -ricci[j,i,k]      ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][j_,k_] := 0 ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][i_,k_] := 0 ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][i_,j_] := 0 ;
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][i_,m_] :=  ricci[m , j , k ] ;        (* 22.04.2016 because usual \[Delta]-replacement rules work only with symm tensors *)
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][j_,m_] :=  ricci[i , m , k ] ;        (* 22.04.2016 *)
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][k_,m_] :=  ricci[i , j , m ] ;        (* 22.04.2016 *)
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][m_,i_ ] :=  ricci[m , j , k ] ;        (* 22.04.2016 *)
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][m_, j_] :=  ricci[i , m , k ] ;        (* 22.04.2016 *)
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * \[Delta][m_, k_ ] :=  ricci[i , j , m ] ;        (* 22.04.2016 *)
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * z_.* v_?vQ[a_?iQ, l_?iQ, m_?iQ] := 0 /; Length[Union[{i,j,k,l,m}]] == 3 ;     (* 10.04.2021 *)   
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * z_.* v_?gvQ[a_?iQ, l_?iQ, m_?iQ] := 0 /; Length[Union[{i,j,k,a,l}]] == 3 ;     (* 10.04.2021 *)  
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * z_.* v_?gvQ[a_?iQ, l_?iQ, m_?iQ] := 0 /; Length[Union[{i,j,k,a,m}]] == 3 ;     (* 10.04.2021 *)  
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * z_.* v_?gvQ[a_?iQ, l_?iQ, m_?iQ] := 0 /; Length[Union[{i,j,k,a,m}]] == 3 ;     (* 10.04.2021 *)  
ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * z_.* v_?gvQ[ l_?iQ, m_?iQ] := 0 /; Length[Union[{i,j,k,l,m}]] == 3 ;           (* 10.04.2021 *) 

ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * ricci[i_,j_,k_] := 6 /; Length[Union[{i,j,k}]] == 3 ;

ricci /: ricci[i_?iQ, j_?iQ, k_?iQ] * z_.* t_?tQ[l_?iQ,m_?iQ] := 0 /; Length[Union[{i,j,k,l,m}]] == 3 ;       (*new Dom.Rep.*)

                  (*   V   *)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[a_?iQ,b_?iQ,m_?iQ] :=  2  \[Delta][q,a] /; Length[Union[{i,j,q,a}]] == 4 ; (*new Dom.Rep.*)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[b_?iQ,c_?iQ,m_?iQ] := -2  \[Delta][q,c] /; Length[Union[{b,m,q,c}]] == 4 ; (*new Dom.Rep.*)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[b_?iQ,m_?iQ,z_?iQ] :=  2  \[Delta][q,z] /; Length[Union[{i,j,q,z}]] == 4 ; (*new Dom.Rep.*)

              (*   V   *)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[a_?iQ,b_?iQ,q_?iQ] := -2  \[Delta][m,a] /; Length[Union[{b,m,q,a}]] == 4 ; (*new Dom.Rep.*)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[b_?iQ,c_?iQ,q_?iQ] :=  2  \[Delta][m,c] /; Length[Union[{b,m,q,c}]] == 4 ; (*new Dom.Rep.*)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[b_?iQ,q_?iQ,z_?iQ] := -2  \[Delta][m,z] /; Length[Union[{b,m,q,z}]] == 4 ; (*new Dom.Rep.*)

          (*   V   *)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[a_?iQ, m_?iQ, q_?iQ] := 2  \[Delta][b,a] /; Length[Union[{b,m,q,a}]] == 4 ;   (*new Dom.Rep.*)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[m_?iQ, n_?iQ, q_?iQ] := -2  \[Delta][b,n] /; Length[Union[{b,m,q,n}]] == 4 ;  (*new Dom.Rep.*)
ricci /: ricci[b_?iQ, m_?iQ, q_?iQ] * ricci[m_?iQ, q_?iQ, z_?iQ] := 2  \[Delta][b,z] /; Length[Union[{b,m,q,z}]] == 4 ;   (*new Dom.Rep.*)


ricci /: ricci[i_?iQ,j_?iQ,k_?iQ] * m_. * ricci[a_?iQ,b_?iQ,c_?iQ]  /; Length[Union[{i,j,k,a,b,c}]] == 5 := Module[{wynik},
                                                  If[MatchQ[i,a], wynik = riccidelta[j,k,b,c] ];
                                                  If[MatchQ[i,b], wynik = riccidelta[j,k,c,a] ];
                                                  If[MatchQ[i,c], wynik = riccidelta[j,k,a,b] ];
                                                  If[MatchQ[j,a], wynik = riccidelta[k,i,b,c] ];
                                                  If[MatchQ[j,b], wynik = riccidelta[k,i,c,a] ];
                                                  If[MatchQ[j,c], wynik = riccidelta[k,i,a,b] ];
                                                  If[MatchQ[k,a], wynik = riccidelta[i,j,b,c] ];
                                                  If[MatchQ[k,b], wynik = riccidelta[i,j,c,a] ];
                                                  If[MatchQ[k,c], wynik = riccidelta[i,j,a,b] ];  wynik * m
 ] ;


(* activate classical and geotechnical invariants definitions *)
useInvariants := Module[{},
        Unprotect[invariant1, invariant2, invariant3,jvariant2,jvariant3] ;
        invariant1[x_?withtQ] := tr[x];
        invariant2[x_?withtQ] := (norm2[x] - tr[x]^2 )/2 ;
        invariant3[x_?withtQ] := 1/6*(2 qubic[x]-3 norm2[x] tr[x]+tr[x]^3) ;
        jvariant2[x_?withtQ] := 1/2*(norm2[x] - 1/3 * tr[x]^2 );
        jvariant3[x_?withtQ] := 1/27 (9 qubic[x] - 9 norm2[x] tr[x] + 2 tr[x]^3 )  ;
        Protect[invariant1, invariant2, invariant3,jvariant2,jvariant3] ;
        Message[nnova::"new input", "invariant1[T],invariant2[T],invariant3[T],jvariant2[T],jvariant3[T]" ] ;
        ];
useRoscoeLode := Module[{},
        Unprotect[roscoeP, roscoeQ, roscoeEP, roscoeEQ, isomorphicP, isomorphicQ, lodeTheta] ;
        roscoeP[x_?withtQ] := -tr[x]/3;                (* apply for stress only *)
        roscoeEP[x_?withtQ] := -tr[x];                 (* apply for  strain only *)
        isomorphicP[x_?withtQ] := -tr[x]/Sqrt[3];
        roscoeQ[x_?withtQ] :=  Sqrt[3/2*( norm2[x] -  1/3 * tr[x]^2 )];    (* apply for stress only *)
        roscoeEQ[x_?withtQ] :=  Sqrt[2/3*( norm2[x] -  1/3* tr[x]^2   )];  (* apply for  strain only *)
        isomorphicQ[x_?withtQ] :=  Sqrt[ norm2[x] - (1/3) * tr[x]^2 ];
        lodeTheta[x_?withtQ] := (1/3) * ArcCos[ -Sqrt[2]*(9*qubic[x] - 9*norm2[x]*tr[x] + 2*tr[x]^3)/(3*norm2[x] - tr[x]^2)^(3/2)] ;
        Protect[roscoeP, roscoeQ,  roscoeEP, roscoeEQ, isomorphicP, isomorphicQ, lodeTheta] ;
        Message[ nnova::"new input", "roscoeP[T],roscoeQ[T] , roscoeEP[e],roscoeEQ[e], isomorphicP[T], isomorhicQ[T],lodeTheta[T]" ];
];

SetAttributes[\[Delta], Orderless];
SetAttributes[scalar, Orderless];
SetAttributes[fD, HoldRest] ;

(**************\[Delta]Rules**************)
\[Delta]/: \[Delta][a_?iQ,a_?iQ]  :=3;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_. * v_?vQ[a_?iQ] := v[b]  z   ;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_. * v_?vQ[b_?iQ] := v[a]  z  ;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_. * v_?vQ[a_?iQ,c_?iQ] := v[b,c]  z   ;    (* AN 2021 *)
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_. * v_?vQ[b_?iQ,c_?iQ] := v[a,c]  z  ;     (* AN 2021 *)
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_. * v_?vQ[c_?iQ,a_?iQ] := v[c,b]  z   ;    (* AN 2021 *)
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_. * v_?vQ[c_?iQ,b_?iQ]  := v[c,a]   z  ;    (* AN 2021 *)

\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_. * x_?tQ[a_?iQ,b_?iQ] := tr[x]  z ;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_.* x_?tQ[b_?iQ,a_?iQ] := tr[x]  z   ;
\[Delta]/:  Power[\[Delta][i_?iQ, j_?iQ], 2] := 3 ;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_.* x_?tQ[a_?iQ,k_?iQ]   :=   x[b,k] z ;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_.* x_?tQ[k_?iQ,a_?iQ]   :=   x[k,b]  z ;
\[Delta]/: \[Delta][b_?iQ,a_?iQ] * z_.* x_?tQ[a_?iQ,k_?iQ]   :=   x[b,k]  z ;
\[Delta]/: \[Delta][b_?iQ,a_?iQ] * z_.* x_?tQ[k_?iQ,a_?iQ]   :=   x[k,b] z  ;
\[Delta]/: \[Delta][i_?iQ,j_?iQ] * z_.* mult[a_?tQ,b_?tQ,i_?iQ,k_?iQ]   :=   mult[a,b,j,k] z   /; different3Q[i,j,k]   ;
\[Delta]/: \[Delta][i_?iQ,j_?iQ] * z_.* mult[a_?tQ,b_?tQ,k_?iQ,i_?iQ]   :=   mult[a,b,k,j] z  /; different3Q[i,j,k]   ;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] * z_.* deviator[x_?tQ[a_?iQ,b_?iQ]] := 0   ;
\[Delta]/: \[Delta][b_?iQ,a_?iQ]* z_.* deviator[x_?tQ[a_?iQ,b_?iQ]] := 0   ;
\[Delta]/: \[Delta][a_?iQ,b_?iQ] (c_  + d_)  := \[Delta][a,b] c  + \[Delta][a,b] d    ;
\[Delta]/:  (c_ + d_) \[Delta][a_?iQ,b_?iQ]  := \[Delta][a,b] c  + \[Delta][a,b] d ;
(* ************* rules for the othogonal direction cosine matrix \[Alpha] 26.04.2019***************************** *)
Unprotect[Plus,Times] ;
 \[Alpha]_?oQ[a_?iQ,i_?iQ] \[Alpha][a_?iQ,j_?iQ]   := \[Delta][i,j];
 \[Alpha]_?oQ[i_?iQ,a_?iQ] \[Alpha][j_?iQ,a_?iQ]   := \[Delta][i,j];
 \[Alpha]_?oQ[a_?iQ,i_?iQ] \[Delta][a_?iQ,j_?iQ]   :=\[Alpha][j,i];
 \[Alpha]_?oQ[i_?iQ,a_?iQ] \[Delta][j_?iQ,a_?iQ]   := \[Alpha][i,j];
 \[Alpha]_?oQ[a_?iQ,i_?iQ] \[Alpha][b_?iQ,j_?iQ] \[Alpha][c_?iQ,k_?iQ]  ricci[i_?iQ,j_?iQ,k_?iQ]   := ricci[a,b,c];
 \[Alpha]_?oQ[i_?iQ,a_?iQ] \[Alpha][j_?iQ,b_?iQ] \[Alpha][k_?iQ,c_?iQ]  ricci[i_?iQ,j_?iQ,k_?iQ]   := ricci[a,b,c];
 \[Alpha]_?oQ[a_?iQ,b_?iQ] (c_+d_):=\[Alpha][a,b] c+\[Alpha][a,b] d;
 (c_+d_) \[Alpha]_?oQ[a_?iQ,b_?iQ]:=\[Alpha][a,b] c+\[Alpha][a,b] d; 
  v_?vQ[a_?iQ,a_?iQ]  * z_ := div[ v ] z ;                             (* AN 2021 *)
  v_?vQ[a_?iQ]  * v_?vQ[a_?iQ,b_?iQ]  := grad[ v^2 , b ] / 2 ;        (* AN 2021 *)
  m_. *v_?vQ[a_?iQ, b_?iQ, c_?iQ]  +  n_. * v_?vQ[a_, c_, b_]  := (m + n) v[a,b,c] ;        (* AN 2021 *)
  v_?vQ[j_?iQ]  * v_?vQ[i_?iQ,j_?iQ]  := igrad[v^2 , i]/2 ;             (* strange gradient v[i,j] v[j] AN 2021 *)    
Protect[Plus,Times] ;

  
 

(******** powRules new 21.9.2006 ******************)
pow/:  pow[T_?tQ, 3, i_ , j_ ] * \[Delta][i_, j_]  :=  qubic[T] ;
pow/:  pow[T_?tQ, 2, i_ , j_ ] * \[Delta][i_, j_]  :=  norm2[T] ;
pow/:  pow[T_?tQ, 1, i_ , j_ ]  :=     T[ i  , j  ] ;
pow/:  pow[T_?tQ, n_, i_ , j_ ]* T_[j_, k_] :=      pow[T, n+1, i  , k ]    /; !MatchQ[i,k]  ;
pow/:  pow[T_?tQ, n_, i_ , j_ ]* T_[i_, k_] :=      pow[T, n+1, j  , k ]     /; !MatchQ[j,k]  ;
pow/:  pow[T_?tQ, n_, i_ , j_ ]* pow[T_, m_, j_ , k_ ]   :=      pow[T, n+m, i  , k ]  /; !MatchQ[i,k]  ;
pow/:  pow[T_?tQ, n_, i_ , j_ ]* pow[T_, m_, i_ , k_ ]   :=     pow[T, n+m, j  , k ]   /; !MatchQ[j,k]  ;
pow/:  pow[T_?tQ, n_, i_ , j_ ]* pow[T_, m_, i_ , j_ ]   :=     pow[T, n+m, i , j ]  \[Delta][i, j]    ;
pow/:  pow[T_?tQ, n_, i_ , j_ ]* pow[T_, m_, j_ , i_ ]   :=     pow[T, n+m, i , j ]  \[Delta][i, j]    ;
pow/:  pow[T_?tQ, 0, i_ , j_ ]   :=     \[Delta][ i  , j ]  /; !MatchQ[i,j] ;
pow/:  pow[\[Delta], n_, i_ , j_ ]   :=     \[Delta][ i  , j ]  /; !MatchQ[i,j] ;

(* 4 rules because the attribute Orderless cannot be applied to the last two arguments only *)
pow/:  mult[T_?tQ,T_,i_,j_]  * pow[T_?tQ, n_, i_ , k_ ]       :=     pow[T, n+2,  j  , k ]   /; !MatchQ[j,k] ;
pow/:  mult[T_?tQ,T_,i_,j_] * pow[T_?tQ, n_, k_ , i_ ]        :=     pow[T, n+2, j  , k ]  /; !MatchQ[j,k]  ;
pow/:  mult[T_?tQ,T_,i_,j_]  * pow[T_?tQ, n_, j_ , k_ ]       :=     pow[T, n+2,  i  , k ]   /; !MatchQ[i,k] ;
pow/:  mult[T_?tQ,T_,i_,j_] * pow[T_?tQ, n_, k_ , j_ ]        :=     pow[T, n+2, i  , k ]  /; !MatchQ[i,k]  ;

pow/:  mult[T_?tQ,T_,i_,j_] *  pow[T_?tQ, n_, i_ , j_ ]       :=     pow[T, n+2, i , j ]  \[Delta][i, j]     ;
pow/:  mult[T_?tQ,T_,i_,j_]  * pow[T_?tQ, n_, i_ , j_ ]      :=     pow[T, n+2, i , j ]  \[Delta][i, j]     ;



pow/:   pow[T_?tQ, n_, i_ , j_ ]^2         :=     pow[T, n*2, i , j ]  \[Delta][i, j]     ;


pow/: T_?tQ[i_,j_]  * pow[T_?tQ, n_, i_ , k_ ]       :=     pow[T, n+1,  j  , k ]   /; !MatchQ[j,k] ;
pow/: T_?tQ[i_,j_] * pow[T_?tQ, n_, k_ , i_ ]           :=     pow[T, n+1, j  , k ]  /; !MatchQ[j,k]  ;
pow/: T_?tQ[i_,j_]  * pow[T_?tQ, n_, j_ , k_ ]       :=     pow[T, n+1,  i  , k ]   /; !MatchQ[i,k] ;
pow/: T_?tQ[i_,j_] * pow[T_?tQ, n_, k_ , j_ ]           :=     pow[T, n+1, i  , k ]  /; !MatchQ[i,k]  ;

pow/: T_?tQ[i_,j_] *  pow[T_?tQ, n_, i_ , j_ ]       :=     pow[T, n+1, i , j ]  \[Delta][i, j]     ;
pow/: T_?tQ[i_,j_]  * pow[T_?tQ, n_, i_ , j_ ]         :=     pow[T, n+1, i , j ]  \[Delta][i, j]     ;

pow/:  pow[T_?tQ, 2, i_?iQ,j_?iQ]  :=  mult[T,T,i,j]  /; !MatchQ[i,j] ;
pow/:  tr[pow[T_?tQ, n_, i_?iQ,j_?iQ]]  :=  pow[T,n,i,j] \[Delta][i,j] /; !MatchQ[i,j] ;
pow/:  pow[T_?tQ, n_, j_, s_] *\[Delta][j_, k_]  :=    pow[ T, n, k, s] /; !MatchQ[k,s];
pow/:  pow[T_?tQ, n_, j_, s_] *\[Delta][s_, k_]  :=    pow[ T, n, j, k] /; !MatchQ[j,k];

(* add Hamiltonian identity here *)

(********multRules ******************)
mult/:   HoldPattern[mult[\[Delta],x_?tQ,i_?iQ,j_?iQ]] := x[i,j] ;
mult/:   HoldPattern[mult[x_?stQ,\[Delta],i_?iQ,j_?iQ]] := x[i,j] ;
mult/:   HoldPattern[mult[x_,x_,j_?iQ,k_?iQ]*z_.*x_[j_?iQ,k_?iQ]] := qubic[x] z ;
mult/:   HoldPattern[mult[x_,x_,j_?iQ,k_?iQ]*z_.*x_[k_?iQ,j_?iQ]] := qubic[x] z ;
mult/:   HoldPattern[ mult[x_,y_,i_?iQ,j_?iQ]*z_.*\[Delta][j_?iQ,i_?iQ] ] /; stQ[x] && stQ[y] := scalar[x,y] z  ;
(* mult/:   HoldPattern[mult[x_,x_,j_?iQ,k_?iQ]^2] :=
           4 * qubic[x] * tr[x] /3 + tr[x]^4 / 6  - (tr[x])^2 * norm2[x]  + (norm2[x])^2 / 2 ;    chyba zle bo kwdrat to mnozenie z jedna kropka *)
mult/:   HoldPattern[mult[x_,x_,j_?iQ,k_?iQ] * mult[x_,x_,k_?iQ,j_?iQ] ] := 4 * qubic[x] * tr[x] /3 + tr[x]^4 / 6  - tr[x]^2 * norm2[x]  + norm2[x]^2 / 2  ;
mult/:   HoldPattern[mult[x_,x_,j_?iQ,k_?iQ] * mult[x_,x_,j_?iQ,k_?iQ] ] := 4 * qubic[x] * tr[x] /3 + tr[x]^4 / 6  - tr[x]^2 * norm2[x]  + norm2[x]^2 / 2  ;
mult/:   HoldPattern[mult[x_,x_,i1_?iQ,i2_?iQ] * mult[x_,x_,i3_?iQ,i4_?iQ] * mult[x_,x_,i5_?iQ,i6_?iQ]] :=
         (((3 norm2[x]^3 - 9 norm2[x]^2 tr[x]^2 +  ((2 qubic[x] + tr[x]^3))^2 + 3 norm2[x] ((4 qubic[x] tr[x] - tr[x]^4)))))/12   /;  qubiclikeQ[i1,i2,i3,i4,i5,i6]   ;
mult/:   HoldPattern[mult[x_,x_,i1_?iQ,i2_?iQ] * mult[x_,x_,i3_?iQ,i4_?iQ] * x_[i5_?iQ,i6_?iQ] ] :=
         (((5 qubic[x] tr[x]^2 + tr[x]^5 + 5 norm2[x]  ((qubic[x] - tr[x]^3)))))/6  /;  qubiclikeQ[i1,i2,i3,i4,i5,i6]       ;
mult/:    HoldPattern[x_[j_?iQ,k_?iQ]*z_.*mult[x_?iQ,x_?iQ,j_?iQ,k_?iQ]] := qubic[x] z   ;
Unprotect[Plus,Minus,Divide,Power,Times] ;
  v_?vQ[i_?iQ] * z_. * v_?vQ[i_?iQ] := len2[v] z ;
  x_?tQ[i_?iQ,k_?iQ] * z_. * y_?tQ[i_?iQ,k_?iQ] := scalar[x,y]  z          /; !MatchQ[i,j] ;
  x_?tQ[i_?iQ,k_?iQ] * z_. * y_?tQ[k_?iQ,j_?iQ]  := mult[x,y,i,j] z        /; !MatchQ[i,j] ;   (* maj 2006 *)
  x_?tQ[i_?iQ,k_?iQ] * z_. * y_?tQ[j_?iQ,k_?iQ]  := mult[x,y,i,j] z        /; !MatchQ[i,j] ;   (* maj 2006 *)
  x_?tQ[k_?iQ,i_?iQ] * z_. * y_?tQ[j_?iQ,k_?iQ]  := mult[x,y,i,j] z        /; !MatchQ[i,j] ;   (* maj 2006 *)
  x_?tQ[k_?iQ,i_?iQ] * z_. * y_?tQ[k_?iQ,j_?iQ]  := mult[x,y,i,j] z        /; !MatchQ[i,j] ;   (* maj 2006 *)
  (*  new 21.9.06  removed 24.10.06 *)
  (* T_?tQ[i_?iQ,j_?iQ]^n_ := pow[T,n,i,j]    ;*)


  x_?tQ[i_?iQ,j_?iQ]^(n_?EvenQ) := If[!MatchQ[i,j], Message[ nnova::"even exponent", x, i, j, n ] ; norm2[x]^(n/2), tr[x]^n ] ;
  x_?tQ[i_?iQ,j_?iQ]^(n_?OddQ)  := If[!MatchQ[i,j],  Message[ nnova::"odd exponent", x, i, j, n ] ; pow[x,n,i,j], tr[x]^n ] ;
  x_?tQ[i_?iQ,j_?iQ]^(n_?OddQ)  := If[!MatchQ[i,j],  Message[ nnova::"odd exponent", x, i, j, n ] ; pow[x,n,i,j], tr[x]^n ] ;

  v_?vQ[i_?iQ]^(n_?EvenQ) :=  len2[v]^(n/2) ;
 (* x_?tQ[a_?iQ,b_?iQ]^(n_?OddQ) :=  x[a,b] norm2[x]^((n-1)/2)  /;  !MatchQ[a,b] && n>1 ;  chyba zle  bo kwadrat to mnozenie z jedna kropka *)
  x_?tQ[a_?iQ,b_?iQ] x_[a_?iQ,b_?iQ]  :=  norm2[x]     /; !MatchQ[a,b] ;
  HoldPattern[  a_. mult[x_,y_,i_?iQ,j_?iQ] +  b_. mult[y_,x_,j_?iQ,i_?iQ]  ] /; stQ[x] && stQ[y]  :=   (a+b) * mult[x,y,i,j]  ;
  (* new 21.9.06 changed 24.10.06 *)
  (a_.*x_?tQ[i_,j_]+b_.*y_?tQ[i_,j_])^2 := Simplify[a^2 norm2[x] +     2 a b scalar[x,y]   + b^2 norm2[y]] ;
Protect[Plus,Minus,Divide,Power,Times] ;

(******** scalarRules ****************)
scalar/: scalar[x_?tQ,x_] := norm2[x] ;
scalar/: scalar[\[Delta],x_?tQ] := tr[x] ;
scalar /: scalar[x_,y_] /;  !(tQ[x] &&  tQ[y] ) := Simplify[Expand[(x) (y)]];
scalar /: scalar[x_[i_,j_],y_[i_,j_]] /;  (tQ[x] &&  tQ[y] ) := scalar[x,y];   (* bo z indeksami to tez tensor *)
scalar /: scalar[x_[i_,j_],y_[j_,k_],z_[k_,i_]] /;  (tQ[x] &&  tQ[y] && tQ[z]) := scalar[x,y,z];   (* bo z indeksami to tez tensor *)
scalar /: scalar[x_,x_,x_] /;  tQ[x]   := qubic[x];
scalar /: scalar[x_,\[Delta],\[Delta]] /;  tQ[x]   := tr[x];               (* 7.12.2005 *)
scalar /: scalar[x_,y_,\[Delta]] /;  (tQ[x] &&  tQ[y] ) := scalar[x, y];   (* 7.12.2005 *)
scalar /: scalar[mx_.*x1_?tQ + nx_.*x2_?tQ,    my_.*y1_?tQ + ny_.*y2_?tQ,   mz_.*z1_?tQ + nz_.*z2_?tQ ]   :=
       mx*my*mz*scalar[x1,y1,z1] + my*mz*nx*scalar[x2,y1,z1] + mx*mz*ny*scalar[x1,y2,z1] +   mz*nx*ny*scalar[x2,y2,z1] + mx*my*nz*scalar[x1,y1,z2] +
       my*nx*nz*scalar[x2,y1,z2]+  mx*ny*nz*scalar[x1,y2,z2] + nx*ny*nz*scalar[x2,y2,z2] ;
scalar /: scalar[mx_.*x1_?tQ + nx_.*x2_?tQ,    my_.*y1_?tQ + ny_.*y2_?tQ,   mz_.*z1_?tQ               ]   :=
        mx*my*mz*scalar[x1,y1,z1]+my*mz*nx*scalar[x2,y1,z1]+mx*mz*ny*scalar[x1,y2,z1]+mz*nx*ny*scalar[x2,y2,z1] ;
scalar /: scalar[mx_.*x1_?tQ + nx_.*x2_?tQ,    my_.*y1_?tQ              ,   mz_.*z1_?tQ               ]   := mx*my*mz*scalar[x1,y1,z1]+my*mz*nx*scalar[x2,y1,z1];

(*********************fDRules********************************)
fD/:  fD[a_ + b_, c_?tQ[i_?iQ,j_?iQ]]  := Simplify[ fD[a,c[i,j]] + fD[b,c[i,j]] ]  ;
fD/:  fD[a_   b_, c_?tQ[i_?iQ,j_?iQ]]  := Simplify[ fD[a,c[i,j]] (b) + fD[b,c[i,j]] (a)  ] ;
fD/:  fD[a_/b_, c_?tQ[i_?iQ,j_?iQ]] := Simplify[(fD[a,c[i,j]] (b) - fD[b,c[i,j]] (a))/(b)^2] /; withTHEitQ[b, c] ;   (* chyba zle : potrzeba fD[a* b^-1]  *)
fD/:  fD[a_,c_?tQ[i_?iQ,j_?iQ]]  :=  0      /; FreeQ[a,c] ;
fD/:  fD[c_[i_,j_],c_?tQ[k_?iQ, l_?iQ]] :=  ( \[Delta][i,k] \[Delta][j,l] + \[Delta][i,l] \[Delta][j,k]  )/2;    (* symmetrized version 2006 *)
(* V new 21.9.06 *)
fD/:  fD[a_^n_,c_?tQ[i_?iQ,j_?iQ] ] :=  n*( a )^(n-1)* fD[a, c[i,j]]  /;  !FreeQ[a, c]  && !withTHEitQ[a,c]    ;   (* w a jest c ale  bez indexow  *)
fD/:  fD[Sqrt[a_],c_?tQ[i_?iQ,j_?iQ] ] := fD[a,c[i,j]] / (2 Sqrt[a] )  /; !FreeQ[a, c]  && !withTHEitQ[a,c] ;   (* w a jest c ale  bez indexow  *)
fD/:  fD[tr[x_] , x_?tQ[i_?iQ,j_?iQ]] := \[Delta][i,j]    ;
fD/:  fD[det[x_] , x_?tQ[i_?iQ,j_?iQ]] :=  det[x] pow[ x, -1, i,j]   ;  (* added  2016 *)
fD/:  fD[scalar[y_?tQ,x_]^n_. , x_?tQ[i_?iQ,j_?iQ]] :=  n scalar[y,x]^(n-1) (y[i,j] + y[j,i] )/2   /;  !MatchQ[x,y]   ;  (* symmetrized version 2006 *)
fD/:  fD[scalar[x_,y_?tQ]^n_. , x_?tQ[i_?iQ,j_?iQ]] :=  n scalar[y,x]^(n-1) (y[i,j] + y[j,i]  )/2   /;  !MatchQ[x,y]   ; (* added and symmetrized  2006 *)
fD/:  fD[norm2[x_],x_[i_?iQ,j_?iQ]] :=   x[i,j] + x[j,i] ; (* symmetrized version 2006 *)
fD/:  fD[qubic[x_?stQ], x_[i_?iQ,j_?iQ] ] :=  3 (mult[x,x,i,j]+ mult[x,x,j,i])/2  ;  (* symmetrized version 2006 *)
fD /: HoldPattern[fD[ mult[y_, x_, i_?iQ, j_?iQ], x_[p_?iQ, q_?iQ]]] /; ! MatchQ[x, y] :=    ( y[i, p] \[Delta][j, q] +  y[i, q] \[Delta][j, p] )/2;   (* symmetrized version 2006 *)
fD /: HoldPattern[fD[ mult[x_, y_, i_?iQ, j_?iQ], x_[p_?iQ, q_?iQ]]] /; ! MatchQ[x, y] :=    ( y[q, j] \[Delta][i, p] +  y[p, j] \[Delta][i, q] )/2  ;   (* symmetrized version 2006 *)
fD/:  HoldPattern[fD[ mult[x_, x_, i_?iQ, j_?iQ], x_[p_?iQ,q_?iQ] ]] :=(\[Delta][i,p] x[q,j] +  \[Delta][i,q] x[p,j]   +   x[i,p] \[Delta][j,q] +  x[i,q] \[Delta][j,p])/2; (* symmetrized version 2006 *)
fD/:  fD[Log[y_],x_?tQ[i_?iQ,j_?iQ]] := fD[y,x[i,j]]/y   /;  !FreeQ[y,x]  && !withTHEitQ[y,x]  ;
fD/:  fD[Exp[y_],x_?tQ[i_?iQ,j_?iQ]] :=  Exp[y]*fD[y,x[i,j]]   /;  !FreeQ[y,x]  && !withTHEitQ[y,x]  ;
fD/:  fD[scalar[x_,y_,z_],a_?tQ[i_?iQ,j_?iQ]] :=
 If[SameQ[x,a] , (mult[y,z,j,i]+ mult[y,z,i,j])/2,0]+ If[SameQ[y,a], (mult[x,z,i,j] + mult[x,z,j,i])/2,0] + If[SameQ[z,a], (mult[x,y,i,j] + mult[x,y,j,i])/2,0];


(* new 21.9.2006 and 17.1.2012    *)

fD/:  fD[pow[x_?tQ, -1, i_, j_  ], x_[r_, s_]] :=  -1/2 *( pow[x,-1, i, r] * pow[x,-1, j,s ]  + pow[x,-1, i, s] * pow[x,-1, j,r ]      )/; different4Q[i,j,r,s] ;
fD/:  fD[pow[x_?tQ, -2, i_, j_  ], x_[r_, s_]] :=  -1/2 *( pow[x,-1, i, r] * pow[x,-2, j,s ] + pow[x,-2, i, r] * pow[x,-1, j,s ] +    pow[x,-1, i, s] * pow[x,-2, j,r ] + pow[x,-2, i, s] * pow[x,-1, j,r ]      )  /; different4Q[i,j,r,s] ;

(* zmult rules = outer product used by Itskov *)
zoutSym/:  HoldPattern[zoutSym[ a_?tQ[i_ ,j_ ]  , b_?tQ[k_ , l_ ]    ] ] := ( a[i,k] b[j, l] + a[i,l] b[j,k]  )/2;
zoutSym/: HoldPattern[zoutSym[ a_?tQ[i_ ,j_ ]  , mult[ b_?tQ, c_?tQ,   k_ , l_ ]    ] ]:=   ( a[i,k] mult[b,c, j,l] + a[i,l] mult[b,c, j,k]  ) /2;
zoutSym/: HoldPattern[zoutSym[mult[a_?tQ, b_?tQ, i_,j_]  , c_?tQ [k_ , l_ ]    ]] :=   (  mult[a, b, i, k] c[j,l]  +  mult[a, b, i, l] c[j,k]   )/2;
zoutSym/: HoldPattern[zoutSym[ a_?tQ[i_ ,j_ ]  , pow[ b_?tQ, n_,   k_ ,  l_ ]    ] ]:=   ( a[i,k]* pow[a,n, j,l] + a[i,l]* pow[b,n, j,k] )/2;
zoutSym/: HoldPattern[zoutSym[pow[a_?tQ, n_,i_,j_]  , c_?tQ [k_ , l_ ]    ]] :=   (  pow[a, n, i, k] *c[j,l]  +  pow[a, n, i, l] *c[j,k]   )/2;
zoutSym/: HoldPattern[zoutSym[mult[a_?tQ, b_,i_,j_]  , pow[c_?tQ, n_,k_, l_]    ] ]:=    ( mult[a,b,i,k]    pow[c,n,j, l]  + mult[a,b,i,l]    pow[c,n,j, k])/2;
zoutSym/: HoldPattern[zoutSym[ pow[a_?tQ, n_,i_,j_]  , mult[b_?tQ, c_?tQ, k_, l_]  ] ]:=  (   pow[a,n,i,k]    mult[b,c,j, l]  +   pow[a,n,i,l]     mult[b,c,j, k]  )/2;
zoutSym/: HoldPattern[zoutSym[  mult[a_?tQ, b_,i_,j_]  ,mult[c_?tQ, d_?tQ, k_, l_]   ]] :=      ( mult[a,b,i,k]   mult[c,d,j, l]  +   mult[a,b,i,l]   mult[c,d,j, k] )/2 ;
zoutSym/:  HoldPattern[zoutSym[ pow[a_?tQ, n_,i_,j_]  , pow[b_?tQ, m_,k_, l_]    ] ]:=    ( pow[a,n,i,k]    mult[b,m,j, l] +   pow[a,n,i,l]    mult[b,m,j, k] )/2;



fD/:  fD[  pow[T_?tQ,   n_Integer, i_, j_ ] , T_?tQ[s_ ,t_ ]   ] :=    Sum[  pow[T,n-1-r1r2r3 ,i,j] ~zoutSym~  pow[T, r1r2r3 ,s,t ]  ,{r1r2r3,0,n-1}]    /;   n>1   ;
fD/:  fD[  pow[T_?tQ, n_Integer, i_, j_] , T_?tQ[s_ ,t_ ]   ] :=    -Sum[pow[T, Evaluate[-(r1r2r3+1)] ,i,j]  ~zoutSym~ pow[T, Evaluate[n+r1r2r3] ,s,t]  ,{r1r2r3,0,-n-1}]    /;   n<0  ;

(* Assumptions 15.2.2008 *)
assumeDeviatoric[s_?tQ] :=  Module[{}, Unprotect[tr]; tr[s] := 0 ; Protect[tr]; ];
assumeUnitary[s_?tQ] :=   Module[{}, Unprotect[norm2]; norm2[s] := 1 ; Protect[norm2]; ];
assumeNotDeviatoric[s_?tQ] :=   Module[{}, Unprotect[tr]; tr[s] =.; Protect[tr]; ];
assumeNotUnitary[s_?tQ] :=   Module[{}, Unprotect[norm2]; norm2[s] =. ; Protect[norm2]; ];
assumeMajorSymmetric[x_] :=   Module[{a, b}, Unprotect[Plus];
                                      a_. *x[i_, j_, k_, l_] + b_. *x[k_, l_, i_, j_] := (a + b)*  x[i, j, k, l];
                                      a_. *x[i_, j_, k_, l_] y_?tQ[ k_, l_ ] + b_. *x[m_, n_, i_, j_] y_?tQ[m_,n_] := (a + b)*  x[i, j, k, l] y[k,l];
                                      Protect[Plus];

                                      ];
assumeMinorSymmetric[x_] :=   Module[{a, b}, Unprotect[Plus];
   a_. *x[i_, j_, k_, l_] + b_. *x[j_, i_ , k_, l_] := (a + b)*  x[i, j, k, l];
   a_. *x[i_, j_, k_, l_] + b_. *x[i_, j_ , l_, k_] := (a + b)*  x[i, j, k, l];
   a_. *x[i_, j_, k_, l_] + b_. *x[j_, i_ , l_, k_] := (a + b)*  x[i, j, k, l];
   Protect[Plus];
   ];


(* mew 8.12.2009  FOURTH order tensors --------------------------------  *)

Unprotect[\[Delta]];
\[Delta] /: A[i_, j_, k_, l_] \[Delta][i_, m_] := A[m, j, k, l];
\[Delta] /: A[i_, j_, k_, l_] \[Delta][j_, m_] := A[i, m, k, l];
\[Delta] /: A[i_, j_, k_, l_] \[Delta][k_, m_] := A[i, j, m, l];
\[Delta] /: A[i_, j_, k_, l_] \[Delta][l_, m_] := A[i, j, k, m];
Protect[\[Delta]];
fD /: fD[ m_. *  A_[i_, j_, k_, l_]  d_[k_, l_]  , d_[a_, b_]   ] :=   m  A[i, j, a, b]  ;
fD /: fD[ m_. *  A_[i_, j_, k_, l_]  d_[i_, j_]   , d_[a_, b_]   ] :=   m  A[a, b, k, l]  ;
fD /: fD[ m_. *  A_[i_, j_, k_, l_]  d_[k_, l_]   d_[i_, j_] ,    d_[a_, b_]   ] :=   m  A[i, j, a, b]  d[i, j] +  m  A[a, b, k, l]  d[k, l]   ;
fD /: fD[ m_. *  A_[i_, j_, k_, l_]  d_[i_, j_]  d_[k_, l_],    d_[a_, b_]   ] :=     m  A[i, j, a, b]  d[i, j] +  m  A[a, b, k, l]  d[k, l]   ;



(* finished FOURTH order tensors --------------------------------  *)


normalized[T_?tQ] := T/Sqrt[norm2[T]];
normalized[T_?withtQ] := T/Sqrt[norm2[T]];

hated[T_?tQ] := T/tr[T] ;
hated[T_?withtQ] := T/tr[T] ;

nnova::"Globals" = "Syntax warning: possibly  unintended usage of globals.  This function is using global variable(s):  `1`   `2` ." ;

(* isotropic elastic stiffness and isotropic elastic compliance: *)

One[i_?iQ,j_?iQ,k_?iQ,l_?iQ] := (1/2) * (\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k]) ;

iE[i_?iQ,j_?iQ,k_?iQ,l_?iQ,EE_,\[Nu]_] := Module[{e,n}, e=EE ; n=\[Nu];
                              e/((1+n) (1-2n))*(n \[Delta][i,j] \[Delta][k,l]+(1-2n)/2*(\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k]))];
iE[i_?iQ,j_?iQ,k_?iQ,l_?iQ] := Module[{e,n},
 Message[nnova::"Globals", "{EE}=", {EE} ] ;
  e=EE ; n=\[Nu];    e/((1+n) (1-2n))*(n \[Delta][i,j] \[Delta][k,l]+(1-2n)/2*(\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k]))
];

iE[ ] := Module[{e,n}, e=EE ; n=\[Nu];
 Message[nnova::"Globals", "{EE,\[Nu],i,j,k,l}=", {EE,\[Nu],i,j,k,l} ] ;
          e/((1+n) (1-2n))*(n \[Delta][i,j] \[Delta][k,l]+(1-2n)/2*(\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k]))
];

iC[i_?iQ,j_?iQ,k_?iQ,l_?iQ,EE_,\[Nu]_] := Module[{e,n}, e=EE ; n=\[Nu];
                           ( -n/e \[Delta][i,j] \[Delta][k,l]+ (1+n)/(2 e) *(\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k] ))];

iC[i_?iQ,j_?iQ,k_?iQ,l_?iQ] := Module[{e,n},
 Message[nnova::"Globals", "{EE}=", {EE} ] ;
e=EE ; n=\[Nu];  ( -n/e \[Delta][i,j] \[Delta][k,l]+ (1+n)/(2 e) *(\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k] ))
];

iC[ ] := Module[{e,n},
 Message[nnova::"Globals", "{EE,\[Nu],i,j,k,l}=", {EE,\[Nu],i,j,k,l} ] ;
 e=EE ; n=\[Nu]; ( -n/e \[Delta][i,j] \[Delta][k,l]+ (1+n)/(2 e) *(\[Delta][i,k] \[Delta][j,l]+\[Delta][i,l]\[Delta][j,k] ))
];

(* conversion nova->anova package.  The version with diagonal tensors *)

transfer33[expression_, diagonal_:{ }]:= Module[{a},
a = Table[expression , {i, 1, 3}, {j, 1, 3}  ];
a = a //. numerical;
a = a //.    (  { #[ 1,2]  ->  0  ,#[1,3] -> 0, #[2,3] -> 0}  & /@  diagonal  // Flatten   ) ;
a
] ;

transfer33ij[expression_,{i_,j_}, diagonal_:{ }]:= Module[{a},
a = Table[expression , {i, 1, 3}, {j, 1, 3}  ];
a = a //. numerical;
a = a //.    (  { #[ 1,2]  ->  0  ,#[1,3] -> 0, #[2,3] -> 0}  & /@  diagonal  // Flatten   ) ;
a
] ;



i123Q[x_] := MemberQ[{1, 2, 3}, x];


transfer99[expression_,diagonal_:{ }]:=     (*convert expression (i,j,k,l)  to  a 9x9 matrix assuming that tensor (s)  are given in diagonal form  *)
Module[ {ii, jj, i9,j9,a},
Message[nnova::"Globals", "{i,j,k,l}=", {i,j,k,l} ] ;
i9 = {1,2,3,1,2,1,3,2,3};
j9 = {1,2,3,2,1,3,1,3,2};
a = Array[0&, {9,9}];
Do[ a[[ii,jj]] =  Evaluate[expression /. {i->i9[[ii]], j->j9[[ii]], k->i9[[jj]], l->j9[[jj]] } ]
   ,{ii,1,9},{jj,1,9}
   ] ;
a = a //. numerical;
a = a //.    (  { #[ 1,2]  ->  0  ,#[1,3] -> 0, #[2,3] -> 0}  & /@  diagonal  // Flatten   ) ;
a
]  ;



transfer99ijkl[expression_, {i_,j_,k_,l_},diagonal_:{ } ]:=      Module[{ii, jj, i9,j9,a },
i9 = {1,2,3,1,2,1,3,2,3};
j9 = {1,2,3,2,1,3,1,3,2};
a = Array[0&, {9,9}];
Do[ a[[ii,jj]] =  Evaluate[expression /. {i->i9[[ii]], j->j9[[ii]], k->i9[[jj]], l->j9[[jj]] } ]
   ,{ii,1,9},{jj,1,9}
   ] ;
a = a //. numerical;
a = a //.    (  { #[ 1,2]  ->  0  ,#[1,3] -> 0, #[2,3] -> 0}  & /@  diagonal  // Flatten   ) ;
a
]  ;



(* obsolete  subscriptForm = { A_?tQ[i_, j_] :> Subscript[A, i j] , pow[A_?tQ, n_ , i_ ,j_] :> Subsuperscript[A, i j, n],   mult[A_,B_,i_,j_] :> Subscript[(A B), i j ]  }  *)

subscriptForm={A_?tQ [i_?NumberQ ,j_?NumberQ]:>Subscript[A,  i*10+ j  ] ,
                                pow[A_,n_,i_?NumberQ,j_?NumberQ]:>Subsuperscript[A, i *10 +  j ,n] ,
                                 HoldPattern[  mult[A_,B_,i_ ?NumberQ,j_?NumberQ ]]  :>Subscript[(A B), 10* i + j ] /;  NumberQ[Evaluate[ i*j]]  ,
                               A_?tQ[i_ ,j_ ]   :>Subscript[A, HoldForm[ i  j]]  /;   \[Not]NumberQ[Evaluate[ i*j]]   ,
                                 HoldPattern[  pow[A_,n_,i_ ,j_ ] ]     :>Subsuperscript[A, HoldForm[i   j] ,n]  /;   \[Not]NumberQ[Evaluate[ i*j]],
                                HoldPattern[ mult[A_,B_,i_ ,j_ ]]   :>Subscript[(A B), HoldForm[ i   j] ] /;   \[Not]NumberQ[Evaluate[ i*j]]
};


beautifierT = { HoldPattern[ n_. T[i_,j_] + m_. \[Delta][i_,j_] tr[T] ]  /; n+3 m == 0 :>   n Td[i,j] ,
                HoldPattern[ -3 T[i_,j_] + \[Delta][i_,j_]tr[T] + x_.]   :>   -3 Td[i,j] + x ,
                HoldPattern[  n___ norm2[T] + m___ tr[T]^2 + x_.]  /; Simplify[n+ 3* m] == 0              :>   n norm2[Td] + x ,
                HoldPattern[  n_. norm2[T] + m_. tr[T]^2 + x_.]  /; Simplify[n+ 3* m] == 0              :>   n norm2[Td] + x ,
                HoldPattern[-3 norm2[T] +  tr[T]^2 + x_. ]              :>   -3 norm2[Td] + x ,
                HoldPattern[ a_. \[Delta][k_, r_]*\[Delta][l_, s_]+b_. \[Delta][k_,l_]*\[Delta][r_,s_] ] /; a+3 b == 0 :> a Dev4[k,l,r,s],
                HoldPattern[\[Delta][i_,k_]* a_. *\[Delta][j_,l_] ]                   :>  a \[Delta]4[i,j,k,l],
                HoldPattern[ m_. qubic[T] + n_. norm2[T]*tr[T] + r_.  tr[T]^3 + x_. ]  /; (Simplify[m+ n ]== 0 && Simplify[m- 9 r/2 ] == 0) :>  m qubic[Td] +x,
                HoldPattern[ n_. \[Delta]4[i_,j_,k_,l_] + n_. \[Delta]4[i_,j_,l_,k_] + x_.]  :>  2*n* \[Delta]4s[i,j,l,k] + x,
                HoldPattern[ n_. \[Delta]4[i_,j_,k_,l_] + n_. \[Delta]4[j_,i_,k_,l_] + x_.]  :>  2*n* \[Delta]4s[i,j,l,k] + x,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[a_,b_,i_,j_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j] ,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[a_,b_,j_,i_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j] ,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[b_,a_,i_,j_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j] ,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[b_,a_,j_,i_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j]
            } ;

beautifierE = { HoldPattern[ n_. e[i_,j_] + m_. \[Delta][i_,j_]tr[e] + x_.]  /; n+3 m == 0 :>   n ed[i,j] + x  ,
                HoldPattern[ -3 e[i_,j_] + \[Delta][i_,j_]tr[e] + x_.]   :>   -3 ed[i,j] + x ,
                HoldPattern[  n___ norm2[e] + m___ tr[e]^2 + x_.]  /; Simplify[n+ 3* m] == 0              :>   n norm2[ed] + x ,
                HoldPattern[  n_. norm2[e] + m_. tr[e]^2 + x_.]  /; Simplify[n+ 3* m] == 0              :>   n norm2[ed] + x ,
                HoldPattern[-3 norm2[e] +  tr[e]^2 + x_. ]              :>   -3 norm2[ed] + x ,
                HoldPattern[ a_. \[Delta][k_, r_]*\[Delta][l_, s_]+b_. \[Delta][k_,l_]*\[Delta][r_,s_] ] /; a+3 b == 0 :> a Dev4[k,l,r,s],
                HoldPattern[\[Delta][i_,k_]* a_. *\[Delta][j_,l_] ]                   :>  a \[Delta]4[i,j,k,l],
                HoldPattern[  m_. qubic[e] + n_. norm2[e]*tr[e] + r_.  tr[e]^3 + x_. ]  /; (Simplify[m+ n ]== 0 && Simplify[m- 9 r/2 ] == 0) :>  m qubic[ed] +x,
                HoldPattern[ n_. \[Delta]4[i_,j_,k_,l_] + n_. \[Delta]4[i_,j_,l_,k_] + x_.]  :>  2*n* \[Delta]4s[i,j,l,k] + x,
                HoldPattern[ n_. \[Delta]4[i_,j_,k_,l_] + n_. \[Delta]4[j_,i_,k_,l_] + x_.]  :>  2*n* \[Delta]4s[i,j,l,k] + x,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[a_,b_,i_,j_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j] ,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[a_,b_,j_,i_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j] ,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[b_,a_,i_,j_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j] ,
                HoldPattern[ m___ \[Delta]4[a_,i_,b_,j_] + n___ \[Delta]4s[b_,a_,j_,i_] + x_.]  /; Simplify[n+3*m] == 0       :>  n* \[Delta]Dev[a,b,i,j]
            } ;

numerical = {  \[Delta][1,2]:>  0, \[Delta][2,3]:> 0, \[Delta][1,3]:> 0 , \[Delta][1,1] :> 1, \[Delta][2,2] :> 1,  \[Delta][3,3] :> 1 ,
                ricci[1,1,2]:>0,ricci[1,2,1]:>0,ricci[2,1,1]:>0,ricci[1,1,3]:>0,ricci[1,3,1]:>0,
                ricci[3,1,1]:>0,ricci[2,2,1]:>0,ricci[2,1,2]:>0,ricci[1,2,2]:>0,ricci[2,2,3]:>0,
                ricci[2,3,2]:>0,ricci[3,2,2]:>0,ricci[3,3,1]:>0,ricci[3,1,3]:>0,ricci[1,3,3]:>0,
                ricci[3,3,2]:>0,ricci[3,2,3]:>0,ricci[2,3,3]:>0,ricci[1,2,3]:>1,ricci[1,3,2]:>-1,
                ricci[2,1,3]:>-1,ricci[2,3,1]:>1,ricci[3,1,2]:>1,ricci[3,2,1]:>-1,ricci[1,1,1]:>0,
                ricci[2,2,2]:>0,ricci[3,3,3]:>0  } ;


(* Messages *)
 nnova::"even exponent" = "Syntax warning: The even  exponent  at tensor `1`  can be  misinterpreted, as pow[`1`,`4`,`2`,`3`]. Nova interprets `1`[`2`,`3`]^`4` as  norm2[`1`]^(`4`/2)" ;
 nnova::"odd exponent" = "Syntax warning: The odd  exponent   at  tensor `1` is very suspicious. Nova interprets `1`[`2`,`3`]^`4` as pow[`1`,`4`,`2`,`3`] " ;
 nnova::"new input" = "Additionally you may now use for input: `1` . "
 nnova::"Globals" = "Syntax warning: possibly  unintended usage of globals.  This function is using global variable(s):  `1`   `2` ." ;

 End[ ] ;  (* private *)

Protect[
addst, addv, addst4, assumeDeviatoric, assumeMajorSymmetric, assumeMinorSymmetric, assumeNotDeviatoric, assumeNotUnitary, assumeUnitary,
beautifierE, beautifierT,
deviator,  different3Q, different4Q, \[Delta], \[Delta]4, \[Delta]4s, Td, ed, \[Delta]Dev, Dev4,
fD,  hated,
iE, iC, iEep, iQ, i123Q,
mult,   noLongerTensor, norm2,  normalized,
One, pow, qubic, qubiclikeQ, ricci, riccidelta, scalar,  stQ, subscriptForm,
 tr, tQ, t4Q,  transfer33, transfer99ijkl, transfer99,
useInvariants, useRoscoeLode,
vQ, withANitQ,  withTHEitQ,  withtQ,
 zoutSym, numerical  ]  ;


EndPackage[ ] ;
$Context = "Tensor`nnova`"   ;

  Print[ "You are in the context Tensor`nnova`. It simplifies index-expressions for Cartesian tensors.
        It provides the following functions:  \
     \n addst,  alltensors,  different3Q,  deviator,  \[Delta], \
     \n fD,   iC,   iE,   iQ,   mult,   noLongerTensor,   norm2,   \
     \n qubiclikeQ,    qubic,   scalar,   stQ,   tQ,   transfer99, transfer33       \
     \n useInvariants,   useRoscoeLode,   withtQ,  withANitQ    pow, different4Q , withTHEitQ , ricci[i,j,k].     \
     \n Delayed rules: \
     \n  beautifierT,  beautifierE, numerical \
     \n Mind the  difference between even and odd exponents at tensors:    \
     \n x[i,j]^(2n) = (x[i,j] x[i,j])^n = (norm2[x])^n  != pow[x,2n,i,j], but  x[i,j]^(2n+1)  = pow[x,2n +1 ,i,j]    \
     \n See also recently added  :       \
     \n normalized, hated, assumeDeviatoric, assumeUnitary,  assumeMajorSymmetric,  assumeMinorSymmetric,    \
     \n the first two can be undone with:  assumeNotDeviatoric , assumeNotUnitary \
     \n Further functions:  subscriptForm, zoutSym \
      " ];

 addst[T] ; Print["and the orthogonal direction cosine matrix \[Alpha][i,j] (for rotations but not for reflections) "] ; 



