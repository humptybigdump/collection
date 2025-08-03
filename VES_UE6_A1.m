%%VES 6. Übung Aufgabe 1
clc
close all
clear


%% a)
a1 = maxplusmatrix(3);
a2 = maxplusmatrix([3, 0; -inf, 5]);
a_mat = mtimes(a1,a2)


%% b)
b1 = maxplusmatrix([2,1;-1,-inf]);
b2 = maxplusmatrix([-2,-1;3,4]);
b_mat = mtimes(b1,b2)


%% c)
c1 = maxplusmatrix([2,1;-1,-inf]);
c2 = maxplusmatrix([-2,-1;3,4]);
c_mat = times(c1,c2)

%% d)
d1 = maxplusmatrix([2,1;-1,-inf]);
d_mat = mtimes(d1,d1)


%% e)
e1 = maxplusmatrix([1,4;2,1]);
e2 = maxplusmatrix([1,4;2,1]);
e_mat = mtimes(e1,inv(e2))


%% f)
f1 = maxplusmatrix([3,0;-inf,5]);
f2 = maxplusmatrix([-2,-1;3,4]);
f_mat = plus(f1,f2)


%% g)
g1 = maxplusmatrix([2,1;-1,inf]);
g2 = maxplusmatrix([-2,-1;3,4]);
g_mat = mtimesmin(g1,g2)


%% h)
h1 = maxplusmatrix([3,0;inf,5]);
h2 = maxplusmatrix([-2,-1;3,4]);
h_mat = minus(h1,h2)

