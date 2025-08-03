(* ::Package:: *)

H = 0.02; EE = 10^4; k = 10^-8; cv = EE*k; sav = 10; savd = 2; sampl = 4; t98 = 1*H^2/cv; dt = 0.002;
sges = sav + savd *t + sampl*Sin[3*2 Pi*t/(1.5*t98)];
pde = -k*D[u[x, t], x, x] == D[sges - u[x, t], t]/EE;
ic = u[x, 0] == sav*Erf[1000 x];
bc1 = u[0.0, t] == 0;
bc2 = Derivative[1, 0][u][H, t] == 0;
solu = NDSolve[{pde, ic, bc1, bc2}, u, {x, 0, H}, {t, 0, 1.5*t98}, StartingStepSize -> 0.0001, MaxSteps -> 3600][[1, 1]];
tableOfPlots= Table[ Plot[Evaluate[u[x,t]/. {solu,t -> dt*i}], {x,0,H},PlotRange -> {-4,14}, Filling -> Axis],{i,0,Floor[1.5*t98/dt],30}];
ListAnimate[ tableOfPlots]
