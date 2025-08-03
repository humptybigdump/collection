function out=fourierr(t)
 v0 = 20;
 L = 3;
 T = 2 * L/v0;
 c = 400e3;
 m = 1000;
 s0 = 0.01;
 
 Omega = 2 * pi / T;
 omega = sqrt(c/m);
 
 summe = -0.5 * s0 * (1 - cos(omega * t));
 
  for k = 1:5
      
      bk = c/m * 4 * s0 / pi^2 /(2*k - 1)^2 / (c/m - Omega^2 * (2*k - 1)^2);
      
      summe = summe + bk * (cos((2 * k -1) * Omega * t) - cos(omega * t));
  end
     
  out = summe;