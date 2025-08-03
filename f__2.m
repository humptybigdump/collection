function y = f(t)
v0 = 20;
 L = 3;
 T = 2 * L/v0;
 c = 400e3;
 m = 1000;
 s0 = 0.01;
 Omega = 2 * pi / T;
 omega = sqrt(c/m);
 
 C1 = - 2 * s0/(omega * T) * (1-cos(omega * T/2))/(sin(omega * T/2));
 C2 = C1;
 S1 = - 2 * s0 / omega / T;
 S2 = - S1;
 
 C = - C1;

 counter = C * cos(omega * t);
 
   for k = 0:1:100
      a  = (C1 * cos(omega * (t - k*T)) + S1 * sin(omega * (t - k*T)) + 2 * s0/T * (t - k*T)) .* ((t <       k*T) - (t <  -T/2 + k*T));
      b  = (C2 * cos(omega * (t - k*T)) + S2 * sin(omega * (t - k*T)) - 2 * s0/T * (t - k*T)) .* ((t < T/2 + k*T) - (t <        k*T));
      counter = counter + a +  b ;
  end

 y = counter;
 