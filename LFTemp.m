function TLF=LFTemp(Tsat)
% Leidenfrost temperature [°C]
% T. Schulenberg & M. Raqué, Int. J. Heat and Mass Transfer 79 (2014) 233–240
%
% input
%  Tsat= Saturation temperature [°C]
CLF=[50.4123542,1.78204027,-7.37277970E-3,1.16708194E-4,-7.76350475E-7,...
     2.23000847E-9,-2.39206824E-12];
TLF=CLF(1)+CLF(2)*Tsat+CLF(3)*Tsat^2+CLF(4)*Tsat^3+CLF(5)*Tsat^4+...
     CLF(6)*Tsat^5+CLF(7)*Tsat^6;