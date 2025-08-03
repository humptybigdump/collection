function Ps = serawgn(EsNo, scheme, M)
%%serawgn Theoretical symbol error rates for different modulation schemes
% 
% INPUT:
%   EsNo    : SNR per symbol.
%   scheme  : Modulation scheme
%   M       : alphabet size, i.e. number of modulation symbols
% 
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPO)
% Written by: Jonas Krimmer
% Institute of Photonics and Quantum Electronics
% Last edit 11.05.2020 by Jonas Krimmer
%

k = log2(M);
EbNo = EsNo/k;

switch scheme
    case "psk"
        switch M
            case 2
                Ps = qfunc(sqrt(2*EbNo));
            case 4
                qarg = sqrt(2*EbNo);
                Ps = 2*qfunc(qarg) .* (1 - 1/2*qfunc(qarg));
            otherwise
                Ps = 1/pi*integral(@(theta) exp(-EsNo * sin(pi/M)^2 / sin(theta)^2), 0, pi*(1 - 1/M), 'ArrayValued', 1);
%                 error("Only BPSK (2PSK) and 4PSK supported.")
        end
        
    case "qam"
        if mod(k,2) % non-square qam
            I = 2^((k-1)/2);
            J = 2^((k+1)/2);
            qarg = sqrt(6*log2(I*J)*EbNo/(I^2 + J^2 - 2));
            Ps = (4*I*J - 2*I - 2*J)/M * qfunc(qarg) - 4/M * (1 + I*J - I - J) * qfunc(qarg).^2;

        else % square qam
            qarg = sqrt(3*k*EbNo/(M-1));
            Ps = 4*(1-1/sqrt(M)) * qfunc(qarg) - 4*(1-1/sqrt(M))^2 * qfunc(qarg).^2;
        end
        
    case "pam"
        Ps = 2*(1-1/M) * qfunc(sqrt(6*k*EbNo/(M^2 - 1)));
        
    otherwise
        error("No supported modulation scheme given.")
end

end



function Q = qfunc(x)
    Q = 0.5*erfc(x/sqrt(2));
end

