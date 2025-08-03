function plotconstellation(y,F,dly)
%plotconstellation Plot constellation diagram. 
% 
% INPUT:
%   y       : Signal vector/matrix - does not matter due to linear indexing.
%   F       : Oversampling rate of the signal, i.e., no. samples / symbol
%             --> downsample by factor F
%   dly     : Optional additional delay
% 
%
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPO)
% Written by: Jonas Krimmer
% Institute of Photonics and Quantum Electronics
% Last edit 12.05.2020 by Jonas Krimmer (IPQ)
%

if nargin <= 2
    dly = 0;
end

scatter(real(y(1+dly:F:end)),imag(y(1+dly:F:end)), '.')
ax = gca();
axis(gca(), 'tight')

axmax = ceil(max(abs(axis(ax))) + 0.1);
axis(ax, axmax*[-1 1 -1 1])
grid(ax, 'on');
axis(ax, 'square');
box(ax, 'on')
xlabel(ax, 'In-phase')
ylabel(ax, 'Quadrature')

if mod(axmax,2) % isodd
    xticks(ax,-(axmax-1):2:(axmax-1));
    yticks(ax,-(axmax-1):2:(axmax-1));
else % iseven
    xticks(ax,-axmax:2:axmax);
    yticks(ax,-axmax:2:axmax);
end

end