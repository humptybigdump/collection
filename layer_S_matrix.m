function S = layer_S_matrix(pol,strc,kpar,wl,varargin)
% S = Scatter_matrix(pol,strc,alpha0,wl)
% S = Scatter_matrix(pol,strc,alpha0,wl,'no reflection')
%
% Return the S matrix of layer system, [a_n;b_0]=S(0,n)*[a_0;b_n]
%
% input arguments:
% pol: polarization, (1-TE,2-TM)
% strc: layer system array
% kpar: complex parallel wavenumber (nm^-1)
% wl: vacuum wavelength (in nm)
%
% If the option 'no reflection' is chosen, the reflection coefficient is
% set to zero in the computation of the transition matrices.

d=strc(:,1);
n=strc(:,2);
k=2*pi/wl*n;
kz=sqrt(k.^2-kpar^2);
kz(imag(kz)<0)=-kz(imag(kz)<0); % make sure imag(kz) >= 0

nlay=length(d);

S=[1,0;0,1];  % start with unit matrix

for j1=1:1:nlay-1
    
    T = layer_P_matrix(kz(j1),d(j1))*layer_D_matrix(pol,kz(j1),kz(j1+1),n(j1),n(j1+1),varargin{:}); % transfer matrix from layer j1 to layer j1+1
    
    S11 = S(1,1)/(T(1,1)-S(1,2)*T(2,1));
    S12 = (S(1,2)*T(2,2)-T(1,2))/(T(1,1)-S(1,2)*T(2,1));
    S21 = S(2,2)*T(2,1)*S11+S(2,1);
    S22 = S(2,2)*T(2,1)*S12+S(2,2)*T(2,2);
    
    S=[S11,S12;S21,S22];
end




