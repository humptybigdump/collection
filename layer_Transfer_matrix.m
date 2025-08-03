function T = layer_Transfer_matrix(pol,strc,kpar,wl,varargin)
% T = layer_Transfer_matrix(pol,strc,kpar,wl)
% T = layer_Transfer_matrix(pol,strc,kpar,wl,'no reflection')
% 
% [a_up;a_down]=T*[b_up;b_down]
%
% with a_up/down the up and downwards propagating wave amplitudes in the
% bottom layer of the stack and b_up/down the same for the top layer of
% strc.
% These coefficients refer to the full field strength amplitudes, not only
% the parallel component as in the Transfer Matrix algorithm employed in 
% the Fermi OLED Project.
%
% If the option 'no reflection' is specified, the reflection coefficients
% are set to 0 in the calculation of the interface transition matrices.
%
% input arguments:
% - pol: 1-TE, 2-TM
% - strc: nx2-matrix containing layer thickness in nm (first column) and
%         complex refractive index n+ik (second column)
% - kpar: in plane wavenumber (nm^-1)
% - wl: vacuum wavelength in nm
 

d=strc(:,1);
n=strc(:,2);
k=2*pi/wl*n;
kz=sqrt(k.^2-kpar^2);
kz(imag(kz)<0)=-kz(imag(kz)<0); % make sure imag(kz) >= 0

nlay=length(d);

T=[1,0;0,1];  % start with unit matrix

for j1=1:nlay-1
    T = T * layer_P_matrix(kz(j1),d(j1)) * layer_D_matrix(pol,kz(j1),kz(j1+1),n(j1),n(j1+1),varargin{:});
end
