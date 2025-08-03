function D = layer_D_matrix(pol,kz1,kz2,n1,n2,varargin)
% D = layer_D_matrix(pol,kz1,kz2,n1,n2)
% D = layer_D_matrix(pol,kz1,kz2,n1,n2,'no reflection')
%
% Interface transition matrix.
%
% input arguments:
% - pol: polarization (1-TE,2-TM)
% - kz1: z-component of wavevector in medium 1
% - kz2: z-component of wavevector in medium 2
% - n1:  complex refractive index in medium 1
% - n2:  complex refractive index in medium 2

if isempty(varargin)
    D=1/fresnel_t(pol,kz1,kz2,n1,n2)*[1,fresnel_r(pol,kz1,kz2,n1,n2);fresnel_r(pol,kz1,kz2,n1,n2),1];
elseif strcmp(varargin{1},'no reflection')   
    D=1/fresnel_t(pol,kz1,kz2,n1,n2)*[1,0;0,1];
else
    error(['unknown option "',varargin{1},'"']);
end
    