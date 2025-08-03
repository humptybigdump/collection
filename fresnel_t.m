function t = fresnel_t(pol,kz1,kz2,n1,n2)
% t = fresnel_t(pol,kz1,kz2,n1,n2)
%
% Return fresnel amplitude transmission coefficient.
%
% input arguments:
% - pol: polarization (1-TE,2-TM)
% - kz1: z-component of wavevector in medium 1 (incoming wave medium)
% - kz2: z-component of wavevector in medium 2
% - n1:  complex refractive index in medium 1
% - n2:  complex refractive index in medium 2

if pol==1
    t = 2*kz1./(kz1+kz2);
else
    t = 2*n1.*n2.*kz1./(n2.^2.*kz1+n1.^2.*kz2);
end