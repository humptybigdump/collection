function P = layer_P_matrix(kz,d)
% P = layer_P_matrix(kz,d)
%
% Layer propagation matrix
%
% input arguments:
% kz: z-component of wavevector in the layer (nm^-1)
% d: layer thickness (in nm)

P=[exp(-1i*kz*d),0;0,exp(1i*kz*d)];

