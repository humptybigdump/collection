function [layn,dz] = layer_number(z,dvec)
% layn = layer_number(z,strc)
% determine the number of the layer containing position z
%
% input arguments:
% - z:    z position in nm
% - strc: Layer structure
%
% output:
% - layn: Number of layer that contains z
% - dz:   z-z(layn), i.e. the distance to layer inteface in nm

d=0;
dz=z;   % output if z<0
layn=1;
for j1=2:length(dvec)
    if z>=d
        layn=j1;
        dz=z-d;
        d=d+dvec(j1);  % layer inteface position, which is under z
    else
        break
    end
end