
function [Cnm,Snm,sCnm,sSnm,GM,R,nmax,time_period_mid,time_period_start,time_period_stop] = ReadCoefficientsICGEM(filename)

fid = fopen(filename);
while 1
      line = fgetl(fid);
      if ~ischar(line)
          break
      elseif isempty(line)
          continue
      end
      str = textscan(line,'%s',1);
      if(strcmp(str{1}, 'time_period_of_data'))
      C = textscan(line,'%s%d%s%d%s%d%s',1);
      time_period_start = C{2};
      time_period_stop  = C{4};
      time_period_mid   = C{6};
      end
      if(strcmp(str{1}, 'radius'))
      C = textscan(line,'%s%f',1);
      R = C{2};
      end
      if(strcmp(str{1}, 'earth_gravity_constant'))
      C = textscan(line,'%s%f',1);
      GM = C{2};
      end
      if(strcmp(str{1}, 'max_degree'))
      C = textscan(line,'%s%f',1);
      nmax = C{2};
      end
      if(strcmp(str{1}, 'end_of_head'))
      break
      end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Load the coefficients
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Coefficients
Cnm = zeros(nmax+1,nmax+1);
Snm = zeros(nmax+1,nmax+1);
%% Accuracy
sCnm = zeros(nmax+1,nmax+1);
sSnm = zeros(nmax+1,nmax+1);

while 1
  line = fgetl(fid);
  if ~ischar(line), break, end
%   if isempty(line), continue, end
  C = textscan(line,'%s%d%d%f%f%f%f');
  Cnm(C{2}+1,C{3}+1)  = C{4};
  Snm(C{2}+1,C{3}+1)  = C{5};
  sCnm(C{2}+1,C{3}+1) = C{6};
  sSnm(C{2}+1,C{3}+1) = C{7};
end

fclose(fid);
