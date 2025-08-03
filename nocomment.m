function [Vecout, Iout] = nocomment(Vecin, nwin)

%
[nr, nc] = size(Vecin);
if nc~=1
    if nr~=1
        fprintf(1, 'nocomment: ERROR, Vecin not as expected\n');
        return
    else
        Vecin = Vecin';
    end
end
nr = numel(Vecin);

%
nwin=round(nwin);
if mod(nwin,2)==1
    nwin=nwin+1;
    fprintf(1, 'nocomment: WARNING, nwin is now %d\n', nwin);
end
half = nwin/2;

%
Weight = [1:half+1, half:-1:1];
Weight = Weight/sum(Weight);

%
Vecout = zeros(nr-nwin,1);
Iout = zeros(nr-nwin,1);
for ii=1:nr-nwin
    Vecout(ii) = Weight*Vecin(ii:ii+nwin);
    Iout(ii) = ii + half;
end

%
plot(Vecin,'.-b');
hold on;
plot(Iout, Vecout, 'o-r');
hold off;

% keyboard;
end