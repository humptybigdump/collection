clear all; close all; clc;

% Verschiedene Reihenentwicklungen für den Einfeldträger unter Dreieckslast

x  = 0:0.02:1;
el = 1;
p0 = 1;
B  = 1;

N  = 2;

% Exakt
p_ex = p0*(1-x/el);

% Antimetrie
p_ant = x*0;
for n = 1:N
    aln   = pi*n/el;
    p_ant = p_ant + 2*p0/pi/n * sin(aln*x);
end

% Symmetrie
p_sym = x*0;
p_sym = p_sym + p0/2;
for n = 1:N
    aln   = pi*(2*n-1)/el;
    p_sym = p_sym + 4*p0/pi^2/(2*n-1)^2 * cos(aln*x);
end

% Plot
figure
subplot(1,2,1)
plot(x,p_ex)
title('Last Antimetrie')
hold on
plot(x,p_ant)
legend('exakt','Antimetrie')
subplot(1,2,2)
plot(x,p_ex)
title('Last Symmetrie')
hold on
plot(x,p_sym)
legend('exakt','Symmetrie')

% Verschiebung Exakt
w_ex = -(p0/120*el)*x.^5 + (p0/24)*x.^4 - (p0*el/18)*x.^3 + (p0*el^3/45)*x ;  
w_ex = w_ex / B;

% Antimetrie
w_ant = x*0;
for n = 1:N
    aln   = pi*n/el;
    w_ant = w_ant + 2*p0*el^4/pi^5/n^5/B * sin(aln*x);
end

% Symmetrie
w_sym = x*0;
for n = 1:N
    aln   = pi*(2*n-1)/el;
    w_sym = w_sym + 4*p0*el^4/pi^6/(2*n-1)^6 * cos(aln*x);
end
w_sym = w_sym + (p0/48)*x.^4 - (p0*el/18)*x.^3 + (p0*el^2/48)*x.^2 + (p0*el^3/45)*x - (p0*el^4/240);
w_sym = w_sym / B;

% Plot Verschiebung
figure
subplot(1,2,1)
plot(x,w_ex)
title('Durchbiegung Antimetrie')
hold on
plot(x,w_ant)
legend('exakt','Antimetrie')
subplot(1,2,2)
plot(x,w_ex)
title('Durchbiegung Symmetrie')
hold on
plot(x,w_sym)
legend('exakt','Symmetrie')