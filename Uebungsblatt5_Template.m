%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Signalverarbeitung in der Geodaesie - SoSe21
%%% Alexandra Heck
%%%
%%% Template zur Uebung 5 
%%%
%%% Bearbeitet von:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all;

%% Definition von dummy-Parametern fuer das Tamplate,
%  diese sind bei der Bearbeitung zu ersetzen.
%  Entsprechend ist "dummy" auch in den Beschriftungen der Abbildungen zu
%  ersetzen.

x_dummy = 0:1:2249-1;
y_dummy = 0:1:1619-1;
DUMMY = meshgrid(x_dummy,y_dummy);
dummy = 100;
d = 4;

%% Einlesen der Daten

DEM = flipud(load('DEM_20140911_shiveluch.txt'));
DEM = DEM(1:end-1,1:end-1);

[N_y,N_x]=size(DEM);

%% Definition der Achsen

% Longitude
x_min = 161.188;
x_max = 161.497;
dx = 1.3739e-04;

x = x_dummy;

% Latitude
y_min = 56.563;
y_max = 56.755;
dy = 1.1859e-04;

y = y_dummy;

%% Umrechnung grad --> km   

%x_spacing_km = 111.3*cos(56.635*2*pi/360)*dx;
%y_spacing_km = 111.3*dy;

%% Fouriertransformation des DEMs

% S = Fouriertransformierte des DEM (mit fft2())
% (fffshift? ifftshift?)
S = DUMMY;

df_x=1/(dummy*dummy);
df_y=1/(dummy*dummy);

fny_x=1/(dummy*dummy);
fny_y=1/(dummy*dummy);

fx= dummy:df_x:dummy;
fy= y_dummy;

fs=15;

figure
imagesc(fx,fy,DUMMY,[-80 0])
xlabel('fx in dummy')
ylabel('fy in dummy')
cb = colorbar('vert');
zlab = get(cb,'ylabel');
set(zlab,'String','Betragsspektrum in dummy')
axis tight
set(gca,'FontSize',fs)

%% Konstruktion der Tiefpassfilter

% alpha = 2
w1x=gausswin(dummy,d);
w1y=gausswin(dummy,d);
[W1x,W1y] = meshgrid(w1x,w1y);

W1 = W1x.*W1y;

% alpha = 3
w2x= gausswin(dummy,d);
w2y= gausswin(dummy,d);
[W2x,W2y] = meshgrid(w2x,w2x);

W2 = W2x.*W2y;

figure;
imagesc(dummy,dummy,W1,[0 1]);
xlabel('Frequenzy f_x in dummy');
ylabel('Frequenzy f_y in dummy');
title('Gauß-Fenster \alpha = 2');
cb = colorbar('vert');
zlab = get(cb,'ylabel');
set(zlab,'String','Amplitude Tiefpassfilter');
set(gca,'FontSize',fs)

figure;
imagesc(dummy,dummy,W2,[0 1]);
xlabel('Frequenzy f_x in dummy');
ylabel('Frequenzy f_y in dummy');
title('Gauß-Fenster \alpha = 3');
cb = colorbar('vert');
zlab = get(cb,'ylabel');
set(zlab,'String','Amplitude Tiefpassfilter','FontSize',fs);
set(gca,'FontSize',fs)

%% Filter anwenden

% Pixelweise Multiplikation
S_filt_1 = DUMMY;
S_filt_2 = DUMMY;

%%
figure
imagesc(dummy,dummy,DUMMY,[-80 0])
xlabel('fx in in dummy')
ylabel('fy in in dummy')
title('Spektrum (gefiltert mit Filter 1)')
cb = colorbar('vert');
zlab = get(cb,'ylabel');
set(zlab,'String','Betragsspektrum in dummy')
axis tight
set(gca,'FontSize',fs)

figure
imagesc(dummy,dummy,DUMMY,[-80 0])
xlabel('fx in dummy')
ylabel('fy in dummy')
title('Spektrum (gefiltert mit Filter 2)')
cb = colorbar('vert');
zlab = get(cb,'ylabel');
set(zlab,'String','Betragsspektrum in dummy')
axis tight
set(gca,'FontSize',fs)

%% Transformation Wellenzahlbereich --> Ortsraum
% (ifftshift nicht vergessen)
% bei ifft2 'symmetric' verwenden

% Gefiltertes DEM
DEM_filt_alpha2 = DUMMY;
DEM_filt_alpha3 = DUMMY;

%% Filter im Ortsraum
% Filter im Wellenzahlbereich --> Uebertragungsfunktion
% Filter im Ortsraum --> Implusantwort
%(ifftshift/fftshift noetig?)

% Gaussfilter
W2_ort = DUMMY;
W2_ort=W2_ort/max(W2_ort(:)); % Auf 1 normieren

figure
imagesc(x_dummy,y_dummy,DUMMY)
xlabel('x in dummy')
ylabel('y in dummy')
title('Filter im Ortsraum')
cb = colorbar('vert');
zlab = get(cb,'ylabel');
set(zlab,'String','Normierte Amplitude in dummy')
axis tight
set(gca,'FontSize',fs)

% Ausschneiden der signifikanten Stellen
W_cut = W2_ort(dummy-4:dummy+4,dummy-4:dummy+4);
[w_y,w_x]=size(W_cut);

figure
imagesc(x_dummy,y_dummy,W_cut)
xlabel('x in dummy')
ylabel('y in dummy')
title('Filter im Ortsraum')
cb = colorbar('vert');
zlab = get(cb,'ylabel');
set(zlab,'String','Normierte Amplitude in dummy')
axis tight
set(gca,'FontSize',fs)

%% Filterung im Ortsraum --> gewichtete Mittelwertbildung

DEM_filt_B = zeros(size(DEM));
for ii=y_dummy(end-6:end)
    for jj=x_dummy(end-6:end)
        DEM_filt_B(ii,jj)=DUMMY(ii,jj);
    end
end




