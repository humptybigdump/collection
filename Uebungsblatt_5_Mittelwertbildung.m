%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Signalverarbeitung in der Geodaesie - SoSe21
%%% Alexandra Heck
%%%
%%% Uebung 5 - Mittelwertbildung
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

%% Gleitende Mittelwertbildung
% in 1D ohne Gewichtung: mittelwert_i = sum(x_i)/Anzahl_x_i;
% in 1D mit Gewichtung: mittelwert_i = sum(w_i*x_i)/sum(w_i);

DEM_filt = zeros(size(DEM));
for ii=3:N_y-2
    for jj=3:N_x-2
        DEM_filt(ii,jj)=sum(sum(DEM(ii-2:ii+2,jj-2:jj+2)))/25;
    end
end

%%

figure
imagesc(DEM_filt,[500 3200])
set(gca,'ydir','normal')
colorbar

figure
imagesc(DEM-DEM_filt,[-10 10])
set(gca,'ydir','normal')
colorbar