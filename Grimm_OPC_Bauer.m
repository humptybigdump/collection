%Grimm data
% GV=gravimetric factor *100
% Location(1-99)
% BV=battery voltage, Batteriespannung
% MVS=Solenoid valve current, Magnetventilstrom
% S4= barometrischer Sensor
% P   19    9   13   14    3    1  100    0  130   40  153  203    0  110  138   0    200140   1412154
%Vorspanndaten 

% Label=[year, month, day, hour, minute, location, GV, error, BV, MVS...
%        S1, S2, S3, S4, intervall]
clear all   
x=Grimm_ImportFNC3("july20.log"); %Reads data with the import function
Vnames=string(x.Properties.VariableNames); %defines table headers
x(find(~contains(x.P,["P","N"])),:)=[]; %Deletes all unnecessary data rows

y=table2array(x); %transforms data table in an array
s=y(:,1);           %First column
y=str2double(y(:,2:end)); %transform in numeric data matrix, "'"=NaN
%% ' Problem. Somestimes the Grimm log contains "'", which needs correction
idx=isnan(y(:,1));
if sum(idx)>0
y(idx,1:end-1)=y(idx,2:end);
y(:,end)=[];
end
%% Transforms raw data into datetime format
didx=y(s=="P",:); %"P" as an indicator for info row including date, time and others
hd=horzcat(didx(:,1),didx(:,2),didx(:,3),didx(:,4),didx(:,5),(zeros(size(didx,1),1)));
d=datetime(hd);
% d(isnat(d))=[]; % check for not a date
%% Calculate meanvalue for N0-N9 in fast modus of Grimm log. One Value 
% equals in mnx equals 10x6sec.
idxxx=contains(s,"N");
nx=y(idxxx,1:3);

mnx=zeros(size(nx,1),3);
mnx_median=zeros(size(nx,1),3);
for ii=1:10:size(nx,1)-10   %Calculate mean
    mnx(ii,1:3)=mean(nx(ii:ii+9,1:3));
    mnx_median(ii,1:3)=median(nx(ii:ii+9,1:3));
end
mnx(sum(mnx,2)==0,:)=[];
mnx_median(sum(mnx_median,2)==0,:)=[];

d=d(1:size(mnx,1),:); %date vecor must have the same size as mean vector
%%  Select data/time
all_data="yes";
if all_data=="yes"
xstart=d(1);
xend=d(end);
elseif all_data=="no"
            % (year,  month, day,   hour, minute, second)  second=0!!!
xstart=datetime(0,     7,    17,     0,    0,     0);
xend=  datetime(0,     7,    17,     23,    59,     0);
end
%%Custom color scheme
%    Color 1 ;   Color 2  ;   Color 3
c=[31,120,180; 255, 127, 0; 227, 26, 28]./255;
%% Display additional content:
disp_hours="yes"
t=[6 18];  % change numbers for hours
disp_weekdays="yes"
%%
figure('Name','Mean Value','Units','normalized','Position',[0.1 0.1 0.8 0.7])
x1=find(d==xstart);
x2=find(d==xend);
%       date,    value,         color 1 2 3,  style of line,  line width
plot(d(x1:x2),mnx(x1:x2,1),'Color',c(1,1:3),'Linestyle','-','LineWidth',1.5) % plot PM10
hold on
plot(d(x1:x2),mnx(x1:x2,2),'Color',c(2,1:3),'Linestyle','-','LineWidth',1.5) % plot PM2.5
hold on
plot(d(x1:x2),mnx(x1:x2,3),'Color',c(3,1:3),'Linestyle','-','LineWidth',1.5)% plot PM1
hold on
% Optional peakfinder function
% [pksCHEG2,locsCHEG2] = findpeaks(mnx(:,1),'SortStr','descend','NPeaks',...
%  sum(mnx(:,1)>=quantile(mnx(:,1),0.998)),'MinPeakDistance',10, 'MinPeakProminence',40); 
% plot(TCHEG2,CountCHEG2,'b');    
% hold on
% scatter(d(locsCHEG2),pksCHEG2,'sr')
%% Find and plot full hours "t" as xlines. 
hold on
h1=hd(x1:x2,4);
h2=hd(x1:x2,5);
h3=hd(x1:x2,6);

hd2=hd(x1:x2,:);

w=h1==t; %find hours = t
w=sum(w,2);
idxt1=w==0;

idxt2=find(h2~=0); %find minutes = 0
idxt3=find(h3~=0); %find seconds = 0

hd2(idxt1,1:end)=0;
hd2(idxt2,1:end)=0;
hd2(idxt3,1:end)=0;

idxt5=find(hd2(:,2)~=0);
dd=d(x1:x2,:);

if disp_hours=="yes"
for ii=1:numel(idxt5)
    
xline(dd(idxt5(ii)),'-',string(hd2(idxt5(ii),4)));
hold on
end
end

% Add weekdays as text
hd3=hd(x1:x2,:);
idxt6=find(h1~=0);
hd3(idxt6,1:end)=0;
hd3(idxt2,1:end)=0;
hd3(idxt3,1:end)=0;
idxt7=find(hd3(:,2)~=0);
[~,wd]=weekday(dd(idxt7));

if disp_weekdays=="yes"
text(dd(idxt7),ones(numel(idxt7),1).*(max(mnx(x1:x2,1))/3),wd,'Fontsize',15);
end
hold on
%% figure properties
xlabel('Time');
ylabel('Counts / cm^3');
title('Grimm Optical Particle Counter');
set(gca,'Fontsize',15);
l=["PM 10","PM 2.5", "PM 1.0"];
legend(l,'Location','NorthEastOutside')
%%
figure('Name','Median Value','Units','normalized','Position',[0.1 0.1 0.8 0.7])
plot(d(x1:x2,:),mnx_median(x1:x2,1),'Color',c(1,1:3),'Linestyle','-','LineWidth',1.5) % plot PM10
hold on
plot(d(x1:x2,:),mnx_median(x1:x2,2),'Color',c(2,1:3),'Linestyle','-','LineWidth',1.5) % plot PM2.5
hold on
plot(d(x1:x2,:),mnx_median(x1:x2,3),'Color',c(3,1:3),'Linestyle','-','LineWidth',1.5)% plot PM1
hold on

if disp_hours=="yes"
for ii=1:numel(idxt5)
    
xline(dd(idxt5(ii)),'-',string(hd(idxt5(ii),4)));
hold on
end
end

if disp_weekdays=="yes"
text(dd(idxt7),ones(numel(idxt7),1).*(max(mnx(x1:x2,1))/3),wd,'Fontsize',15);
end 

xlabel('Time');
ylabel('Counts / cm^3');
title('Grimm Optical Particle Counter');
set(gca,'Fontsize',15);
l=["PM 10","PM 2.5", "PM 1.0"];
legend(l,'Location','NorthEastOutside')
%%
figure('Name','Smooth Mean Data','Units','normalized','Position',[0.1 0.1 0.8 0.7])

mnx1=smooth(mnx(:,1),0.005,'rlowess');
mnx2=smooth(mnx(:,2),0.005,'rlowess');
mnx3=smooth(mnx(:,3),0.005,'rlowess');

plot(d(x1:x2),mnx1(x1:x2),'Color',c(1,1:3),'Linestyle','-','LineWidth',1.5) % plot PM10
hold on
plot(d(x1:x2),mnx2(x1:x2),'Color',c(2,1:3),'Linestyle','-','LineWidth',1.5) % plot PM2.5
hold on
plot(d(x1:x2),mnx3(x1:x2),'Color',c(3,1:3),'Linestyle','-','LineWidth',1.5)% plot PM1
hold on

if disp_hours=="yes"

for ii=1:numel(idxt5)
    
xline(dd(idxt5(ii)),'-',string(hd2(idxt5(ii),4)));
hold on
end
end

if disp_weekdays=="yes"
text(dd(idxt7),ones(numel(idxt7),1).*20,wd,'Fontsize',15);
end
xlabel('Time');
ylabel('Counts / cm^3')
title('Grimm Optical Particle Counter')
set(gca,'Fontsize',15);
l=["PM 10","PM 2.5", "PM 1.0"];
legend(l,'Location','NorthEastOutside')

%% Correlation
Correlation_mean=corr(normalize(mnx(x1:x2,1:3)),'rows','pairwise')
Correlation_median=corr(normalize(mnx_median(x1:x2,1:3)),'rows','pairwise')