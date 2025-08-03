%%%%%%%%%%%%%%%%%%%%%%%%%
% Numerische Mathematik %
% WS 20/21              %
% Matlab-Blatt 1        %
% Lea Hansen            %
% Alexandra Schiefer    %
%%%%%%%%%%%%%%%%%%%%%%%%%

clc;
clear;
close all;

%% Daten einlesen
data = readtable('coviddata.csv');
% Format: PEOPLE_POSITIVE_CASES_COUNT; REPORT_DATE; PEOPLE_DEATH_NEW_COUNT;
% COUNTRY_SHORT_NAME; PEOPLE_POSITIVE_NEW_CASES_COUNT; PEOPLE_DEATH_COUNT
country = 'United Kingdom';
ind_country = find(strcmp(data.COUNTRY_SHORT_NAME, country));
data_country = data(ind_country,:);
data_country_sort = sortrows(data_country, {'REPORT_DATE'});
pos_test = table2array(data_country_sort(:,{'PEOPLE_POSITIVE_CASES_COUNT'}));
t = 1:1:height(data_country_sort);

%% a) Gauss-Newton Verfahren
iter = 20;
x0gn =[100000 0.01];
syms alpha beta
f_t = alpha*exp(beta*t');
res = pos_test - f_t;
J = jacobian(res, [alpha, beta]);
for i=1:1:iter
    res_i = double(subs(res, [alpha, beta], x0gn));
    J_i = double(subs(J, [alpha, beta], x0gn));
    k = cond(J_i);
    if k>1000
        [U,S,V] = svd(J_i);
        S(2,2) = 0;
        eps = V*pinv(S)*U'*-res_i;
    else
        eps = (J_i'*J_i)\(-J_i'*res_i);
    end
    x0gn = x0gn + eps';
end

%% b) Levenberg-Marquardt-Methode
options = optimoptions('lsqcurvefit','Algorithm','levenberg-marquardt');
x0lm =[100000 0.01];
fun = @(x,xdata)x(1)*exp(x(2)*xdata);
x = lsqcurvefit(fun,x0lm,t',pos_test);

%% c) Plot der Daten und geschaetzten Funktionen
figure;
plot(t,pos_test','r');
hold on;
plot(t',x0gn(1)*exp(x0gn(2)*t'),'g');
hold on;
plot(t,fun(x,t')','b');
legend('Daten','Gauss-Newton','Levenberg-Marquardt');
title(['Coronafaelle in ', convertCharsToStrings(country)]);
xlabel(['Tage seit ', datestr(table2array(data_country_sort(1,2)))]);
ylabel('Anzahl Infizierter');