%% Numerische Mathematik
%  Matlab Uebung 1
%  Kate Clintworth, Maike Zoller, Niklas Pfeffer
%
%%%%%%%%%%%%%%%%%

clc
clear all
close
%% Aufgabe 1
%Setup the Import Options
opts = delimitedTextImportOptions("NumVariables", 6);

% Specify range and delimiter
opts.DataLines = [2, Inf];
opts.Delimiter = ",";

% Specify column names and types
opts.VariableNames = ["PEOPLE_POSITIVE_CASES_COUNT", "REPORT_DATE", "PEOPLE_DEATH_NEW_COUNT", "COUNTRY_SHORT_NAME", "PEOPLE_POSITIVE_NEW_CASES_COUNT", "PEOPLE_DEATH_COUNT"];
opts.VariableTypes = ["double", "datetime", "double", "string", "double", "double"];
opts = setvaropts(opts, 2, "InputFormat", "yyyy-MM-dd");
opts = setvaropts(opts, 4, "EmptyFieldRule", "auto");
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Import the data

coviddata = readtable("C:\Users\Nikla\Documents\KIT\Master\Semester 2\Numerische Mathematik\Übung\Matlab Uebung\coviddata.csv", opts);


%% Clear temporary variables
clear opts


%% Aufgabe 2
% a)
% select data of one country
country_data = coviddata(strcmp(coviddata.COUNTRY_SHORT_NAME, "France"),:);

% Daten nach Datum aufsteigend sortieren
country_data = sortrows(country_data, 2);

% Fälle extrahieren
y = country_data.PEOPLE_POSITIVE_CASES_COUNT;

% Anzahl der Tage
t_1 = (0:1:309)';

syms alpha beta

% Funktion
f_t = alpha*exp(beta*t_1);

% Startwerte
x_0 = [5000
       0.01];

% Residuen
r = y - f_t;

% Jacobianmatrix
jacob = jacobian(r, [alpha, beta]);

for i=1:20
    % In Residuen Alpha und Beta einsetzen
    r_1 = subs(r, alpha, x_0(1));
    r_1 = double(subs(r_1, beta, x_0(2)));
    
    % Jacobian Matrix
    jacob_1 = subs(jacob, alpha, x_0(1));
    jacob_1 = double(subs(jacob_1, beta, x_0(2)));
    
    % Konditionszahl
    konditionszahl = cond(jacob_1)
    
    % Falls Konditionszahl > 1000: Truncated Singular Value Decomposition
    if konditionszahl > 1000
       [U, S, V] = svd(jacob_1);
       S(2,2) = 0
       jacob_1 = U*S*V
    end
    
    % epsilon (verbesserungen)
    epsilon = (jacob_1'*jacob_1)\(-jacob_1'*r_1);
    
    % neues alpha und beta
    x_0 = x_0 + epsilon;
end


%% b)

options = optimoptions('lsqcurvefit','Algorithm','levenberg-marquardt')

fun = @(x,t_1)x(1)*exp(x(2)*t_1);

% Startwerte
x0 = [30000,0.05];

% Levenberg-Marquardt
x = lsqcurvefit(fun,x0,t_1,y)


%% c) Plot

f_t_mitwert = subs(f_t, alpha, x_0(1));
f_t_mitwert = double(subs(f_t_mitwert, beta, x_0(2)));

y_lm = fun(x,t_1)

plot(country_data.REPORT_DATE, y)
hold on
plot(country_data.REPORT_DATE, f_t_mitwert)
plot(country_data.REPORT_DATE, y_lm)
hold off
title('Gemeldete Corona Fälle in Deutschland');
legend('Daten','Gauss-Newton','Levenberg-Marquard')
xlabel('Datum')
ylabel('Anzahl der gemeldeten Fälle')
% xlim([-0.03 0.03])
% ylim([-1*10^5 1*10^5])
