%  Matlab Sheet 1                                                       %
%  WS20/21                                                              %
%  Bo Wang                                                              %
%  Qilin Zhang                                                          %

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all
clc
covid = readtable('coviddata.csv');            % load covid data
% select country palestine
country_ind = find(strcmp(covid.COUNTRY_SHORT_NAME, 'France'));
country_data = covid(country_ind,:);           % extract 
As = sortrows(country_data, {'REPORT_DATE'});
%gossnewton method to fit
t = (1:height(As))';                             %time 
Y=As.PEOPLE_POSITIVE_CASES_COUNT;                %real_value
syms e h;
alpha=500;
beta=0.03;
R_s = Y - (e .* exp(h .* t));                     % residual function
J_s = jacobian(R_s, [e h]);
for i=1:20
    J = double(subs(J_s, [e h],[alpha beta])); % jacobin
    R = double(subs(R_s, [e h],[alpha beta]));%resiual value

  A = J'*J;
  B = -J'* R; 
  K = cond(J);     %condition number
  if K < 1000      %condition lager than 1000 is one very bad martrix
     dA=A\B;
  else
       [U,S,V] = svd(J);     %Tsvd to solve bad martrix
        if S(1,1) - S(2,2) > 0             
            S(1,1) = 1/S(1,1);
            S(2,2) = 0;
         else
            S(1,1) = 0;
            S(2,2) = 1/S(2,2);
        end
      J=U*S*V;
      dA= A\B;
  end
 alpha=alpha+dA(1);
 beta=beta+dA(2);
 fprintf('Iteration #%g: alpha = %8.4f, beta = %8.4f\n',i,alpha,beta)
end
 ft=alpha*(exp(beta*t));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% levenberg-marquardt method
options = optimoptions('lsqcurvefit','Algorithm','levenberg-marquardt');
ydata = Y;        
xdata = (1:height(As))';                     % X data-days
fun = @(x,xdata)x(1)*exp(x(2)*xdata);        % model function
x0 = [50,0];                                 % start point (50,0)
lb = [];                                       % lower bound
ub = [];                                       % upper bound

xLM = lsqcurvefit(fun,x0,xdata,ydata,lb,ub,options);%plot LM with rot,GN with grenn,real_value with blue
yLM = xLM(1) * exp(xLM(2)*xdata);
hold on;
plot(t,Y,'mo');
plot(xdata,yLM,'r^');
plot(t,ft,'g*');
ylabel('positive count');                       % y label
xlabel('Days');                                % x label
title({' The relationship between the number of infections over time'});
legend('Real cases','LMfit','GNfit');
hold off;

