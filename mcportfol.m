%Monte Carlo portfolio program;

%initialization of counters, parameters and weights;
nruns = 100; popsize = 8; 
beta = 2;
mu = [8 12 15]';
sigma = [6  -5   4;
        -5  17 -11;
         4 -11  24];
const = 0.1;
pwm = (1/3) * ones(3,popsize);


for k = 1:nruns;
    % generation of vectors of returns, variance cost and criterion function
    pret = pwm' * mu;
    for j = 1:popsize;
        pvar(j) = 0.5 * beta * pwm(:,j)' * sigma * pwm(:,j);
    end
    pcrit = pret - pvar';

    % selection of the best portfolio;
    [top topi] = max(pcrit);
    wnew = pwm(:,topi);

    % store the best portfolio and the optimal criterion value for each run
    wbest(:,k) = wnew;
    pcritvec(:,k) = top;

    % random generation of popsize minus one new porfolios;
    for i = 1:popsize-1;
        w1 = wnew(1) + rand * const;
        w2=  wnew(2) + rand * const;
        w3 = wnew(3) + rand * const;
        temp = w1 + w2 + w3;
        w1 = w1/temp;
        w2 = w2/temp;
        w3 = w3/temp;
        pwnew(:,i) = [w1;w2;w3];
    end

    % put the best portfolio for the run in the last column of the matrix
    pwnew(:,popsize) = wnew ;
    pwm = pwnew;
end

%print optimal weights and optimal crierion value
wnew
top
pcrit
%print and graph optimal weights and criterion value
%wbest
xaxis = [1:1:nruns]';
plot(xaxis,wbest(:,:));
%plot(xaxis,pcritvec(:,:));