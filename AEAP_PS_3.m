%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   AEAP - Problem set 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath(genpath(pwd))

% Problem 1: Load data
load AEAP_data_3


% Problem 2: Run Fama/MacBeth regression

% Select test assets and factors
test_assets = [1];  % 1 --> 25 size/book-to-market sorted portfolios
                    % 2 --> 25 size/investment sorted portfolios
                    % 3 --> 25 size/operating profitability sorted portfolios
                    % 4 --> 17 industry sorted portfolios

factors = [1,2,3];  % 1 --> MKT, 2 --> SMB, 3 --> HML, 4 --> RMW, 5 --> CMA, 6 --> MOM

const_in_xsec =0;  % set to 1 to have a constant in the cross-sectional regression

% collect data
F = [MKT, SMB, HML, RMW, CMA, MOM];
F = F(:,factors);
F = [ones(size(MKT)),F]; % perform time series regression with intercept

R = [];
if ismember(1,test_assets); R = [R,Size_BM_5x5]; end
if ismember(2,test_assets); R = [R,Size_Inv_5x5]; end
if ismember(3,test_assets); R = [R,Size_OP_5x5]; end
if ismember(4,test_assets); R = [R,Industry_17]; end

T = size(R,1); I = size(R,2); N = size(factors,2);

R = R - repmat(RF,1,I); % we want to perform time series regressions with excess returns

% 1) perform a time series regression for each test asset for estimate betas
AlphaBeta =  F\R; % run OLS regressions

% 2) perform a cross-sectional regression in every period
Beta = AlphaBeta(2:(N+1),:); % throw away the alphas
if const_in_xsec == 0
    Lambda_t = Beta'\R';
else
    Lambda_t = [ones(I,1),Beta']\R';
end
Lambda = mean(Lambda_t,2);
 
% Fama/MacBeth standard errors
sigma_lambda = std(Lambda_t,0,2)/sqrt(T);
t_lambda = Lambda./sigma_lambda;

disp('Standard Fama MacBeth:');
disp([Lambda';t_lambda']);

% Problem 3

[LambdaGMM, t_lambdaGMM, R2adj] = XSReg(R, F(:,2:end), const_in_xsec, 0, [], 1, 'HACC_B');

disp('GMM:')
disp([LambdaGMM';t_lambdaGMM']);

R2adj

%Problem 4

% plot model-implied returns vs average realized returns
if const_in_xsec == 0
    mi_returns = Beta'*Lambda;
else
    mi_returns = [ones(I,1),Beta']*Lambda;
end
av_returns = mean(R)';
figure('Name','Realized versus Predicted Returns','PaperType','a4letter','PaperOrientation','Portrait','Position',[100 100 750 800]);
min_val=min([mi_returns;av_returns])-0.1*abs(1-min([mi_returns;av_returns]));
max_val=max([mi_returns;av_returns])+0.1*abs(1-max([mi_returns;av_returns]));
plot([min_val, max_val],[min_val, max_val],'k','LineWidth',2); hold on;
plot(mi_returns,av_returns,'.','MarkerEdgeColor','r','MarkerFaceColor','r');
text(mi_returns+0.001, av_returns+0.001, cellstr(num2str((1:I)')));
daspect([10,10,10]);
xlabel('Predicted Return'), ylabel('Realized Return');
