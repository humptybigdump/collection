%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   AEAP - Problem set 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath(genpath(pwd))

% Problem 1: Load data
load AEAP_data_4
 
% Problem 2: Check if cay predicts returns

% Loop over alternative return horizons
R2vec = nan(100,1);
for h=1:100
    return_horizon = h;
    ret = sum(lagmatrix(log(1+MKT),-(1:return_horizon)),2);
    ret = ret(1:end-return_horizon);
    cay_lag = cay(1:end-return_horizon);

    %ret = exp(ret)-1;

    % Run OLS regression with our GMM estimator from problem set 1 (alternatively, use "regress")
    optset('gmmest','method','HACC_B');
    warning('off','optim:fminunc:SwitchingMethod'); % suppresses annoying warning
    options = optimset('Display','off');
    stval   = zeros(2,1);
    W       = eye(2);
    data    = [cay_lag,cay_lag,ret];
    [b,~,~,~,final_moments,~,~,~,b_SE] = gmmest(options,data,'MomentCondOLS',stval,W);
    R2 = 1-var(final_moments(:,1))/var(ret);
    disp('Point estimates and t statistics:');
    disp([b,b./b_SE]);
    disp('R2 (GMM):');
    disp(R2);
    R2vec(h) = R2;
end

plot(R2vec);

% Consider best horizon again
return_horizon = 12;
ret = sum(lagmatrix(log(1+MKT),-(1:return_horizon)),2);
ret = ret(1:end-return_horizon);
cay_lag = cay(1:end-return_horizon);

%ret = exp(ret)-1;

% Run OLS regression with our GMM estimator from problem set 1 (alternatively, we can use "regress")
data    = [cay_lag,cay_lag,ret];
[b,~,~,~,final_moments,~,~,~,b_SE] = gmmest(options,data,'MomentCondOLS',stval,W);
R2 = 1-var(final_moments(:,1))/var(ret);
disp('Point estimates and t statistics:');
disp([b,b./b_SE]);
disp('R2 (GMM):');
disp(R2);

plot(dates(1:end-return_horizon),ret); 
hold on
plot(dates(1:end-return_horizon),[ones(size(cay_lag)),cay_lag]*b); 
hold off


% Problem 3: Perform cross-sectional regression
% Select test assets and factors
test_assets = [1];  % 1 --> 25 size/book-to-market sorted portfolios
                    % 2 --> 25 size/investment sorted portfolios
                    % 3 --> 25 size/operating profitability sorted portfolios
                    % 4 --> 17 industry sorted portfolios

factors = [1,7,8,9,10];   
                    % 1 --> MKT, 2 --> SMB, 3 --> HML, 4 --> RMW, 5 --> CMA, 6 --> MOM
                    % 7 --> labor income growth, 8 --> lagged cay, 
                    % 9 --> lagged cay x MKT,   10 --> lagged cay x labor income growth

% calculate labor income growth
g_y = exp(y(2:end) - y(1:end-1))-1; % labor income is in logs. g_y starts in second quarter of the sample

% collect data
F = [MKT(2:end), SMB(2:end), HML(2:end), RMW(2:end), CMA(2:end), MOM(2:end),...
    g_y, cay(1:end-1), cay(1:end-1).*MKT(2:end), cay(1:end-1).*g_y];
F = F(:,factors);

R = [];
if ismember(1,test_assets); R = [R,Size_BM_5x5]; end
if ismember(2,test_assets); R = [R,Size_Inv_5x5]; end
if ismember(3,test_assets); R = [R,Size_OP_5x5]; end
if ismember(4,test_assets); R = [R,Industry_17]; end

R = R - repmat(RF,1,size(R,2)); % we perform time series regressions with excess returns

[LambdaGMM, t_lambdaGMM, R2adj] = XSReg(R(2:end,:), F, 1, 2, [], 1, 'HACC_B');
