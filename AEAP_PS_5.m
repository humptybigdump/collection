%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   AEAP - Problem set 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath(genpath(pwd))

% Problem 1: Load data
load AEAP_data_5
  
% Problem 2: Estimate gamma in CCAPM with GMM

% write moment condition function
  
% function [mom, grad] = momentCondCCAPM(gamma, data, delta)
% % momentCondCCAPM Provides moment condition in case of CCAPM
% 
% gC = data(:,1);
% MKT = data(:,2);
% 
% M = delta*exp(-gamma*gC);
% mom = M.*MKT;
% 
% grad = mean(mom.*(-gC));
% 
% end

% general settings
format bank
stval = 50;
W = 1;
optset('gmmest','method','HACC_B');
warning('off','optim:fminunc:SwitchingMethod'); % suppresses annoying warning
options = optimset('Display','off');

deltaA = 0.95;          % annual discount factor
deltaQ = deltaA^(1/4);  % quarterly discount factor
 
% use quarterly data
data  = [gNDS_q,MKT_q-RF_q];
[b,~,~,~,~,~,~,~,~,bint] = gmmest(options,data,'momentCondCCAPM',stval,W,deltaQ);
disp('Point estimate and confidence interval:'); disp([b,bint]);

% use "beginning of period timing convention"
% quarterly data
data  = [gNDS_q(2:end),MKT_q(1:end-1)-RF_q(1:end-1)];
[b,~,~,~, mom,~,~,~,~,bint] = gmmest(options,data,'momentCondCCAPM',stval,W,deltaQ);
disp('Point estimate and confidence interval:'); disp([b,bint]);

% use annual data
data = [gNDS_a(2:end),MKT_a(1:end-1)-RF_a(1:end-1)];
data = [gNDS_a(1:end),MKT_a(1:end)-RF_a(1:end)];

[b,~,~,~,~,~,~,~,~,bint] = gmmest(options,data,'momentCondCCAPM',stval,W,deltaA);
disp('Point estimate and confidence interval:'); disp([b,bint]);


% Problem 3: Estimate gamma using Q4/Q4 consumption growth
logC   = cumsum([1;gNDS_q]);
logCQ4 = logC(3:4:end);
gQ4Q4  = logCQ4(2:end) - logCQ4(1:end-1);
plot(gNDS_a); hold on; plot(gQ4Q4);

% use Q4/Q4 data in the GMM estimation of gamma
%data = [gQ4Q4,MKT_a-RF_a];
data = [gQ4Q4(2:end),MKT_a(1:end-1)-RF_a(1:end-1)];
[b,~,~,~,~,~,~,~,~,bint] = gmmest(options,data,'momentCondCCAPM',stval,W,deltaA);
disp('Point estimate and confidence interval:'); disp([b,bint]);

% Problem 4: Use the unfilter rule introduced in Kroencke (2016) 
% quarterly data
fil   = 0.4;
UFC_q = (gNDS_q(2:end) - (1-fil)*gNDS_q(1:end-1))./fil;

data = [UFC_q,MKT_q(1:end-1)-RF_q(1:end-1)];
[b,~,~,~,~,~,~,~,~,bint] = gmmest(options,data,'momentCondCCAPM',stval,W,deltaQ);
disp('Point estimate and confidence interval:'); disp([b,bint]);

% annual data
fil = 0.4;
UFC_a = (gNDS_a(2:end) - (1-fil)*gNDS_a(1:end-1))./fil;

data = [UFC_a,MKT_a(1:end-1)-RF_a(1:end-1)];
[b,~,~,~,~,~,~,~,~,bint] = gmmest(options,data,'momentCondCCAPM',stval,W,deltaA);
disp('Point estimate and confidence interval:'); disp([b,bint]);

% Problem 5: Use ultimate consumption risk by Parker and Julliard (2005)
% We need a new moment condition function because the functional form of the pricing kernel has changed

% function [mom, grad] = momentCondUCR(gamma, data, DeltaH)
% %momentCondUCR Provides moment condition in case of ultimate consumption
% %risk
% 
% gC  = data(:,1);
% MKT = data(:,2);
% RF  = data(:,3);
% delta = DeltaH(1);
% h = DeltaH(2);
% 
% % account for horizon h (time series must be shorter)
% gCPK = sum(lagmatrix(gC,-(0:(h-1))),2);
% RFPK = exp(sum(lagmatrix(log(1+RF),-(1:(h-1))),2)-1);
% erase = isnan(RFPK);
% RFPK(erase) = [];
% gCPK(erase) = [];
% MKT(erase)  = [];
% 
% M = delta^h * exp(-gamma*gCPK) .* RFPK
% mom = M .* MKT;
% 
% grad = mean(mom.*(-gCPK));
% 
% end

% use quarterly data
deltaH = [deltaQ;8];
data   = [gNDS_q(1:end-1),MKT_q(2:end)-RF_q(2:end),RF_q(2:end)];
[b,~,~,~,~,~,~,~,~,bint] = gmmest(options,data,'momentCondUCR',stval,W,deltaH);
disp('Point estimate and confidence interval:'); disp([b,bint]);

 
% use annual data
deltaH = [deltaA;3];
data   = [gNDS_a(1:end-1),MKT_a(2:end)-RF_a(2:end),RF_a(2:end)];
[b,~,~,~,~,~,~,~,~,bint] = gmmest(options,data,'momentCondUCR',stval,W,deltaH);
disp('Point estimate and confidence interval:'); disp([b,bint]);

% Problem 6: Perform cross-sectional regression
annual  = 1;        % 1 -> use annual data
bptc    = 1;        % 1 -> use beginning-of-period-timing convention
factors = [2];  
                    % 1 -> NIPA consumption of NDS, 2 -> unfiltered consumption, 
                    % 3 -> NIPA consumption of durable goods
                    % 4 -> MKT factor, 5 -> SMB, 6 -> HML
                    
% collect data
if annual == 0
    if bptc == 0 % 
        F = [gNDS_q(2:end), UFC_q, gDUR_q(2:end), MKT_q(2:end), SMB_q(2:end), HML_q(2:end)];
    else
        F = [gNDS_q(1:end-1), UFC_q, gDUR_q(1:end-1), MKT_q(2:end), SMB_q(2:end), HML_q(2:end)];
    end
    R = FF5_q(2:end,:)-repmat(RF_q(2:end),1,25);
else
    if bptc == 0
        F = [gNDS_a(2:end), UFC_a, gDUR_a(2:end), MKT_a(2:end), SMB_a(2:end), HML_a(2:end)];
    else
        F = [gNDS_a(1:end-1), UFC_a, gDUR_a(1:end-1), MKT_a(2:end), SMB_a(2:end), HML_a(2:end)];
    end
    R = FF5_a(2:end,:)-repmat(RF_a(2:end),1,25);
end
F = F(:,factors);

[LambdaGMM, t_lambdaGMM, R2adj] = XSReg(R, F, 1, 2, [], 1, 'HACC_B');

