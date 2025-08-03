%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   AEAP - Problem set 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load data
load AEAP_data_2
 
% Set some variables
num_port        = [5,5];    % number of portfolios
sorting_crit    = [1,2];    % 1=size, 2=B/M, 3=OP, 4=INV, 5=LIQ
value_weighted  = 0;        % 0=equal weighted returns, 1=value weighted returns
minimum_pps     = 1;        % minimum price per share in dollars (exclude penny stocks from sample)
 
% Here, the code starts
BM = BE./ME;

if sorting_crit(1)==1
    C1 = ME;
elseif sorting_crit(1)==2
    C1 = BM;
elseif sorting_crit(1)==3
    C1 = OP;
elseif sorting_crit(1)==4
    C1 = INV;
elseif sorting_crit(1)==5
    C1 = LIQ; 
end
if sorting_crit(2)==1 
    C2 = ME;
elseif sorting_crit(2)==2
    C2 = BM;
elseif sorting_crit(2)==3
    C2 = OP;
elseif sorting_crit(2)==4
    C2 = INV;
elseif sorting_crit(2)==5
    C2 = LIQ; 
end

T = size(R,1); n = size(R,2);
num_port_total = num_port(1)*num_port(2);

out = nan(T,num_port_total);
num = nan(T,num_port_total);
MKT = nan(T,1);

% Loop over all months
for t = 2:T
    
    % erase missing values
    erase = isnan(C1(t-1,:)) | isnan(C2(t-1,:)) | isnan(ME(t-1,:)) | isnan(PPS(t-1,:)) | PPS(t-1,:)<minimum_pps; % logical vector that is TRUE if a value is missing
    
    C1_t = C1(t-1,~erase); % ~ means NOT, it turns TRUE into FALSE and vice versa
    C2_t = C2(t-1,~erase);
    ME_t = ME(t-1,~erase); 
    PPS_t = PPS(t-1,~erase);
    R_t = R(t,~erase);
  
    % calculate "market" return (can be used later in CAPM tests)
    MKT(t) = (ME_t./sum(ME_t)) * R_t';
    
    for i = 1:num_port(1)
        for j = 1:num_port(2)
            
            % find the right stocks for each portfolio
            idx = C1_t>=quantile(C1_t,(i-1)/num_port(1))...
                & C1_t<quantile(C1_t,i/num_port(1))...
                & C2_t>=quantile(C2_t,(j-1)/num_port(2))...
                & C2_t<quantile(C2_t,j/num_port(2)); % logical vector that selects all stocks in the right quantiles
            
            % calculate returns
            if value_weighted==0
                out(t,(j-1)*num_port(1)+i) = mean(R_t(idx));
            else
                out(t,(j-1)*num_port(1)+i) = sum(ME_t(idx).*R_t(idx))/sum(ME_t(idx));
            end
            num(t,(j-1)*num_port(1)+i) = sum(idx);
        end
    end
end
MKT(isnan(sum(out,2)))=[]; out(isnan(sum(out,2)),:)=[]; 

reshape(mean(out),num_port(1),num_port(2))
                
% Check if there are significant alphas relative to the CAPM:
% Use GMM program from tutorial 3.

% addpath(genpath(pwd))
% 
% options = optimset('Display','off');
% warning('off','optim:fminunc:SwitchingMethod'); % suppresses annoying warning
% optset('gmmest','method','HACC_B');
% 
% data    = [MKT,MKT,out];
% stval   = [zeros(num_port_total,1);ones(num_port_total,1)];
% W       = eye(2*num_port_total);
% 
% [theta,~,~,~,~,~,~,~,~,conf_inter] = gmmest(options, data, 'MomentCondOLS', stval, W);
% 
% reshape(theta(1:num_port_total),num_port(1),num_port(2))
% [theta,conf_inter]
