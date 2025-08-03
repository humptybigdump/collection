%% Script to calculate and show the price with VAT of an object from its price without VAT

%% Inputs

VAT = 0.19;
%pwov = 150;

% check if this is a float that was given

% solution 1: we repeat the question until answer is acceptable!
% pwov ='';
% while ~isfloat(pwov) 
%     pwov = input('Please, enter a price without VAT: ');
% end

% solution 2: if answer is unacceptable, we stop the script
pwov = input('Please, enter a price without VAT: ');
if ~isfloat(pwov)
    return
end

%% Calculate and define other variables

pwv = pwov*(1+VAT);

%% Output

disp(['The price with VAT is ', sprintf('%f', pwv)]);