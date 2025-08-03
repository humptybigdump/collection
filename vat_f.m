%% Function to calculate and show the price with VAT of an object from its price without VAT
% The function needs 2 input parameters:
%   - VAT: a real number corresponding to the VAT ratio (ex. 0.19)
%   - pwov: a real number corresponding to the price of the object without
%   VAT.
% The function delivers 1 output:
%   - pwv: a real number corresponding to the price once the VAT was
%   applied.
% NOTE: the function will show on screen the price once the VAT was applied 

function pwv = vat_f(VAT, pwov)
%% Inputs: they are given by the function input parameters VAT and pwov

%VAT = 0.19;
%pwov = 150;

% check if float (real) numbers have been given as input
if ~isfloat(VAT)
    disp('The VAT input parameter should be a real number');
    return
end
if ~isfloat(pwov)
    disp('The pwov input parameter should be a real number');
    return
end

%% Calculate and define other variables
pwv = pwov*(1+VAT);

%% Output

% show result on screen
disp(['The price with VAT is ', sprintf('%f', pwv)]);
% provide result in the pwv variable that is an output parameter of the function