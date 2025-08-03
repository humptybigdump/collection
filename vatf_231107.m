%% This is the vat function:
% to calculate and show the VAT amount and the VAT-included price of an
% object from its price without VAT (VAT = 19%)
% WARNING:
% - Printing results only works if price is a row vector
%
%% function call
function [vat_a, price_t] = vatf_231107(price)

%% variable assignment
% we ask for the free of tax price
% price = input("Please give the free of tax price: ");
% define the VAT percentage
VAT=0.19;

%% do the calculation
% calculate the VAT amount
vat_a = VAT*price;
% calculate the price including the taxes
price_t = price*(1+VAT);
% price_t = price + vat_a; % this could replace the previous command line

%% show the results on the screen
% possibility 1
% disp(['The VAT amount is ', num2str(vat_a),...
%     ' and the final price is ', num2str(price_t)]);
% possibility 2
fprintf(1, 'The VAT amount is %.2f and the final price is %.2f\n',...
    [vat_a; price_t]);
    
% finished