% the script will give the price including VAT of an object whose price was
% given without VAT

% create variable VAT
VAT=0.19;

% ask for the price without VAT
price=input('Please, enter a price without VAT: ');

% below if we want to be more robust vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
% check if this is a float that was given
% we could also check if the number is >= 0
%
% solution 1: we repeat the question until answer is acceptable!
% price ='';
% while ~isfloat(price) 
%     price = input('Please, enter a price without VAT: ');
% end

% solution 2: if answer is unacceptable, we stop the script
%price = input('Please, enter a price without VAT: ');
%if ~isfloat(price)
%    return
%end

% now compute VAT amount
vat_price = price*VAT;

% and then compute object price with VAT
price_with = price*(1+VAT);

% give the result on the screen
% fprintf(1, 'The VAT amount for this initial price is: %.2f.\nSo, the final price with VAT is: %.2f\n', vat_price, price_with);
fprintf(1, 'The VAT amount for this initial price is: %.2f.\nSo, the final price with VAT is: %.2f\n', price*VAT, price*(VAT+1));