function [price_vat, vat_amount] = vat_221115(price_ini, VAT)

%  = 0.19;

vat_amount=price_ini*VAT;
price_vat=price_ini*(1+VAT);

fprintf(1, 'Price with VAT is: %.2f, (VAT amount is: %.2f)\n', ...
    [price_vat', vat_amount']');

end