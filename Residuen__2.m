function [ Error] = Residuen( U_Bat_Model, U_Bat_Mess )

%Berechnung des Spannungshub auf den der absolute Fehler normiert wird
U_norm=U_Bat_Mess(end,1)-U_Bat_Mess(1,1);

%Berechnung der Residuen
Error=((U_Bat_Model-U_Bat_Mess)./U_norm).*100; 


end

