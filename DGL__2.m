function Zp = DGL(t,Z, k01, k02, Ea1, Ea2, dH1, dH2, R, U, A, CK, ...
    cp, mP, VP, To0, alpha)

% Zuerst alle einzelnen Zustandsgr��en aus dem
% Zustandsvektor Z herausnehmen (optional):
CA = Z(1);
CB = Z(2);
CC = Z(3);
TR = Z(4);
TP = Z(5);

% Hilfsgr��en berechnen
k1 = k01*exp(-Ea1/(R*TP));  % m�/s
k2 = k02*exp(-Ea2/(R*TP));  % m�/s
To = To0+alpha*t;           % K
q1 = -k1*CA*dH1;            % W/m�
q2 = -k2*CB*dH2;            % W/m�

% Die �nderungsraten der Zustandsgr��en aus
% je einer DGL ausrechnen:

CAp = -k1*CA;
CBp = k1*CA-k2*CB;
CCp = k2*CB;

TRp = U*A*(To-TR)/CK;
TPp = (U*A*(To-TP)+(q1+q2)*VP)/(CK+cp*mP);

% Zuletzt R�ckgabevektor Zp schreiben:
Zp=[ CAp
     CBp
     CCp
     TRp
     TPp ];
end