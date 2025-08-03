function Zp = G(t,Z,k)
    % Konzentrationen aus Vektor Z ziehen (optional)
    CA=Z(1);
    CB=Z(2);
    CC=Z(3);
    CD=Z(4);

    % Geschwindigkeitsraten der beiden Reaktionen berechnen
    r1=k(1)*CA*CB;
    r2=k(2)*CC*CD;

    % Konzentrationsveränderung berechnen
    CAp=-r1+r2;
    CBp=-r1+r2;
    CCp=r1-r2;
    CDp=r1-r2;

    % Rückgabevektor deklarieren
    Zp=[CAp; CBp; CCp; CDp];
end
