
function G_eval = G(n, T, p, Poly_Koeff, R, p_ref)

    % Summe von G initialisiern
    G_eval = 0;
    % Summenterme f�r jeden Stoff auswerten
    for i=1 : length(n)
        G_eval = G_eval + n(i)*(polyval(Poly_Koeff(:,i),T) + ... 
            R*T*log(n(i)*p/p_ref/sum(n)));
    end

end

