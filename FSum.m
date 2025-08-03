% FQSum.m  (Fehlerquadratsumme)

function s = FSum(k, t_exp, c_A_exp, Z0, tspan)

    % Mithilfe des ode-Solvers Konzentrationsverlauf für aktuelle k's
    % berechnen
    [t,Z]=ode15s(@(t,Z)G(t,Z,k),tspan,Z0);
    
    % c_A(t) aus Z kopieren 
    c_A_mod = Z(:,1); 
    
    % Zur Animation, bzw. Kontrolle: Messpunkte und simulierte Kurve plotten:
    plot(t_exp,c_A_exp,'ro')
    hold on
    plot(t,c_A_mod,'b-')
    xlabel('t [h]')
    ylabel('c_{A} [kmol / m³]')
    hold off
    
    % Werte für c_A(t) zu den gemessen Zeitpunkten interpolieren
    c_A_fit = interp1(t,c_A_mod,t_exp,'PCHIP'); 

    % Vektor der Fehler berechnen
    s  = c_A_fit - c_A_exp;
    sq = sum((c_A_fit - c_A_exp).^2);
    
    % Zur Animation k und Fehlerquadrat in den Plot schreiben:
    text(100,5.5,['k_{1}= ', num2str( k(1) ), ' kmol/(m^{3}*h)'])
    text(100,5.0,['k_{2}= ', num2str( k(2) ), ' kmol/(m^{3}*h)'])
    text(100,4.5,['sq = ', num2str( sq   )])
    
    drawnow
 
end
