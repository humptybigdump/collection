function PT3_Model_Error = PT3_Model_Error( theta )
  
    global u;               % gemessener Eingang (arbeitspunktbereinigt)
    global y;               % gemessener Ausgang (arbeitspunktbereinigt)
    global t;               % Messzeitpunkte
    
    K = theta(1);           % Verstärkung 
    b0 = theta(2);          % Verstärkung b0
    T1 = theta(3);          % Zeitkonstante 1
    T2 = theta(4);          % Zeitkonstante 2   
    T3 = theta(5);          % Zeitkonstante 3
    
    x_0 = [0; 0; 0];        % Anfangswerte auf 0 geschätzt
    
    
    % Bestimmung des Zustandsraummodells ausgehend von der DGL und den Parametern
    A = [0 1 0; 0 0 1; -1/T3 -T1/T3 -T2/T3];
    B = [0 0 1]';
    C = [K/T3 b0*K/T3 0];
    D = 0;
    system = ss(A,B,C,D);
    
    % Simulation und Fehlerberechnung
    y_sim = lsim(system, u, t, x_0); 
    e_N = y - y_sim;
    PT3_Model_Error = e_N'*e_N;
end

