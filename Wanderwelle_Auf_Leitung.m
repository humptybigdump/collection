Amplitude = 1;                  % Amplitude des Rechtecksignals
Leitungslaenge = 10;            % Länge der Leitung
Abschlusswiderstand = 50;        % Abschlusswiderstand
Kurvenform = 'sinus';         % Art der Kurve (sinus, sprung, dreieck)
Leitung2 = 1;                   % Zweite Leitung angeschlossen (0 = nein, 1 = ja)
schrittgroesse = 0.5;           % Schrittgröße auf der Leitung


% Berechnung der Position
Position = [0:schrittgroesse:Leitungslaenge Leitungslaenge:-schrittgroesse:0]; % Gesamte Positionsliste erstellen
 

% Erstellen der Animation
animation_figure = figure;
Leitungswiderstand = 50;
hold on;

 

i = 0;
for t = 1:length(Position)-1
    ampl1 = Amplitude;
    ampl=ampl1;
    ampl2=0;
    
    % Anpassung der Achsenbeschriftungen
    xlim([0 Leitungslaenge]);
    ylim([-Amplitude*2-0.5 Amplitude*2+0.5]);
    xlabel('Distanz in m');
    ylabel('Spannung in V');
    title('Reflexion');

   
    % Überprüfen, ob die Position abnimmt

    if Position(t+1) < Position(t) || Position(t) == Leitungslaenge
        R = (Abschlusswiderstand - Leitungswiderstand) / (Leitungswiderstand + Abschlusswiderstand);
        ampl2 = ampl1 * R; % Amplitude entsprechend anpassen
        if Leitung2 == 1
            ampl = ampl2 + Amplitude;
        end
    end

    % Eingangssignal erzeugen
    switch Kurvenform
        case 'sinus'
            % Sinuswelle erzeugen         
            x = linspace(0, Position(t), 100);
            y = ampl * sin(x-Position(t));
            if Position(t+1) < Position(t) || Position(t) == Leitungslaenge %reflexion

                p1  = plot(linspace(0, Position(t), 100), ampl1*sin(linspace(0, Position(t), 100)-t*0.5), 'r','LineWidth', 1.5, 'DisplayName','Gesamt'); % Sprungpunkt zeichnen
                plot(linspace(Position(t), Leitungslaenge, 100), ampl*sin(linspace(Position(t), Leitungslaenge, 100)-t*0.5), 'r', 'LineWidth', 1.5); % Sprunglinie zeichnen
                plot([Position(t) Position(t)], [ampl*sin(Position(t)-t*0.5) ampl1*sin(Position(t)-t*0.5)], 'r','LineWidth', 1.5); % Sprungpunkt zeichnen
                p2 = plot(linspace(0, t*0.5, 100), ampl1*sin(linspace(0, t*0.5, 100)-t*0.5), 'k--', 'LineWidth', 1.5,'DisplayName','Einlaufend'); % Sinuswelle zeichnen
                p3 = plot([0 Position(t)], [0 0], 'g:', 'LineWidth', 1.5, 'DisplayName','Rücklaufend'); % 0-Pos
                plot(linspace(Position(t), Leitungslaenge, 100), ampl2*sin(linspace(Position(t), Leitungslaenge, 100)-t*0.5), 'g:', 'LineWidth', 1.5); % Sprunglinie zeichnen
                plot([Position(t) Position(t)], [ampl2*sin(Position(t)-t*0.5) 0], 'g:','LineWidth', 1.5); % Sprungpunkt zeichnen

            else

                p1 = plot(x, y, 'r', 'LineWidth', 1.5,  'DisplayName','Gesamt'); % Sinuswelle zeichnen
                plot([Position(t) Leitungslaenge], [0 0], 'r', 'LineWidth', 1.5); % X-Achse zeichnen
                plot(x, y, 'k--', 'LineWidth', 1.5); % Sinuswelle zeichnen
                p2 = plot([Position(t) Leitungslaenge], [0 0], 'k--', 'LineWidth', 1.5,'DisplayName','Einlaufend'); % X-Achse zeichnen
                p3 = plot([0 Position(t)], [0 0], 'g:','LineWidth', 1.5, 'DisplayName','Rücklaufend'); % Sprungpunkt zeichnen

            end

        case 'sprung'

            % Sprungfunktion erzeugen
            if Position(t+1) < Position(t) || Position(t) == Leitungslaenge

                p1 = plot([0 Position(t)], [ampl1 ampl1], 'r', 'LineWidth', 1.5, 'DisplayName','Gesamt'); % Sprunglinie zeichnen
                plot([Position(t) Leitungslaenge], [ampl ampl], 'r', 'LineWidth', 1.5); % Sprunglinie zeichnen
                plot([Position(t) Position(t)], [ampl1 ampl], 'r','LineWidth', 1.5); % Sprungpunkt zeichnen
                p2 = plot([0 Leitungslaenge], [ampl1 ampl1], 'k--', 'LineWidth', 1.5, 'DisplayName','Einlaufend'); % Y-Achse zeichnen
                p3 = plot([0 Position(t)], [0 0], 'g:', 'LineWidth', 1.5, 'DisplayName','Rücklaufend'); % Sprunglinie zeichnen
                plot([Position(t) Leitungslaenge], [ampl2 ampl2], 'g:', 'LineWidth', 1.5); % Sprunglinie zeichnen
                plot([Position(t) Position(t)], [0 ampl2], 'g:','LineWidth', 1.5); % Sprungpunkt zeichnen

            else

                plot([Position(t) Leitungslaenge], [0 0], 'r', 'LineWidth', 1.5); % Sprunglinie zeichnen
                plot([0 Position(t)], [ampl ampl], 'r', 'LineWidth', 1.5); % Y-Achse zeichnen
                p1 = plot([Position(t) Position(t)], [0 ampl], 'r','LineWidth', 1.5, 'DisplayName','Gesamt'); % Sprungpunkt zeichnen
                plot([Position(t) Leitungslaenge], [0 0], 'k--', 'LineWidth', 1.5); % Sprunglinie zeichnen
                plot([0 Position(t)], [ampl1 ampl1], 'k--', 'LineWidth', 1.5); % Y-Achse zeichnen
                p2 = plot([Position(t) Position(t)], [0 ampl1], 'k--','LineWidth', 1.5, 'DisplayName','Einlaufend'); % Sprungpunkt zeichnen
                p3 = plot([0 Position(t)], [0 0], 'g:','LineWidth', 1.5, 'DisplayName','Rücklaufend'); % Sprungpunkt zeichnen
            end

    end   

    if Kurvenform ~= "dreieck"
    %Hinzufügen der Legende    
    legend([p1 p2 p3])
    
    end
    % Verzögerung für die Animation
    pause(0.1);
    
    % Löschen der vorherigen Plots
    if t < length(Position)-1
        cla;
    end
end