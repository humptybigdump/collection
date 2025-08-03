function Ch02_Two_Path_Multipath
    zwei_pfad_kanal_interaktiv_main
    return

    h1 = 0.52
    h2 = 0.48
    figure
    fdt = [0:0.01:2]
    semilogy(fdt, two_path_magnitude(h1, h2, fdt), "b-")
    hold on
    h3 = 0.8
    h4 = 0.2
    semilogy(fdt, two_path_magnitude(h3, h4, fdt), "k-")
    grid on
    legend("h_1=0.52, h_2=0.48", "h_1=0.8, h_2=0.2")
    xlabel("Normalized frequency f\Delta\tau")
    ylabel("Magnitude |H(f)|")
    axis([0, 2, 0.01, 1])

    figure
    fdt = [0:0.01:2]
    plot(fdt, two_path_angle(h1, h2, fdt), "b-")
    hold on    
    plot(fdt, two_path_angle(h3, h4, fdt), "k-")
    grid on
    legend("h_1=0.52, h_2=0.48", "h_1=0.8, h_2=0.2")
    xlabel("Normalized frequency f\Delta\tau")
    ylabel("Phase of H(f)")    
end

function y = two_path_magnitude(h1, h2, fdt)
    y = sqrt(h1^2 + h2^2 + 2*h1*h2*cos(2*pi*fdt));
end

function y = two_path_angle(h1, h2, fdt)
    y = angle(h1 + h2*exp(-j*2*pi*fdt));
end

% Interaktives MATLAB-Skript zur Darstellung des frequenzselektiven Verhaltens
% eines Zwei-Pfad-Kanals ohne verschachtelte Funktionen.

function zwei_pfad_kanal_interaktiv_main
    % Parameterinitialisierung
    h1_initial = 1; % Anfangsverstärkung von Pfad 1
    h2_initial = 1; % Anfangsverstärkung von Pfad 2
    tau = 1;        % Laufzeitunterschied zwischen den Pfaden (kann auf 1 gesetzt werden)
    
    % Frequenzbereich
    f = linspace(0, 1, 1000); % Frequenz von 0 bis 1 (normierte Frequenz)
    
    % Erstellen der Figur
    fig = figure('Name', 'Frequenzselektives Verhalten eines Zwei-Pfad-Kanals');
    
    % Erstellen der Schieberegler für h1 und h2
    hSlider1 = uicontrol('Style', 'slider',...
        'Min', 0, 'Max', 2, 'Value', h1_initial,...
        'Units', 'normalized', 'Position', [0.15 0.02 0.3 0.04],...
        'Callback', @(src, event)updatePlot(src, event, f, tau));
    
    hSlider2 = uicontrol('Style', 'slider',...
        'Min', 0, 'Max', 2, 'Value', h2_initial,...
        'Units', 'normalized', 'Position', [0.55 0.02 0.3 0.04],...
        'Callback', @(src, event)updatePlot(src, event, f, tau));
    
    % Beschriftungen für die Schieberegler
    uicontrol('Style', 'text', 'Units', 'normalized',...
        'Position', [0.15 0.06 0.3 0.02], 'String', 'Verstärkung h1');
    
    uicontrol('Style', 'text', 'Units', 'normalized',...
        'Position', [0.55 0.06 0.3 0.02], 'String', 'Verstärkung h2');
    
    % Speichern der Slider in der Figur für Zugriff in Callback
    setappdata(fig, 'hSlider1', hSlider1);
    setappdata(fig, 'hSlider2', hSlider2);
    
    % Initiales Plotten
    initialPlot(f, tau, hSlider1, hSlider2);
end

% Funktion zum Initialen Plotten
function initialPlot(f, tau, hSlider1, hSlider2)
    % Abrufen der Anfangswerte von den Schiebereglern
    h1 = get(hSlider1, 'Value');
    h2 = get(hSlider2, 'Value');
    
    % Berechnung von Betrag und Phase
    magnitude = sqrt(h1^2 + h2^2 + 2*h1*h2*cos(2*pi*f*tau));
    phase = angle(h1 + h2*exp(-1j*2*pi*f*tau));
    
    % Plotten des Betrags
    subplot(2,1,1);
    plot(f, magnitude, 'LineWidth', 2);
    title('Betragsfrequenzgang');
    xlabel('Frequenz (normiert)');
    ylabel('Betrag');
    grid on;
    
    % Plotten der Phase
    subplot(2,1,2);
    plot(f, unwrap(phase), 'LineWidth', 2);
    title('Phasenfrequenzgang');
    xlabel('Frequenz (normiert)');
    ylabel('Phase (Radianten)');
    grid on;
end

% Funktion zum Aktualisieren der Plots
function updatePlot(~, ~, f, tau)
    % Abrufen der aktuellen Figur und der Slider
    fig = gcf;
    hSlider1 = getappdata(fig, 'hSlider1');
    hSlider2 = getappdata(fig, 'hSlider2');
    
    % Abrufen der Werte von den Schiebereglern
    h1 = get(hSlider1, 'Value');
    h2 = get(hSlider2, 'Value');
    
    % Berechnung von Betrag und Phase
    magnitude = sqrt(h1^2 + h2^2 + 2*h1*h2*cos(2*pi*f*tau));
    phase = angle(h1 + h2*exp(-1j*2*pi*f*tau));
    
    % Löschen der aktuellen Achsen
    %clf;
    
    % Plotten des Betrags
    subplot(2,1,1);
    plot(f, 10*log10(magnitude), 'LineWidth', 2);
    title(['Magnitude (h1 = ' num2str(h1, '%.2f') ', h2 = ' num2str(h2, '%.2f') ')']);
    xlabel('Frequency (normalized)');
    ylabel('Magnitude [dB]');
    grid on;
    
    % Plotten der Phase
    subplot(2,1,2);
    plot(f, unwrap(phase), 'LineWidth', 2);
    title('Phase');
    xlabel('Frequency (normalized)');
    ylabel('Phase (rad)');
    grid on;
end
