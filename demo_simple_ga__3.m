%Skript demo_simple_ga
%Bilder und Demos für Evolutionäre Algorithmen
%Material zur Vorlesung Computational Intelligence
%Global Optimization Toolbox muss installiert sein! 
%
%Copyright [Ralf Mikut, KIT]
% 
%Licensed under the Apache License, Version 2.0 (the "License");
%you may not use this file except in compliance with the License.
%You may obtain a copy of the License at
% 
%http://www.apache.org/licenses/LICENSE-2.0
% 
%Unless required by applicable law or agreed to in writing, software
%distributed under the License is distributed on an "AS IS" BASIS,
%WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%See the License for the specific language governing permissions and
%limitations under the License.

%Hier können verschiedene Probleme umgeschaltet werden
problem_number = 2;

%Fertige Probleme
switch problem_number
    case 1
        %Rastrigin-Funktion
        FitnessFcn = @rastriginsfcn; 
        %Dimensionalität von p
        p_dimension = 5;
        %reellwertiges p
        ChromosomeType = 'doubleVector';
        
        %Einstellungen für die Visualisierung
        myaxis.min = 0;
        myaxis.logarithmic = true;
    case 2
        %Rastrigin-Funktion (Verschiebung um 1) 
        FitnessFcn = @rastriginsfcn1;
        %Dimensionalität von p
        p_dimension = 10
        %reellwertiges p
        ChromosomeType = 'doubleVector';
        
        %Einstellungen für die Visualisierung
        myaxis.min = 0.8;
        myaxis.logarithmic = true;
    case 3
        %Rastrigin-Funktion (Verschiebung um 1) mit Binärkodierung
        FitnessFcn = @rastriginsfcn1_binary;
        p_dimension = 16;
        %binäre Kodierung
        ChromosomeType = 'bitstring';
        
        %Einstellungen für die Visualisierung
        myaxis.min = 0.8;
        myaxis.logarithmic = true;
        
    case 4
        %Eigene Funktion aus der Vorlesung
        FitnessFcn = @own_function;
        p_dimension = 5;
        %rellwertige Kodierung
        ChromosomeType = 'doubleVector';
        
        %Einstellungen für die Visualisierung
        myaxis.min = -20;
        myaxis.logarithmic = false;
end;

%Hier werden die Parameter ausgewählt
ga_options = optimoptions(@ga);

%Wahl der Anzeigefunktion
ga_options = optimoptions(ga_options,'PlotFcn',@gaplotbestf);

%Anpassung der Populationsgröße
ga_options = optimoptions(ga_options,'PopulationSize',100);

%bestimmt den Typ des Chromosoms
ga_options = optimoptions(ga_options,'PopulationType',ChromosomeType);

%bestimmt den Anteil der elitären Akzeptanz der Nachkommen \mu + \lamda! 
ga_options = optimoptions(ga_options,'EliteCount',1);


%Memetische Algorithmen: Nach dem GA kann noch ein lokaler Optimierer
%ausgeführt werden, entsprechend ein- oder auskommentieren
%Gradientensuche
ga_options = optimoptions(ga_options,'HybridFcn','fminsearch');
%Patternsearch
%ga_options = optimoptions(ga_options,'HybridFcn','patternsearch');
%ohne lokale Optimierung
ga_options = optimoptions(ga_options,'HybridFcn','');

%Anzeigen von Zwischenergebnissen auf Kommandozeilenebene: iter, {[]}
ga_options = optimoptions(ga_options,'Display','iter');

%Initialisierung der Zufallszahlen
%rng default

%Lauf des GA, Syntax mit
%>>doc ga
%anschauen
[x,fval,exitflag,output,population,scores] = ga(FitnessFcn,p_dimension,[],[],[],[],[],[],[],ga_options);

%Ergebnis anzeigen
fprintf('Finales Ergebnis: %f\n',fval);

%Visualisierungen
if exist('myaxis','var')
    ax = axis;
    axis([ax(1) output.generations myaxis.min ax(4)]);
    if myaxis.logarithmic
        set(gca,'YScale','log');
    end;
    ylabel('Wert der Zielfunktion Q');
    title(strcat('Bestes Individuum: p = [',sprintf('%g ',x), sprintf(']; Q(p) = %g',fval)));
    legend off;
end;