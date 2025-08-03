%Skript demo_gamultiobj
%Bilder und Demos für multikriterielle Otimierung mit Evolutionären Algorithmen
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

    
problem_number = 1;
%Fertige Probleme
switch problem_number
    case 1
        %Rastrigin-Funktion & Quadratische Funktion (zweikriteriell)
        FitnessFcn = @pareto_rastriginsfcn1_quadrfun;  
        %Dimensionalitätvon p
        p_dimension = 2;
        %reellwertiges p
        ChromosomeType = 'doubleVector';
        
        %Einstellungen für die Visualisierung
        myaxis.min = 0;
        myaxis.logarithmic = true;
   
end;

%Hier werden die Parameter ausgewählt
gamultiobj_options = optimoptions(@gamultiobj);

%Wahl der Anzeigefunktion
gamultiobj_options = optimoptions(gamultiobj_options,'PlotFcn',@gaplotpareto);

%Anpassung der Populationsgröße
gamultiobj_options = optimoptions(gamultiobj_options,'PopulationSize',500);

%bestimmt den Typ des Chromosoms
gamultiobj_options = optimoptions(gamultiobj_options,'PopulationType',ChromosomeType);

%Anzeigen von Zwischenergebnissen auf Kommandozeilenebene: iter, {[]}
gamultiobj_options = optimoptions(gamultiobj_options,'Display','iter');

%Initialisierung der Zufallszahlen
%rng default

%Lauf des GA, Syntax mit
%>>doc gamultiobj
%anschauen
[x,fval,exitflag,output,population,scores] = gamultiobj(@pareto_rastriginsfcn1_quadrfun,p_dimension,[],[],[],[],[],[],[],gamultiobj_options)

%Visualisierung der Ergebnisse
fprintf('Finales Ergebnis: %f\n',fval);
figure; 
subplot(2,1,1);
plot(fval(:,1),fval(:,2),'x');xlabel('Q_1');ylabel('Q_2')
subplot(2,1,2);
plot(x(:,1),x(:,2),'x');xlabel('p_1');ylabel('p_2')