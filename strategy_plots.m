%Skript strategy_plots
%Zeigt die Auswirkungen veränderter Parameter des EA, demo_simple_ga muss
%vorher ausgeführt werden!
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

%demo_simple_ga;


%Anzeigen von Zwischenergebnissen auf Kommandozeilenebene: iter, {[]}
ga_options = optimoptions(ga_options,'Display','none');

%Wahl der Anzeigefunktion
ga_options = optimoptions(ga_options,'PlotFcn',{[]});

mystrategy = 1;
switch mystrategy
    case 1
        %Veränderung der Populationsgröße
        strategy.value_list = [5:5:100];
        strategy.xaxis = strategy.value_list;
        strategy.name = 'PopulationSize';
        strategy.variable_name = 'PopulationSize';
    case 2
        %Auswirkungen memetischer Algorithmen
        strategy.value_list = {'','fminsearch','patternsearch'};
        strategy.xaxis = 1:length(strategy.value_list);
        strategy.name = 'HybridFcn';
        strategy.variable_name = 'HybridFcn';
end;

%Anzahl Wiederholungen für den gleichen Parameterwert
number_of_repetitions = 20;

%Aufbau Lösungsarchiv
clear fval_archiv;
fval_archiv.fitness = nan(length(strategy.value_list),number_of_repetitions);
fval_archiv.time    = nan(length(strategy.value_list),number_of_repetitions);
fval_archiv.ga_options = ga_options;

for value_ind = 1:length(strategy.value_list)
    
    %Anpassung der Optionen
    if iscell(strategy.value_list)
        %Parameter kommen als Cell Array
        ga_options = optimoptions(ga_options,strategy.name,strategy.value_list{value_ind});
    else
        %Parameter kommen als Matrix
        ga_options = optimoptions(ga_options,strategy.name,strategy.value_list(value_ind));
    end;
    
    %Zeitmessung für Rechenaufwand
    start_time = clock;
    for i_rep = 1:number_of_repetitions
        start_time = clock;
        %EA ausführen und Ergebnisse speichern
        [x,fval_archiv.fitness(value_ind,i_rep)] = ga(FitnessFcn,p_dimension,[],[],[],[],[],[],[],ga_options);
        %Rechenzeit speichern
        fval_archiv.time(value_ind,i_rep) = etime(clock,start_time);
        
    end;
    %Anzeige Fortschritt
    fprintf('%d von %d\n',value_ind,length(strategy.value_list))
end;

%Visualisierungen
figure(2);clf;
subplot(1,4,1);
semilogy(strategy.xaxis,fval_archiv.fitness,'o');
xlabel(strategy.name);
ylabel('Wert der Zielfunktion')
%Umschalten auf logarithmische Ausgabe
%set(gca,'YScale','log')
subplot(1,4,2);
plot(strategy.xaxis,fval_archiv.time,'o');
xlabel(strategy.name);
ylabel('Rechenzeit pro Run [s]')

subplot(1,4,3);
semilogy(fval_archiv.time,fval_archiv.fitness,'x')
ylabel('Wert der Zielfunktion')
xlabel('Rechenzeit pro Run [s]')

subplot(1,4,4);
boxplot(fval_archiv.fitness');
ylabel('Wert der Zielfunktion')
xlabel(strategy.name);
