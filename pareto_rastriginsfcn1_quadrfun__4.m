function scores = pareto_rastriginsfcn1_quadrfun(pop)
%function scores = pareto_rastriginsfcn1_quadrfun(pop)
%gibt die Werte von zwei Zielfunktionen für eine multikriterielle Optimierung zurück
%Funktion 1: Um 1 verschobenene Rastrigin-Funktion
%Funktion 2: Quadratische Funktion
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

scores(:,1) = rastriginsfcn1(pop);
scores(:,2) = quadrfcn(pop);



