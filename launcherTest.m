%% Simulator Launcher File
% launches a simulation and displays results
% This is the example function for how to use the 5G systemlevel simulator.
% Launches the simulation defined in +scenarios by handing the defined
% scenario and simulation type to simulate. After the simulation the
% simulation results can be displayed with the functions in
% simulation.results or by creating customized plots for the scenario.
%
% see also scenarios.basicScenario, simulate, scenarios.example
% parameters.setting.SimulationType, scenarios, simulation.results

% clear working space and close all figures
close all
clear
clc

% launch a local simulation with the scenario defined in scenarios.basicScenario
% To launch a parallel simulation, change the second input to
% parameters.setting.SimulationType.parallel.

result = simulate(@scenarios.testScenario, parameters.setting.SimulationType.local);
            result.plotUserThroughputEcdf;
            result.plotUserLiteSinrEcdf;
            result.plotUserEffectiveSinrEcdf;
            result.plotUserLatencyEcdf;
            %result.plotUserBler;
            result.plotUserWidebandSinr;
% result.showAllPlots

%% display results

figure()
result.params.regionOfInterest.plotRoiBorder(tools.myColors.darkGray);
baseStations = result.networkResults.baseStationList;
% plot base stations and show attached users
for iBS = 1:size(baseStations,2)
    % plot base station
    antHandle = baseStations(iBS).antennaList.plot2D(1, tools.myColors.matlabRed);
    hold on;
    % plot line to attached user
    posAnt   =  baseStations(iBS).antennaList.positionList(:,1);
    for iUser = baseStations(iBS).attachedUsers
        posUe = iUser.positionList(:,1);
        % plot first user position in green
        handleFirst = iUser.plot2D(1, tools.myColors.matlabGreen);
        hold on;
        plot([posUe(1),posAnt(1)],[posUe(2),posAnt(2)],'Color', tools.myColors.lightGray);
        hold on;
    end
end
title('Simulation Scenario');

% plot SINR and throughput
% result.plotUserLiteSinrEcdf;
% result.plotUserThroughputEcdf;



