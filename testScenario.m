function [params] = basicScenario(params)
% simulation scenario with Poissin point proces distributed base stations and moving users
% This scenario file shows how to set up a scenario with three sector
% antennas that are distributed through a Poisson point process and moving
% users of the three different movement types. A building is set up in the
% simulation scenario.
% This scenario also shows how tpo set up chunks for parallelization of the
% simulation.
%
% input:
%   params: [1x1]handleObject parameters.Parameters
%
% output:
%   params: [1x1]handleObject parameters.Parameters
%
% initial author: Lukas Nagel
% extended by: Agnes Fastenbauer
%
% see also launcherFiles.launcherExample, parameters.Parameters

%% General Parameters
% time config
params.time.numberOfChunks              = 10;   % a sufficently large number of chunks to achieve paralleization gain
params.time.feedbackDelay               = 1;    % number of slots it takes for feedback to reach base station
params.time.slotsPerChunk               = 20;	% the first 3 slots in a chunk are discarded, since no feedback is available
params.time.timeBetweenChunksInSlots    = 100;	% the chunks should be independent

% set the carrier frequency and bandwidth
params.carrierDL.centerFrequencyGHz             = 2;    % in GHz
params.transmissionParameters.DL.bandwidthHz    = 5e6;  % in Hz

% disable HARQ - is not implemented for a feedback delay larger than 1
params.useHARQ = false;

% define the region of interest
params.regionOfInterest.xSpan = 600; % the ROI will go from -250 m to 250 m
params.regionOfInterest.ySpan = 600;
params.regionOfInterest.zSpan = 100; % make sure ROI is high enough to include all base stations
params.regionOfInterest.interferenceRegionFactor = 1.5; % add interference region with additional radius of 0.5 of the ROI
params.regionOfInterest.interference = parameters.setting.Interference.regionIndependentUser; % no users will be placed in the interference region

% set channel trace length
% This should be large enough to get independent channel realizations for
% all users.
params.smallScaleParameters.traceLengthSlots = 50000;

%% save additional results
params.save.losMap                  = false;    % default value
params.save.isIndoor                = false;    % default value
params.save.antennaBsMapper         = true;    % default value
params.save.macroscopicFading       = false;    % default value
params.save.wallLoss                = false;    % default value
params.save.shadowFading            = true;    % default value
params.save.antennaGain             = false;    % default value
params.save.pathlossTable           = true;    % default value

%% shadow fading
%params.shadowFading.on          = false;        % default value
%params.shadowFading.resolution	= 5;            % default value
%params.shadowFading.mapCorr     = 0.5;          % default value
%params.shadowFading.meanSFV     = 0;            % default value
%params.shadowFading.stdDevSFV	= 1;            % default value
%params.shadowFading.decorrDist	= log(2)*20;    % default value

%% postprocessor
params.postprocessor                = simulation.postprocessing.FullPP;	% default value
% params.postprocessor = simulation.postprocessing.LiteWithNetworkPP;

%% fastAveraging
params.fastAveraging                = false;                             % default value

%% scheduler
params.schedulerParameters.type = parameters.setting.SchedulerType.roundRobin;  % default value
% Setup 4+
% params.schedulerParameters.type = parameters.setting.SchedulerType.bestCqi;  % default value

%% users distributed with a two dimensional Poisson point process
poissonUsers = parameters.user.Poisson2D();
poissonUsers.density            = 25e-5;                                                 % example value - default is 0
poissonUsers.nElements          = 0;                                                    % default value
poissonUsers.height             = 1.5;                                                  % default value
poissonUsers.nRX                = 1;                                                    % default value
poissonUsers.speed              = 0;                                                    % default value
% poissonUsers.indoorDecision     = parameters.indoorDecision.Random;                   % default value
poissonUsers.indoorDecision     = parameters.indoorDecision.Static(parameters.setting.Indoor.outdoor);                   % default value
% poissonUsers.losDecision        = parameters.losDecision.UrbanMacro5G;                      % default value
poissonUsers.losDecision                   = parameters.losDecision.Random;
poissonUsers.losDecision.losProbability    = 0; % default value
poissonUsers.userMovement.type  = parameters.setting.UserMovementType.ConstPosition;    % default value
poissonUsers.rxNoiseFiguredB    = 9;                                                    % default value
poissonUsers.schedulingWeight   = 1;                                                    % default value
poissonUsers.numerology         = 0;                                                    % default value
poissonUsers.technology         = parameters.setting.NetworkElementTechnology.LTE;	    % default value


% more user parameters
poissonUsers.rxNoiseFiguredB  = 9;                                             	% default value
poissonUsers.channelModel     = parameters.setting.ChannelModel.AWGN;           	% default value
% poissonUsers.channelModel     = parameters.setting.ChannelModel.PedA;           	% default value
poissonUsers.trafficModelType = parameters.setting.TrafficModelType.FullBuffer;	% default value
poissonUsers.schedulingWeight = 1;                                                % default value
poissonUsers.numerology       = 0;                                              	% default value
poissonUsers.technology       = parameters.setting.NetworkElementTechnology.LTE;	% default value
params.userParameters('poissonUser') = poissonUsers;

% interference region users
interferenceUser = parameters.user.InterferenceRegion;
interferenceUser.nElements                          = 30;
interferenceUser.nRX                                = 1;
interferenceUser.nTX                                = 1;
interferenceUser.indoorDecision                     = parameters.indoorDecision.Random(0.5);
interferenceUser.losDecision                        = parameters.losDecision.Random;
interferenceUser.losDecision.losProbability         = 0.5;
interferenceUser.transmitPower                      = 1;
interferenceUser.channelModel                       = parameters.setting.ChannelModel.PedA;
% params.userParameters('interferenceUser') = interferenceUser;

%% ********************************************************************* %%
%% *************************** base stations *************************** %%
%% ********************************************************************* %%

%% Antennas
% three sector
antennaThreeSector = parameters.basestation.antennas.ThreeSector;
% Setup 4+
antennaThreeSector.nTX                      = 4;                                                % default value
%antennaThreeSector.baseStationType          = parameters.setting.BaseStationType.pico;         % default value
%antennaThreeSector.precoderAnalogType       = parameters.setting.PrecoderAnalogType.none;       % default value
%antennaThreeSector.height                   = 30;                                               % default value
%antennaThreeSector.transmitPower            = NaN;                                              % default value - transmit power will be chosen according to base station type
%antennaThreeSector.alwaysOn                 = true;                                             % default value
%antennaThreeSector.rxNoiseFiguredB          = 9;                                                % default value
%antennaThreeSector.azimuth              	= 0;                                                % default value
%antennaThreeSector.elevation              	= 80;                                               % default value
%antennaThreeSector.numerology            	= 0;                                                % default value
%antennaThreeSector.technology               = parameters.setting.NetworkElementTechnology.LTE;	% default value

%% Precoders

% LTE DL precoder
precoderLteDL = parameters.precoders.LteDL;

% random precoder
precoderRandom = parameters.precoders.Random;

% 5G downlink precoder
precoder5G = parameters.precoders.Precoder5G;

% Kronecker product based precoder
precoderKronecker = parameters.precoders.Kronecker;
precoderKronecker.beta = 1;                    % default value
precoderKronecker.maxLayer = 8;                % default value
precoderKronecker.horizontalOversampling = 4;  % default value
precoderKronecker.verticalOversampling   = 4;  % default value

% technology precoder - for base stations with multiple technologies
% see also the base station section for an example on how to use this
% precoder
precoderTechnology = parameters.precoders.Technology();
precoderTechnology.setTechPrecoder(parameters.setting.NetworkElementTechnology.LTE,     precoderLteDL);  % default value
precoderTechnology.setTechPrecoder(parameters.setting.NetworkElementTechnology.NRMN_5G, precoder5G);     % default value


%% Base Station

% base station placed in rings of a hexagonal grid
hexRing = parameters.basestation.HexRing;
hexRing.interBSdistance     = 150;                                              % default value
hexRing.nRing               = 2;                                                % default value
hexRing.nSectors            = 3;                                                % example value - default is 1
hexRing.antenna             = antennaThreeSector;                                 % example value - default is Omnidirectional
%hexRing.precoder.DL         = precoderLteDL;                                   % example value - default is LTE DL
params.baseStationParameters('hexRing') = hexRing;

% free space path loss
% Setup 1
%freeSpace = parameters.pathlossParameters.FreeSpace;
%freeSpace.alpha = 2; % default value
% Setup 2+
% urban 5G
urban5G_NLOS = parameters.pathlossParameters.UrbanMacro5G;
% urban5G_NLOS = parameters.pathlossParameters.FreeSpace;
urban5G_NLOS.isLos = false; % default value
urban5G_LOS = parameters.pathlossParameters.UrbanMacro5G;
% urban5G_LOS = parameters.pathlossParameters.FreeSpace;
% urban5G_LOS.isLos = true; % default value

% path loss model container setting - default model is free space for all link types
%NOTE: the path loss models are set randomly to show to set them, choose an
%appropriate model for each link type used in a simulation
indoor	= parameters.setting.Indoor.indoor;
outdoor	= parameters.setting.Indoor.outdoor;
LOS     = parameters.setting.Los.LOS;
NLOS	= parameters.setting.Los.NLOS;
% set path loss models for macro base station
macro = parameters.setting.BaseStationType.macro;
params.pathlossModelContainer.modelMap{macro,	indoor,     LOS}    = urban5G_LOS;	% default value
% Setup 4+
params.pathlossModelContainer.modelMap{macro,	indoor,     NLOS}   = urban5G_NLOS;  % example value
params.pathlossModelContainer.modelMap{macro,	outdoor,	LOS}    = urban5G_LOS;        % example value
% Setup 4+
params.pathlossModelContainer.modelMap{macro,	outdoor,	NLOS}   = urban5G_NLOS;      % example value

end

