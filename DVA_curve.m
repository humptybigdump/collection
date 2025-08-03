function [ dvadata ] = DVA_curve( measure, aging, varargin )
%DVACURVE calculates DVA curve from CV or constant current measurement
%data.
%
% SYNTAX
% [ dvadata ] = dvacurve( measure, aging, [optional capacity] )
%
% DESCRIPTION
% The function calculates the DVA data of a CV or constant current 
% measurement including the influence of variant active mass using the
% input variable "aging". See chapter 2.5.2 of Jan Richter's diploma thesis
% for details on the influence of the aging parameter.
%
% INPUT
% measure           = Measurement Data. Structure containing .t [s], .U [V]
%                     and .I [A] as time, voltage and current.
% aging             = Parameter of the electrode active mass. Real number
%                     >= 0. The bigger aging the bigger is the active mass,
%                     at 0 the electrode has no active mass. Default is 1 
%                     and leaves the active mass of the measurement
%                     unchanged.
% optional capacity = For the DVA calculation a normalization to a capacity
%                     is needed. Default is the normalization to the
%                     electrode capacity but sometimes the DVA is
%                     normalized to the full cell capacity which stays
%                     constant even if aging parameters change. In order to
%                     be able to supply this capacity to the function the
%                     input is needed. It is optional, if the function is
%                     called without this input everything is normalized to 
%                     the electrode capacity (Unit: [Ah]).
%
% OUTPUT
% dvadata = Structure containing .Q [Ah] and .DVA [V] as charge and
%           differential capacity.
%
% INFO
% Author: Jan Richter
% Date: 07.04.2011


%% Input
% Calculate OCV Curve
% ocvdata = ocvcurve(measure, aging);
charge = measure.Q.*aging;
voltage = measure.U;


% Setup Optional Capacity Input
if isempty(varargin) == 1
    capacity = max(charge);
else
    capacity = varargin{1,1};
end    

%% DVA Calculation (Central Difference Methode)
dva = zeros(length(voltage),1);
for i = 1:length(voltage)
    if i == 1;
        % First Value
        dva(1) = (voltage(2)-voltage(1)) / (charge(2)-charge(1));
    elseif i == length(voltage)
        % Last Value
        dva(length(voltage)) = (voltage(end)-voltage(end-1)) / (charge(end)-charge(end-1)); 
    else
        % All Other Values
        dva(i) = (voltage(i+1)-voltage(i-1)) / (charge(i+1)-charge(i-1));
    end
end    

%% Output
dvadata.Q = charge;
dvadata.DVA = dva; %[V/Ah]

end

