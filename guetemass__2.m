function [result] = guetemass(measurement, modelValues, f)
% Berechnen der Modellimpedanz, und deren Abweichung von den Messwerten

%....

ErrorReal = [];
ErrorImag = [];

result=sum(ErrorReal)+sum(ErrorImag);