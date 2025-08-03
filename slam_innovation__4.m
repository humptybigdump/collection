function [ state_est, cov_est] = slam_innovation(state, cov, observations, noise_observation)
% SLAM_INNOVATION The innovation of the EKF-SLAM
% Input
%  state: The current (predicted) state
%  cov: The current (predicted) covariance matrix
%  observations: The list of observations as [ID_1, x_1, y_1; ... ID_N,
%                x_N, y_N].
%  noise_observation: The variance of the measurement (a scalar).
%
% Output
%  state_est: The estimated state
%  cov_est: The estimated covariance

num_landmarks = (length(state)-3)/2;

%%% Initialization code

if (length(observations) == 0 )
    return;
end

H = zeros(2*length(observations), length(state));

for i=1:length(observations);
    id = observations(i).id;
    if (id > num_landmarks)        
        %%% Implement your code here (1. Dynamic extension of state, covariance matrix, oberservation matrix H)
    end
    
    %%% Implement your code here (2. Observation matrix H (Jacobian of observation model))
end

%%% Implement your code here (3. Innovation of EKF)

end

