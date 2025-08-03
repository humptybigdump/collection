% clear all;
% close all;
% clc;

load('data.mat');
dims = 4;
initial_state = zeros(dims, 1)
initial_state_uncertainty = 100
initial_state_cov_matrix = initial_state_uncertainty * initial_state_uncertainty * eye(dims)

state_history = zeros(numT, dims);

A = [eye(dims/2), eye(dims/2); zeros(dims/2), eye(dims/2)]
H = [eye(dims/2), zeros(dims/2)]

uncertainty_prediction_postion = 0.01;
uncertainty_prediction_velocity = 0.01;
Q1 = uncertainty_prediction_postion * uncertainty_prediction_postion * eye(dims/2);
Q2 = uncertainty_prediction_velocity * uncertainty_prediction_velocity * eye(dims/2);
Q = [Q1, zeros(dims/2); zeros(dims/2), Q2]

uncertainty_measurement_position = 0.2;
R = uncertainty_measurement_position * uncertainty_measurement_position * eye(dims/2)

figure('Name', 'Positions');
hold on;
plot(true_poses(:, 1), true_poses(:, 2), 'ro-');
plot(measurements(:, 1), measurements(:, 2), 'ks');

state = initial_state;
state_cov_matrix = initial_state_cov_matrix;
for t = 1:1:numT
    %% Prediction
    state = A * state;
    state_cov_matrix = A * state_cov_matrix * A' + Q;
    
    %% Innovation
    K = state_cov_matrix * H' * inv(H * state_cov_matrix * H' + R);
    state = state + K * (measurements(t, :)' - H * state);
    state_cov_matrix = (eye(dims) - K * H) * state_cov_matrix;
    
    %% Plot
    plot(state(1, 1), state(2, 1), 'bx');
    error_ellipse(state_cov_matrix(1:dims/2, 1:dims/2), state(1:dims/2, 1));
    plot([state(1, 1), state(1, 1) + state(3, 1)], [state(2, 1), state(2, 1) + state(4, 1)], 'k:');
    
    %% Save state in state_history
    state_history(t, :) = state';
    
end

axis equal;
legend('true positions', 'measurements', 'states');

mean_squared_error = analyze_state_error(state_history, true_poses)
