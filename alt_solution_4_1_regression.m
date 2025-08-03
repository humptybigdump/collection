clear all;
close all;
clc;

load('data.mat');

sliding_window_length_max = 150;
iterator = 15;
vector_length = sliding_window_length_max / iterator; 
mean_squared_error_vec = zeros(vector_length, 1);
s_iter = 1;

for s = 1:iterator:sliding_window_length_max

    sliding_window_length = s;

    estimated_positions = zeros(numT, 1);
    estimated_velocities = zeros(numT, 1);

    A = [0, 0; 
         0, 0];
    b = [0; 0];

    for t = 1:numT    
        A = A + [1,     t;
                t, t * t];
        b = b + [    measurements(t, 1);
                 t * measurements(t, 1)];

        if (t > sliding_window_length)
            A = A - [                        1,                                t - sliding_window_length;
                     t - sliding_window_length, (t - sliding_window_length) * (t - sliding_window_length)];
            b = b - [                              measurements(t - sliding_window_length, 1);
                     (t - sliding_window_length) * measurements(t - sliding_window_length, 1)];
        end    

        if (det(A) ~= 0 ) 
            x = linsolve(A, b);
            estimated_positions(t) = x(1, 1) + t * x(2, 1);
            estimated_velocities(t) = x(2, 1);
        end
    end

    figure('Name', 'Positions'); 
    hold on;
    plot(1:numT, true_poses(:, 1),'ro-');
    plot(1:numT, measurements(:, 1),'ks');
    plot(1:numT, estimated_positions(:, 1),'bx');
    legend('true positions', 'measurements', 'estimated positions');
    
    figure('Name', 'Velocity'); 
    hold on;
    plot(1:numT, ones(numT, 1),'ro-');
    plot(1:numT, estimated_velocities(:, 1),'bx');
    legend('true velocities', 'estimated velocities');

    mean_squared_error = analyze_residual_error(estimated_positions, true_poses(:, 1))
    mean_squared_error_vec(s_iter, 1) = mean_squared_error;
    s_iter = s_iter + 1;
end

figure('Name', 'Mean Squared Error'); 
plot(1:vector_length, mean_squared_error_vec(:, 1),'ro-');