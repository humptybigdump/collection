load('data.mat');

window_length = 50;    % the length of the sliding window

est_poses = zeros(numT,1);        % array to store the estimated vehicle position for each point in time
est_velocities = zeros(numT,1);   % array to store the estimated vehicle speed for each point in time

% the matrix A and vector b that are incrementally build as sufficient
% statistics for the regression approach. Finally, the unknown motion
% parameters are estimated from the system of linear equations A*xv=b
A = [ 0 0 ; 0 0 ];
b = [ 0 ; 0 ];

% Implement regression here
for k=1:numT
    % incrementally build A and b
    A = A + [ 1 k; k k*k];
    b = b + [ measurements(k,1); k*measurements(k,1) ];
    
    if (k>window_length) 
        % remove contributions of measurements which have left the sliding
        % window
        A = A - [ 1 k-window_length; k-window_length (k-window_length)*(k-window_length)];
        b = b - [ measurements(k-window_length,1); (k-window_length)*measurements(k-window_length,1) ];
    end
    
    % solve A*xv=b with respect to xv
    if (det(A)~=0)
        xv = linsolve(A,b);
        est_poses (k) = xv(1)+xv(2)*k;
        est_velocities (k) = xv(2);
    end
end

% visualize results
figure('Name', 'vehicle position'); hold on;
plot(1:numT, true_poses(:,1), 'r*-');
plot(1:numT, measurements(:,1), 'ks');
plot(1:numT, est_poses, 'bx', 'LineWidth', 2);
legend ('true position', 'measurement', 'estimated position');

figure('Name', 'vehicle velocity'); hold on;
plot([1 numT], [1 1], 'r-');
plot(1:numT, est_velocities, 'bx', 'LineWidth', 2);
legend ('true velocity', 'estimated velocity');

squared_mean_error = analyze_residual_error(est_poses, true_poses)
