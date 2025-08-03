load('data.mat');

window_length = 50;    % the length of the sliding window

est_poses = zeros(numT,1);        % array to store the estimated vehicle position for each point in time
est_velocities = zeros(numT,1);   % array to store the estimated vehicle speed for each point in time


% Implement regression here
for k=1:numT







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
