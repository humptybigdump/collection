T = 1000;
initial_state = [0 0];
true_delta_motion = [1 0];
numT = T - 1;

% noise terms
noise_tr_x = 0.1;
noise_tr_y = 0.05;
noise_system = diag([noise_tr_x^2, noise_tr_y^2]);

noise_meas_x = 0.25;
noise_meas_y = 0.25;
noise_meas = diag([noise_meas_x^2, noise_meas_y^2]);

% data
true_poses = zeros(numT, 2);
poses = zeros(numT + 1, 2);
delta_motion = zeros(numT, 2);
measurements = zeros(numT, 2);

true_state = initial_state;
poses(1,:) = true_state;
for i = 1:numT
    delta_motion(i,:) = mvnrnd(true_delta_motion, noise_system);
    poses(i+1,:) = poses(i,:) + delta_motion(i,:);    
    true_state = true_state + true_delta_motion;
    true_poses(i,:) = true_state;
    measurements(i,:) = mvnrnd(true_state(1:2), noise_meas);
end

% plot
plot(true_poses(:,1), true_poses(:,2), 'bx');
axis equal
hold on;
plot(poses(:,1), poses(:,2), 'ro');
plot(measurements(:,1), measurements(:,2), 'ks');
save('data.mat', 'delta_motion', 'measurements', 'true_poses', 'initial_state', 'numT', 'noise_system', 'noise_meas');
