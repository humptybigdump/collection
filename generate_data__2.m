load('data_lecture.base.mat');

cov_pose_stability = [ 1e-2 0 0; 0 1e-2 0; 0 0 (pi/36)^2 ];
chol_pose_stability = sqrt(cov_pose_stability)/4;

true_poses = wp';
initial_state = true_poses(:,1);
true_landmarks = lm';

num_poses = size(true_poses,2);

odom_meas = zeros(3, num_poses-1);
for i = 2:num_poses
    prev_phi = true_poses(3,i-1);
    odom_mea = zeros(3,1);
    odom_mea(1:2) = [cos(prev_phi), sin(prev_phi); -sin(prev_phi) cos(prev_phi)]*(true_poses(1:2,i)-true_poses(1:2,i-1));
	odom_mea(3) = true_poses(3,i)-true_poses(3,i-1);
	while (odom_mea(3) > pi) 
        odom_mea(3) = odom_mea(3) - 2*pi; 
    end
	while (odom_mea(3) < -pi) 
        odom_mea(3) = odom_mea(3) + 2*pi; 
    end
	odom_meas(:,i-1) = (odom_mea + chol_pose_stability * normrnd(0,1,3,1));
end
noise_odom = cov_pose_stability;
noise_observation = 1e-2;
save('data_lecture.mat', 'initial_state', 'true_poses', 'true_landmarks', 'odom_meas', 'noise_odom', 'noise_observation');