% generate poses
% wp = zeros(0, 3);
% for i = 0:300
%     wp = [wp; 2*i 10 0];
% end
% center = [602 110];
% radius = 100;
% for i = 0:200
%     phi_pos = 3*pi/2 + pi*i/200;
%     wp = [wp; center(1) + radius * cos(phi_pos), center(2) + radius * sin(phi_pos), phi_pos + pi/2];
% end
% for i = 300:-1:151
%     wp = [wp; 2*i 210 pi];
% end
% center = [300 110];
% radius = 100;
% for i = 0:200
%     phi_pos = pi/2 + pi*i/200;
%     wp = [wp; center(1) + radius * cos(phi_pos), center(2) + radius * sin(phi_pos), phi_pos + pi/2];
% end
% for i = 151:200
%     wp = [wp; 2*i 10 0];
% end
% 
% figure(); 
% plot(wp(:,1), wp(:,2), 'bx'); hold on;
% lm = [64.2473   13.5267; ...
%   126.0753    6.6819; ...
%   182.5269   13.2008; ...
%   286.2903   12.5489; ...
%   309.4086   12.8748; ...
%   517.4731   13.5267; ...
%   634.1398   10.2673; ...
%   695.4301   87.1904; ...
%   671.2366  186.9296; ...
%   544.3548  212.6793; ...
%   490.0538  205.8344; ...
%   351.8817  213.6571;
%   288.9785  206.1604;
%   233.6022  191.1669;
%   205.6452  107.3990;
%   216.3978   48.0769;
%   265.3226   19.0678];
% plot(lm(:,1), lm(:,2), 'rs');
% return;

load('data_long.base.mat');

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
save('data_long.mat', 'initial_state', 'true_poses', 'true_landmarks', 'odom_meas', 'noise_odom', 'noise_observation');