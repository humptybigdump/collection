load('data_long.mat')
plot(true_poses(1,:), true_poses(2,:), 'bx');
axis equal; hold on;
plot(true_landmarks(1,:), true_landmarks(2,:), 'rs');

x0 = initial_state;

S  = [cos(x0(3)) -sin(x0(3)) x0(1); ...
      sin(x0(3)) cos(x0(3)) x0(2); ...
      0 0 1];
plot(S(1,3),S(2,3), 'kd');

for i = 1:size(odom_meas,2)
    d = odom_meas(:,i);
    deltaS = [cos(d(3)) -sin(d(3)) d(1); ...
              sin(d(3)) cos(d(3)) d(2); ...
              0 0 1];
    S = S * deltaS;
    plot(S(1,3),S(2,3), 'kd');
    plot([S(1,3), S(1,3)+S(2,2)] ,[S(2,3), S(2,3) + S(2,1)], 'b-');
end