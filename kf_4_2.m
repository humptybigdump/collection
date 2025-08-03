load('data.mat');

% initialization
initial_state = [0 0 0 0]';     % initial state: we do not know anything, hence let us use 0 as arbitrary initialization
initial_state_uncertainty = 100;    % initial uncertainty: very large
initial_state_cov_matrix = diag([initial_state_uncertainty^2 initial_state_uncertainty^2 initial_state_uncertainty^2 initial_state_uncertainty^2]);

state = initial_state;
unc = initial_state_cov_matrix;

% this vectors stores the list of estimated states (excluding the initial state).
state_history = zeros(numT, 4);

% initialize A and H: fill in your code
A = [ 1 0 1 0 ; 0 1 0 1 ; 0 0 1 0 ; 0 0 0 1 ];
H = [ 1 0 0 0 ; 0 1 0 0 ];

% initalize Q and R: fill in your code
uncertainty_transition_position = 0.1;
uncertainty_transition_velocity = 0.1;
Q = [ uncertainty_transition_position*uncertainty_transition_position 0 0 0 ; 
      0 uncertainty_transition_position*uncertainty_transition_position 0 0 ;
      0 0 uncertainty_transition_velocity*uncertainty_transition_velocity 0 ;
      0 0 0 uncertainty_transition_velocity*uncertainty_transition_velocity ];
uncertainty_measurement_position = 0.1;
R = [ uncertainty_measurement_position*uncertainty_measurement_position 0 ;
      0 uncertainty_measurement_position*uncertainty_measurement_position ];

% plotting of true poses
figure ('Name', 'Poses');
hold on;
plot(true_poses(:,1), true_poses(:,2), 'r*-');
plot(measurements(:,1), measurements(:,2), 'ks');

% main loop of kalman filter
for i = 1:numT
    % prediction: fill in your code
    state = A*state;
    unc = A*unc*A'+Q;
        
    % innovation: fill in your code
    %z = measurement(i,:)';
    z = [measurements(i,:)]';
    K = unc*H'*inv(H*unc*H'+R);
    state = state + K*(z-H*state);
    unc = (eye(4)-K*H)*unc;
    
    % store the estimated state for error analysis in 'state_history'
    state_history(i, :) = state;

    % plot estimated state and covariance after innovation
    if (i>1)
        plot ([state_history(i-1, 1) state_history(i-1, 1)+delta_motion(i, 1)], [state_history(i-1, 2) state_history(i-1, 2)+delta_motion(i, 2)], 'k:');
    end
    plot (state(1), state(2), 'bx');
    error_ellipse(unc(1:2,1:2), state);
end

axis equal;
mean_squared_error = analyze_state_error(state_history, true_poses)
