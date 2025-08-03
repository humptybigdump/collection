%% Assignment 05
clear all;

%% Exercise 1
close all;
clc;

Icalib = im2double(imread('calibration.png'));
figure;
imagesc(Icalib);
hold on;

world_positions = load('calibration_worldpos.txt');

image_positions = [135, 418;
                   307, 398;
                   436, 355;
                   552, 314;
                   81,  282;
                   56,  215;
                   39,  168;
                   216, 178;
                   393, 214;
                   29,  54;
                   178, 42;
                   300, 42;
                   370, 74;
                   411, 93;
                   530, 151;
                   262, 280];
               
C = [world_positions, image_positions];
[ A, R, t ] = tsai(C)

u_0 = A(1,3)
v_0 = A(2,3)
alpha_ = A(1,1)
theta = rad2deg(acos(-A(1,2) / A(2,2)))
beta_ = A(2,2) * sin(deg2rad(theta))

%% Exercise 2
plot(image_positions(:,1), image_positions(:,2), 'ro', 'LineWidth', 4, 'MarkerSize', 8);

projection = [R, t; 0, 0, 0, 1] * [world_positions'; ones(1, size(world_positions, 1))];
projection = [A, zeros(3, 1); 0, 0, 0, 1] * projection ./ projection(3, :);
projection = projection(1:2, :);

plot(projection(1,:), projection(2,:), 'go', 'LineWidth', 4, 'MarkerSize', 8);

reprojection_error = (image_positions' - projection).^2;
reprojection_error = sqrt(sum(reprojection_error, 1));
reprojection_error = sum(reprojection_error, 2) / length(reprojection_error)

%% Exercise 3
image_positions_ = [385, 188;
                    525, 287;
                    606, 261;
                    464, 175;
                    397, 68;
                    560, 141];

plot(image_positions_(:,1), image_positions_(:,2), 'bo', 'LineWidth', 4, 'MarkerSize', 8);
hold off;

syms xi eta zeta
camera_coords = [R, t; 0, 0, 0, 1] * [xi; eta; zeta; 1];
image_coords = [A, zeros(3, 1); 0, 0, 0, 1] * camera_coords ./ camera_coords(3, :);
image_coords = image_coords(1:2)

for n = 1:4
    eqn = [image_positions_(n,:)'; 0] == [image_coords; zeta];
    [xi_n, eta_n, zeta_n] = solve(eqn, [xi, eta, zeta]);
    xi_n = double(xi_n);
    eta_n = double(eta_n);
    zeta_n = double(zeta_n);
    world_positions_(n,:) = [xi_n, eta_n, zeta_n];
end

for n = 5:6
    eqn = [image_positions_(n,:)'; world_positions_(n - 4, 1)'] == [image_coords; xi];
    [xi_n, eta_n, zeta_n] = solve(eqn, [xi, eta, zeta]);
    xi_n = double(xi_n);
    eta_n = double(eta_n);
    zeta_n = double(zeta_n);
    world_positions_(n,:) = [xi_n, eta_n, zeta_n];
end

world_positions_

figure
plot3(world_positions(:, 1), world_positions(:, 2), world_positions(:, 3), 'go', 'LineWidth', 4, 'MarkerSize', 8);
hold on;
plot3(world_positions_(:, 1), world_positions_(:, 2), world_positions_(:, 3), 'bo', 'LineWidth', 4, 'MarkerSize', 8);

% grid in the xi, eta - plane for visualization
xt = get(gca, 'XTick');
yt = get(gca, 'YTick');
lx = xlim;
ly = ylim;
plot([xt; xt], lx(:) * ones(size(xt)), 'k');
plot(ly(:) * ones(size(yt)), [yt; yt], 'k');
hold off;