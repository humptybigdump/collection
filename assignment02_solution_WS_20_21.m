%% assignment 02
clear all;

%% Exercise 1
close all; clc;

%% Load image
I = imread('postit2g.png');
I_d = im2double(I);

%% Sobel
sobel_fm_u = (1/8) * [1, 0, -1;
                   2, 0, -2;
                   1, 0, -1];
sobel_fm_v = sobel_fm_u';

g_u_sobel = conv2(I_d, sobel_fm_u);
g_v_sobel = conv2(I_d, sobel_fm_v);
g_u_sobel_alternative = imfilter(I_d, flip(sobel_fm_u, 2));
g_v_sobel_alternative = imfilter(I_d, flip(sobel_fm_v, 1));

figure;
smplot(3,1,1);
imshow(I_d);
smplot(3,1,2);
imshow(g_u_sobel, [], 'Colormap', parula);
colorbar;
smplot(3,1,3);
imshow(g_u_sobel_alternative, [], 'Colormap', parula);
colorbar;

figure;
smplot(3,1,1);
imshow(I_d);
smplot(3,1,2);
imshow(g_v_sobel, [], 'Colormap', parula);
colorbar;
smplot(3,1,3);
imshow(g_v_sobel_alternative, [], 'Colormap', parula);
colorbar;

%% Prewitt
prewitt_fm_u = (1/6) * [1, 0, -1;
                     1, 0, -1;
                     1, 0, -1];
prewitt_fm_v = prewitt_fm_u';

g_u_prewitt = conv2(I_d, prewitt_fm_u);
g_v_prewitt = conv2(I_d, prewitt_fm_v);
 
%% Gradient length and angle
g_sobel_length = sqrt(g_u_sobel.^2 + g_v_sobel.^2);
g_sobel_angle = atan2(g_v_sobel, g_u_sobel);
g_prewitt_length = sqrt(g_u_prewitt.^2 + g_v_prewitt.^2);
g_prewitt_angle = atan2(g_v_prewitt, g_u_prewitt);

figure;
smplot(2,2,1);
imshow(g_sobel_length, [], 'Colormap', jet);
smplot(2,2,2);
imshow(g_prewitt_length, [], 'Colormap', jet);
smplot(2,2,3);
imshow(g_sobel_angle * 180 / pi, [-180, 180], 'Colormap', hsv);
smplot(2,2,4);
imshow(g_prewitt_angle * 180 / pi, [-180, 180], 'Colormap', hsv);
 
%% Exercise 2
%close all; clc;

%% Laplacian and Sobel

%% Canny and Laplacian of Gaussian
I_sobel = edge(I_d, 'sobel');
laplacian_fm = fspecial('laplacian');
I_laplacian = edge(I_d, 'zerocross', 0.3, laplacian_fm);
I_canny = edge(I_d, 'canny', [0.05, 0.14], 2);
I_log = edge(I_d, 'log', 0.0008, 3);

figure;
smplot(2,2,1);
imshow(I_sobel);
smplot(2,2,2);
imshow(I_laplacian);
smplot(2,2,3);
imshow(I_log);
smplot(2,2,4);
imshow(I_log);
 
%% Exercise 3
%close all; clc;
n_lines = 10;

hough_data = robust_hough(I_canny);
hough_lines = robust_hough_lines(hough_data, n_lines, I_canny);
figure;
robust_hough_plot_lines(I_d, hough_lines);
 
[~, index]= sort(hough_data.peaks(:, 3));
peaks = hough_data.peaks(index, :);
peaks = peaks(end - n_lines:end, :);

figure;
imagesc(hough_data.accumulator);
colormap hot;
hold on;
plot(peaks(:,2),peaks(:,1),'wo');
hold off;