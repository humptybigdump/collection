%% assignment 01

%% Exercise 1
clear all; close all; clc;

(4*7-8)/(3^4-1)
exp(sin(3.2))
(5+log(37/(2.9*1.7)))/(9)

%% Exercise 2
clear all; close all; clc;

A = [ 3, 5, 1;
      2, 0, 1;
     -1, 1, 0];
b = [-2; 1; -4];
c = inv(A) * b

%% Exercise 3
clear all; close all; clc;

format shortG;
[-3:0.1:5]'
[10.^(-3:1:5)]'
u = [1:1:100];
v = [100:-1:1];
u*v'

%% Exercise 4
clear all; close all; clc;

x = -10:0.1:10;
y1 = sinc(x);
y2 = 1./(pi*x);
y3 = -1./(pi*x);

figure;
hold on;
axis([-10, 10, -1.5, 1.5]);
plot(x, y1, 'r-');
plot(x, y2, 'k.');
plot(x, y3, 'k.');
plot(0, 0, 'bx');
hold off;

%% Exercise 5
clear all; close all; clc;
u=[1:100]; v = (2:101);
d = euclidean_distance(u, v)

%% Exercise 6
clear all; close all; clc;

I = imread('kabel_salat.png');

I_size = size(I)
I_min = min(min(I))
I_max = max(max(I))
I_average = mean(mean(I))
I_median = median(median(I))

gauss_filter = fspecial('gaussian', 51, 4);
I_gauss = imfilter(I, gauss_filter, 'conv');
I_gauss_wiener = deconvwnr(I_gauss, gauss_filter, 0.0001);

figure;
imshow(I);

motion_blur_filter = fspecial('motion', 15, 0);
I_motion_blur = imfilter(I, motion_blur_filter, 'conv');
I_motion_blur_wiener = deconvwnr(I_motion_blur, motion_blur_filter, 0.01);

I_fadeout = fadeout(I_gauss, 50);
I_fadeout_wiener = deconvwnr(I_fadeout, gauss_filter, 0.0001);
I_pattern = [I, I, I; I, I, I; I, I, I];
I_pattern_gauss = imfilter(I_pattern, gauss_filter, 'conv');
I_pattern_gauss_wiener = deconvwnr(I_pattern_gauss(I_size(1)+1:I_size(1)*2, I_size(2)+1:I_size(2)*2), gauss_filter, 0.0001);

figure;
subplot(2,2,1);
imshow(I_gauss);
subplot(2,2,2);
imshow(I_gauss_wiener);
subplot(2,2,3);
imshow(I_pattern_gauss);
subplot(2,2,4);
imshow(I_pattern_gauss_wiener);

figure;
subplot(3,2,1);
imshow(I_gauss);
subplot(3,2,2);
imshow(I_gauss_wiener);
subplot(3,2,3);
imshow(I_motion_blur);
subplot(3,2,4);
imshow(I_motion_blur_wiener);
subplot(3,2,5);
imshow(I_fadeout);
subplot(3,2,6);
imshow(I_fadeout_wiener);