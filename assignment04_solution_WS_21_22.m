%% Assignment 04
clear all;

%% Exercise 1
close all;
clc;

Irgb = im2double(imread('stack.png'));

figure;
imagesc(Irgb);
title('RGB Image'); axis image; axis off;

% HSV
Ihsv = rgb2hsv(Irgb);
% L*a*b
Ilab = rgb2lab(Irgb);
Ilab = Ilab / 100;
% Intensity
Iintensity = rgb2gray(Irgb);
% Max values
Imax = max(Irgb, [], 3);
% Min values
Imin = min(Irgb, [], 3);
% Average values
Iaverage = mean(Irgb, 3);

figure;
subplot(2,2,1);
imagesc(Imin, [0, 1]);
title('RGB Min'); axis image; axis off;
colorbar;
subplot(2,2,2);
imagesc(Imax, [0, 1]);
title('RGB Max'); axis image; axis off;
colorbar;
subplot(2,2,3);
imagesc(Iaverage, [0, 1]);
title('RGB Average'); axis image; axis off;
colorbar;
subplot(2,2,4);
imagesc(Iintensity, [0, 1]);
title('RGB Intensity'); axis image; axis off;
colorbar;
colormap('jet');

figure;
subplot(2,2,1);
imagesc(Irgb, [0, 1]);
title('RGB Image'); axis image; axis off;
s2 = subplot(2,2,2);
imagesc(Ihsv(:,:,1), [0, 1]);
colormap(s2, 'hsv');
colorbar;
title('HSV Hue'); axis image; axis off;
s3 = subplot(2,2,3);
imagesc(Ihsv(:,:,2), [0, 1]);
colormap(s3, 'jet');
colorbar;
title('HSV Saturation'); axis image; axis off;
s4 = subplot(2,2,4);
imagesc(Ihsv(:,:,3), [0, 1]);
colormap(s4, 'jet');
colorbar;
title('HSV Value'); axis image; axis off;

figure;
colormap('jet');
subplot(2,2,1);
imagesc(Ilab(:,:,1), [0, 1]);
colorbar;
title('L*a*b Luminance'); axis image; axis off;
subplot(2,2,2);
imagesc(Iintensity, [0, 1]);
colorbar;
title('Intensity'); axis image; axis off;
subplot(2,2,3);
imagesc(Ihsv(:,:,3), [0, 1]);
colorbar;
title('HSV Value'); axis image; axis off; 
subplot(2,2,4);
imagesc(Imax, [0, 1]);
colorbar;
title('Max Value'); axis image; axis off;

diff_luminance = abs(Ilab(:,:,1) - Iintensity);
diff_value = abs(Ihsv(:,:,3) - Imax);

figure;
subplot(2,1,1);
colormap('jet');
imagesc(diff_luminance);
colorbar;
title('Difference between luminance of L*a*b and intensity'); axis image; axis off;
subplot(2,1,2);
colormap('jet');
imagesc(diff_value);
colorbar;
title('Difference between value of HSV and max value of RGB'); axis image; axis off;


%% Exercise 2
% close all; 
% clc;

rgb_ccl_figure = figure;
colormap('jet');
for i=1:1:12
    % first set of parameters
%     threshold = 0.12 - i/100;
    % refine parameters
    threshold = 0.0625 - 0.0025 * i;
    label = ccl(Irgb(:,:,:), threshold, 100);
    
    figure(rgb_ccl_figure);
    subplot(3,4,i);
    imagesc(label);
    title("RGB, Threshold: " + threshold); 
    axis image; axis off;
end

Lab_ccl_figure = figure;
colormap('jet');
for i=1:1:12
    % first set of parameters
%     threshold = 0.12 - i/100;
    % refine parameters
    threshold = 0.06 - 0.0025 * i;
    label = ccl(Ilab(:,:,:), threshold, 100);
    
    figure(Lab_ccl_figure);
    subplot(3,4,i);
    imagesc(label);
    title("L*a*b, Threshold: " + threshold); 
    axis image; axis off;
end

luminance_ccl_figure = figure;
colormap('jet');
for i=1:1:12
    % first set of parameters
%     threshold = 0.12 - i/100;
    % refine parameters
    threshold = 0.04 - 0.0025 * i;
    label = ccl(Ilab(:,:,1), threshold, 100);
    
    figure(luminance_ccl_figure);
    subplot(3,4,i);
    imagesc(label);
    title("L*a*b Luminance, Threshold: " + threshold); 
    axis image; axis off;
end

chrominance_ccl_figure = figure;
colormap('jet');
for i=1:1:12
    % first set of parameters
%     threshold = 0.12 - i;
    % refine parameters
    threshold = 0.045 - 0.0025 * i;
    label = ccl(Ilab(:,:,2:3), threshold, 100);
    
    figure(chrominance_ccl_figure);
    subplot(3,4,i);
    imagesc(label);
    title("L*a*b Chrominance, Threshold: " + threshold); 
    axis image; axis off;
end

I_ccl = ccl(Irgb(:,:,:), 0.0425, 100);
Ilab_ccl = ccl(Ilab(:,:,:), 0.034, 100);
Iluminance_ccl = ccl(Ilab(:,:,1), 0.0225, 100);
Ichrominance_ccl = ccl(Ilab(:,:,2:3), 0.025, 100);

figure;
colormap('jet');
subplot(2,2,1);
imagesc(I_ccl);
title('RGB'); axis image; axis off;
subplot(2,2,2);
imagesc(Ilab_ccl);
title('L*a*b'); axis image; axis off;
subplot(2,2,3);
imagesc(Iluminance_ccl);
title('L*a*b Luminance'); axis image; axis off;
subplot(2,2,4);
imagesc(Ichrominance_ccl);
title('L*a*b Chrominance'); axis image; axis off;


%% Exercise 3
% close all;
% clc;

I_kmeans = color_kmeans(Irgb, 4);
Ilab_kmeans = color_kmeans(Ilab, 4);
Iluminance_kmeans = color_kmeans(Ilab(:,:,1), 4);
Ichrominance_kmeans = color_kmeans(Ilab(:,:,2:3), 4);

figure;
colormap('jet');
subplot(2,2,1);
imagesc(I_kmeans);
title('RGB'); axis image; axis off;
subplot(2,2,2);
imagesc(Ilab_kmeans);
title('L*a*b'); axis image; axis off;
subplot(2,2,3);
imagesc(Iluminance_kmeans);
title('L*a*b Luminance'); axis image; axis off;
subplot(2,2,4);
imagesc(Ichrominance_kmeans);
title('L*a*b Chrominance'); axis image; axis off;


%% Exercise 4
% close all;
% clc;

Isegments = im2double(imread('segments.png'));      
Isegments = Isegments==0;

% instead of 'segments.png' we use the segmentation of L*a*b by ccl
figure;
subplot(1,2,1);
imagesc(Ilab_ccl);
title('Edge detection'); axis image; axis off;

four_neighborhood = [0, 1, 0;
                     1, 1, 1;
                     0, 1, 0];

eight_neighborhood = [1, 1, 1;
                      1, 1, 1;
                      1, 1, 1];

circle = [1, 1, 1, 1, 1;
          1, 0, 0, 0, 1;
          1, 0, 0, 0, 1;
          1, 0, 0, 0, 1;
          1, 1, 1, 1, 1];
      
cross =  [1, 0, 0, 0, 1;
          0, 1, 0, 1, 0;
          0, 0, 1, 0, 0;
          0, 1, 0, 1, 0;
          1, 0, 0, 0, 1];
      
disk = strel('disk', 2);
square = strel('square', 6);

Ifilled = Ilab_ccl;
% Ifilled = imdilate(Ifilled, four_neighborhood);
% Ifilled = imerode(Ifilled, eight_neighborhood);
% Ifilled = imdilate(Ifilled, cross);
% Ifilled = imerode(Ifilled, circle);
Ifilled = imdilate(Ifilled, square);
Ifilled = imerode(Ifilled, square);

subplot(1,2,2);
imagesc(Ifilled);
title('Fill holes'); axis image; axis off;