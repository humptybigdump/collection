close all;
imagename = '0000.png';
I1 = imread(['images/left/' imagename]);
I2 = imread(['images/right/' imagename]);
search_size = [5 5];   %%% set reasonable values here
window_size = [3 3];   %%% set reasonable values here

matches = feature_matching(I1, I2, search_size, window_size);
visualize_feature_matching(I1, I2, matches, imagename);
plot_patches(I1, I2, matches, window_size);
