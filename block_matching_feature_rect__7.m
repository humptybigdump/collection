I1 = imread('images/left/0006.png');
I2 = imread('images/right/0006.png');
window_size = [31 51];

matches = feature_matching_rect(I1, I2, window_size);
visualize_feature_matching(I1, I2, matches, '0000.png');
plot_patches(I1, I2, matches, window_size);