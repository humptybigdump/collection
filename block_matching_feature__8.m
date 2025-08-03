I1 = imread('images/left/0000.png');
I2 = imread('images/right/0000.png');
search_size = [101 101];
window_size = [31 51];

matches = feature_matching(I1, I2, search_size, window_size);
visualize_feature_matching(I1, I2, matches, '0000.png');
plot_patches(I1, I2, matches, window_size);