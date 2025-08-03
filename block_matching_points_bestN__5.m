image_point_locations = [161, 297;  980, 350; ...
                         600,  26;  597,  87; ...
                         680, 109;  356, 121; ...
                         840, 101;  603, 262; ...
                        1046, 320; 1086,  86];

I1 = imread('images/left/0000.png');
I2 = imread('images/right/0000.png');
search_size = [301 301];
window_size = [31 51];

for i = 1:size(image_point_locations,1)
    p = image_point_locations(i,:);
    matches = match_point_bestN(I1, I2, p, search_size, window_size);
    visualize_matched_points(I1, I2, p, matches, ['[', int2str(i), ']: matches = ', int2str(size(matches,1))]);
end