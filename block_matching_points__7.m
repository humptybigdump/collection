close all;
image_point_locations = [161, 297;  980, 350; ...
                         600,  26;  597,  87; ...
                         680, 109;  356, 121; ...
                         840, 101;  603, 262; ...
                        1046, 320; 1086,  86];

imagename = '0000.png';
I1 = imread(['images/left/' imagename]);
I2 = imread(['images/right/' imagename]);
search_size = [0 0];   %%% set reasonable values here
window_size = [0 0];   %%% set reasonable values here

for i = 1:size(image_point_locations,1)
    p = image_point_locations(i,:);
    matches = match_point(I1, I2, p, search_size, window_size);
    visualize_matched_points(I1, I2, p, matches, ['Point_', int2str(i)]);
end
