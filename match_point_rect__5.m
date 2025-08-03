function match = match_point_rect(I1, I2, p, window_size) 
%MATCH_POINT_RECT Matches a point from I1 to a point
% in image I2 under the assumption that both images are rectified. 
% The window_size as [height, width] specifies the matching window
% for block matching.
% If a match is found, the point in I2 is
% returned as [u, v]. Otherwise zeroes(0,2) is returned.

match_value = zeros(0, 3);
patch1 = extract_image_patch(I1, p, window_size);
% due to rectification, the search area only spans across the image row of
% the feature
[~, width] = size(I2);
for u = 1:width
    patch2 = extract_image_patch(I2, [u p(2)], window_size);
    sum_of_abs_diff = sum(sum(abs(double(patch1) - double(patch2))));
    match_value = [match_value; u p(2) sum_of_abs_diff]; %#ok<AGROW>
end
match = zeros(0,2);
match_value = sortrows(match_value, 3);
match = match_value(1, 1:2);
end

