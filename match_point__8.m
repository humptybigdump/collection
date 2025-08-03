function match = match_point(I1, I2, p, search_size, window_size)
%MATCH_POINT Matches a point f from I1 to a point
% in image I2. The search_size as [height, width] specifies the size of the
% window to search in I2 for p. The window_size as [height, width] specifies 
% the window size for applying the comparison function. The best corresponding 
% point [u1, v1; ... uN, vN] is returned. If no point is found, zeroes(0,2) is
% returned.

if (mod(search_size(1),2) == 0 || mod(search_size(2),2) == 0)
    error('The search size must contain odd numbers');
end

% since the search size dimensions must be odd. An odd number can be expressed by: 2n+1
% with n being half of size from the center point to the boundary
search_sz_half_u = (search_size(2)-1) / 2;
search_sz_half_v = (search_size(1)-1) / 2;

[height, width] = size(I2);

% min() and max() functions are used here to restrict the search boundaries to the image
search_boundaries_u = [max(1, p(1) - search_sz_half_u), min(width, p(1) + search_sz_half_u)];
search_boundaries_v = [max(1, p(2) - search_sz_half_v), min(height, p(2) + search_sz_half_v)];

match_value = zeros(0, 3);  % each row contains [u v sum_of_absolute_diff]
patch1 = extract_image_patch(I1, p, window_size);    % extract the image patch from I1

% iterate all pixes within the search boundary
for u = search_boundaries_u(1):search_boundaries_u(2)
    for v = search_boundaries_v(1):search_boundaries_v(2)
        patch2 = extract_image_patch(I2, [u v], window_size);    % extract the image patch from I2
        % compute the sum of absolute differences, the conversion to double is required here
        % to get negative numbers, remember: uint8(10) - uint8(20) = 0(!!)
        sum_of_absolute_diff = sum(sum(abs((double(patch1) - double(patch2)))));    
        match_value = [match_value; u v sum_of_absolute_diff]; %#ok<AGROW>
    end
end
match = zeros(0,2);
% sort the match_value matrix by the thrid column (= sum_of_absolute_diff)
% order: ascending
match_value = sortrows(match_value, 3);
% get the pixel location with minimal difference
match = match_value(1, 1:2);
end

