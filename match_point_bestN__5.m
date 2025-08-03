function match = match_point_bestN(I1, I2, p, search_size, window_size)
%MATCH_POINT_BESTN Matches a point f from I1 to a point
% in image I2. The search_size as [height, width] specifies the size of the
% window to search in I2 for p. The window_size as [height, width] specifies 
% the window size for applying the comparison function. A best N corresponding 
% points [u1, v1; ... uN, vN] is returned. If no point is found, zeroes(0,2) is
% returned.

if (mod(search_size(1),2) == 0 || mod(search_size(2),2) == 0)
    error('The search size must contain odd numbers');
end
search_sz_half_u = (search_size(2)-1) / 2;
search_sz_half_v = (search_size(1)-1) / 2;

search_boundaries_u = [max(1, p(1) - search_sz_half_u), min(size(I2, 2), p(1) + search_sz_half_u)];
search_boundaries_v = [max(1, p(2) - search_sz_half_v), min(size(I2, 1), p(2) + search_sz_half_v)];

match_value = zeros(0, 3);
patch1 = extract_image_patch(I1, p, window_size);
for u = search_boundaries_u(1):search_boundaries_u(2)
    for v = search_boundaries_v(1):search_boundaries_v(2)
        patch2 = extract_image_patch(I2, [u v], window_size);
        sum_of_absolute_diff = sum(sum(abs((double(patch1) - double(patch2)))));
        match_value = [match_value; u v sum_of_absolute_diff]; %#ok<AGROW>
    end
end
match = zeros(0,2);
if (size(match_value, 1) > 0)
    match_value = sortrows(match_value, 3);
    best_diff = match_value(1, 3);   % the best (minimal) difference value
    % threshold to include all those pixels in the output with the sum_of_absolute_diff being
    % within 10% of the best value, other thresholds could be chosen here
    threshold = 1.1 * best_diff;
    for i = 1:size(match_value,1)
        if (match_value(i,3) <= threshold)
            match = [match; match_value(i, 1:2)]; %#ok<AGROW>
        end
    end
end
end

