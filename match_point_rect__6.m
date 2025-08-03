function match = match_point_rect(I1, I2, p, search_size, window_size)
%MATCH_POINT Matches a point f from I1 to a point
% in image I2. The search_size as [height, width] specifies the size of the
% window to search in I2 for p. The window_size as [height, width] specifies 
% the window size for applying the comparison function. The best corresponding 
% point [u1, v1; ... uN, vN] is returned. If no point is found, zeroes(0,2) is
% returned.

if (mod(search_size(1),2) == 0 || mod(search_size(2),2) == 0)
    error('The search size must contain odd numbers');
end

%%% add you code here


end
