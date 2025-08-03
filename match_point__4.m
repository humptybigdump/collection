function match = match_point(I1, I2, p, search_area, window_size)
% Find the best match (in image I2) for a given point p (from Image I1). 
% Arguments:
%   I1: left image
%   I2: right image
%   p:  The image point coordinates (x, y) of a point in the left image
%       (I1).
%   search_area: [height, width] of search area
%   window_size: [height, width] of window to use to compute the comparison
%                 metric
%   Output: image coordinates (match = [u, v]) of a point in the right
%           image (I2) that match point p
% Note: the height and width parameters of the search_area and window_size
% arguments are expected to be odd.

%% Step 1: Perform quality checks on input arguments

% Get dimensions of images and check if they are identical
if ~(all(size(I1) == size(I2)))
    error('The image dimensions are not identical');
end
[height, width] = size(I1);

% Check whether point p is within image I1
if (p(1) <= 0 || p(2) <= 0 || p(1) > width || p(2) > height)
    error('The point is not within the image');
end

% Check the input for the search_area and window_size
if (mod(search_area(1),2) == 0 || mod(search_area(2),2) == 0)
    error('The width and height of the search area must be odd');
end
if (mod(window_size(1),2) == 0 || mod(window_size(2),2) == 0)
    error('The width and height of the window size must be odd');
end

%% Part 2: Perform Search

% Add your code here: ...
end