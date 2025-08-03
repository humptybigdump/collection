function [features_clean, descriptors] = extract_sift_light_descriptors(I, features)
% EXTRACT_SIFT_LIGHT_DESCRIPTORS Extracts SIFT (light) descriptors from the
% image given the interest points as returned by
% detect_sift_light_features(..). It eliminates all features points for
% which any descriptor can be calculated.
%
% function [features_clean, descriptors] = extract_sift_light_descriptors(I, features)
%   with        I: the input image (a graylevel image)
%        features: the list of interest points as N-by-5 matrix. Each row
%                  represents one interest point with entries
%                  [u v scale(as integer) L-value sigma(as double)]
%  features_clean: the subset of features for which descriptors could be
%                  calculated
%     descriptors: the descriptors calculated, represented as N-by-32
%                  matrix. Each row represents the descriptor for the
%                  corresponding interest point in features_clean

[height, width] = size(I);
[Gu, Gv] = gradient (double(I));  % calculate the graylevel gradient (Gu, Gv are partial derivatives)
n = size(features,1);     % number of interest points

descriptors = -ones (n, 4*8);  % initalize descriptors with negative numbers (negative numbers indicate invalid entries)

for i=1:n
    % calculate the size of the box around the interest point depending on
    % the sigma value
    v = features(i,1);
    u = features(i,2);
    sigma = features(i,5);
    half_block_size = round(sigma*5);
    
    if ((u-half_block_size<1) || (v-half_block_size<1) || (u+half_block_size>width) || (v+half_block_size>height))
        continue;  % the box extends over the image size, hence do not calculate a descriptor for this interest point and go to the next interest point
    end
    
    descriptor(1:8) = calculate_orientation_histogram (Gu(v-half_block_size:v,u-half_block_size:u), Gv(v-half_block_size:v,u-half_block_size:u));
    descriptor(9:16) = calculate_orientation_histogram (Gu(v-half_block_size:v,u+1:u+half_block_size), Gv(v-half_block_size:v,u+1:u+half_block_size));
    descriptor(17:24) = calculate_orientation_histogram (Gu(v+1:v+half_block_size,u-half_block_size:u), Gv(v+1:v+half_block_size,u-half_block_size:u));
    descriptor(25:32) = calculate_orientation_histogram (Gu(v+1:v+half_block_size,u+1:u+half_block_size), Gv(v+1:v+half_block_size,u+1:u+half_block_size));
    
    % normalize descriptor (in order to get rid of scale)
    s = sqrt(sum(descriptor.^2))+1e-50;   % Euclidean lenght of descriptor; adding 1e-50 in order to avoid division by zero
    descriptors(i,:) = descriptor/s;
end

idx_select = (descriptors(:,1)>-0.5);   % filter features with valid descriptors
features_clean = features (idx_select,:);
descriptors = descriptors (idx_select,:);
end


function [ histogram ] = calculate_orientation_histogram (Gu, Gv) 
% function [ histogram ] = calculate_orientation_histogram (Gu, Gv)
%
% calculates an orientation histogram with 8 bins from gradient
% information of a certain image region provided by Gu, Gv. Gu, Gv are the
% partial derivatives for the respective image area.

histogram = zeros (1, 8);
% ----- Your code here (task 3.2) ------



% ----- -------------------------- ------
end
