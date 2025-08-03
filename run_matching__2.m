% run_detection must have been called in advance in order to generate the
% struct I with images and interest points.

for i=1:34  % cycle through all images
    [f d] = extract_sift_light_descriptors (I(i).image, I(i).features);   % extract descriptors for interest point
    I(i).features = f;  % f is a subset of the previous feature set
    I(i).descriptors = d;
    
    if (i>=2)
        % calculate back matches for interest points from image i to image
        % i-1
        [idx_neighbors dist] = nearest_neighbor (I(i).descriptors, I(i-1).descriptors);  % for each interest point in image i, calculate the most similar interest point in image i-1 by comparing their descriptors
        seq = [ 1:size(f,1) ]';
        M = [ seq idx_neighbors dist ];
        idx_select = (dist<0.1);  % select only matches with a small Euclidean distance between the descriptors
        I(i).back_matches = M(idx_select,:);  % store the remaining matches
        visualize_feature_matching (I(i).image, I(i-1).image, I(i).features, I(i-1).features, I(i).back_matches, ['Matches ' num2str(i) '->' num2str(i-1)] );  % visualize matches
    end
end
