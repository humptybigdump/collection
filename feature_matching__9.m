function matches = feature_matching(I1, I2, search_size, window_size)
    features = detect_features(I1, 10);         % detect the best 10 features in the image
    num_features = size(features, 1);
    matches = zeros(0, 4);
    for i = 1:num_features
        p = features(i,:);
        m = match_point(I1, I2, p, search_size, window_size);     % match each feature to its correspondence in I2
        if (size(m, 1) > 0)
            matches = [matches; p m]; 
        end
    end
end

