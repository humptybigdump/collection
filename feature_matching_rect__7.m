function matches = feature_matching_rect(I1, I2, window_size)
    features = detect_features(I1, 10);
    num_features = size(features, 1);
    matches = zeros(0, 4);
    for i = 1:num_features
        p = features(i,:);
        m = match_point_rect(I1, I2, p, window_size);
        if (size(m, 1) > 0)
            matches = [matches; p m]; 
        end
    end
end

