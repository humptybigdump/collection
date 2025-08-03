function visualize_feature_matching(I1, I2, matches, name)
% VISUALIZE_FEATURE_MATCHING Visualizes the feature matches for two images
% I1 and I2.
    figure('Name', name); hold on;
    [height1, width1] = size(I1);
    [height2, width2] = size(I2);
    if (width1 ~= width2 || height1 ~= height2)
       error('Mismatching image sizes provided'); 
    end
    I = 255*zeros(height1 + height2, width1, 3, 'uint8');
    I(1:height1,:,:) = repmat(I1, [1 1 3]);
    I(height1+1:height1+height2,:,:) = repmat(I2, [1 1 3]);
    for i = 1:size(matches, 1)
        p1 = matches(i, 1:2);
        p2 = matches(i, 3:4) + [0 height1];
        I = insertShape(I, 'Line', [p1(1), p1(2), p2(1), p2(2)], 'LineWidth', 2, 'Color', 'red');
        I = insertShape(I, 'Circle', [p1(1), p1(2), 3; p2(1), p2(2), 3], 'LineWidth', 3);
    end
    imshow(I);
end

