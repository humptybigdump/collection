function plot_patches(I1, I2, matches, window_size)
%PLOT_PATCHES Summary of this function goes here
%   Detailed explanation goes here
for i = 1:size(matches,1)
    m = matches(i,:);
    name = ['Patch_[', int2str(m(1)), '/', int2str(m(2)), ']_[', int2str(m(3)), '/', int2str(m(4))];
    figure('Name', name); hold on;
    patch1 = extract_image_patch(I1, m(1:2), window_size);
    patch2 = extract_image_patch(I2, m(3:4), window_size);
    patch = repmat(cat(2,patch1,patch2), [1 1 3]);
    patch = insertShape(patch, 'Circle', [window_size(1) + 1, window_size(2) + 1, 3; 3*window_size(1) + 2, window_size(2) + 1, 3], 'LineWidth', 3);
    imshow(patch);
end
end

