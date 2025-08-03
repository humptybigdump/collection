mirrored_sobel_u = rot90(rot90(int16([1 0 -1; 2 0 -2; 1 0 -1])));
mirrored_sobel_v = mirrored_sobel_u';

img = imread('0001.png');
img = int16(img);

[height, width] = size(img);

result_u = zeros(size(img));
result_v = zeros(size(img));

for r=2:(height - 1)
    for c=2:(width - 1)
        product = img(r-1:r+1,c-1:c+1) .* mirrored_sobel_u;
        result_u(r,c) = sum(product(:));

        product = img(r-1:r+1,c-1:c+1) .* mirrored_sobel_v;
        result_v(r,c) = sum(product(:));
    end
end

montage([uint8(result_u), uint8(result_v)]);
