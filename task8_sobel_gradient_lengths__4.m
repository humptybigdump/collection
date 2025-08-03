h = fspecial('sobel');
v = h';

gray_img = int16(imread('0001.png'));
[height, width] = size(gray_img);

filtered = imfilter(gray_img, v);
 imshow(filtered, [])

sobel_u = double(imfilter(gray_img, h));
sobel_v = double(imfilter(gray_img, v));

% one way 
derivative_img = zeros(size(sobel_u));
for x=1:height
    for y=1:width        
        derivative_img(x,y) =  euclidean_length([sobel_u(x,y), sobel_v(x,y)]);         
    end
end

% or the other
% derivative_img=sqrt(sobel_u.^2+sobel_v.^2);

% Viz 
imshow((derivative_img), []);
