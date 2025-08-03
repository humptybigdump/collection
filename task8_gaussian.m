I = double(imread('imgs/0001.png'));
f = fspecial('gaussian',11,4);
out = imfilter(I,f);
imshow(uint8(out));
