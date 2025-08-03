rgb = imread('imgs/0002.png');
gray = rgb2gray(rgb);
montage([rgb, cat(3, gray, gray, gray)]);