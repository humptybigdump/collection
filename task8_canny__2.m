I = imread('imgs/2176.png');
BW = edge(I,'canny',[0.1 0.17]);
imshow(BW)