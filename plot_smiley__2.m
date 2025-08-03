function [ ] = plot_smiley( I )
%PLOT_SMILEY plot a 20x20 smiley image represented as 400-dimensional 
%row vector
imagesc(col2im(I',[20 20],[20 20], 'distinct')')
end
