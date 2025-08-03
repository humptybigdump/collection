% We load one of the extracted data files, so we have some image data to
% work with.
sourceFile = '../Day1_ImageExtractionDiskSaving/ExtractedStacks/Cond_1/Image_10.mat';
loadStruct = load(sourceFile,'imgStack','imgSize','condName');

%% --- Structure variables

% The loaded file is stored as a structure array. Structure arrays are a
% more complex type of array. They consist of fields, and each field can
% have various types of content.

loadStruct

% By issuing this command, we get a view into the contents of this
% structure array. We can see the different fields. Each field has a name,
% by which it can be addressed. Also, each field can have different types
% of content. In this structure array, we can see the different types for
% each field:

class(loadStruct.condName)
class(loadStruct.imgSize)
class(loadStruct.imgStack)

% If we want to get the content of only one of these fields, we can
% directly address it by the name of the field:

imgStack_cell = loadStruct.imgStack

% We can even directly request a specific element of the array stored in a
% given field:

imgStack = size(loadStruct.imgStack{2})

% We can add fields very conveniently

loadStruct.newField = [1,2,3]

% And we can also directly write into fields

loadStruct.newField(2) = 15


%% --- Denoising approaches

% Another basic we will need today are simple approaches to denosing.
% First, let us get an image matrix that we can denoise.
rawImage = squeeze(loadStruct.imgStack{2}(:,:,12));

figure(1)
subplot(2,3,1)
imagesc(rawImage)
axis tight equal
colormap(gray)
title('Raw image')

% My favorite denoising approach is to use a simple Gaussian filter. Here,
% it is always a trade-off how large you choose the blurring kernel.
gaussBlur_Image = imgaussfilt(rawImage,4);

subplot(2,3,2)
imagesc(gaussBlur_Image)
axis tight equal
colormap(gray)
title('Gaussian blur image, 4 pixels')

% If you blur with too large a kernel, all details in the image are lost.
% This is not a good thing if you want to preserve detail. However, as we
% will see later, this can be very useful for local background removal.
gaussBlur_Image = imgaussfilt(rawImage,20);

subplot(2,3,3)
imagesc(gaussBlur_Image)
axis tight equal
colormap(gray)
title('Gaussian blur image, 20 pixels')

% Here, we try a simple average filter with filter size 3 pixels
average_Image = filter2(fspecial('average',3),rawImage);

subplot(2,3,4)
imagesc(average_Image)
axis tight equal
colormap(gray)
title('Average-filtered image, 3 pixels')


% Another popular filter is the median filter.
median_Image = medfilt2(rawImage,[3,3]);

subplot(2,3,5)
imagesc(median_Image)
axis tight equal
colormap(gray)
title('Median-filtered image, 4 pixels')


% Let us try also an adaptive Wiener filter.
wiener_Image = wiener2(rawImage,[5 5]);

subplot(2,3,6)
imagesc(wiener_Image)
axis tight equal
colormap(gray)
title('Wiener-filtered image, 4 pixels')


%% --- Logical masks – for image segmentation and for criterion-based selection

% Today we will make use of a lot (!) of logical masks. So it is worth
% taking a closer look at these masks. At the core of it, logical masks are
% simply matrices with 0 or 1 as a value.

% Let us start with random-valued matrix:
exampleMatrix = rand(3,4)

% We can turn it into a logical matrix as we apply a logical operation
logicalMatrix = exampleMatrix>0.5

% As we can see, the matrix has a number 1 where the condition is
% fulfilled, and a number 0 where the condition is not fulfilled. We can
% use the same approach to check where in an image we find bright areas,
% labeled by strong fluorescence:

figure(2)
subplot(1,3,1)
imagesc(wiener_Image)
axis tight equal
colormap(gray)
title('Intensity image')

subplot(1,3,2)
imagesc(wiener_Image>mean(wiener_Image(:)))
axis tight equal
colormap(gray)
title('Logical mask')

% Now, that does not look so great yet. So, we can adjust the cut-off
% criterion a bit. This is one of the major tasks today as well, adjusting
% threshold values for object segmentation:

subplot(1,3,3)
imagesc(wiener_Image>1.25.*mean(wiener_Image(:)))
axis tight equal
colormap(gray)
title('Logical mask')

% From this, we can see two important points. First, we can use thresholds
% to create logical masks from intensity images. Second, when we pick the
% treshold right, this can help us detect fluorescence-labeled objects in
% an automated fashion. We will talk about threshold choice quite a bit
% more today.

% Another important point about logical masks is that they can be used to
% pull values out of arrays in a selective fashion. For example, we can see
% how the above mask can help to pull out high intensity pixels.

logicalMask = wiener_Image>1.25.*mean(wiener_Image(:));

% Not selective pulling of all values
noMaskApplied = rawImage(:);
disp('No mask applied')
size(noMaskApplied)
mean(noMaskApplied)

% Selective pulling of values using the logical mask
MaskApplied = rawImage(logicalMask);
disp('Mask applied')
size(MaskApplied)
mean(MaskApplied)