% Putting the clear all command at the start of a script is good practice.
% It clears all left-over variables from scripts that were previously
% executed. This prevents accidental use of variables that, the next time
% the script is executed, might no longer be available in the Workspace.

clear all

%% --- loading of saved image data

% We first load in one of the files that we extracted previously. We need
% to specify the file name, and then also request that the file be loaded
% into a structure array.
sourceFile = '../Day1_ImageExtractionDiskSaving/ExtractedStacks/Cond_1/Image_10.mat';
loadStruct = load(sourceFile,...
    'imgStack','imgSize','pixelSize','zStepSize');

% To work more easily with the extracted information, we take the fields of
% the loaded structure and assign the to more easily manageable cell and
% vector arrays.
imgStack = loadStruct.imgStack;
imgSize = loadStruct.imgSize;
pixelSize = loadStruct.pixelSize;
zStepSize = loadStruct.zStepSize;

%% --- Nuclear marker review

% We need to decide on a segmentation channel. This is typically the DNA
% channel, but in our case we could also use the Pol II or the nuclear
% actin channel. The DNA channel, if the quality is sufficient, is still
% the best choice in this data set, because both the Pol II and nuclear
% actin are detected at low levels also in the cytoplasm. The DNA channel
% was on the channel number 1, so that is the value we have to assign here.

NucSegChannel = 1;

% During the script, we will make frequent use of plotting commands. In
% this way, we can see whether the processing steps we are taking are
% having the desired effect, and whether the data and arrays we have
% prepared really are what we think they are.

figure(1)
subplot(2,3,1)
imagesc(squeeze(imgStack{NucSegChannel}(:,:,ceil(imgSize(3)./2))))
title('Nuclear marker, unprocessed')
axis tight equal
% The index calculation in the third, z index position is used to pick out
% the central image plane of the 3D stack. This is done by getting the
% overall number of image planes, divding by half, and rounding up (ceil
% command). The rounding up is done in case there is an odd number of image
% planes, and a plane index with non-integer value would be produced. Only
% integer index values are valid.
%
% There is also the funny 'squeeze' command. The reason it is there is that
% the imagesc command strictly expects a 2D array. The array we are working
% with, however, has three dimensions. Even if we restrict the third
% dimension to only one index, the array will still have a third dimension.
% To remove all dimensions that cover only a single index, one can use the
% squeeze command.


%% --- preprocessing to assist with segmentation

% In many cases, the segmentation results can be improved by preprocessing
% of the raw images. A common step is slight blurring of the image, which
% removes noise. Also, local differences in overall brightness can make a
% good segmentation diffcult. That can be taken care of by subtracting the
% image from itself, but after it has gone through a blurring filter with a
% large kernel size.

% Filter radius for the slight blur to remove noise. If set to 0, no
% blurring will be executed.
nuc_segBlurSigma_nucleus = 0.8; % in units of micrometers

% Filter radius for the large blur needed for background removal.
nuc_segBlurSigma_BG_removal = 7.0; % in units of micrometers

% We now pull the channel we want to use for segmentation from the loaded
% data, and make a new array on which we will apply the blurring and
% subtration operations. In this way, we do not manipulate the original
% array, but make a new array used only for this segmentation step.
segImg = imgStack{NucSegChannel};

% In the following case distinction, we use the imgaussfilt command to
% carry out the two blurring steps, and immediately carry out the
% background subtration as well. Note that we use the physical pixel size,
% so that we get the blur radius into actual physical units.
if nuc_segBlurSigma_nucleus>0
    segImg = ...
        + imgaussfilt(segImg,nuc_segBlurSigma_nucleus./pixelSize) ...
        - imgaussfilt(segImg,nuc_segBlurSigma_BG_removal./pixelSize);
else
    segImg = ...
        + segImg ...
        - imgaussfilt(segImg,nuc_segBlurSigma_BG_removal./pixelSize);
end

figure(1)
subplot(2,3,2)
imagesc(squeeze(segImg(:,:,ceil(imgSize(3)./2))))
axis tight equal
title('Blurred and background-subtracted')

%% --- Threshold-based segmentation

% For the segmentation, we will use a global thresholding approach based on
% the intensity histogram. Accordingly, the first thing we will do is to
% calculate that histogram.
[bin_counts,bin_centers] = hist(segImg(:),1000);

% The counts in each of the histogram bins can now be passed to a function
% that calculates the Otso threshold. If you need more info, feel free to
% use "help otsuLimit".
[nuc_seg_thresh,~] = otsuLimit(bin_centers,bin_counts,[0,Inf]);

% We also quickly plot the histogram and resulting threshold, so we get an
% idea how the thresholding performs with respect to the intensity
% distribution in the image.
figure(1)
subplot(2,3,4)
plot(bin_centers,bin_counts,'k-','LineWidth',1)
hold on
plot([1,1].*nuc_seg_thresh,[0,max(bin_counts).*1.05],'r--')
xlabel('Pixel intensity DNA channel')
ylabel('Number of pixels')
legend('Number of pixels','Otsu threshold')
hold off

% Now that we have a threshold in hand, we can actually go ahead and divide
% the image into pixels above and below the threshold value. The result
% will be a mask, or a logical array. Where the intensity is above
% threshold, the value true (or 1) will be assigned. Where the intensity is
% at or below the threshold, the value false (or 0) will be assigned.
NucSegMask = segImg>1.0.*nuc_seg_thresh;

% We could see that in the DNA channel, the nuclei are filled with numerous
% holes. This only means that, in these regions inside the nucleus, low
% levels of DNA are present. It does not necessarily mean that these
% regions are not inside the nucleus. To still catch these regions with the
% segmentation mask, we can use hole-filling algorithms:
fillHolesFlag = 2; % Choose no filling (0), 3D hole-filling (1), or 2D (2)
if fillHolesFlag == 1
    % 3D hole-filling, default
    NucSegMask = imfill(NucSegMask,18,'holes');
elseif fillHolesFlag == 2
    % 2D hole-filling, useful if the stack cuts most nuclei on top or
    % bottom
    NucSegMask = imfill(NucSegMask,8,'holes');
end

subplot(2,3,5)
imagesc(squeeze(NucSegMask(:,:,ceil(imgSize(3)./2))))
axis tight equal
title('Threshold-based segmentation')

% We will finish the segmentation process with an optional erosion step.
% During the blurring step, it can happen that the objects become a little
% bigger than they appear in the unblurred image. To counteract this
% growth-by-blurring, one can use a so-called erosion algorithm. This moves
% in on the mask by a pre-specified distance, thereby removing the outer
% layer of the mask.
nuc_segErosion = 0.5;
if nuc_segErosion>0
    se = strel('disk', ...
        round(nuc_segErosion./pixelSize)); % Two-dimensional erosion disk
    NucSegMask = imerode(NucSegMask,se);
end

subplot(2,3,6)
imagesc(squeeze(NucSegMask(:,:,ceil(imgSize(3)./2))))
axis tight equal
title('Segmentation after erosion')



%% --- Connected component segmentation of nuclei

% The segmentation mask provides information on the level of each pixel. If
% we want information that actually represents objects formed from these
% pixels, we need to assign pixels to objects. A common approach is to use
% a connected components algorithm. This algorithm connects all pixels that
% are direct neighbors into one object.
comps = bwconncomp(NucSegMask,18); % 18 refers to number of 3D neighbors

% We can see how the connected components algorithm assigns different
% indices by plotting with a colorful colormap
figure(2)
subplot(1,3,1)
compPlotMatrix = labelmatrix(comps);
imagesc(compPlotMatrix(:,:,ceil(imgSize(3)./2)))
axis tight equal
colormap(lines)
title('Connected components, unfiltered')

%% --- removal of too small components

% Sometimes, very small objects make their way through the segmentation. We
% can remove these with a minimum volume cut-off.
Nuc_min_vol = 10; % in cubic micrometers
minPixels = Nuc_min_vol./(pixelSize.^2)./zStepSize; % unit conversion

% Calculate number of pixels for each connected component
numPxls = cellfun(@(elmt)numel(elmt),comps.PixelIdxList);

% Logical mask that keeps only components that have minimum volume
keepCompsMask = numPxls>=minPixels;

% The mask is 0/1, so summing up over all values gives the number of
% components that has a value of 1, meaning that fulfill the minimum
% volume. We use this information to update the structure array that holds
% the connected components information.
comps.NumObjects = sum(keepCompsMask);

% We also need to update the field of the structure that holds the location
% of the pixels contained in each component. We can also use the logical
% mask here, in this case directly as an index -- all that is 0 will be
% remove, all that is 1 will be kept.
comps.PixelIdxList = comps.PixelIdxList(keepCompsMask);

% Let us check how that removal operation worked out.
figure(2)
subplot(1,3,2)
compPlotMatrix = labelmatrix(comps);
imagesc(compPlotMatrix(:,:,ceil(imgSize(3)./2)))
axis tight equal
colormap(lines)
title('Connected components, filtered')

%% --- Calculation of nuclei properties for filtering

% Once we have obtained labeled components, it is very easy to calcualte
% various properties for these components. The main function for this is
% regionprops, and we will use the version for 3D data, regionprops3
% The function accepts as input the components structure, optionally an
% image that contains intensity information for one color channel, and
% finally a potentially very long list of object properties that we want to
% obtain from the function. If you want an overview of the properties that
% can be calculated, you can use the help function, or even better check
% the different properties online on the MathWorks homepage.
help regionprops

% We will only extract the shape property Solidity.
props = regionprops3(comps,'Solidity');

% The extracted properties can, again, be retrieved in the form of fields
% of a structure array. We will use them to obtain the solidity (measure of
% how many indentations an object has) and the coefficient of variation of
% the DNA intensity throughout the nuclei (very unstructured DNA has a low
% coefficient of variation).
Solidity_array = [props.Solidity];

% We can now calculate a logical mask to keep only those nuclei that have
% minimum solidity. In this way, we can remove most cells that are 
% undergoing mitosis - their chromosomes are condensed and will show
% relatively low solidity.
Nuc_min_sol = 0.8; % to ensure round nuclei
inclNucInds = Solidity_array>=Nuc_min_sol;

comps.NumObjects = sum(inclNucInds);
comps.PixelIdxList = comps.PixelIdxList(inclNucInds);

numNuclei = comps.NumObjects;

% And we will check on the removal operation again
figure(2)
subplot(1,3,3)
compPlotMatrix = labelmatrix(comps);
imagesc(compPlotMatrix(:,:,ceil(imgSize(3)./2)))
axis tight equal
colormap(lines)
title('Connected components, solidity-filtered')


%% Calculate nuclear and cytoplasmic intensity values

% The next thing is the color channels from which we want to do the
% quantification. Here, we will simply collect all three color channels,
% for DNA, Pol II, and actin.
%
% Target channels for intensity quantification, applied for all objects
quantChannels = [1,2,3];

% One can also blur the color channels before quantification. Especially
% for very noisy data, that option can help. In our case, the data seem
% fine, and we just leave it at 0, so no blurring for any channel.
quantBlurSigma = [0,0,0];
numQuantChannels = numel(quantChannels);

% Along with the intensities inside the cell nuclei, we will also quantifiy
% the intensities in the surrounding cytoplasm. We do not have a
% cytoplasmic marker, that we can use for segmentation of the cytoplasm.
% Accordingly, we have to fall back to the distance method. This method
% basically extends a ring mask around the nucleus, and that ring mask is
% used to quantify intensity in the cytoplasm. The ring mask is put at a
% certain distance away from the nucleus (cytoMask_distance), and has a
% certain thickness (cytoMask_extension).
%
% Inner and outer extension of a nuclear masks to cover cytoplasm
cytoMask_extension = 1.5; % in microns
cytoMask_distance = 1.0; % in microns

% The first thing we will do are the intensity quantifications on the
% per-nucleus level. Let us allocate cell arrays to store the
% quantification for nuclear and cytoplasmic intensity for all color
% channels. Also, we will quantifiy the standard deviation of intensity in
% the nucleus, so we have a measure of the variability of intensities.
nuc_intCell = cell(1,numQuantChannels);
cyto_intCell = cell(1,numQuantChannels);
nuc_stdCell = cell(1,numQuantChannels);

% For loop over all color channels that need to be quantified
for qq = 1:numQuantChannels

    % First, pull the image that we want to quantify from
    quantImg = imgStack{quantChannels(qq)};
    % Now use the regionprops function to calculate the mean intensity, but
    % also the pixel values of individual pixels (needed for the standard
    % deviation calculation) as well as the indices of all pixels that are
    % part of the object (needed for making cytoplasmic ring masks).
    quantProps = regionprops3(comps,quantImg,...
        'MeanIntensity','VoxelIdxList','VoxelValues');
    % The meanintensity inside of the nuclei can be directly stored.
    nuc_intCell{qq} = [quantProps.MeanIntensity];
    % For the standard deviation, we need to work directly with the single
    % pixel values of each nucleus. These values are returned in the form
    % of a cell array, and each element contains the pixel values for one
    % object. So, to calculate the standard deviation, we can use the
    % cellfun construct. It runs over every element of a cell array, and
    % applies a function. The function is defined 'on the fly'. This
    % definition can be done by use of the @ symbol. For example, @std
    % creates on the fly a function that calculates the standard deviation
    % of an input array. If you want to know more about the cellfun
    % construct, use help cellfun
    nuc_stdCell{qq} = cellfun(...
        @std,quantProps.VoxelValues);

    % Now still comes the trickier part, making the cytoplasm ring masks,
    % and using them for segmentation. In fact, in the scope of this
    % practical course, this is a little bit too detailed and nitty gritty,
    % so we will not go into the details of this. What you need to know,
    % the following loop runs over all nuclei that were detected, and there
    % constructs a ring mask, and then uses the ring mask to get the
    % intensity values for that ring mask. The mean of these values is then
    % stored as the mean cytoplasm intensity for that particular cell.
    cyto_intCell{qq} = zeros(numNuclei,1);
    for nn = 1:numNuclei
        cytoMask = false(size(quantImg));
        cytoMask(quantProps.VoxelIdxList{nn}) = true;
        coreMask = cytoMask;
        se = strel('disk',round(cytoMask_extension./pixelSize));
        cytoMask = imdilate(cytoMask,se);
        se = strel('disk',round(cytoMask_distance./pixelSize));
        coreMask = imdilate(coreMask,se);
        cytoMask(coreMask) = false;
        cytoMask = cytoMask & ~NucSegMask;
        cyto_intCell{qq}(nn) = mean(quantImg(cytoMask));
    end
end

%% --- For each nucleus, get objects from the different channels

% We are planning to extract information for two types of objects, Pol II
% clusters and actin bundles. We need to decide on are the objects of
% interest. These are, in this case Pol II Ser5P clusters and actin
% filaments. The according channels are known, they are channels 2 and 3,
% respectively.

% Channels for segmentation
S5P_SegChannel = 2; % Channel used to detect Pol II S5P clusters
Actin_SegChannel = 3; % Channel used to detect Actin clusters

% Parameters for the blurring during image preprocessing
S5P_segBlurSigma_object = 0.05; % in microns
S5P_segBlurSigma_BG_removal = 2.0; % in microns
% Number of standard deviations above background for the robust threshold
S5P_seg_numStdDev = 1.5;

% Blurring and segmentation parameters for the actin bundles
Actin_segBlurSigma_object = 0.05; % in microns
Actin_segBlurSigma_BG_removal = 3.0; % in microns
Actin_seg_numStdDev = 2.0; % number of standard deviations in robust threshold

% Minimum volumes for objects inside the nuclei
S5P_minVol = 0.01; % cubic microns
Actin_minVol = 0.005; % cubic microns

% For each of the two object types, we are now pre-allocating
% the required cell arrays. The cell arrays are the same for both obejct
% types, they just get different names.

% Nucleus-wide characteristics
S5P_nucMedianVol = NaN(1,numNuclei);
S5P_perNucCount = NaN(1,numNuclei);
S5P_xyVoxelSize = cell(1,numNuclei);
S5P_zVoxelSize = cell(1,numNuclei);

% Per-object characteristics
S5P_volume = cell(1,numNuclei);
S5P_solidity = cell(1,numNuclei);
S5P_intensity = cell(numQuantChannels,numNuclei);
S5P_nucIntensity = cell(numQuantChannels,numNuclei);

% Nucleus-wide characteristics
Actin_nucMedianVol = zeros(1,numNuclei);
Actin_perNucCount = zeros(1,numNuclei);
Actin_xyVoxelSize = cell(1,numNuclei);
Actin_zVoxelSize = cell(1,numNuclei);

% Per-object characteristics
Actin_volume = cell(1,numNuclei);
Actin_solidity = cell(1,numNuclei);
Actin_intensity = cell(numQuantChannels,numNuclei);
Actin_nucIntensity = cell(numQuantChannels,numNuclei);



% To get started, we will get a few properties of the nuclei that we
% detected. These properties will be needed lateron, when we want to detect
% objects only within a given nucleus, and also normalize intensities
% against levels present in that particular nucleus.
props = regionprops3(comps,imgStack{NucSegChannel},...
    'VoxelIdxList','BoundingBox');

% For loop running over all detected nuclei
for nn = 1:numNuclei

    % The bounding box refers to indices that reach exactly around the
    % pixels that contain the object. So, if we want to process the object
    % more efficiently, it is helpful to focus specifically into that
    % bounding box. We do this by pulling out subimages from within the
    % bounding box below, which we will use for the segmentation.
    boxArray = props.BoundingBox(nn,:);

    S5P_subImage = imgStack{S5P_SegChannel}(...
        boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
        boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
        boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);

    Actin_subImage = imgStack{Actin_SegChannel}(...
        boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
        boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
        boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);

    figure(3)

    centerPlaneInd = ceil(boxArray(6).*0.5);

    subplot(3,3,2)
    imagesc(squeeze(S5P_subImage(:,:,centerPlaneInd)))
    axis tight equal
    set(gca,'Colormap',gray)

    subplot(3,3,3)
    imagesc(squeeze(Actin_subImage(:,:,centerPlaneInd)))
    axis tight equal
    set(gca,'Colormap',gray)


    % Within these subimages, we again carry out blurring and background
    % subtraction, exactly as we did for the nuclei segmentation earlier.
    S5P_subImage = ...
        + imgaussfilt(S5P_subImage,S5P_segBlurSigma_object./pixelSize) ...
        - imgaussfilt(S5P_subImage,S5P_segBlurSigma_BG_removal./pixelSize);
    Actin_subImage = ...
        + imgaussfilt(Actin_subImage,Actin_segBlurSigma_object./pixelSize) ...
        - imgaussfilt(Actin_subImage,Actin_segBlurSigma_BG_removal./pixelSize);
    
    % To segment objects only within the nucleus, and not throughout the
    % entire bounding box, we still need to create a mask that covers only
    % the nucleus. We do this by first making a completely false mask.
    NucMask = false(imgSize);

    % Within this mask, we assign only the pixels for this nucleus the
    % value true.
    NucMask(props.VoxelIdxList{nn}) = true;

    % This full-size image now is cut to the same subimage as the two
    % images above, which will be used for segmentation of Pol II clusters
    % and actin bundles.
    NucMask_subImage = NucMask(...
        boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
        boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
        boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);

    % We are now equipped with the necessary subimages and the single
    % nucleus submask to do all the required work inside only the small
    % bounding box. The first task will be the segmentation of the two
    % types of objects. The objects we want to segement can be expected to
    % be very small in comparison to the overall nucleus volume. In this
    % situation, a histogram-based thresholding approach does not work
    % well. Instead, we will calculate the background mean intensity, the
    % variability in the background in the form of a standard deviation,
    % and then set a threshold a few standard deviations outside that
    % typical range of variability. This approach is also called robust
    % background thresholding.
    
    % In concrete terms, we pull all intensity values from within the
    % nucleus. To this end, we use the nucleus subimage mask.
    seg_intensities = S5P_subImage(NucMask_subImage);

    % Now we calcuate the mean and standard deviation.
    seg_mean = mean(seg_intensities);
    seg_std = std(seg_intensities);

    % Finally, we choose the threshold so that we are a few standard
    % deviations above the background. This allows us to robustly detect
    % only objects above the background intensity levels. Note that we are
    % additionally multiplying with the subimage mask. The subimage mask
    % has the value 0 everywhere outside the nucleus. Accordingly, objects
    % that are above threshold outside the nucleus will be set to 0, and
    % thus not detected by the logical mask contained in S5P_mask.
    S5P_mask = (S5P_subImage.*NucMask_subImage)...
        >(seg_mean+S5P_seg_numStdDev.*seg_std);

    % Here, we do the same for the actin bundles.
    seg_intensities = Actin_subImage(NucMask_subImage);
    seg_mean = mean(seg_intensities);
    seg_std = std(seg_intensities);
    Actin_mask = (Actin_subImage.*NucMask_subImage)...
        >(seg_mean+Actin_seg_numStdDev.*seg_std);

    % This odd expression just makes sure that we always have a 3D array.
    % Sometimes MatLab is inconsistent, and accidentally makes the 3D
    % arrays with a single z section into 2D arrays. This makes them back
    % into 3D arrays.
    subImgSize = size(S5P_subImage);
    if numel(subImgSize)==2
        subImgSize(3)=1;
    end

    % Remove Pol II components that are too small
    S5P_comps = bwconncomp(S5P_mask,18);
    S5P_numPxls = cellfun(@(elmt)numel(elmt),S5P_comps.PixelIdxList);
    minPixels = S5P_minVol./(pixelSize.^2)./zStepSize;
    S5P_comps.NumObjects = sum(S5P_numPxls>minPixels);
    S5P_comps.PixelIdxList = S5P_comps.PixelIdxList(S5P_numPxls>minPixels);
    S5P_numPxls = cellfun(@(elmt)numel(elmt),S5P_comps.PixelIdxList);

    % Remove actin components that are too small
    Actin_comps = bwconncomp(Actin_mask,18);
    Actin_numPxls = cellfun(@(elmt)numel(elmt),Actin_comps.PixelIdxList);
    minPixels = Actin_minVol./(pixelSize.^2)./zStepSize;
    Actin_comps.NumObjects = sum(Actin_numPxls>minPixels);
    Actin_comps.PixelIdxList = Actin_comps.PixelIdxList(Actin_numPxls>minPixels);
    Actin_numPxls = cellfun(@(elmt)numel(elmt),Actin_comps.PixelIdxList);
    
    if S5P_comps.NumObjects>0 && Actin_comps.NumObjects>0

        % This if-clause still needs to be split into the two
        % separate object classes. There is no reason to not
        % analyze one object class because the other one is not
        % detectable

        
        % Now, we will check the segmentation masks. Checking these masks
        % against the raw and the preprocessed data will allow us to adjust
        % the image analysis paramters until we get useful results.
        figure(3)

        centerPlaneInd = ceil(boxArray(6).*0.5);
        
        subplot(3,3,4)
        imagesc(squeeze(NucMask_subImage(:,:,centerPlaneInd)))
        axis tight equal
        set(gca,'Colormap',gray)

        subplot(3,3,5)
        imagesc(squeeze(S5P_subImage(:,:,centerPlaneInd)))
        axis tight equal
        set(gca,'Colormap',gray)

        subplot(3,3,6)
        imagesc(squeeze(Actin_subImage(:,:,centerPlaneInd)))
        axis tight equal
        set(gca,'Colormap',gray)

        subplot(3,3,8)
        LL = labelmatrix(S5P_comps);
        imagesc(squeeze(LL(:,:,centerPlaneInd)))
        axis tight equal
        set(gca,'Colormap',lines)

        subplot(3,3,9)
        LL = labelmatrix(Actin_comps);
        imagesc(squeeze(LL(:,:,centerPlaneInd)))
        axis tight equal
        set(gca,'Colormap',lines)

        % This command ensures that we get to see each nucleus segmentation
        % results, and only continue once we press a button.
        waitforbuttonpress


        % This for loop carries out the intensity calculation for both
        % object types, and for all requested color channels. The loop
        % itself runs over the color channels. Fpr each color channel, the
        % subimage is obtained, and used for the quantification for both
        % object types.

        for qq = 1:numQuantChannels

            % As above, we pull out the subimage that fits into the
            % bounding box.
            channelInd = quantChannels(qq);
            quant_subImage = imgStack{channelInd}(...
                boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
                boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
                boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);

            % Optional blurring step before the intensity quantification.
            % If blurring with zero radius is specified, this is skipped.
            if quantBlurSigma(qq)>0
                quant_subImage = imgaussfilt(quant_subImage,...
                    quantBlurSigma(qq)./pixelSize);
            end

            % For normalization, we also extract the whole-nucleus median
            % for this colro channel.
            Quant_nucleusMedian = ...
                median(quant_subImage(NucMask_subImage));

            % Calculation of intensity properties for the Pol II Ser5P
            % clusters. We use the regionprops3 function for the components
            % that represent Pol II clusters. In combination with the
            % intensity subimage, this allows us to extract all the
            % intensity values of the pixels within each object. Based on
            % these intensity values, we can calculate the intensity
            % median. This median is also normalized by the whole-nucleus
            % median. This normalization corrects for labeling differences,
            % but also brings the values to a scale that is relative to the
            % background.
            S5P_quant_props = regionprops3(...
                S5P_comps,quant_subImage,'VoxelValues');
            Quant_ClusterMedian = cellfun(...
                @(vals)median(vals),S5P_quant_props.VoxelValues);
            S5P_intensity{qq,nn} = ...
                Quant_ClusterMedian./Quant_nucleusMedian;
            % We also store the nucleus intensity for every object that is
            % detected in a given nucleus. Lateron, this makes it much
            % easier when objects need to be sorted on the basis of
            % whole-nucleus intensities.
            S5P_nucIntensity{qq,nn} = ...
                ones(size(S5P_intensity{qq,nn})) ...
                .*nuc_intCell{qq}(nn);

            % Same quantification for nuclear actin.
            Actin_quant_props = regionprops3(...
                Actin_comps,quant_subImage,'VoxelValues');
            Quant_ClusterMedian = cellfun(...
                @(vals)median(vals),Actin_quant_props.VoxelValues);
            Actin_intensity{qq,nn} = ...
                Quant_ClusterMedian./Quant_nucleusMedian;
            Actin_nucIntensity{qq,nn} = ...
                ones(size(Actin_intensity{qq,nn})) ...
                .*nuc_intCell{qq}(nn);

        end


        % In addition to intensity characteristics, we also obtain shape
        % characteristics. Most of these are provided nicely by the
        % regionprops3 function, as seen below.
        S5P_props = regionprops3(S5P_comps,...
            'Volume','Solidity','Centroid','BoundingBox');

        % Storing of the obtained characteristics again happens by
        % converting from cell array contents into vector array contents.
        % The volume is given as number of pixels, and still needs to be
        % scaled into physical units.
        S5P_Volume_array = ...
            [S5P_props.Volume].*pixelSize.^2.*zStepSize;
        S5P_Solidity_array = [S5P_props.Solidity];
        S5P_Centroid_array = ...
            S5P_props.Centroid.*[pixelSize,pixelSize,zStepSize];

        Actin_props = regionprops3(Actin_comps,...
            'Volume','Solidity','Centroid','BoundingBox');

        Actin_Volume_array = ...
            [Actin_props.Volume].*pixelSize.^2.*zStepSize;
        Actin_Solidity_array = [Actin_props.Solidity];
        Actin_Centroid_array = ...
            Actin_props.Centroid.*[pixelSize,pixelSize,zStepSize];

        % --- all commands below are used to systematicall store the
        % obtained arrays and characteristics into cell container arrays
        % that will hold the information across the different nucleis.
        
        S5P_volume{nn} = S5P_Volume_array;
        S5P_solidity{nn} = S5P_Solidity_array;

        S5P_perNucCount(nn) = numel(S5P_Volume_array);
        Actin_perNucCount(nn) = numel(Actin_Volume_array);

        S5P_nucMedianVol(nn) = median(S5P_Volume_array);
        Actin_nucMedianVol(nn) = median(Actin_Volume_array);

        S5P_xyVoxelSize{nn} = ...
            pixelSize.*ones(size(S5P_Volume_array));
        S5P_zVoxelSize{nn} = ...
            zStepSize.*ones(size(S5P_Volume_array));

        Actin_xyVoxelSize{nn} = ...
            pixelSize.*ones(size(Actin_Volume_array));
        Actin_zVoxelSize{nn} = ...
            zStepSize.*ones(size(Actin_Volume_array));

        Actin_Elongation_array = ...
            zeros(size(Actin_Solidity_array));
        Actin_Slices_cell = ...
            cell(size(Actin_Solidity_array));

        Actin_volume{nn} = Actin_Volume_array;
        Actin_solidity{nn} = Actin_Solidity_array;

    else

        % This section is used in case no Pol II clusters or no actin
        % bundles are detected. In this case, empty arrays are assigned,
        % indicating that no objects were detected for this nucleus.

        S5P_xyVoxelSize{nn} = [];
        S5P_zVoxelSize{nn} = [];
        Actin_xyVoxelSize{nn} = [];
        Actin_zVoxelSize{nn} = [];

        S5P_volume{nn} = [];
        S5P_solidity{nn} = [];

        Actin_volume{nn} = [];
        Actin_solidity{nn} = [];

        for qq = 1:numQuantChannels
            S5P_intensity{qq,nn} = [];
            S5P_nucIntensity{qq,nn} = [];
            Actin_intensity{qq,nn} = [];
            Actin_nucIntensity{qq,nn} = [];
        end
    end
end