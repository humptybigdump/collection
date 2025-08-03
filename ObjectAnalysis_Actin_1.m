clear all

% Specify the directory that contains the extracted files from the last
% step, where you extracted from the raw files obtained from the microscope
sourceDirectory = './ExtractedStacks/**/';

% Channels for segmentation
NucSegChannel = 1; % Channel used to detect nuclei
S5P_SegChannel = 2; % Channel used to detect Pol II S5P clusters
Actin_SegChannel = 3; % Channel used to detect Actin clusters

% Save images of the clusters
ImgSquareExtension = 0; % pixels for cut-out image extension, set 0 for no images
% Which image channels to store in example images
storeImgChannels = [];
numStoreChannels = numel(storeImgChannels);

% Target channels for intensity quantification, applied for all objects
quantChannels = [1,2,3];
quantBlurSigma = [0,0,0];

nuc_segBlurSigma_nucleus = 0.8; % in microns
nuc_segBlurSigma_BG_removal = 7.0; % in microns
nuc_segErosion = 1.5; % range of erosion (in microns) to avoid margin effects
% Use topological operation to fill holes in the nuclei segmentation masks?
% Default: 3D hole-filing, set flag to value 1
% 2D hole-filling, usefil if the stack cuts nuclei on top or bottom, so
% that 3D hole-filling does not work, set flag value to 2
% To deactivate hole-filling, set flag to any other number
fillHolesFlag = 1;

% Minimum volume of nuclei, typical ranges for a full nucieus 10-100 cubic
% microns of volume, so set a cut-off oof 10 or 30 or so
Nuc_min_vol = 10; % cubic microns
Nuc_min_sol = 0.8; % to ensure round nuclei
Nuc_min_CoV = 0.0; % to ensure transcriptionally active foci

% Inner and outer extension of a nuclear masks to cover cytoplasm
cytoMask_extension = 1.5; % in microns
cytoMask_distance = 1.0; % in microns

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

% end of analysis parameter section, do not change anything else in
% this section, all necessary parameters are listed above


%% --- analysis procedure begins here

% Recursive directory search to find all .mat files we previously saved
% during extraction
listing = rdir([sourceDirectory,'*Image*.mat']);
numFiles = numel(listing);

% Condition index retrieval
condInds = [];
condNames = {};
for ff = 1:numFiles
	thisFilePath = listing(ff).name;
	thisCondInd = load(thisFilePath,'condInd');
	thisCondInd = thisCondInd.condInd;
	condInds = [condInds,thisCondInd];
	
    thisCondName = load(thisFilePath,'condName');
	thisCondName = thisCondName.condName;
	condNames = [condNames,thisCondName];
end


%% --- analyze image stacks one by one

% Flag to log files that were successfully analyzed and contained nuclei
validFileFlag = false(1,numFiles);

% Variables to store properties of nuclei
numNuclei_vec = zeros(1,numFiles);
nuc_intCell = cell(1,numFiles);
cyto_intCell = cell(1,numFiles);
nuc_stdCell = cell(1,numFiles);
nuc_medianVolCell = cell(1,numFiles);
perNuc_countCell = cell(1,numFiles);

% Variable to store the pixel sizes
S5P_xyVoxelSizeCell = cell(1,numFiles);
S5P_zVoxelSizeCell = cell(1,numFiles);
Actin_xyVoxelSizeCell = cell(1,numFiles);
Actin_zVoxelSizeCell = cell(1,numFiles);

% Variables to store properties of objects inside nuclei
S5P_volCell = cell(1,numFiles);
S5P_solCell = cell(1,numFiles);
S5P_eloCell = cell(1,numFiles);
S5P_intCell = cell(1,numFiles);
S5P_nucIntCell = cell(1,numFiles);
S5P_centCell = cell(1,numFiles);
S5P_imgCell = cell(1,numFiles);
S5P_Actin_distCell = cell(1,numFiles);

Actin_volCell = cell(1,numFiles);
Actin_solCell = cell(1,numFiles);
Actin_eloCell = cell(1,numFiles);
Actin_intCell = cell(1,numFiles);
Actin_nucIntCell = cell(1,numFiles);
Actin_centCell = cell(1,numFiles);
Actin_imgCell = cell(1,numFiles);
Actin_S5P_distCell = cell(1,numFiles);

numQuantChannels = numel(quantChannels);

for ff = 1:numFiles
	
	fprintf('Processing file %d of %d\n',ff,numFiles)
	
	thisCondInd = condInds(ff);	
	thisFilePath = listing(ff).name;
	
	loadStruct = load(thisFilePath,...
		'imgStack','imgSize','pixelSize','zStepSize');
	imgStack = loadStruct.imgStack;
	imgSize = loadStruct.imgSize;
	pixelSize = loadStruct.pixelSize;
	zStepSize = loadStruct.zStepSize;

	% Nuclei segmentation
	segImg = imgStack{NucSegChannel};
    if nuc_segBlurSigma_nucleus>0
        segImg = ...
            + imgaussfilt(segImg,nuc_segBlurSigma_nucleus./pixelSize) ...
            - imgaussfilt(segImg,nuc_segBlurSigma_BG_removal./pixelSize);
    else
        segImg = ...
            + segImg ...
            - imgaussfilt(segImg,nuc_segBlurSigma_BG_removal./pixelSize);
    end

	[bin_counts,bin_centers] = hist(segImg(:),1000);
	[nuc_seg_thresh,~] = otsuLimit(bin_centers,bin_counts,[0,Inf]);
	NucSegMask = segImg>1.0.*nuc_seg_thresh;
    if fillHolesFlag == 1
        % 3D hole-filling, default
        NucSegMask = imfill(NucSegMask,18,'holes');
    elseif fillHolesFlag == 2
        % 2D hole-filling, useful if the stack cuts most nuclei on top or
        % bottom
        NucSegMask = imfill(NucSegMask,8,'holes');
    end
    if nuc_segErosion>0
        se = strel('disk', ...
            round(nuc_segErosion./pixelSize)); % Two-dimensional erosion disk
        NucSegMask = imerode(NucSegMask,se);
    end
    
    
	subplot(1,3,1)
	imagesc(squeeze(imgStack{NucSegChannel}(:,:,ceil(imgSize(3)./2))))
	axis tight equal
	
	subplot(1,3,2)
	imagesc(squeeze(segImg(:,:,ceil(imgSize(3)./2))))
	axis tight equal

	subplot(1,3,3)
	imagesc(squeeze(NucSegMask(:,:,ceil(imgSize(3)./2))))
	axis tight equal
	
% Uncomment the following two lines, and remove the par in parfor above, if
% you want to check the extracted images one by one
%   fprintf('File name: %s\n',thisFilePath)
% 	waitforbuttonpress
	
	% --- Connected component segmentation of nuclei
	comps = bwconncomp(NucSegMask,18);
	numPxls = cellfun(@(elmt)numel(elmt),comps.PixelIdxList);
	minPixels = Nuc_min_vol./(pixelSize.^2)./zStepSize;
	comps.NumObjects = sum(numPxls>=minPixels);
	comps.PixelIdxList = comps.PixelIdxList(numPxls>=minPixels);
	numPxls = cellfun(@(elmt)numel(elmt),comps.PixelIdxList);
	
	props = regionprops3(comps,imgStack{NucSegChannel},...
		'Solidity','VoxelValues');
	
	Solidity_array = [props.Solidity];
	CoV_array = ...
        cellfun(@(vals)std(vals(:))./mean(vals(:)),...
        props.VoxelValues);
    inclNucInds = Solidity_array>=Nuc_min_sol ...
        & CoV_array>=Nuc_min_CoV;
	comps.NumObjects = sum(Solidity_array>=Nuc_min_sol);
	comps.PixelIdxList = comps.PixelIdxList(Solidity_array>=Nuc_min_sol);
	numPxls = cellfun(@(elmt)numel(elmt),comps.PixelIdxList);
	
	numNuclei = comps.NumObjects;
	numNuclei_vec(ff) = numNuclei;
	
    if comps.NumObjects>0
        
        validFileFlag(ff) = true;
        
        nuc_intCell{ff} = cell(1,numQuantChannels);
        cyto_intCell{ff} = cell(1,numQuantChannels);
        nuc_stdCell{ff} = cell(1,numQuantChannels);
        for qq = 1:numQuantChannels
            quantImg = imgStack{quantChannels(qq)};
            quantProps = regionprops3(comps,quantImg,...
                'MeanIntensity','VoxelIdxList','VoxelValues');
            nuc_intCell{ff}{qq} = [quantProps.MeanIntensity];
            cyto_intCell{ff}{qq} = zeros(numNuclei,1);
            nuc_stdCell{ff}{qq} = cellfun(...
                @std,quantProps.VoxelValues);
            
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
                cyto_intCell{ff}{qq}(nn) = mean(quantImg(cytoMask));
            end
        end
        
        
        props = regionprops3(comps,imgStack{NucSegChannel},...
            'Volume','VoxelValues','Solidity','VoxelIdxList',...
            'BoundingBox');
        
        Volume_array = [props.Volume].*pixelSize.^2.*zStepSize;
        Intensity_array = cellfun(@(vals)median(vals),props.VoxelValues);
        Solidity_array = [props.Solidity];
        
        
        % --- For each nucleus, get objects from the different channels
        
        nuc_medianVolCell{ff} = cell(1,2);
        perNuc_countCell{ff} = cell(1,2);
        for qq = 1:2
            nuc_medianVolCell{ff}{qq} = zeros(numNuclei,1);
            perNuc_countCell{ff}{qq} = zeros(numNuclei,1);
        end
        
        S5P_xyVoxelSize = cell(1,numNuclei);
        S5P_zVoxelSize = cell(1,numNuclei);
        
        Actin_xyVoxelSize = cell(1,numNuclei);
        Actin_zVoxelSize = cell(1,numNuclei);
        
        S5P_volume = cell(1,numNuclei);
        S5P_solidity = cell(1,numNuclei);
        S5P_elongation = cell(1,numNuclei);
        S5P_centralSlices_store = cell(1,numNuclei);
        S5P_intensity = cell(numQuantChannels,numNuclei);
        S5P_nucIntensity = cell(numQuantChannels,numNuclei);
        S5P_cent_store = cell(1,numNuclei);
        S5P_Actin_dist = cell(1,numNuclei);
        
        Actin_volume = cell(1,numNuclei);
        Actin_solidity = cell(1,numNuclei);
        Actin_elongation = cell(1,numNuclei);
        Actin_centralSlices_store = cell(1,numNuclei);
        Actin_intensity = cell(numQuantChannels,numNuclei);
        Actin_nucIntensity = cell(numQuantChannels,numNuclei);
        Actin_cent_store = cell(1,numNuclei);
        Actin_S5P_dist = cell(1,numNuclei);

        for nn = 1:numNuclei
            
            boxArray = props.BoundingBox(nn,:);
            
            S5P_subImage = imgStack{S5P_SegChannel}(...
                boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
                boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
                boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);
            
            Actin_subImage = imgStack{Actin_SegChannel}(...
                boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
                boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
                boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);
            
            S5P_subImage = ...
                + imgaussfilt(S5P_subImage,S5P_segBlurSigma_object./pixelSize) ...
                - imgaussfilt(S5P_subImage,S5P_segBlurSigma_BG_removal./pixelSize);
            Actin_subImage = ...
                + imgaussfilt(Actin_subImage,Actin_segBlurSigma_object./pixelSize) ...
                - imgaussfilt(Actin_subImage,Actin_segBlurSigma_BG_removal./pixelSize);
            
            NucMask = false(imgSize);
            NucMask(props.VoxelIdxList{nn}) = true;
            NucMask_subImage = NucMask(...
                boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
                boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
                boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);
            
            
            seg_intensities = S5P_subImage(NucMask_subImage);
            seg_mean = mean(seg_intensities);
            seg_std = std(seg_intensities);
            S5P_mask = (S5P_subImage.*NucMask_subImage)...
                >(seg_mean+S5P_seg_numStdDev.*seg_std);
            
            seg_intensities = Actin_subImage(NucMask_subImage);
            seg_mean = mean(seg_intensities);
            seg_std = std(seg_intensities);
            Actin_mask = (Actin_subImage.*NucMask_subImage)...
                >(seg_mean+Actin_seg_numStdDev.*seg_std);
            
            subImgSize = size(S5P_subImage);
            if numel(subImgSize)==2
                subImgSize(3)=1;
            end
            
            % For storage of example images
            if numStoreChannels > 0
                store_subImages = cell(1,numStoreChannels);
                for color = 1:numStoreChannels
                    store_subImages{color} = imgStack{storeImgChannels(color)}(...
                        boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
                        boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
                        boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);
                    nuc_intensities = ...
                        store_subImages{color}(NucMask_subImage);
                    store_subImages{color} = ...
                        store_subImages{color}./median(nuc_intensities(:));
                end
            end
            
            S5P_comps = bwconncomp(S5P_mask,18);
            S5P_numPxls = cellfun(@(elmt)numel(elmt),S5P_comps.PixelIdxList);
            minPixels = S5P_minVol./(pixelSize.^2)./zStepSize;
            S5P_comps.NumObjects = sum(S5P_numPxls>minPixels);
            S5P_comps.PixelIdxList = S5P_comps.PixelIdxList(S5P_numPxls>minPixels);
            S5P_numPxls = cellfun(@(elmt)numel(elmt),S5P_comps.PixelIdxList);
            
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
                                
                LL = labelmatrix(S5P_comps);
                
                subplot(2,2,1)
                centerPlaneInd = round(boxArray(6).*0.5);
                imagesc(squeeze(S5P_subImage(:,:,centerPlaneInd)))
                axis tight equal
                set(gca,'Colormap',gray)
                
                subplot(2,2,2)
                imagesc(squeeze(Actin_subImage(:,:,centerPlaneInd)))
                axis tight equal
                set(gca,'Colormap',gray)
                
                subplot(2,2,3)
                imagesc(squeeze(LL(:,:,centerPlaneInd)))
                axis tight equal
                set(gca,'Colormap',lines)
                
                subplot(2,2,4)
                imagesc(squeeze(Actin_mask(:,:,centerPlaneInd)))
                axis tight equal
                set(gca,'Colormap',gray)
                
                % Uncomment this waitforbuttonpress command to see the
                % segmentation results for the two types of foci
                % waitforbuttonpress
                
                
                
                
                S5P_props = regionprops3(S5P_comps,S5P_subImage,...
                    'Volume','Solidity',...
                    'Centroid','Image','BoundingBox');
                
                S5P_Volume_array = ...
                    [S5P_props.Volume].*pixelSize.^2.*zStepSize;
                S5P_Solidity_array = [S5P_props.Solidity];
                S5P_Centroid_array = ...
                    S5P_props.Centroid.*[pixelSize,pixelSize,zStepSize];
                
                Actin_props = regionprops3(Actin_comps,Actin_subImage,...
                    'Volume','Solidity',...
                    'Centroid','Image','BoundingBox');
                
                Actin_Volume_array = ...
                    [Actin_props.Volume].*pixelSize.^2.*zStepSize;
                Actin_Solidity_array = [Actin_props.Solidity];
                Actin_Centroid_array = ...
                    Actin_props.Centroid.*[pixelSize,pixelSize,zStepSize];
                
                perNuc_countCell{ff}{1}(nn) = numel(S5P_Volume_array);
                perNuc_countCell{ff}{2}(nn) = numel(Actin_Volume_array);
                
                nuc_medianVolCell{ff}{1}(nn) = median(S5P_Volume_array);
                nuc_medianVolCell{ff}{2}(nn) = median(Actin_Volume_array);
                
                S5P_xyVoxelSize{nn} = ...
                    pixelSize.*ones(size(S5P_Volume_array));
                S5P_zVoxelSize{nn} = ...
                    zStepSize.*ones(size(S5P_Volume_array));
                
                Actin_xyVoxelSize{nn} = ...
                    pixelSize.*ones(size(Actin_Volume_array));
                Actin_zVoxelSize{nn} = ...
                    zStepSize.*ones(size(Actin_Volume_array));
                
                
                % --- get cluster central plane and elongation in-plane
                S5P_Elongation_array = ...
                    zeros(size(S5P_Solidity_array));
                S5P_Slices_cell = ...
                    cell(size(S5P_Solidity_array));
                
                for object_nn = 1:numel(S5P_Volume_array)
                    
                    boundingBox = S5P_props.BoundingBox(object_nn,:);
                    thisImage = squeeze(S5P_subImage(...
                        boundingBox(2)+0.5:boundingBox(2)+boundingBox(5)-0.5,...
                        boundingBox(1)+0.5:boundingBox(1)+boundingBox(4)-0.5,...
                        ceil(boundingBox(3)+0.5+0.5.*(boundingBox(6)-1))));
                    thisImage = thisImage-min(thisImage(:));
                    thisImage = thisImage./max(thisImage(:));
                    
                    thisMask = S5P_props.Image(object_nn);
                    if iscell(thisMask)
                        % There is some weird inconsistent behavior here,
                        % sometimes .Image(object_nn) results in a cell
                        % output, sometimes in a matrix output. It is not
                        % clear if there is a system to it.
                        thisMask = thisMask{1};
                    else
                        thisMask = S5P_props.Image;
                    end
                    centerInd = ceil(size(thisMask,3)./2);
                    thisMask = squeeze(thisMask(:,:,centerInd));
                    thisImage((bwperim(thisMask))) = 0;
                    
                    S5P_Slices_cell{object_nn} = cell(1,numStoreChannels+2);
                    centroid_1 = round(S5P_props.Centroid(object_nn,2));
                    centroid_2 = round(S5P_props.Centroid(object_nn,1));
                    center_z = round(S5P_props.Centroid(object_nn,3));
                    img_limits = [...
                        centroid_1-ImgSquareExtension,...
                        centroid_1+ImgSquareExtension,...
                        centroid_2-ImgSquareExtension,...
                        centroid_2+ImgSquareExtension];
                    img_limits = [...
                        max(1,img_limits(1)),...
                        min(subImgSize(1),img_limits(2)),...
                        max(1,img_limits(3)),...
                        min(subImgSize(2),img_limits(4))];
                    
                    for color = 1:numStoreChannels
                        % 						S5P_Slices_cell{object_nn}{color} = ...
                        % 							squeeze(store_subImages{color}(:,:,...
                        % 							ceil(boundingBox(3)+0.5+0.5.*(boundingBox(6)-1))));
                        
                        S5P_Slices_cell{object_nn}{color} = ...
                            squeeze(store_subImages{color}(...
                            img_limits(1):img_limits(2),...
                            img_limits(3):img_limits(4),...
                            center_z));
                        
                    end
                    S5P_Slices_cell{object_nn}{numStoreChannels+1} = ...
                        squeeze(LL(...
                        img_limits(1):img_limits(2),...
                        img_limits(3):img_limits(4),...
                        center_z));
                    S5P_Slices_cell{object_nn}{numStoreChannels+2} = ...
                        squeeze(Actin_mask(...
                        img_limits(1):img_limits(2),...
                        img_limits(3):img_limits(4),...
                        center_z));
                    
                    if sum(sum(uint8(thisMask)))>0
                        thisProps = regionprops(uint8(thisMask),...
                            'MajorAxisLength','MinorAxisLength');
                        S5P_Elongation_array(object_nn) = ...
                            thisProps.MajorAxisLength./thisProps.MinorAxisLength;
                    else
                        S5P_Elongation_array(object_nn) = NaN;
                    end
                end
                S5P_volume{nn} = S5P_Volume_array;
                S5P_solidity{nn} = S5P_Solidity_array;
                S5P_elongation{nn} = S5P_Elongation_array;
                S5P_cent_store{nn} = S5P_Centroid_array;
                S5P_centralSlices_store{nn} = S5P_Slices_cell;
                
                
                
                Actin_Elongation_array = ...
                    zeros(size(Actin_Solidity_array));
                Actin_Slices_cell = ...
                    cell(size(Actin_Solidity_array));
                
                for object_nn = 1:numel(Actin_Volume_array)
                    
                    boundingBox = Actin_props.BoundingBox(object_nn,:);
                    thisImage = squeeze(Actin_subImage(...
                        boundingBox(2)+0.5:boundingBox(2)+boundingBox(5)-0.5,...
                        boundingBox(1)+0.5:boundingBox(1)+boundingBox(4)-0.5,...
                        ceil(boundingBox(3)+0.5+0.5.*(boundingBox(6)-1))));
                    thisImage = thisImage-min(thisImage(:));
                    thisImage = thisImage./max(thisImage(:));
                    
                    thisMask = Actin_props.Image(object_nn);
                    if iscell(thisMask)
                        % There is some weird inconsistent behavior here,
                        % sometimes .Image(object_nn) results in a cell
                        % output, sometimes in a matrix output. It is not
                        % clear if there is a system to it.
                        thisMask = thisMask{1};
                    else
                        thisMask = Actin_props.Image;
                    end
                    centerInd = ceil(size(thisMask,3)./2);
                    thisMask = squeeze(thisMask(:,:,centerInd));
                    thisImage((bwperim(thisMask))) = 0;
                    
                    centroid_1 = round(Actin_props.Centroid(object_nn,2));
                    centroid_2 = round(Actin_props.Centroid(object_nn,1));
                    center_z = round(Actin_props.Centroid(object_nn,3));
                    img_limits = [...
                        centroid_1-ImgSquareExtension,...
                        centroid_1+ImgSquareExtension,...
                        centroid_2-ImgSquareExtension,...
                        centroid_2+ImgSquareExtension];
                    img_limits = [...
                        max(1,img_limits(1)),...
                        min(subImgSize(1),img_limits(2)),...
                        max(1,img_limits(3)),...
                        min(subImgSize(2),img_limits(4))];
                    
                    for color = 1:numStoreChannels
                        Actin_Slices_cell{object_nn}{color} = ...
                            squeeze(store_subImages{color}(...
                            img_limits(1):img_limits(2),...
                            img_limits(3):img_limits(4),...
                            center_z));
                    end
                    Actin_Slices_cell{object_nn}{numStoreChannels+1} = ...
                        squeeze(S5P_mask(...
                        img_limits(1):img_limits(2),...
                        img_limits(3):img_limits(4),...
                        center_z));
                    Actin_Slices_cell{object_nn}{numStoreChannels+2} = ...
                        squeeze(Actin_mask(...
                        img_limits(1):img_limits(2),...
                        img_limits(3):img_limits(4),...
                        center_z));
                    
                    if sum(sum(uint8(thisMask)))>0
                        thisProps = regionprops(uint8(thisMask),...
                            'MajorAxisLength','MinorAxisLength');
                        Actin_Elongation_array(object_nn) = ...
                            thisProps.MajorAxisLength./thisProps.MinorAxisLength;
                    else
                        Actin_Elongation_array(object_nn) = NaN;
                    end
                    
                end
                Actin_volume{nn} = Actin_Volume_array;
                Actin_solidity{nn} = Actin_Solidity_array;
                Actin_elongation{nn} = Actin_Elongation_array;
                Actin_cent_store{nn} = Actin_Centroid_array;
                Actin_centralSlices_store{nn} = Actin_Slices_cell;
                
                
                % --- quantification for all target channels
                for qq = 1:numQuantChannels
                    
                    channelInd = quantChannels(qq);
                    quant_subImage = imgStack{channelInd}(...
                        boxArray(2)+0.5:boxArray(2)+boxArray(5)-0.5,...
                        boxArray(1)+0.5:boxArray(1)+boxArray(4)-0.5,...
                        boxArray(3)+0.5:boxArray(3)+boxArray(6)-0.5);
                    
                    if quantBlurSigma(qq)>0
                        quant_subImage = imgaussfilt(quant_subImage,...
                            quantBlurSigma(qq)./pixelSize);
                    end
                    
                    Quant_nucleusMedian = ...
                        median(quant_subImage(NucMask_subImage));
                    
                    S5P_quant_props = regionprops3(...
                        S5P_comps,quant_subImage,'VoxelValues');
                    Quant_ClusterMedian = cellfun(...
                        @(vals)median(vals),S5P_quant_props.VoxelValues);
                    S5P_intensity{qq,nn} = ...
                        Quant_ClusterMedian./Quant_nucleusMedian;
                    S5P_nucIntensity{qq,nn} = ...
                        ones(size(S5P_intensity{qq,nn})) ...
                        .*nuc_intCell{ff}{qq}(nn);
                    
                    Actin_quant_props = regionprops3(...
                        Actin_comps,quant_subImage,'VoxelValues');
                    Quant_ClusterMedian = cellfun(...
                        @(vals)median(vals),Actin_quant_props.VoxelValues);
                    Actin_intensity{qq,nn} = ...
                        Quant_ClusterMedian./Quant_nucleusMedian;
                    Actin_nucIntensity{qq,nn} = ...
                        ones(size(Actin_intensity{qq,nn})) ...
                        .*nuc_intCell{ff}{qq}(nn);
                    
                end

                % ---
                % Beginning nearest-neighbor distance calculation
                %
                % Placeholder for the calculation of the distance to the
                % nearest object in the other object set

                % These two arrays contain the two sets of object
                % positions, which you need to calculate the pair-wise
                % distances, and onwards from there, also the distance ot
                % the nearest neighbour in the other set of objects. Note
                % that these arrays contain the centroid positions already
                % in units of micrometers - so you can directly calculate
                % distances with actual physical meaning!
                S5P_Centroid_array;
                Actin_Centroid_array;


                % These expressions must be replaced by you with
                % expressions that actually calculate the nearest-neighbor
                % distances
                S5P_Actin_dist{nn} = [];
                Actin_S5P_dist{nn} = [];

                % End nearest-neighbor distance calculation
                % ---
                
            else
                
                S5P_xyVoxelSize{nn} = [];
                S5P_zVoxelSize{nn} = [];
                Actin_xyVoxelSize{nn} = [];
                Actin_zVoxelSize{nn} = [];
                
                S5P_volume{nn} = [];
                S5P_solidity{nn} = [];
                S5P_elongation{nn} = [];
                S5P_centralSlices_store{nn} = {};
                S5P_cent_store{nn} = [];
                S5P_Actin_dist{nn} = [];
                
                Actin_volume{nn} = [];
                Actin_solidity{nn} = [];
                Actin_elongation{nn} = [];
                Actin_centralSlices_store{nn} = {};
                Actin_cent_store{nn} = [];
                Actin_S5P_dist{nn} = [];
                
                for qq = 1:numQuantChannels
                    S5P_intensity{qq,nn} = [];
                    S5P_nucIntensity{qq,nn} = [];
                    Actin_intensity{qq,nn} = [];
                    Actin_nucIntensity{qq,nn} = [];
                end
                
            end
            
        end
        
        S5P_xyVoxelSizeCell{ff} = vertcat(S5P_xyVoxelSize{:});
        S5P_zVoxelSizeCell{ff} = vertcat(S5P_zVoxelSize{:});
        Actin_xyVoxelSizeCell{ff} = vertcat(Actin_xyVoxelSize{:});
        Actin_zVoxelSizeCell{ff} = vertcat(Actin_zVoxelSize{:});
        
        S5P_volCell{ff} = vertcat(S5P_volume{:});
        S5P_solCell{ff} = vertcat(S5P_solidity{:});
        S5P_eloCell{ff} = vertcat(S5P_elongation{:});
        S5P_imgCell{ff} = vertcat(S5P_centralSlices_store{:});
        S5P_centCell{ff} = vertcat(S5P_cent_store{:});
        S5P_Actin_distCell{ff} = vertcat(S5P_Actin_dist{:});
        S5P_intCell{ff} = cell(1,numQuantChannels);
        S5P_nucIntCell{ff} = cell(1,numQuantChannels);
        for qq = 1:numQuantChannels
            S5P_intCell{ff}{qq} = vertcat(S5P_intensity{qq,:});
            S5P_nucIntCell{ff}{qq} = vertcat(S5P_nucIntensity{qq,:});
        end
        
        Actin_volCell{ff} = vertcat(Actin_volume{:});
        Actin_solCell{ff} = vertcat(Actin_solidity{:});
        Actin_eloCell{ff} = vertcat(Actin_elongation{:});
        Actin_imgCell{ff} = vertcat(Actin_centralSlices_store{:});
        Actin_centCell{ff} = vertcat(Actin_cent_store{:});
        Actin_S5P_distCell{ff} = vertcat(Actin_S5P_dist{:});
        Actin_intCell{ff} = cell(1,numQuantChannels);
        Actin_nucIntCell{ff} = cell(1,numQuantChannels);
        for qq = 1:numQuantChannels
            Actin_intCell{ff}{qq} = vertcat(Actin_intensity{qq,:});
            Actin_nucIntCell{ff}{qq} = vertcat(Actin_nucIntensity{qq,:});
        end
        
    end
       
end

% Retain only files that returned nuclei

condNames = condNames(validFileFlag);
nuc_intCell = nuc_intCell(validFileFlag);
cyto_intCell = cyto_intCell(validFileFlag);
nuc_stdCell = nuc_stdCell(validFileFlag);
nuc_medianVolCell = nuc_medianVolCell(validFileFlag);
perNuc_countCell = perNuc_countCell(validFileFlag);

S5P_xyVoxelSizeCell = S5P_xyVoxelSizeCell(validFileFlag);
S5P_zVoxelSizeCell = S5P_zVoxelSizeCell(validFileFlag);
Actin_xyVoxelSizeCell = Actin_xyVoxelSizeCell(validFileFlag);
Actin_zVoxelSizeCell = Actin_zVoxelSizeCell(validFileFlag);

S5P_volCell = S5P_volCell(validFileFlag);
S5P_solCell = S5P_solCell(validFileFlag);
S5P_eloCell = S5P_eloCell(validFileFlag);
S5P_imgCell = S5P_imgCell(validFileFlag);
S5P_centCell = S5P_centCell(validFileFlag);
S5P_Actin_distCell = S5P_Actin_distCell(validFileFlag);
S5P_intCell = S5P_intCell(validFileFlag);
S5P_nucIntCell = S5P_nucIntCell(validFileFlag);

Actin_volCell = Actin_volCell(validFileFlag);
Actin_solCell = Actin_solCell(validFileFlag);
Actin_eloCell = Actin_eloCell(validFileFlag);
Actin_imgCell = Actin_imgCell(validFileFlag);
Actin_centCell = Actin_centCell(validFileFlag);
Actin_S5P_distCell = Actin_S5P_distCell(validFileFlag);
Actin_intCell = Actin_intCell(validFileFlag);
Actin_nucIntCell = Actin_nucIntCell(validFileFlag);

%% Sort into conditions

% My recommendation is to not actually look into this section too close.
% It is a very tough section of the code. The purpose of this section is to
% take the results that were obtained from all image files, and sort them
% based on the condition names that were assigned to them. All that really
% matters, in the end they are sorted into these conditions.

uniqueCondNames = unique(condNames);
numConds = numel(uniqueCondNames);
fileIndsCell = cell(1,numConds);
numFiles_perCond = zeros(1,numConds);
for cc = 1:numConds
	fileIndsCell{cc} = cellfun(...
		@(elmt)strcmp(elmt,uniqueCondNames{cc}),condNames);
	numFiles_perCond(cc) = sum(fileIndsCell{cc});
end

sortedCondNames = cell(1,numConds);
sortedNumNuclei = zeros(1,numConds);
sortedNumFiles = zeros(1,numConds);

sortedNucIntCell = cell(1,numQuantChannels);
sortedCytoIntCell = cell(1,numQuantChannels);
sortedNucStdCell = cell(1,numQuantChannels);

sortedS5PPixelSize_xy = cell(1,numConds);
sortedS5PPixelSize_z = cell(1,numConds);
sortedActinPixelSize_xy = cell(1,numConds);
sortedActinPixelSize_z = cell(1,numConds);

sortedS5PNumCell = cell(1,numConds);
sortedS5PVolCell = cell(1,numConds);
sortedS5PSolCell = cell(1,numConds);
sortedS5PEloCell = cell(1,numConds);
sortedS5PCentralSliceCell = cell(1,numConds);
sortedS5PCentroidsCell = cell(1,numConds);
sortedS5PDistCell = cell(1,numConds);
sortedS5PIntCell = cell(1,numQuantChannels);
sortedS5PNucIntCell = cell(1,numQuantChannels);

sortedActinNumCell = cell(1,numConds);
sortedActinVolCell = cell(1,numConds);
sortedActinSolCell = cell(1,numConds);
sortedActinEloCell = cell(1,numConds);
sortedActinCentralSliceCell = cell(1,numConds);
sortedActinCentroidsCell = cell(1,numConds);
sortedActinDistCell = cell(1,numConds);
sortedActinIntCell = cell(1,numQuantChannels);
sortedActinNucIntCell = cell(1,numQuantChannels);

for qq = 1:numQuantChannels
	sortedNucIntCell{qq} = cell(1,numConds);
	sortedCytoIntCell{qq} = cell(1,numConds);
    sortedNucStdCell{qq} = cell(1,numConds);
	sortedS5PIntCell{qq} = cell(1,numConds);
    sortedS5PNucIntCell{qq} = cell(1,numConds);
	sortedActinIntCell{qq} = cell(1,numConds);
    sortedActinNucIntCell{qq} = cell(1,numConds);
end


for cc = 1:numConds
	
	sortedCondNames{cc} = ...
		condNames(fileIndsCell{cc});
	sortedCondNames{cc} = sortedCondNames{cc}{1};
	
	sortedNumNuclei(cc) = ...
		sum(numNuclei_vec(fileIndsCell{cc}));
	sortedNumFiles(cc) = sum(fileIndsCell{cc});
	
    sortedS5PPixelSize_xy{cc} = ...
        vertcat(S5P_xyVoxelSizeCell{fileIndsCell{cc}});
    sortedS5PPixelSize_z = ...
        vertcat(S5P_zVoxelSizeCell{fileIndsCell{cc}});
    sortedActinPixelSize_xy = ...
        vertcat(Actin_xyVoxelSizeCell{fileIndsCell{cc}});
    sortedActinPixelSize_z = ...
        vertcat(Actin_zVoxelSizeCell{fileIndsCell{cc}});

	S5P_nums = vertcat(arrayfun(...
		@(val)perNuc_countCell{val}{1},find(fileIndsCell{cc}),...
		'UniformOutput',false));
	S5P_nums = vertcat(S5P_nums{:});
	S5P_vols = vertcat(S5P_volCell{fileIndsCell{cc}});
	S5P_sols = vertcat(S5P_solCell{fileIndsCell{cc}});
	S5P_elos = vertcat(S5P_eloCell{fileIndsCell{cc}});
	S5P_slices = vertcat(S5P_imgCell{fileIndsCell{cc}});
	S5P_centroids = vertcat(S5P_centCell{fileIndsCell{cc}});
    S5P_distances = vertcat(S5P_Actin_distCell{fileIndsCell{cc}}); 
	S5P_ints = S5P_intCell(fileIndsCell{cc});
    S5P_nucInts = S5P_nucIntCell(fileIndsCell{cc});
    
	sortedS5PNumCell{cc} = S5P_nums;
	sortedS5PVolCell{cc} = S5P_vols;
	sortedS5PSolCell{cc} = S5P_sols;
	sortedS5PEloCell{cc} = S5P_elos;
	sortedS5PCentralSliceCell{cc} = S5P_slices;
	sortedS5PCentroidsCell{cc} = S5P_centroids;
    sortedS5PDistCell{cc} = S5P_distances;

	Actin_nums = vertcat(arrayfun(...
		@(val)perNuc_countCell{val}{2},find(fileIndsCell{cc}),...
		'UniformOutput',false));
	Actin_nums = vertcat(Actin_nums{:});
	Actin_vols = vertcat(Actin_volCell{fileIndsCell{cc}});
	Actin_sols = vertcat(Actin_solCell{fileIndsCell{cc}});
	Actin_elos = vertcat(Actin_eloCell{fileIndsCell{cc}});
	Actin_slices = vertcat(Actin_imgCell{fileIndsCell{cc}});
	Actin_centroids = vertcat(Actin_centCell{fileIndsCell{cc}});
    Actin_distances = vertcat(Actin_S5P_distCell{fileIndsCell{cc}});
	Actin_ints = Actin_intCell(fileIndsCell{cc});
    Actin_nucInts = Actin_nucIntCell(fileIndsCell{cc});
    
	sortedActinNumCell{cc} = Actin_nums;
	sortedActinVolCell{cc} = Actin_vols;
	sortedActinSolCell{cc} = Actin_sols;
	sortedActinEloCell{cc} = Actin_elos;
	sortedActinCentralSliceCell{cc} = Actin_slices;
	sortedActinCentroidsCell{cc} = Actin_centroids;
    sortedActinDistCell{cc} = Actin_distances;
    
	for qq = 1:numQuantChannels
		
		sortedNucIntCell{qq}{cc} = vertcat(arrayfun(...
			@(ind)nuc_intCell{ind}{qq},....
			find(fileIndsCell{cc}),...
			'UniformOutput',false));
		sortedNucIntCell{qq}{cc} = vertcat(sortedNucIntCell{qq}{cc}{:})';
		
		sortedCytoIntCell{qq}{cc} = vertcat(arrayfun(...
			@(ind)cyto_intCell{ind}{qq},....
			find(fileIndsCell{cc}),...
			'UniformOutput',false));
		sortedCytoIntCell{qq}{cc} = vertcat(sortedCytoIntCell{qq}{cc}{:})';
		
        sortedNucStdCell{qq}{cc} = vertcat(arrayfun(...
			@(ind)nuc_stdCell{ind}{qq},....
			find(fileIndsCell{cc}),...
			'UniformOutput',false));
		sortedNucStdCell{qq}{cc} = vertcat(sortedNucStdCell{qq}{cc}{:})';
		
		sortedS5PIntCell{qq}{cc} = vertcat(arrayfun(...
			@(ind)S5P_intCell{ind}{qq},....
			find(fileIndsCell{cc}),...
			'UniformOutput',false));
		sortedS5PIntCell{qq}{cc} = vertcat(sortedS5PIntCell{qq}{cc}{:});

        sortedS5PNucIntCell{qq}{cc} = vertcat(arrayfun(...
            @(ind)S5P_nucIntCell{ind}{qq},....
            find(fileIndsCell{cc}),...
            'UniformOutput',false));
        sortedS5PNucIntCell{qq}{cc} = vertcat(sortedS5PNucIntCell{qq}{cc}{:});

        
		sortedActinIntCell{qq}{cc} = vertcat(arrayfun(...
			@(ind)Actin_intCell{ind}{qq},....
			find(fileIndsCell{cc}),...
			'UniformOutput',false));
		sortedActinIntCell{qq}{cc} = vertcat(sortedActinIntCell{qq}{cc}{:});
		
        sortedActinNucIntCell{qq}{cc} = vertcat(arrayfun(...
            @(ind)Actin_nucIntCell{ind}{qq},....
            find(fileIndsCell{cc}),...
            'UniformOutput',false));
        sortedActinNucIntCell{qq}{cc} = vertcat(sortedActinNucIntCell{qq}{cc}{:});

        
	end
	
end



%% Plotting of result overview

% This section is relevant for you again. It shows how we can calculate
% overview statistics for the different conditions, and plot the overview.

sorted_central_slices = cell(1,numConds);

S5P_Num_median = zeros(1,numConds);
S5P_Num_CI = zeros(2,numConds);
S5P_S5P_median = zeros(1,numConds);
S5P_S5P_CI = zeros(2,numConds);
S5P_Actin_median = zeros(1,numConds);
S5P_Actin_CI = zeros(2,numConds);
S5P_Vol_median = zeros(1,numConds);
S5P_Vol_CI = zeros(2,numConds);
S5P_Elo_median = zeros(1,numConds);
S5P_Elo_CI = zeros(2,numConds);
S5P_Sol_median = zeros(1,numConds);
S5P_Sol_CI = zeros(2,numConds);

Actin_Num_median = zeros(1,numConds);
Actin_Num_CI = zeros(2,numConds);
Actin_S5P_median = zeros(1,numConds);
Actin_S5P_CI = zeros(2,numConds);
Actin_Actin_median = zeros(1,numConds);
Actin_Actin_CI = zeros(2,numConds);
Actin_Vol_median = zeros(1,numConds);
Actin_Vol_CI = zeros(2,numConds);
Actin_Elo_median = zeros(1,numConds);
Actin_Elo_CI = zeros(2,numConds);
Actin_Sol_median = zeros(1,numConds);
Actin_Sol_CI = zeros(2,numConds);

% The conditions are alphabatically ordered, you can use this vector to
% resort according to your own preferences.
subplotInds = [1,5,2,3,4]; 

for cc = 1:numConds
		
	S5P_Num_vals = [sortedS5PNumCell{cc}];
	Actin_Num_vals = [sortedActinNumCell{cc}];
	Nuc_S5P_vals = [sortedNucIntCell{1}{cc}-sortedCytoIntCell{1}{cc}];
	Nuc_Actin_vals = [sortedNucIntCell{2}{cc}-sortedCytoIntCell{1}{cc}];
	
    S5P_S5P_vals = [sortedS5PIntCell{1}{cc}];
	S5P_Actin_vals = [sortedS5PIntCell{2}{cc}];
	S5P_Vol_vals = [sortedS5PVolCell{cc}];
    S5P_Elo_vals = [sortedS5PEloCell{cc}];
	S5P_Sol_vals = [sortedS5PSolCell{cc}];

    % --- From here on, you will have to replace my code with your code to
    % plots histograms or density plots, and your own scatter plots! Above
    % here, I would not change too much in this section.



    % We can apply additional selection filters to the results here. This
    % can be used, for example, to pick out selectively only the objects
    % with high levels of actin intensity.
	S5P_Actin_threshold = 1.2;
	inclInds = sortedS5PIntCell{3}{cc}>=S5P_Actin_threshold;

	selected_S5P_S5P_vals = [sortedS5PIntCell{1}{cc}(inclInds)];
	selected_S5P_Actin_vals = [sortedS5PIntCell{2}{cc}(inclInds)];
	selected_S5P_Vol_vals = [sortedS5PVolCell{cc}(inclInds)];
    selected_S5P_Elo_vals = [sortedS5PEloCell{cc}(inclInds)];
	selected_S5P_Sol_vals = [sortedS5PSolCell{cc}(inclInds)];
	
	Actin_S5P_vals = [sortedActinIntCell{1}{cc}];
	Actin_Actin_vals = [sortedActinIntCell{2}{cc}];
	Actin_Vol_vals = [sortedActinVolCell{cc}];
    Actin_Elo_vals = [sortedActinEloCell{cc}];
	Actin_Sol_vals = [sortedActinSolCell{cc}];

    % Now picking out actin bundles with high actin intensity
    Actin_Actin_threshold = 1.2;
	inclInds = sortedActinIntCell{3}{cc}>=Actin_Actin_threshold;

    selected_Actin_S5P_vals = [sortedActinIntCell{1}{cc}];
	selected_Actin_Actin_vals = [sortedActinIntCell{2}{cc}];
	selected_Actin_Vol_vals = [sortedActinVolCell{cc}];
    selected_Actin_Elo_vals = [sortedActinEloCell{cc}];
	selected_Actin_Sol_vals = [sortedActinSolCell{cc}];
    
	numPoints = numel(S5P_Actin_vals);
	
	n_boot = 200;
	
    S5P_Num_median(cc) = median(S5P_Num_vals);
	S5P_Num_CI(:,cc) = bootci(n_boot,@median,S5P_Num_vals);
    S5P_Vol_median(cc) = median(S5P_Vol_vals);
	S5P_Vol_CI(:,cc) = bootci(n_boot,@median,S5P_Vol_vals);
    S5P_S5P_median(cc) = median(selected_S5P_S5P_vals);
	S5P_S5P_CI(:,cc) = bootci(n_boot,@median,selected_S5P_S5P_vals);
	S5P_Actin_median(cc) = median(selected_S5P_Actin_vals);
	S5P_Actin_CI(:,cc) = bootci(n_boot,@median,selected_S5P_Actin_vals);
	
    Actin_Num_median(cc) = median(Actin_Num_vals);
	Actin_Num_CI(:,cc) = bootci(n_boot,@median,Actin_Num_vals);
    Actin_Vol_median(cc) = median(Actin_Vol_vals);
	Actin_Vol_CI(:,cc) = bootci(n_boot,@median,Actin_Vol_vals);
    Actin_S5P_median(cc) = median(selected_Actin_S5P_vals);
    Actin_S5P_CI(:,cc) = bootci(n_boot,@median,selected_Actin_S5P_vals);
    Actin_Actin_median(cc) = median(Actin_Actin_vals);
    Actin_Actin_CI(:,cc) = bootci(n_boot,@median,Actin_Actin_vals);
	
    S5P_Elo_median(cc) = median(selected_S5P_Elo_vals);
	S5P_Elo_CI(:,cc) = bootci(n_boot,@median,selected_S5P_Elo_vals);
    S5P_Sol_median(cc) = median(selected_S5P_Sol_vals);
	S5P_Sol_CI(:,cc) = bootci(n_boot,@median,selected_S5P_Sol_vals);
	
    Actin_Elo_median(cc) = median(selected_Actin_Elo_vals);
	Actin_Elo_CI(:,cc) = bootci(n_boot,@median,selected_Actin_Elo_vals);
    Actin_Sol_median(cc) = median(selected_Actin_Sol_vals);
	Actin_Sol_CI(:,cc) = bootci(n_boot,@median,selected_Actin_Sol_vals);
	

    figure(1)
    subplot(2,numConds,subplotInds(cc))
    plot(S5P_S5P_vals,S5P_Actin_vals,...
        'k.','MarkerSize',1)
    xlabel('Pol II Ser5P intensity')
    ylabel('Actin intensity')
    title(sprintf('Pol II clusters, %s',sortedCondNames{cc}))
    set(gca,'XLim',[0.4,1.8],'YLim',[0.8,1.6])
    
    subplot(2,numConds,numConds+subplotInds(cc))
    plot(Actin_S5P_vals,Actin_Actin_vals,...
        'k.','MarkerSize',1)
    xlabel('Pol II Ser5P intensity')
    ylabel('Actin intensity')
    title(sprintf('Actin bundles, %s',sortedCondNames{cc}))
    set(gca,'XLim',[0.4,1.8],'YLim',[0.8,1.6])
	
end

%% -- plot overviews

% Please ignore this section. It is too complicated to understand int he F2
% practical. I put this in for my own interest, as this was the first
% analysis of a data set for an ongoing project.

figure(2)
clf

datasetInds = {[1],[3],[4],[5],[2]};
xaxisInds = {[1],[2],[3],[4],[5]};
numPlotSets = numel(xaxisInds);
setSymbols = {'ko','rs','rs','b^','b^'};
setFaceColors = {[0,0,0],[1,0,0],[1,0,0],[0,0,1],[0,0,1]};
setNames = condNames;

numLabels = sum(cellfun(@(elmt)numel(elmt),xaxisInds));
labelCallInds = [datasetInds{:}];

for pp = 1:numPlotSets
	
    subplot(7,1,1)
	errorbar(xaxisInds{pp},Actin_Num_median(datasetInds{pp}),...
		Actin_Num_CI(1,(datasetInds{pp}))-Actin_Num_median(datasetInds{pp}),...
		Actin_Num_median(datasetInds{pp})-Actin_Num_CI(2,(datasetInds{pp})),...
		setSymbols{pp},'MarkerFaceColor',setFaceColors{pp})
	hold on
	set(gca,'XTick',1:numLabels,...
		'XTickLabels',sortedCondNames(labelCallInds),...
        'XLim',[0.5,numLabels+0.5])
	xlabel('')
	ylabel('Number per nucleus')
    title('Actin bundles, all')
	xtickangle(30)
    
    subplot(7,1,2)
	errorbar(xaxisInds{pp},Actin_Vol_median(datasetInds{pp}),...
		Actin_Vol_CI(1,(datasetInds{pp}))-Actin_Vol_median(datasetInds{pp}),...
		Actin_Vol_median(datasetInds{pp})-Actin_Vol_CI(2,(datasetInds{pp})),...
		setSymbols{pp},'MarkerFaceColor',setFaceColors{pp})
	hold on
	set(gca,'XTick',1:numLabels,...
		'XTickLabels',sortedCondNames(labelCallInds),...
        'XLim',[0.5,numLabels+0.5])
	xlabel('')
	ylabel('Volume (\mum^3)')
    title('Actin bundles, all')
	xtickangle(30)

    subplot(7,1,3)
	errorbar(xaxisInds{pp},Actin_Actin_median(datasetInds{pp}),...
		Actin_Actin_CI(1,(datasetInds{pp}))-Actin_Actin_median(datasetInds{pp}),...
		Actin_Actin_median(datasetInds{pp})-Actin_Actin_CI(2,(datasetInds{pp})),...
		setSymbols{pp},'MarkerFaceColor',setFaceColors{pp})
	hold on
	set(gca,'XTick',1:numLabels,...
		'XTickLabels',sortedCondNames(labelCallInds),...
        'XLim',[0.5,numLabels+0.5])
	xlabel('')
	ylabel('Actin Int.')
    title('Actin bundles, all')
	xtickangle(30)
    
    subplot(7,1,4)
	errorbar(xaxisInds{pp},Actin_Elo_median(datasetInds{pp}),...
		Actin_Elo_CI(1,(datasetInds{pp}))-Actin_Elo_median(datasetInds{pp}),...
		Actin_Elo_median(datasetInds{pp})-Actin_Elo_CI(2,(datasetInds{pp})),...
		setSymbols{pp},'MarkerFaceColor',setFaceColors{pp})
	hold on
	set(gca,'XTick',1:numLabels,...
		'XTickLabels',sortedCondNames(labelCallInds),...
        'XLim',[0.5,numLabels+0.5])
	xlabel('')
	ylabel('Elongation')
    title('Actin bundles, high actin')
	xtickangle(30)
	
	subplot(7,1,5)
	errorbar(xaxisInds{pp},Actin_S5P_median(datasetInds{pp}),...
		Actin_S5P_CI(1,(datasetInds{pp}))-Actin_S5P_median(datasetInds{pp}),...
		Actin_S5P_median(datasetInds{pp})-Actin_S5P_CI(2,(datasetInds{pp})),...
		setSymbols{pp},'MarkerFaceColor',setFaceColors{pp})
	hold on
	set(gca,'XTick',1:numLabels,...
		'XTickLabels',sortedCondNames(labelCallInds),...
        'XLim',[0.5,numLabels+0.5])
	xlabel('')
	ylabel('Pol II Ser5P Int.')
    title('Actin bundles, high actin')
	xtickangle(30)

    subplot(7,1,6)
	errorbar(xaxisInds{pp},S5P_Num_median(datasetInds{pp}),...
		S5P_Num_CI(1,(datasetInds{pp}))-S5P_Num_median(datasetInds{pp}),...
		S5P_Num_median(datasetInds{pp})-S5P_Num_CI(2,(datasetInds{pp})),...
		setSymbols{pp},'MarkerFaceColor',setFaceColors{pp})
	hold on
	set(gca,'XTick',1:numLabels,...
		'XTickLabels',sortedCondNames(labelCallInds),...
        'XLim',[0.5,numLabels+0.5])
	xlabel('')
	ylabel('Number per nucleus')
    title('Pol II Ser5P clusters, all')
	xtickangle(30)

	subplot(7,1,7)
	errorbar(xaxisInds{pp},S5P_Actin_median(datasetInds{pp}),...
		S5P_Actin_CI(1,(datasetInds{pp}))-S5P_Actin_median(datasetInds{pp}),...
		S5P_Actin_median(datasetInds{pp})-S5P_Actin_CI(2,(datasetInds{pp})),...
		setSymbols{pp},'MarkerFaceColor',setFaceColors{pp})
	hold on
	
	set(gca,'XTick',1:numLabels,...
		'XTickLabels',sortedCondNames(labelCallInds),...
        'XLim',[0.5,numLabels+0.5])
	xlabel('')
	ylabel('Actin Int.')
    title('Pol II Ser5P clusters, high actin')
	xtickangle(30)

	
end