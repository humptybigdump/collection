clear all

% Specify the directory that contains the extracted files from the last
% step, where you extracted from the raw files obtained from the microscope
sourceDirectory = ...
    './ExtractedStacks/Cond_1/**/';

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
nuc_segErosion = 0.3; % range of erosion (in microns) to avoid margin effects
% Use topological operation to fill holes in the nuclei segmentation masks?
% Default: 3D hole-filing, set flag to value 1
% 2D hole-filling, usefil if the stack cuts nuclei on top or bottom, so
% that 3D hole-filling does not work, set flag value to 2
% To deactivate hole-filling, set flag to any other number
fillHolesFlag = 1;

% Minimum volume of nuclei, typical ranges for a full nucieus 10-100 cubic
% microns of volume, so set a cut-off oof 10 or 30 or so
Nuc_min_vol = 15; % cubic microns
Nuc_min_sol = 0.7; % to ensure round nuclei
Nuc_min_CoV = 0.0; % to ensure transcriptionally active foci

% Inner and outer extension of a nuclear masks to cover cytoplasm
cytoMask_extension = 1.5; % in microns
cytoMask_distance = 1.0; % in microns

% Parameters for the blurring during image preprocessing
S5P_segBlurSigma_object = 0.05; % in microns
S5P_segBlurSigma_BG_removal = 1.0; % in microns
% Number of standard deviations above background for the robust threshold
S5P_seg_numStdDev = 2.0;

% Blurring and segmentation parameters for the actin bundles
Actin_segBlurSigma_object = 0.05; % in microns
Actin_segBlurSigma_BG_removal = 1.0; % in microns
Actin_seg_numStdDev = 2.2; % number of standard deviations in robust threshold

% Minimum volumes for objects inside the nuclei
S5P_minVol = 0.01; % cubic microns
Actin_minVol = 0.01; % cubic microns

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
	NucSegMask = segImg>0.85.*nuc_seg_thresh;
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
	
% Uncomment the following two lines if you want to check the extracted
% images one by one
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
                LL = labelmatrix(S5P_comps);
                imagesc(squeeze(LL(:,:,centerPlaneInd)))
                axis tight equal
                set(gca,'Colormap',lines)
                
                subplot(2,2,4)
                LL = labelmatrix(Actin_comps);
                imagesc(squeeze(LL(:,:,centerPlaneInd)))
                axis tight equal
                set(gca,'Colormap',lines)
                
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


%% --- Plotting of object properties

% Before we can plot the analysis results, we need to bring them into the
% format of vectors of numbers. Below, we carry out the according
% reorganization operations.

% ---
% Reorganize object shape properties into vector arrays. In this case, it
% is relatively easy. We have a cell array with one element per cell
% nucleus. We then call up the content of each element using the :
% operator. Each element contains a vector, which then gets concatenated by
% the vertcat command.

S5P_Volume = vertcat(S5P_volCell{:});
S5P_Solidity = vertcat(S5P_solCell{:});
S5P_Elongation = vertcat(S5P_eloCell{:});

Actin_Volume = vertcat(Actin_volCell{:});
Actin_Solidity = vertcat(Actin_solCell{:});
Actin_Elongation = vertcat(Actin_eloCell{:});

% ---
% Reorganize the object intensities into vector arrays. For the object
% intensities, it is a little tricker. We need to resort ot using the
% cellfun operation, as we need to make a two-index call, calling into each
% element of the top cell array for a specific sub-element.

S5P_DNA_intensity = cellfun(@(elmt)elmt{1}',S5P_intCell,...
    'UniformOutput',false);
S5P_DNA_intensity = [S5P_DNA_intensity{:}];
S5P_S5P_intensity = cellfun(@(elmt)elmt{2}',S5P_intCell,...
    'UniformOutput',false);
S5P_S5P_intensity = [S5P_S5P_intensity{:}];
S5P_Actin_intensity = cellfun(@(elmt)elmt{3}',S5P_intCell,...
    'UniformOutput',false);
S5P_Actin_intensity = [S5P_Actin_intensity{:}];

Actin_DNA_intensity = cellfun(@(elmt)elmt{1}',Actin_intCell,...
    'UniformOutput',false);
Actin_DNA_intensity = [Actin_DNA_intensity{:}];
Actin_S5P_intensity = cellfun(@(elmt)elmt{2}',Actin_intCell,...
    'UniformOutput',false);
Actin_S5P_intensity = [Actin_S5P_intensity{:}];
Actin_Actin_intensity = cellfun(@(elmt)elmt{3}',Actin_intCell,...
    'UniformOutput',false);
Actin_Actin_intensity = [Actin_Actin_intensity{:}];


figure(1)
clf







subplot(3,3,1)
plot(S5P_DNA_intensity,S5P_Actin_intensity,...
    'k.','MarkerSize',1)
title('Pol II clusters')
xlabel('DNA intensity')
ylabel('Actin intensity')
set(gca,'XLim',[0.4,2.4],'YLim',[0.4,2.4])

subplot(3,3,2)
plot(S5P_DNA_intensity,S5P_S5P_intensity,...
    'k.','MarkerSize',1)
title('Pol II clusters')
xlabel('DNA intensity')
ylabel('Pol II Ser5P intensity')
set(gca,'XLim',[0.4,2.4],'YLim',[0.4,2.4])

subplot(3,3,3)
plot(S5P_S5P_intensity,S5P_Actin_intensity,...
    'k.','MarkerSize',1)
title('Pol II clusters')
xlabel('Pol II Ser5P intensity')
ylabel('Actin intensity')
set(gca,'XLim',[0.4,2.4],'YLim',[0.4,2.4])




subplot(3,3,4)
plot(Actin_DNA_intensity,Actin_Actin_intensity,...
    'k.','MarkerSize',1)
title('Actin bundles')
xlabel('DNA intensity')
ylabel('Actin intensity')
set(gca,'XLim',[0.4,2.4],'YLim',[0.4,2.4])

subplot(3,3,5)
plot(Actin_DNA_intensity,Actin_S5P_intensity,...
    'k.','MarkerSize',1)
title('Actin bundles')
xlabel('DNA intensity')
ylabel('Pol II Ser5P intensity')
set(gca,'XLim',[0.4,2.4],'YLim',[0.4,2.4])

subplot(3,3,6)
plot(Actin_S5P_intensity,Actin_Actin_intensity,...
    'k.','MarkerSize',1)
title('Actin bundles')
xlabel('Pol II Ser5P intensity')
ylabel('Actin intensity')
set(gca,'XLim',[0.4,2.4],'YLim',[0.4,2.4])


%% --- Selective analysis of high actin intensity objects

% Based on the scatter plots of object intensities, it looks as if there is
% a distinct population of high actin intensity objects. This population is
% present no matter whether we segment actin bundles directly, or actually
% Pol II clusters. This suggests that Pol II clusters with high actin
% levels might be reflecting the same population of objects as the high
% intensity actin bundles. We can check on this a bit more closely. In
% concrete terms, we will compare the shapes of the objects obtained by
% both approaches of segmentation.

ActinIntCutoff = 1.25;
S5P_highActin_inds = S5P_Actin_intensity>ActinIntCutoff;
Actin_highActin_inds = Actin_Actin_intensity>ActinIntCutoff;

figure(1)

subplot(3,3,7)
cla
nBins = 10;
[binCounts,binCenters] = ...
    hist(S5P_DNA_intensity(S5P_highActin_inds),nBins);
plot([1,1],[0,0.3],'k-','Color',[0.5,0.5,0.5])
hold on
plot(binCenters,binCounts./sum(binCounts),...
    'k-','LineWidth',1)
[binCounts,binCenters] = ...
    hist(Actin_DNA_intensity(Actin_highActin_inds),nBins);
plot(binCenters,binCounts./sum(binCounts),...
    'r--','LineWidth',1)
[binCounts,binCenters] = ...
    hist(S5P_DNA_intensity(~S5P_highActin_inds),nBins);
plot(binCenters,binCounts./sum(binCounts),...
    'b:','LineWidth',1)
hold off
legend('Pol II Ser5P (high actin)','Actin (high actin)',...
    'Pol II Ser5P (low actin)')
xlabel('Normalized DNA intensity')
ylabel('Normalized count')
set(gca,'Box','on')
title('Selective analysis')


subplot(3,3,8)
cla
[binCounts,binCenters] = ...
    hist(S5P_Elongation(S5P_highActin_inds),nBins);
hold on
plot(binCenters,binCounts./sum(binCounts),...
    'k-','LineWidth',1)
[binCounts,binCenters] = ...
    hist(Actin_Elongation(Actin_highActin_inds),nBins);
plot(binCenters,binCounts./sum(binCounts),...
    'r--','LineWidth',1)
[binCounts,binCenters] = ...
    hist(S5P_Elongation(~S5P_highActin_inds),nBins);
plot(binCenters,binCounts./sum(binCounts),...
    'b:','LineWidth',1)
hold off
legend('Pol II Ser5P (high actin)','Actin (high actin)',...
    'Pol II Ser5P (low actin)')
xlabel('Elongation')
ylabel('Normalized count')
set(gca,'Box','on')
title('Selective analysis')


subplot(3,3,9)
cla
[binCounts,binCenters] = ...
    hist(S5P_Solidity(S5P_highActin_inds),nBins);
hold on
plot(binCenters,binCounts./sum(binCounts),...
    'k-','LineWidth',1)
[binCounts,binCenters] = ...
    hist(Actin_Solidity(Actin_highActin_inds),nBins);
plot(binCenters,binCounts./sum(binCounts),...
    'r--','LineWidth',1)
[binCounts,binCenters] = ...
    hist(S5P_Solidity(~S5P_highActin_inds),nBins);
plot(binCenters,binCounts./sum(binCounts),...
    'b:','LineWidth',1)
hold off
legend('Pol II Ser5P (high actin)','Actin (high actin)',...
    'Pol II Ser5P (low actin)','Location','Southeast')
xlabel('Solidity')
ylabel('Normalized count')
set(gca,'Box','on')
title('Selective analysis')