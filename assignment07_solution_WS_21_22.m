clear all;
close all;
clc;

%% assignment 07
folder = '~/education/MachineVisionAdditionalMaterial/assignments/assignment08/contrib/';

net = alexnet;
layers = net.Layers
classes = net.Layers(end).ClassNames

%% exercise 1
figure(1);
for i=1:1:12
    clf;    
    number = num2str(i,'%02.f');
    path = strcat(folder, 'images/', number, '.jpg');
    img = imread(path);
    
    scale = min(size(img, 1), size(img, 2)) / 227;
    imgResize = imresize(img, 1/scale);
    x = (size(imgResize, 2) - 227) / 2 + 1;
    y = (size(imgResize, 1) - 227) / 2 + 1;
    
    if (i == 12)
        imgCrop = imcrop(imgResize, [1, y, 227-1, 227-1]);
    else 
        imgCrop = imcrop(imgResize, [x, y, 227-1, 227-1]);        
    end
    
    label = classify(net, imgCrop);
    
    subplot(1,2,1);
    imshow(img);
    subplot(1,2,2);
    imshow(imgCrop);
    title(['\fontsize{24}Class: ', char(label)]);
    
    pause;
end


%% exercise 2
% Load labeled dataset
labeledPath = strcat(folder, 'traffic_sign_training/Images_scaled');
labeledImages = imageDatastore(labeledPath,...
                               'IncludeSubfolders', true,...
                               'LabelSource', 'foldernames');                          
[trainingImages, validationImages] = splitEachLabel(labeledImages,0.85,'randomized');                        

% Load test dataset
testPath = strcat(folder, 'traffic_sign_test/Images_scaled');
testImages = imageDatastore(testPath,...
                            'IncludeSubfolders', true,...
                            'LabelSource', 'foldernames');

% Check for the number of labels
numClasses = length(categories(trainingImages.Labels));

layersTransfer = net.Layers(1:end-3);
layersConcat = [layersTransfer
                fullyConnectedLayer(numClasses,'WeightLearnRateFactor',20,'BiasLearnRateFactor',20) %High learning rate so that these layers get trained
                softmaxLayer
                classificationLayer];

% Training
miniBatchSize = 100; %Number of training examples in each fw/bw-pass
validationFrequency = floor(1/2 * length(trainingImages.Labels) / miniBatchSize); %validate twice per epoch
checkpointsPath = strcat(folder, 'alexnet_checkpoints');
options = trainingOptions('sgdm',...
    'CheckpointPath', checkpointsPath, ...
    'MiniBatchSize',miniBatchSize,...
    'MaxEpochs', 30,...
    'InitialLearnRate', 1e-4,... %prevent large changes to old layers
    'Verbose', 1,...
    'ValidationData', validationImages,...
    'Plots','training-progress',...
    'ValidationFrequency',validationFrequency);
% netTransfer= trainNetwork(trainingImages, layersConcat, options);

% Load pretraining network instead
load(strcat(folder, 'netTransfer.mat'));

% Remap foldernames to class names
keySet = 1:numClasses;
valueSet = {...
    'Speed limit 20', ...
    'Speed limit 30', ...
    'Speed limit 50', ...
    'Speed limit 60', ...
    'Speed limit 70', ...
    'Speed limit 80', ...
    'Restriction ends 80', ...
    'Speed limit 100', ...
    'Speed limit 120', ...
    'No overtaking', ...
    'No overtaking Trucks', ...
    'Priority at next intersection', ...
    'Priority road ', ...
    'Give Way', ...
    'Stop', ...
    'No traffic both ways', ...
    'No trucks', ...
    'No entry', ...
    'Welcome to the dangerzone', ...
    'Bend left (danger)', ...
    'Bend right (danger)', ...
    'Bend (danger)', ...
    'Uneven road', ...
    'Slippery road', ...
    'Road narrows', ...
    'Construction', ...
    'Traffic signal', ...
    'Pedestrian crossing', ...
    'School crossing', ...
    'Cycles crossing', ...
    'Snow', ...
    'Animals', ...
    'Restrictions end', ...
    'Go right (mandatory)', ...
    'Go left (mandatory)', ...
    'Go straight (mandatory)', ...
    'Go right or straight (mandatory)', ...
    'Go left or straight (mandatory)', ...
    'Keep right (mandatory)', ...
    'Keep left (mandatory)', ...
    'Roundabout', ...
    'Restriction ends (overtaking)', ...
    'Restriction ends trucks (overtaking)', ...
    };
map = containers.Map(keySet,valueSet);

% Good 2 know: How does 'numel' work compared to 'length' and 'size'
exampleMatrix = [1,2;
                 3,4;
                 5,6]
size(exampleMatrix)
length(exampleMatrix)
numel(exampleMatrix)

% Visualize the classification of the modified alexnet
plotSize = 5;
IDs = randi([1, 12630], plotSize)
figure(3);
for i = 1:numel(IDs)
    subplot(plotSize, plotSize, i)
    label = classify(netTransfer, readimage(testImages,IDs(i)));
    I = readimage(testImages, IDs(i));
    imshow(I)
    title(['\fontsize{16}', map(int8(label))])
end

% Visualize the weights in the first convolutional layertestImages
figure(4);
for i = 1:96
    smplot(8, 12,i)
    imagesc(netTransfer.Layers(2).Weights(:,:,1,i));
    axis off;
end


%% exercise 3
% Different approaches for data tuning
id = 3496;
imgSample = readimage(labeledImages,id);

% Constant factor
imgSampleBrightness = imgSample * 1.5;
% Gamma correction
imgSampleGamma = imadjust(imgSample,[],[],1.75);
% Motion blur
h = fspecial('motion', 30, 6);
imgSampleBlur = imfilter(imgSample, h, 'replicate');
% Gaussian white noise
imgSampleGaussian = imnoise(imgSample, 'gaussian',0.0, 0.005);
% Salt and pepper noise
imgSampleSnP = imnoise(imgSample, 'salt & pepper',0.01);
% Rotation
imgSampleRotate = imrotate(imgSample, 8, 'bicubic', 'crop');
% Flip
imgSampleFlip = flip(imgSample, 2);

figure(2);
smplot(2,4,1)
imshow(imgSample);
smplot(2,4,2)
imshow(imgSampleBrightness);
smplot(2,4,3)
imshow(imgSampleGamma);
smplot(2,4,4)
imshow(imgSampleBlur);
smplot(2,4,5)
imshow(imgSampleGaussian);
smplot(2,4,6)
imshow(imgSampleSnP);
smplot(2,4,7)
imshow(imgSampleRotate);
smplot(2,4,8)
imshow(imgSampleFlip);