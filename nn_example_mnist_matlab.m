%MATLAB implementation deduced from 
%https://www.mathworks.com/help/deeplearning/ug/train-network\
%-using-model-function.html

% **************************************************************************
%(i) Load data sets for training and evaluation

[XTrain, YTrain] = digitTrain4DArrayData;
[XTest, YTest] = digitTest4DArrayData;

% **************************************************************************
%(ii) Define neural network model and topology

layers = [
    imageInputLayer([28 28 1])
    fullyConnectedLayer(128)
    reluLayer
    dropoutLayer(0.2)
    fullyConnectedLayer(128)
    reluLayer
    %dropoutLayer(0.2)
    fullyConnectedLayer(10)
    softmaxLayer
    classificationLayer];

% **************************************************************************
%(iii) Define loss function and initialize the optimization algorithm

options = trainingOptions('sgdm', ...
    'MaxEpochs', 20, ...
    'InitialLearnRate', 0.01, ...
    'Verbose', false, ...
    'Plots', 'training-progress');

% **************************************************************************
%(iv) Perform the training based on the training data set

net = trainNetwork(XTrain, YTrain, layers, options);

% **************************************************************************
%(v) Evaluate the trained neural network based on the evaluation data set

YPred = classify(net, XTest);
accuracy = sum(YPred == YTest) / numel(YTest);
disp(['Accuracy: ' num2str(accuracy*100) '%'])

%NOTE: The dataset above has different ordering compared to the one that
%      is used by TensorFlow or PyTorch

figure(1);
num_row = 2;
num_col = 5;
idx = [1:500:5000];
for j=1:length(idx)
    data = XTest(:,:,1,idx(j));
    label_exact = double(YTest(idx(j)))-1;
    [label_pred,prob]  = classify(net, data);
    label_pred  = double(label_pred)-1; 
    subplot(num_row,num_col,j)
    imshow(data);
    title(sprintf('%1.0f (%1.0f %%) / %1.0f',label_pred,max(prob)*100,...
                  label_exact));
end

%print(gcf,'nn_mnist_matlab_predict.pdf','pdf');
exportgraphics(gcf, 'nn_mnist_matlab_predict.pdf');
