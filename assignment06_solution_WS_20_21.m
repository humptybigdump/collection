%% Assignment 06
clear all;
close all;
clc;


%% Load dataset
load('smileys_train_fixed.mat');
% load('smileys_train.mat');
load('smileys_test.mat');

faces.train = train;
faces.test = test;

%% Plot dataset
faces_plot = figure;
i = 1;
while(i < 7)
    figure(faces_plot);
    subplot(2, 3, i);
    plot_smiley(faces.train(i, :))
    axis off equal;
    title("Smiley class: " + trainlabel(i)); 

    i = i + 1;
end


%% Train SVM
size(faces.train)

train_subset = 1 : 3 / 4 * size(faces.train, 1);
validation_subset = 3 / 4 * size(faces.train, 1) + 1 : size(faces.train, 1);

size(train_subset)
size(validation_subset)

% Train SVM with RBF as kernel function, sigma = 2 and C = 5
svm = fitcsvm(faces.train(train_subset, :), trainlabel(train_subset), ...
              'KernelFunction', 'rbf', ...
              'KernelScale', 2.0, ...
              'BoxConstraint', 5.0);
              
disp('training error:');
[error, confusion_matrix] = test_svm(svm, faces.train(train_subset, :), trainlabel(train_subset))
disp('validation error:');
[error, confusion_matrix] = test_svm(svm, faces.train(validation_subset, :), trainlabel(validation_subset))
disp('test error:');
[error, confusion_matrix] = test_svm(svm, faces.test, testlabel)


%% Cross-Validation, k-fold 
c_vals = [1:5:25]; %10.^[-5:5];
sigma_vals = [1:5:25]; %10.^[-5:5];

% 3-fold and 10-fold
for k = [3, 10]
    k
    [svm cverror confmat cmin sigmamin] = train_svm_cv (faces.train, trainlabel, k, c_vals, sigma_vals);

    % validate
    disp('training error:');
    cverror
    confmat
    cmin
    sigmamin
    disp('test error:');
    [error, confusionmatrix] = test_svm(svm, faces.test, testlabel)
end

% Run 3-fold/ 10-fold cross validation for single values of sigma and C
% and save the cross-validation error for the contour plot
% Display one plot per value of k

for k = [3, 10]
    score = zeros(length(c_vals), length(sigma_vals));
    i = 1;
    for c = c_vals
        j=1;
        for sigma = sigma_vals     
            [svm cverror confmat cmin sigmamin] = train_svm_cv (faces.train, trainlabel, k, c, sigma);
            score(i,j) = cverror;
            j = j+1;
        end
        i = i+1;
    end

    % Plot
    figure;
    contourf(c_vals,sigma_vals,score');
end


%% Resize train and test data
% Size
size(faces.train)
size(faces.test)

bottom_half_temp.train = reshape(faces.train, size(faces.train, 1), 20, 20);
bottom_half_temp.test = reshape(faces.test, size(faces.test, 1), 20, 20);

bottom_square_temp.train = reshape(faces.train, size(faces.train, 1), 20, 20);
bottom_square_temp.test = reshape(faces.test, size(faces.test, 1), 20, 20);

mouth_temp.train = reshape(faces.train, size(faces.train, 1), 20, 20);
mouth_temp.test = reshape(faces.test, size(faces.test, 1), 20, 20);

% Reshaped data
size(bottom_half_temp.train)

bottom_half_plot = figure;
bottom_square_plot = figure;
mouth_plot = figure;

for i = [1:size(faces.train, 1)]    
    bhtr = bottom_half_temp.train(i, 1:20, 11:20);
    
    bsttr = bottom_square_temp.train(i, 6:15, 11:20);
    
    mtr = mouth_temp.train(i, 6:15, 13:18);    
    
    if i < 7
        figure(bottom_half_plot);
        subplot(2, 3, i);
        imagesc(squeeze(bhtr)')
        axis off equal;
        title("Smiley class: " + trainlabel(i)); 
        
        figure(bottom_square_plot);
        subplot(2, 3, i);
        imagesc(squeeze(bsttr)')
        axis off equal;
        title("Smiley class: " + trainlabel(i)); 
        
        figure(mouth_plot);
        subplot(2, 3, i);
        imagesc(squeeze(mtr)')
        axis off equal;
        title("Smiley class: " + trainlabel(i)); 
    end      
    
    bhtr = reshape(bhtr, 1, []);
    bottom_half.train(i,:) = bhtr;
    
    bsttr = reshape(bsttr, 1, []);
    bottom_square.train(i,:) = bsttr;
    
    mtr = reshape(mtr, 1, []);
    mouth.train(i,:) = mtr;   
end

for i = [1:size(faces.test, 1)]    
    bhte = bottom_half_temp.test(i, 1:20, 11:20);
    bhte = reshape(bhte, 1, []);
    bottom_half.test(i,:) = bhte;
    
    bstte = bottom_square_temp.test(i, 6:15, 11:20);
    bstte = reshape(bstte, 1, []);
    bottom_square.test(i,:) = bstte;
    
    mte = mouth_temp.test(i, 6:15, 13:18);
    mte = reshape(mte, 1, []);
    mouth.test(i,:) = mte;
end

size(bottom_half.train)
size(bottom_square.train)
size(mouth.train)

size(bottom_half.test)
size(bottom_square.test)
size(mouth.test)


%% Evaluating the bottom half of the image
disp('Training and testing - bottom half of the image');
for k = [3, 10]
    [svm cverror confmat cmin sigmamin] = train_svm_cv (bottom_half.train, trainlabel, k, c_vals, sigma_vals);

    % validate
    disp('training error:');
    cverror
    confmat
    cmin
    sigmamin
    disp('test error:');
    [error, confusionmatrix] = test_svm(svm, bottom_half.test, testlabel)
end


%% Evaluating a centred square at the bottom half of the image
disp('Training and testing - centered square at the bottom of the image');
for k = [3, 10]
    [svm cverror confmat cmin sigmamin] = train_svm_cv (bottom_square.train, trainlabel, k, c_vals, sigma_vals);

    % validate
    disp('training error:');
    cverror
    confmat
    cmin
    sigmamin
    disp('test error:');
    [error, confusionmatrix] = test_svm(svm, bottom_square.test, testlabel)
end


%% Evaluating mouth area
disp('Training and testing - mouth area');
for k = [3, 10]
    [svm cverror confmat cmin sigmamin] = train_svm_cv (mouth.train, trainlabel, k, c_vals, sigma_vals);

    % validate
    disp('training error:');
    cverror
    confmat
    cmin
    sigmamin
    disp('test error:');
    [error, confusionmatrix] = test_svm(svm, mouth.test, testlabel)
end