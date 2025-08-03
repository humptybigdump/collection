%% Assignment 06
clear all;
close all;
clc;

%% Load dataset
load('smileys_train.mat');
load('smileys_test.mat');

faces.train = train;
faces.test = test;

%% Plot dataset
faces_plot = figure;
i = 1;
while(i < 7)
    figure(faces_plot);
    subplot(2, 3, i);
    plot_smiley(faces.train(i, :));
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
              
disp('Training error:');
[error, confusion_matrix] = test_svm_solution(svm, faces.train(train_subset, :), trainlabel(train_subset))
disp('Validation error:');
[error, confusion_matrix] = test_svm_solution(svm, faces.train(validation_subset, :), trainlabel(validation_subset))
disp('Test error:');
[error, confusion_matrix] = test_svm_solution(svm, faces.test, testlabel)


%% Cross-Validation, k-fold
C_values = [1:5:26]
sigma_values = [1:5:26]

% 3-fold and 10-fold
for k = [3, 10]
    k
    [svm, cv_error, confusion_matrix, C, sigma] = train_svm_cv_solution(faces.train, trainlabel, k, C_values, sigma_values);

    % validate
    disp('Cross Validation error:');
    cv_error
    confusion_matrix
    C
    sigma
    disp('Test error:');
    [error, confusion_matrix] = test_svm_solution(svm, faces.test, testlabel)
end

% Run 3-fold/ 10-fold cross validation for single values of sigma and C
% and save the cross-validation error for the contour plot
% Display one plot per value of k

for k = [3, 10]
    score = zeros(length(C_values), length(sigma_values));
    i = 1;
    for C = C_values
        j=1;
        for sigma = sigma_values     
            [~, cv_error, ~, ~, ~] = train_svm_cv_solution(faces.train, trainlabel, k, C, sigma);
            score(i,j) = cv_error;
            j = j+1;
        end
        i = i+1;
    end

    % Plot
    figure;
    contourf(C_values, sigma_values, score');
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
    [svm, cv_error, confusion_matrix, C, sigma] = train_svm_cv_solution(bottom_half.train, trainlabel, k, C_values, sigma_values);

    % validate
    disp('Cross Validation error:');
    cv_error
    confusion_matrix
    C
    sigma
    disp('Test error:');
    [error, confusion_matrix] = test_svm_solution(svm, bottom_half.test, testlabel)
end


%% Evaluating a centred square at the bottom half of the image
disp('Training and testing - centered square at the bottom of the image');
for k = [3, 10]
    [svm, cv_error, confusion_matrix, C, sigma] = train_svm_cv_solution(bottom_square.train, trainlabel, k, C_values, sigma_values);

    % validate
    disp('Cross Validation error:');
    cv_error
    confusion_matrix
    C
    sigma
    disp('Test error:');
    [error, confusion_matrix] = test_svm_solution(svm, bottom_square.test, testlabel)
end


%% Evaluating mouth area
disp('Training and testing - mouth area');
for k = [3, 10]
    [svm, cv_error, confusion_matrix, C, sigma] = train_svm_cv_solution(mouth.train, trainlabel, k, C_values, sigma_values);

    % validate
    disp('Cross Validation error:');
    cv_error
    confusion_matrix
    C
    sigma
    disp('Test error:');
    [error, confusion_matrix] = test_svm_solution(svm, mouth.test, testlabel)
end