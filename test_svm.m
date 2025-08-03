function [ error confusionmatrix ] = test_svm( svm, patterns, labels )
%TEST_SVM calculate relative test error of a svm on a testset
%[ error confusionmatrix ] = test_svm( svm, patterns, labels )
%   where:
%     'error' refers to the relative classification error
%     'confusionmatrix' refers to the confusion matrix
%     'svm' contains the support vector machine
%     'patterns' (matrix) contains the test patterns, one pattern per row
%     'labels' (vector) contains the true labels of the patterns

error = 0;
confusionmatrix = zeros (2,2);

% add your code below this line!
