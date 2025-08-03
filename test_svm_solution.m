function [ error confusionmatrix ] = test_svm_solution( svm, patterns, labels )
%TEST_SVM calculate relative test error of a svm on a testset
%[ error confusionmatrix ] = test_svm( svm, patterns, labels )
%   where:
%     'error' refers to the relative classification error
%     'confusionmatrix' refers to the confusion matrix
%     'svm' contains the support vector machine
%     'patterns' (matrix) contains the test patterns, one pattern per row
%     'labels' (vector) contains the true labels of the patterns

pred = predict(svm, patterns)';

true_positive = sum((pred == labels) .* (pred == 1));
false_positive = sum((pred ~= labels) .* (pred == 1));
false_negative = sum((pred ~= labels) .* (pred == -1));
true_negative = sum((pred == labels) .* (pred == -1));

%error = sum(pred ~= labels) / length(pred);
error = (false_positive + false_negative) / (true_positive + false_positive + false_negative + true_negative);

confusionmatrix = [true_positive, false_positive; 
                   false_negative, true_negative];
