function [ error confusionmatrix ] = test_svm( svm, patterns, labels )
%TEST_SVM calculate relative test error of a svm on a testset
%[ error confusionmatrix ] = test_svm( svm, patterns, labels )
%   where:
%     'error' refers to the relative classification error
%     'confusionmatrix' refers to the confusion matrix
%     'svm' contains the support vector machine
%     'patterns' (matrix) contains the test patterns, one pattern per row
%     'labels' (vector) contains the true labels of the patterns

    pred = predict(svm, patterns)';    
    
    true_positives = sum((pred == labels) .* (pred == 1));
    false_positives = sum((pred ~= labels) .* (pred == 1));
    false_negatives = sum((pred ~= labels) .* (pred == -1));
    true_negatives = sum((pred == labels) .* (pred == -1));
        
    error = sum(pred ~= labels) / length(pred);

    confusionmatrix = [true_positives, false_positives;
                       false_negatives, true_negatives];
end
% add your code below this line!
