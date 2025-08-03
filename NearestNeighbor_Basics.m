% As we are analyzing objects in 3D space, a common analysis question will
% be: how far is an object away from its closest neighbor? This can also be
% referred to finding the nearest neighbor.
%
% To answer this question, we need to be able to measure the distance
% between two objects in 3D space. Assuming that we have the xyz
% coordinates of two objects A and B, this should not be too hard. One
% common approach is to use the Euclidean distance:
% d = sqrt((xB-xA)^2+(yB-yA)^2+(zB-zA)^2)
%
% This can, of course be done directly if only two object are considered.
% In most cases, however, we are dealing with large number of objects, or
% even seveal objects from two different sets. Here, it would be easier to
% just calculate all distances between all objects from one set to all
% objects of the other set. Here, MatLab comes to our help with functions
% for the calculation of so-called pair-wise distances:

% Generate some random xyz positions in two sets of objects
centroidsA = rand(4,3);
centroidsB = rand(7,3);

% The function we need to calculate the distances is pdist2. Let us first
% look at the help description for this function.
help pdist2

% Yes, that looks like just the thing we need. We can directly calculate
% the distances, and even specify which distance measure should be used.
pwDistMatrix = pdist2(centroidsA,centroidsB,'euclidean')

% Now we still have to find the neighbors with the smallest distance. We
% can use the comannd min to this end. The expressions below use the
% command min along the row dimension (index 2) and along the column
% dimension (index 1). As a result, we obtain the smallest distance for a
% given object, along with the index of the object that is at this
% particular position.

[minVector_AB,minInds_AB] = min(pwDistMatrix,[],2)
[minVector_BA,minInds_BA] = min(pwDistMatrix,[],1)

% It looks like our job is done: we calculated for each object

%% --- In-line function definitions

% MatLab offers the possiblity to define very simple functions in-line.
% This is often super practical, and will be crucial for application of the
% cellfun construct. Below, you can find a few in-line definitions to
% introduce the idea.

% The full length way of defining an inline function is as follows. It
% offers flexibility in terms of the arguments and operations. It also
% stores the function under a name, so it can be used later whenever it
% might be needed.
square_fun = @(xx) xx.^2;
square_fun(5)

% Functions with several input arguments are also possible:
product_fun = @(xx,yy) xx.*yy;
product_fun(5,7)

% In-line functions can also be generated from already existing functions.
% In this way, very short function definitions are possible
squareRoot_fun = @sqrt;
squareRoot_fun(36)

% On first sight, this approach to define in-line functions seems pretty
% useless. AFter all, we already have a square root function. It will come
% in handy later during the application of the cellfun construct, though.

% Considering the access to lower levels of arrays, in-line functions can
% also be useful. For example, when you would like to access some result
% obtained for the third color channel, you can define a function that
% always calls up the element at the third position of a cell array.
thirdElement_fun = @(elmt) elmt{3};
demoCell = {'A','B','C'};
thirdElement_fun(demoCell)


%% --- Using the cellfun construct to process cell arrays

% Cell with random numbers, to approximate a result cell from our analysis
demoCell = {rand(5,1),rand(5,1),rand(5,1),rand(5,1)}

% --- Processing all elements in a cell array
% We now extract only the second number from each element of the array
secondNumber_array = cellfun(@(elmt)elmt(2),demoCell)

% --- Concatenating data into a joint vector array
% In the situation where we obtained results from several nuclei, we might
% want to combine the results into one single vector. With the demoCell
% array as we have it, this can still be done directly with the : operator.
combined_array = [demoCell{:}]

% --- Dealing with row vs. column problems
% In this example, you can already see one problem that can emerge. Instead
% of getting a one-dimensional vector, we have obtained a two-dimensional
% matrix. Sometimes, this is what you want. But in most cases, a simple
% one-dimensional vector is what we want to obtain. To get the vectors
% proper lined up, we need to change the direction of the concatenation. In
% the call above, we used the [] brackets. These are a short form of
combined_array = horzcat(demoCell{:})

% To get the vectors all properly lined up, we must contatenate in the
% other direction, namely in a vertical fashion.
combined_array = vertcat(demoCell{:})

% --- Deep indexing with cellfun call
% In the operations above, we still did not need *really* need the cellfun
% construct yet. Now, we will construct a cell array with multiple layers
% of indexing - we often encounter these in our analysis. Here, it is no
% longer possible to extract the desired information by a simple call as
% above, and the cellfun construct becomes necessary.
multiIndexCell = cell(1,5);
for kk = 1:5
    multiIndexCell{kk} = {rand(1,4),rand(1,5),rand(1,4)};
end
disp(multiIndexCell)

% Obtain the third number from the second array in each element
singleNumberExtraction = cellfun(@(elmt)elmt{2}(3),multiIndexCell)

% --- Uniform / Non-uniform output
% Sometimes we would like to not only extract a single number, but whole
% arrays of numbers. In this situation, we need to indicate to the cellfun
% construct that more than just a single number will come as a result.
wholeArrayExtraction = cellfun(@(elmt)elmt{2},multiIndexCell,...
    'UniformOutput',false)

% When we place the additional info that the output might not be uniform,
% the cellfun construct places the results for each element in a cell
% array. This means that, whatever result comes, it can be properly stored.
% On the other hand, it also means that we might still have to concatenate
% the extracted information into a joint vector.
concatenatedVec = [wholeArrayExtraction{:}]