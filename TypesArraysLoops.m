%% --- Storing numbers
% One of the most basic variables in MatLab is a matrix of numbers. It can
% be assigned directly by the use of square brackets:

demoMatrix = [1,2,;3,4]

%% ---
% You can already see that the numbers placed within the square brackets
% get turned into a 2D-matrix. The comma seperates the different values in
% a row (horizontal), the semicolon separates the values into consecuitve
% rows. One can also make one-dimensional array, also called a "vector":

demoRowVector = [1,2,3,4]
demoColumnVector = [5;6;7;8]

%% ---
% To get the information back out of such a vector, one can use single
% indices:

demoRowVector(3)

% One can also request specific ranges of data from such a vector array

demoColumnVector(3:4)

% One can go further, and even specify a vector that contains not a range,
% but a group of very specific indices, even ordered as we want:

demoRowVector([4,2,3])

% In the case of a matrix, you can also specify the indices for both
% dimensions:

demoMatrix(2,1)

% Another option is to specify that, for one of the dimensions, all data
% should be displayed, but for the other dimension only from one index:

demoMatrix(:,2)

%% --- Assigning values into existing arrays
% In case you would like to store a certain value inside of an array, or
% you might want to change the value of an array at one or several
% positions, you can again use indices towards that goal. You just have to
% make sure that the data you want to write to the array, and the place you
% choose to assign these data to match up.

demoRowVector(2) = 15

demoMatrix(:,2) = [16;17]

demoMatrix(1,2) = [16;17]

%% ---
% Different from basically any other programming languate, MatLab does care
% whether you have row or column arrays. You can see that putting together
% row and column vectors does not work:

mergedVector = [demoRowVector,demoColumnVector]

%% ---
% We can, however, make this work when we first make sure to turn the
% vectors both into a row vector, using the transposition operator '

mergedVector = [demoRowVector,demoColumnVector']

% Do not worry about this too much, it will not be needed for most of the
% scripts that we will use in this course. But it is worth noting this odd
% aspect of MatLab number array.




%% --- Storing letters and words
% A different type of variable is a string, also called an character array.
% This type of variable does not contain numbers, but characters, and when
% several characters come together, can serve to hold words.

demoString = 'Sequence of characters'

% This can also be called using the indexing system, in this case referring
% to the position in the string of characters:

demoString(13:22)

% Here, we can also illustrate another nice trick, the "end" command in
% index ranges:

demoString(13:end)

% The end command gives automatically the highest index of an array. It can
% even be further modified, as seen here:

demoString(13:end-3)

%% --- Storing data of any type: cell arrays

% Now, imaging a situation where you want to throw data of various type
% into one and the same array. For this purpose, cell arrays are very
% useful. Cell arrays can be defined using curly brackets:

demoCell = {'word A',[5,8,6],'word B',[1,2,3]}

% Now, this is interesting! So how would you get the information out of a
% cell array again? Intuitively, we would again go ahead and simply use the
% index-based approach we learned about above:

demoCell(3)

% Hm, the result of this operation, however, is not a character array, but
% instead still a cell array, but containing only the element at position
% three!
%
% If we want to get the actual content of the cell at position three, we
% need to use curly brackets with the index 3:

demoCell{3}

% In this way, we could retrieve the actual content of the cell, not only a
% smaller cell array. In the same way, we can also send content into a cell
% array, overwriting the current content:

demoCell{3} = 'no longer word B'

% We could see that cell arrays can hold data of different type at the same
% time. This is not limited to numbers and words, it can in fact be any
% type of data. Cell arrays can even hold cell arrays!

demoCell_two = {'word C','word D'}

demoCell{4} = demoCell_two

% Now, we see that there is a cell array stored in a cell array - is it not
% beautiful? But how do we get directly to the data stored inside of the
% second cell array? The answer is: double indexing!

demoCell{4}{2}

%% --- Loops
% Now that we have laid out a few of the fundamental variables and arrays,
% we still have to deal with one really central concept: loops! Loops are
% extremely powerful, as they can carry out one and the same procedure,
% with slight modifications. Let us start with a simple for loop:

for kk = 1:15
    disp(kk)
end

% This already shows most of the key aspects of a for loop. First, you have
% an iteration variable, in this case called kk (never just call a variable
% k, it will be very hard to find this veriable with a search function).
% For every execution of the loop, the variable kk will be assigned one
% specific value, in this case in the range from 1 to 15. This value is
% then used to execute all commands contained with in the for loop. Then,
% the next value of kk is assigned, and the content of the loop is executed
% another time, and again, and again, until all specified values for the
% variable kk have been dealt with.

%% ---
% One can also come up with more complex loops, like the following:

stringCell = {'word A','word B','word C'};
numWords = numel(stringCell);

for kk = 1:numWords
    disp(kk)
    disp(stringCell{kk})
end