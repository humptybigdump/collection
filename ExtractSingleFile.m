% We state the location where the file can be found on the hard drive
% This variable is called a string, it contains a sequence of letters, or
% characters. Note the semicolon ; at the end of the line, it suppresses
% printing of the result to the console. In MatLab, unless you put the
% semicolon, the result of each command will be printed to the console
filepath = '../DemonstrationData_NuclearActin/CtrlA_002.nd2';

% For MatLab to interact with the file saved on the hard drive, we set up a
% reader object. The reader object then handles all the requests from
% MatLab, and serves up information and data from the raw image file.

reader = bfGetReader(filepath);

% --- extract stack and microscope info from meta data

% Get the number of different image series from the file. This typically
% refers to multiple positions, at which images were recored. The way that
% this information is requested is by using a 'method' of the reader
% object. The method is a command, or operation, that can be executed by
% an object, an can return results with which we continue to work. In this
% case, the methods returns a number, which tells us how many positions
% were acquired and saved into the raw image file.

numSeries = reader.getSeriesCount;

% Once we know how many series are contained in the file, we can
% pre-allocate variables that will contain all the crucial information for
% each of these positions. We are going to store this information in cell
% arrays. Cell arrays are a specific type of variable, which can have
% several positions, but each position is completely free in terms of what
% type of content it can hold. For example, one position can hold a number,
% another position can hold an object, and yet another position can even
% hold another cell array. Preallocation is done to improve performance:
% resizing a cell or a number array always requires that the current memory
% representation of the object is completely erased, and a new one of the
% changed size needs to be created. So, it is better to create a variable
% of the right size immediately, and then later fill it with content.

ImgSeries_cell = cell(1,numSeries); % To hold the actual image data
arrayDim_cell = cell(1,numSeries); % XYZ pixel count of the image data
voxelSize_cell = cell(1,numSeries); % Physical size of each voxel
seriesName_cell = cell(1,numSeries); % Name of the positions

for nn = 1:numSeries
	
    % While we runnign through the different series by counting up from an
    % index of 1, the reader object used a counting index that starts from
    % 0. This so-called 0-based indexing needs to be taken into
    % consideration when we request data and information from the reader,
    % so that we generate a shifted index:
	shiftedSeriesNumber = nn-1; 
		
    % For this series, we can now request the MetaData. The MetaData are
    % not the actual images taken by the microscope. Instead, they contain
    % all the additional information that it required to make this images
    % meaningful. The MetaData will, again, be stored in an object, with
    % which we can interact to obtain the relevant information.
	omeMeta = reader.getMetadataStore();
    
    % --- get the voxel edge sizes
    % One crucial piece of information is the physical size of a voxel
    % (three-dimensional pixel, or volume pixel). We can request this
    % information from the MetaData. First, we request the information
    % stored in the MetaData for the series we are currently interested in.
    voxelSizeX = omeMeta.getPixelsPhysicalSizeX(shiftedSeriesNumber);
    % Then, we need to convert the data obtained in this way into a
    % specific physical unit (in this case, micrometers), and also into a
    % variable type that MatLab can easily work with (in this case a real
    % number with double precision, or in short, a 'double')
    voxelSizeX = voxelSizeX.value(ome.units.UNITS.MICROM);
    % We still do the same for the y and the z dimensions
    rawVoxelSizeX = voxelSizeX.doubleValue();
    voxelSizeY = omeMeta.getPixelsPhysicalSizeY(shiftedSeriesNumber);
    voxelSizeY = voxelSizeY.value(ome.units.UNITS.MICROM);
    rawVoxelSizeY = voxelSizeY.doubleValue();
    % Some data sets do not contain a z voxel size dimensions. This is the
    % case, for example, if only a single z section was recorded. The
    % try-catch construct takes care of this problem: in case the
    % expression in the try statement throws an error, the program
    % execution will continue, and instead of an actual number, NaN will be
    % saved (Not-a-Number). This way the script does not crash, but also no
    % misleading information is stored for the z dimension.
	try
		voxelSizeZ = omeMeta.getPixelsPhysicalSizeZ(shiftedSeriesNumber);
		voxelSizeZ = voxelSizeZ.value(ome.units.UNITS.MICROM);
		rawVoxelSizeZ = voxelSizeZ.doubleValue();
	catch
		rawVoxelSizeZ = NaN;
    end
	
    % Now that we have obtained the information on the voxel dimension, we
    % need to somehow store it for future reference. The next line contains
    % a little bit of variable conversion magic. The first thing to
    % consider is, what variables are we starting with? In this case,
    % rawVoxelSizeX, rawVoxelSizeY, rawVoxelSizeZ are simple numbers. These
    % can be combined into a vector with three elements by using the square
    % brackets []. You can see below how the three variables are placed
    % inside the brackets, and seperated by a comma. In this fashion, they
    % become 'concatenated' into a vector. This vector now is a new
    % variable, that contains the three numbers in it. We still need to
    % store this newly generated variable somehow. So, what we do is to use
    % the previously generated cell arra voxelSize_cell, and place the
    % vector into the position that is associated with the current image
    % series. For cell arrays, addressing the content at a given position
    % requires the use of curly brackets {}. This is different from
    % vectors, so keep in mind to use curly brackets for cell arrays, if
    % you want to directly access the data saved at a specific position.
    voxelSize_cell{nn} = [rawVoxelSizeX,rawVoxelSizeY,rawVoxelSizeZ];
    
    % --- get the spatial stack dimensions
    % Other than the physical dimension of a single voxel, it is also
    % crucial to know the overall dimension of a given image stack. This is
    % necessary to read the image data into MatLab in a controlled fashion,
    % and also to allocate the right size of array for storing the data we
    % obtain via the reader object. Again, we are using a method of the
    % MEtaData object to get these numbers. In this case, not so much
    % conversion is needed, because the pixel size does not need a physical
    % unit, it is simply an integer number.
    rawStackSizeX = omeMeta.getPixelsSizeX(shiftedSeriesNumber).getValue(); % image width, pixels
    rawStackSizeY = omeMeta.getPixelsSizeY(shiftedSeriesNumber).getValue(); % image height, pixels
    rawStackSizeZ = omeMeta.getPixelsSizeZ(shiftedSeriesNumber).getValue(); % image height, pixels
	
    % Like the physical voxel size, the XYZ size of the image stack is
    % stored in the form of a vector with three elements, which is placed
    % at a specific position of a cell array.
	arrayDim_cell{nn} = [rawStackSizeY,rawStackSizeX,rawStackSizeZ];
    
    % Now, we have obtained the crucial MetaData, and it is time to extract
    % actual raw image data. For this, we will again address the reader
    % object with have craeted ('instantiated') above.
	
    % The reader object always is set to one of the series contained in the
    % raw image data. If we want to access the data from one of these
    % positions, we first need to set the reader to the according series:
	reader.setSeries(shiftedSeriesNumber);

    % Most fluorescence microscopy images contain data from multiple color
    % channels. To properly read in the data from the different color
    % channels, we first need to obtain the number of channels in the data
    % set. This is done by obtaining the 'size' of the stack in the 'color
    % dimension', which is indicated by the letter C in the bioformats
    % convention. The command is as follows:
	numChannels = reader.getSizeC();
	
    % Now, we need to prepare (pre-allocate) space to store the data
    % obtained from each color channel. This is the point where we put a
    % cell array into another cell array. The top level cell array contains
    % image information for each of the multiple positions. Then, for each
    % position, we allocated inside of the first cell array a second cell
    % array, which is large enough to contain the information for the
    % different color channels.
	ImgSeries_cell{nn} = cell(1,numChannels);
    
    % We now will use a for loop, so we can step over the different color
    % channels and pull out the information for that color channel.
	for cc = 1:numChannels
		
        % Now, we do even more pre-allocation. This time, we pre-allocate
        % at the third level: we put a matrix of size X-by-Y-by-Z into the
        % second level cell array we created just above. This way, we have
        % a place to put the images obtained at each z section. 
		ImgSeries_cell{nn}{cc} = ...
			zeros(rawStackSizeY,rawStackSizeX,rawStackSizeZ);
		
        % And here comes the for loop that we use to actually go over each
        % and every z position in the data set, so that we can read in the
        % 2D images one by one from the reader object, and store them into
        % a matrix in MatLab format.
		for zz = 1:rawStackSizeZ
			
            % Here, we finally have the two commands that get the image
            % data from the reader object into MatLab. The first command
            % serves to calculate the index of the image that should be
            % retrieved from the reader object. Here, getIndex means that
            % we give information which color channel and which z plane we
            % would like to obtain, and get back the exact index number
            % that we need to get the corresponding image from the reader
            % object.
			planeInd = reader.getIndex(zz-1,cc-1,0)+1;
            % Then, as we now which exact image to request, we can use the
            % bfGetPlane function to obtain that 'image plane' (a 2D image)
            % from the reader object. This returns a 2D matrix of numbers.
			planeImg = bfGetPlane(reader,planeInd);
            % This 2D matrix of numbers still needs to be stored in a
            % useful way to later work with it inside MatLab. Luckily, we
            % already pre-allocated the perfect place to store that
            % infomration:
			ImgSeries_cell{nn}{cc}(:,:,zz) = planeImg;
            % In this command, note that for the x and the y coordinate, we
            % used the colon : symbol. Only for the z coordinate we used a
            % specific value, contained in the variable zz. This means that
            % in the pre-allocated array to which we are sending the
            % information, we are going to fill the whole range in the x
            % direction, and the whole range in the y direction, but in the
            % z direction we will address only one index. This is exactly
            % how it has to be, as we are storing a single 2D matrix.
			
		end
		
    end
	
    % This last command is not super-crucial, but a nice addition to store
    % the name of the image. For some microscope software, you can assign
    % names to the images and image series, which later during analysis you
    % can retrieve to better sort and assess your data. The command below
    % gets the image name, and stores it for later use.
	ImgName = omeMeta.getImageName(shiftedSeriesNumber);
    % The image name comes in a somewhat funny Bioformats form, and again
    % it needs to be converted into a normal MatLab variable. Once that is
    % done, we can store it into a cell array, which will then contain the
    % name of the image for all series.
	seriesName_cell{nn} = ImgName.toCharArray';
	
end

% Again, this is for good form: When you are done with the reader object,
% it is recommended that you close it. This frees up memory, and prevents
% accidental reading from the wrong reader object by another script.
reader.close()

%% -- Displaying some of the extracted data

% This section is not strictly required for the image extraction.
% Everything is already stored in the different arrays by now. This section
% is only to take a look at some of the stored data, so you can see how it
% can be accessed.

% First, we can simply take a look at the different arrays that we saved
% to. If we call the array names without a semicolon at the end of the
% line, the returned values will be displayed to the console. Let us give
% that a try.

arrayDim_cell
voxelSize_cell
seriesName_cell

% This gives a relatively useful view into the arrays. But in some cases,
% it also would be nice to dig a little deeper to see what is actually
% contained in the ddifferent positions of the cell. We can do this by
% calling one of the positions.

voxelSize_cell{5}
seriesName_cell{5}

%% --- displaying some image data

% We can also try to get some of the actual image data, and have a look at
% it. We have to dig through a bit of indexing here, but it could also be a
% good example to learn about indexing. Simply calling the cell with image
% data will give the following result:

ImgSeries_cell

% We can see that, at every position in the cell array, we have another
% cell array with four elements. To find out what is hidden in these four
% elements, let us choose one of the positions in the cell array. For your
% information, the different positions in the cell array correspond to the
% different positions in the sample, from which images were recorded.

ImgSeries_cell{5}

% So, from this we can see that, in each of the four elements, we find a
% three-dimensional array of numbers. These are the actual image stacks,
% for the four different color channels. So, let us get one of these image
% stacks out to display it:

display_array = ImgSeries_cell{5}{1}(:,:,14);

% Here, we choose the position 5 from a cell array, then the color channel
% 1 (again from a cell array), and set the z position to 14. We take all
% data in the x and y direction, using the colon operator. The resulting
% array actually is two-dimensional, as we can see using the size command:

size(display_array)

% Two-dimensional images are fairly easy to display in MatLab, using the
% imagesc command. This command takes 2D matrices, transfers their number
% values into a color, and automatically scales the color lookup table to
% fit the range of the data in the matrix.

imagesc(display_array)

%% --- making the image display "proper"

% This is already looking very cool, but also needs a few adjustments. For
% example, the axis ratio can be totally off. We can force the axis ratio
% to be equal:

axis equal

% This, however, results in not too good-looking spaces at the side of the
% display - we can fix this by specifying a tight display:

axis tight

% Now you might wonder, how do we get the physical units into the image, so
% that we can see the axes labeled in micrometers, not just pixel counts?
% For this, we can recall the physical voxel size in x direction (the y
% direction has the same pixel size, the z direction is not displayed for
% the image).

pixel_edge = voxelSize_cell{5}(1)

% Now, we can set the proper axis ranges, using also the size of the image
% at hand. Attention, the x and y dimensions are actually mixed up, so we
% have to use the image sizes in the opposite order.

imagesc([0,arrayDim_cell{5}(2)].*pixel_edge,...
    [0,arrayDim_cell{5}(1)].*pixel_edge,...
    display_array)

% Now, we should still give some useful axis labels:
xlabel('x (\mum)')
ylabel('y (\mum)')