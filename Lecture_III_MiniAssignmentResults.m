%% ================== MINI ASSIGNMENT LECTURE III ==================== %%
% ======================================================================= %
% ======================================================================= %
% ======================================================================= %
% ======================================================================= %


% Initialize script
clear all; clc; close all; 

% ========================= Mini Assignment 1 =========================== %

[NumericData]   = xlsread('TestData.xlsx'); 

[~, TextData]   = xlsread('TestData.xlsx'); 

[~, ~, RawData] = xlsread('TestData.xlsx'); 

% ========================= Mini Assignment 2 =========================== %

% Read in the sheet names automatically based on the file name
[~, SheetNames] = xlsfinfo('TestData.xlsx');

% ========================= Mini Assignment 3 =========================== %
% Initiate script
clear all; clc; 
 
% List all documents that are .xlsx format and store them in the variable
% FileName 
% *** THIS ONLY WORKS LIKE THIS IF TESTDATA.XLSX IS THE ONLY XLSX-FILE IN
% YOUR CURRENT FOLDER  ***
FileName = dir('*.xlsx');
 
% Read in the sheet names automatically based on the file name
[~, SheetNames] = xlsfinfo(FileName.name);
 
% Read data from the first and the second sheet
for ii = 1:2
    [NumData{ii}] = xlsread(FileName.name, SheetNames{ii});
end
päih
