clear all; clc;
a = dir('*.xlsx');
FileName = a(2).name;
SheetNames = sheetnames(FileName);
NumData = cell(length(SheetNames),1);
for ii = 1:1:length(SheetNames)
    NumData{ii,1} = xlsread(FileName, SheetNames(ii));
end