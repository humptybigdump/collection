function [EIS_Data] = readData(File_Str)
% READ_IN_ZVIEW_DATA_FILES reads in ZView data files in ASCII-Format and
% return a cell Array with Structures containing Impedance and additional
% data
%--------------------------------------------------------------------------
%   Input: Path of the .z file as a 'string'
%--------------------------------------------------------------------------
%   Output:
%   EIS_Data	Cell Array of Structures
%       .freq           frequency [mx1], in Hz, m number of frquencies
%       .Z1_re          real part of impedance [mx1], in Ohm, CE-WE
%       .Z1_im          imaginary part of impedance [mx1], in Ohm, CE-WE
%       .Z1_errorcode   error code for the measurement of this frequency
%                       [mx1], CE-WE
%       .Z2_re          real part of impedance [mx1], in Ohm, Aux1
%       .Z2_im          imaginary part of impedance [mx1], in Ohm, Aux1
%       .Z2_errorcode   error code for the measurement of this frequency
%                       [mx1], Aux1
%       .Z3_re          real part of impedance [mx1], in Ohm, Aux2
%       .Z3_im          imaginary part of impedance [mx1], in Ohm, Aux2
%       .Z3_errorcode   error code for the measurement of this frequency
%                       [mx1], Aux2
%--------------------------------------------------------------------------

    % read out data in header
    fid = fopen(File_Str);
    b_header = 1;
    count_i = 0;
    while b_header
        count_i = count_i+1;
        A = textscan(fid,  '%s', 1,'Delimiter', '!"�$!"�$XYZ');
        if  length((strfind(A{1}{1}, 'Date:')))
            tmp = textscan(A{1}{1},  '%s %s');
            Date_str = tmp{2}{1};
            A = textscan(fid,  '%s', 1,'Delimiter', '!"�$!"�$XYZ');
            tmp = textscan(A{1}{1},  '%s %s');
            Time_str = tmp{2}{1};
        end
        if  length((strfind(A{1}{1}, 'Open Circuit Potential (V):')))
            tmp = textscan(A{1}{1},  '%s %s', 'Delimiter', ':');
            OCV_str = tmp{2}{1};
        end
        if  length((strfind(A{1}{1}, 'Measure Rate #2:')))
            tmp = textscan(A{1}{1},  '%s %s', 'Delimiter', ':');
            f_Tast = str2double(tmp{2}{1});
        end               
        b_header = length(strfind(A{1}{1}, 'End Header:')) == 0;
    end
    EIS_Data.t_abs = datenum([Date_str ' ' Time_str], 'dd.mm.yyyy HH:MM:SS');
    EIS_Data.OCV = str2double(regexprep(OCV_str, ',', '.'));

    B = textscan(fid, '%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f',...
            'Delimiter', '\t');

    fclose(fid);
    EIS_Data.freq = B{1};
    EIS_Data.t = B{4};
    EIS_Data.Z1_re = B{5};
    EIS_Data.Z1_im = B{6};
    EIS_Data.Z1_errcode = B{8};
    EIS_Data.Z2_re = B{10};
    EIS_Data.Z2_im = B{11};
    EIS_Data.Z2_errcode = B{13};
    EIS_Data.Z3_re = B{15};
    EIS_Data.Z3_im = B{16};
    EIS_Data.Z3_errcode = B{18};
    EIS_Data.f_Tast = f_Tast;