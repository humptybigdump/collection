function [ mean ] = stud_mittelwert(a,b)
%STUD_MITTELWERT Calculation of the mean of two given values.
%
%  Syntax:
%    [ mean ] = STUD_MITTELWERT(a, b)
%
%  Description:
%    Calculation of the mean of the given values a and b.
%
%  Inputs:
%    A          Value one (scalar).
%
%    B          Value two (scalar).
%
%  Outputs:
%    MEAN       Mean of value one and two (scalar).
%
%  Authors: Jan Reinhold (ACON Kiel), Thomas Meurer (KIT)
%  Email: thomas.meurer@kit.edu
%  Website: https://www.mvm.kit.edu/dpe.php
%  Creation date: 01.04.2019
%  Last revision date: 31.10.2023
%  Last revision author: Thomas Meurer (KIT)
%
%  Copyright (c) 2023, DPE/MVM, KIT
%  All rights reserved.
%
%  See also

mean = (a + b) * 0.5;

end
