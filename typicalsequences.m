%% AIT tutorial 2024/2025, Exercise 3, Problem 10: Typical Sequences

clear all

%% Given data
K=10;
pa=.7; pb=.3;
delta=.1 * log2(pa/pb)

%% Information of a single symbol
Ia=-log2(pa)

Ib=-log2(pb)

%% Information of a single meta symbol
Is=[10*Ia, ...
    7*Ia+3*Ib, ...
    6*Ia+4*Ib, ...
    3*Ia+7*Ib, ...
    10*Ib]

%% Entropy of the source
Hq=pa*Ia+pb*Ib

%% Limit for the definition of the typica sequences
limitH=K*[Hq-delta Hq+delta]

%% Are the sequences 1-5 in the limits?
istypical =(Is>=limitH(1) & Is<=limitH(2)) 

%% Estimation of the number of typical sequences
limitN = [(1-delta)*2^(K*(Hq-delta)) 2^(K*(Hq+delta))] 

%% Consider all possible sequences
I10= (10:-1:0)*Ia + (0:10)*Ib                            % potential information of a sequence
istypical10=(I10>=limitH(1) & I10<=limitH(2))     % which are typical? --> all with exactly 2, exactly 3 or exactly 4 b
num_coeff=nchoosek(10,2) + nchoosek(10,3) + nchoosek(10,4)  % number via binomial coefficients (n over k) 

%% Comparison with estimation:
vergl_b = num_coeff>=limitN(1) && num_coeff<=limitN(2)

%% Probability for a typical sequence
ptypical = nchoosek(10,2)*(pa^8 * pb^2) + ...
           nchoosek(10,3)*(pa^7 * pb^3) + ...
           nchoosek(10,4)*(pa^6 * pb^4)

%% Comparison with 1-delta       
limitP=1-delta
vergl_c = ptypical >= limitP

