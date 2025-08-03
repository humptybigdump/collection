function ave = aver(Vec)

ss=0;
for ii=1:numel(Vec)
    ss=ss+Vec(ii);
end
ave=ss/numel(Vec);
fprintf(1,'The average of each element of Vec is: %f\n', ave);