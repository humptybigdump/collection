function ma=maxi(Vec)

ma=Vec(1);
for ii=1:numel(Vec)
    if Vec(ii)>ma
        ma=Vec(ii);
    end
end
fprintf(1,'The maximum in the vector Vec is: %f\n', ma);