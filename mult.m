function pro=mult(MAT)

[nr, nc]=size(MAT);
pro=1;
for ir=1:nr
    for ic=1:nc
        pro=pro*MAT(ir,ic);
    end
end
fprintf(1,'The product of all elements of the matrix MAT is: %f\n', pro);