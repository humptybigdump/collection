A = [17 3 14 2 15 9 5; 12 5 12 7 13 9 10; 11 4 15 6 10 8 9; 9 4 29 5 10 8 8; 7 4 14 5 10 7 8; 7 3 13 4 8 6 8; 5 6 12 9 7 6 7; 4 3 29 4 7 5 15];



for i=1:7
    summe(i) = 0;
    for j = 1:8
        summe(i) = summe(i) + A(j,i);
    end
    for j=1:8
        A(j,i) = A(j,i)/summe(i);
    end
end
disp(A);


q1 = [2, 4, 1, 3, 2, 2, 1, 5];

cosdq(i) = 0;
quadratsumme(i) = 0;

for i=1:7
    for j =1:8
        cosdq(i) = cosdq(i) + A(j,i)*q1(j);        
    end
end
disp("cosdq für q1:");
disp(cosdq/norm(cosdq));


q2 = [1, 6, 3, 1, 4, 1, 2, 3];

cosdq(i) = 0;
quadratsumme(i) = 0;

for i=1:7
    for j =1:8
        cosdq(i) = cosdq(i) + A(j,i)*q2(j);        
    end
end
disp("cosdq für q2:");
disp(cosdq/norm(cosdq));

q3 = [5, 1, 1, 2, 1, 5, 7, 4];

cosdq(i) = 0;
quadratsumme(i) = 0;

for i=1:7
    for j =1:8
        cosdq(i) = cosdq(i) + A(j,i)*q3(j);        
    end
end
disp("cosdq für q3:");
disp(cosdq/norm(cosdq));




        