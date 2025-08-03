%% Geometrische Modelle der Geodäsie
% Übungsblatt 6 Matlabaufgabe
% Elisabeth Kral, 2352489

clc;
clear variables;

%% Beispiele
x = quaternion(1,2,3,4);
y = quaternion(1,1,1,1);
disp("Addition:");
disp(add(x,y));
disp("Multiplikation: ");
disp(mult(x,y));
disp("Skalare Multiplikation: ");
disp(scal_mult(x,2));
disp("Division:");
disp(div(x,y));
disp("Skalare Division:");
disp(scal_div(x, 0));
disp(scal_div(x, 2));
disp("Betrag:");
disp(mult(x, conj(x)));
disp(magn(x)^2);

%% Funktionsdefinitionen
function r = quaternion(A, B, C, D)
    r = [A B C D];
end
function r = add(a, b)
    r = a + b;
end
function r = mult(a, b)
    r = zeros(1, 4);
    r(1) = (a(1)*b(1) - a(2)*b(2) - a(3)*b(3) - a(4)*b(4));
    r(2) = (a(1)*b(2) + a(2)*b(1) + a(3)*b(4) - a(4)*b(3));
    r(3) = (a(1)*b(3) - a(2)*b(4) + a(3)*b(1) + a(4)*b(2));
    r(4) = (a(1)*b(4) + a(2)*b(3) - a(3)*b(2) + a(4)*b(1));
end
function r = scal_mult(a, B)
    r = a*B;
end
function y = div(a,b)
    y = mult(a, scal_div(conj(b), magn(b)^2));
end
function  r = scal_div(a, B)
    if B == 0
        disp("DIVISION BY 0 ERROR");
        r = NaN;
    else
        r = a/B;
    end
end
function r = conj(a)
    r = [a(1) -a(2) -a(3) -a(4)];
end
function y = magn(a)
    y = sqrt(a(1)^2 + a(2)^2 + a(3)^2 + a(4)^2);
end
