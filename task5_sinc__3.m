x = [-10 : 0.01 : 10];
y_sinc = sinc(x);
y_pos = 1 ./ (pi .* x);
y_neg = -1 ./ (pi .* x);

hold on
plot(x, y_sinc, 'r-')
plot(x, y_pos, 'k:')
plot(x, y_neg, 'k:')
plot(0,0,'bx')