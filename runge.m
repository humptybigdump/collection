%
% Runges example for divergence of polynom interpolation
%

% Points for the plots
x = linspace(-1,1,1001);

% Runge's function
f = 1 ./ ( 1 + 25 * x.^2 );

% Plot of the function
plot(x,f)
axis( [-1.1 1.1 -0.5 1.5] );

%%

% we interpolate with  polynomials of degree
interp_grad = 4;

% interpolation in the points
x_interp = linspace(-1,1,interp_grad+1);
y_interp = 1 ./ (1 + 25 * x_interp.^2 );

p = polynom_interpol(x_interp,y_interp,interp_grad);

% plot
plot(x,f)
hold on
axis( [-1.1 1.1 -0.5 1.5] );
p_val = polyval(p,x);
plot(x,p_val,'k',x_interp,y_interp,'ro');
hold off

fprintf('n = %2d: Maximal error %e\n', interp_grad, max(abs(f-p_val)) );

%%

% we interpolate with  polynomials of degree
interp_grad = 8;

% interpolation in the points
x_interp = linspace(-1,1,interp_grad+1);
y_interp = 1 ./ (1 + 25 * x_interp.^2 );

p = polynom_interpol(x_interp,y_interp,interp_grad);

% plot
plot(x,f)
hold on
axis( [-1.1 1.1 -0.5 1.5] );
p_val = polyval(p,x);
plot(x,p_val,'k',x_interp,y_interp,'ro');
hold off

fprintf('n = %2d: Maximal error %e\n', interp_grad, max(abs(f-p_val)) );

%%

% we interpolate with  polynomials of degree
interp_grad = 12;

% interpolation in the points
x_interp = linspace(-1,1,interp_grad+1);
y_interp = 1 ./ (1 + 25 * x_interp.^2 );

p = polynom_interpol(x_interp,y_interp,interp_grad);

% plot
plot(x,f)
hold on
axis( [-1.1 1.1 -0.5 1.5] );
p_val = polyval(p,x);
plot(x,p_val,'k',x_interp,y_interp,'ro');
hold off

fprintf('n = %2d: Maximal error %e\n', interp_grad, max(abs(f-p_val)) );

%%

% we interpolate with  polynomials of degree
interp_grad = 20;

% interpolation in the points
x_interp = linspace(-1,1,interp_grad+1);
y_interp = 1 ./ (1 + 25 * x_interp.^2 );

p = polynom_interpol(x_interp,y_interp,interp_grad);

% plot
plot(x,f)
hold on
axis( [-1.1 1.1 -0.5 1.5] );
p_val = polyval(p,x);
plot(x,p_val,'k',x_interp,y_interp,'ro');
hold off

fprintf('n = %2d: Maximal error %e\n', interp_grad, max(abs(f-p_val)) );