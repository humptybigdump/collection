%% Plot der Funktion f(x) = x, x \in (-pi, pi)

x = linspace(-pi, pi, 1001);
f = x;

plot(x, f, 'LineWidth', 1.5)
hold on
plot([-1.1*pi, 1.1*pi], [0, 0], 'k')
plot([0 0], [-1.1*pi, 1.1*pi], 'k')
hold off
axis equal

%% Plot der Fourier-Poylone p_1, p_3, p_5, p_11

k = (1:11).';
b_k = 2 ./ k .* (-1).^(k+1);

fourier_terms = (b_k*ones(size(x))) .* sin(k*x);

p_1 = fourier_terms(1, :);
p_3 = sum(fourier_terms(1:3, :));
p_5 = sum(fourier_terms(1:5, :));
p_11 = sum(fourier_terms(1:11, :));

plot(x, f, 'LineWidth', 1.5);
hold on
plot(x, p_1, 'LineWidth', 1.5);
plot(x, p_3, 'LineWidth', 1.5);
plot(x, p_5, 'LineWidth', 1.5);
plot(x, p_11, 'LineWidth', 1.5);
legend({'f', 'p_1', 'p_3', 'p_5', 'p_{11}'}, 'AutoUpdate', 'off', 'Location', 'northwest')

plot([-1.1*pi, 1.1*pi], [0, 0], 'k')
plot([0 0], [-1.1*pi, 1.1*pi], 'k')
hold off
axis equal

%% Plot der 2\pi-periodischen Fortsetzung von f(x) = x, x \in (-pi, pi)

x = linspace(-3*pi, 3*pi, 3001);
x1 = linspace(-3*pi, -pi, 1001);
x2 = linspace(-pi, pi, 1001);
x3 = linspace(pi, 3*pi, 1001);
f = x2;
f2 = x2;
f3 = x2;

f_handle = plot(x1, f, 'LineWidth', 1.5);
blue_col = get(f_handle, 'Color');
hold on
plot(x2, f2, 'LineWidth', 1.5, 'Color', blue_col)
plot(x3, f3, 'LineWidth', 1.5, 'Color', blue_col)
plot([-3.1*pi, 3.1*pi], [0, 0], 'k')
plot([0 0], [-1.1*pi, 1.1*pi], 'k')
hold off
axis([-3.1*pi, 3.1*pi, -1.1*pi, 1.1*pi])
axis equal

%% Plot der Fourier-Poylone p_1, p_3, p_5, p_11

k = (1:11).';
b_k = 2 ./ k .* (-1).^(k+1);

fourier_terms = (b_k*ones(size(x))) .* sin(k*x);

p_1 = fourier_terms(1, :);
p_3 = sum(fourier_terms(1:3, :));
p_5 = sum(fourier_terms(1:5, :));
p_11 = sum(fourier_terms(1:11, :));

f_handle = plot(x1, f, 'LineWidth', 1.5);
blue_col = get(f_handle, 'Color');
hold on
plot(x, p_1, 'LineWidth', 1.5);
plot(x, p_3, 'LineWidth', 1.5);
plot(x, p_5, 'LineWidth', 1.5);
plot(x, p_11, 'LineWidth', 1.5);
legend({'f', 'p_1', 'p_3', 'p_5', 'p_{11}'}, 'AutoUpdate', 'off', 'Location', 'northwest')

plot(x2, f2, 'LineWidth', 1.5, 'Color', blue_col)
plot(x3, f3, 'LineWidth', 1.5, 'Color', blue_col)
plot([-3.1*pi, 3.1*pi], [0, 0], 'k')
plot([0 0], [-1.1*pi, 1.1*pi], 'k')
hold off
axis([-3.1*pi, 3.1*pi, -1.1*pi, 1.1*pi])
axis equal

%% Plot der 2\pi-periodischen Fortsetzung von f(x) = x^2, x \in (-pi, pi)

x = linspace(-3*pi, 3*pi, 3001);
x1 = linspace(-3*pi, -pi, 1001);
x2 = linspace(-pi, pi, 1001);
x3 = linspace(pi, 3*pi, 1001);
f = x2.^2;
f2 = x2.^2;
f3 = x2.^2;

f_handle = plot(x1, f, 'LineWidth', 1.5);
blue_col = get(f_handle, 'Color');
hold on
plot(x2, f2, 'LineWidth', 1.5, 'Color', blue_col)
plot(x3, f3, 'LineWidth', 1.5, 'Color', blue_col)
plot([-3.1*pi, 3.1*pi], [0, 0], 'k')
plot([0 0], [-1, 10], 'k')
hold off
axis equal

%% Plot der Fourier-Poylone p_0, p_2, p_4, p_11

k = (1:11).';
a_0 = pi^2/3;
a_k = 4 ./ k.^2 .* (-1).^(k);

fourier_terms = [ a_0 * ones(size(x)); (a_k*ones(size(x))) .* cos(k*x)];

p_0 = fourier_terms(1, :);
p_2 = sum(fourier_terms(1:3, :));
p_4 = sum(fourier_terms(1:5, :));
p_11 = sum(fourier_terms(1:12, :));

f_handle = plot(x1, f, 'LineWidth', 1.5);
blue_col = get(f_handle, 'Color');
hold on
plot(x, p_0, 'LineWidth', 1.5);
plot(x, p_2, 'LineWidth', 1.5);
plot(x, p_4, 'LineWidth', 1.5);
plot(x, p_11, 'LineWidth', 1.5);
legend({'f', 'p_0', 'p_2', 'p_4', 'p_{11}'}, 'AutoUpdate', 'off', 'Location', 'northwest')

plot(x2, f2, 'LineWidth', 1.5, 'Color', blue_col)
plot(x3, f3, 'LineWidth', 1.5, 'Color', blue_col)
plot([-3.1*pi, 3.1*pi], [0, 0], 'k')
plot([0 0], [-1, 10], 'k')
hold off
axis equal

%% Plot der Funktion f(x) = exp(sin(x))

x = linspace(-3*pi, 3*pi, 3001);
f = exp(sin(x));

f_handle = plot(x, f, 'LineWidth', 1.5);
blue_col = get(f_handle, 'Color');
hold on
plot([-3.1*pi, 3.1*pi], [0, 0], 'k')
plot([0 0], [-0.25, 3.5], 'k')
hold off
axis([-10, 10, -0.5, 3.75]);

%% Plot der Fourier-Poylone p_0, p_2, p_4, p_11

k = (-11:11).';
c_k = zeros(23, 1);
for l = -11:11
    integrand = @(t) exp(sin(t)) .* exp(-1i * l * t) / (2*pi);
    c_k(l+12) = integral(integrand, -pi, pi);
end

fourier_terms = [(c_k * ones(size(x))) .* exp(1i*k*x)];

p_0 = fourier_terms(12, :);
p_2 = sum(fourier_terms(10:14, :));
p_4 = sum(fourier_terms(8:16, :));
p_11 = sum(fourier_terms(1:23, :));

f_handle = plot(x, f, 'LineWidth', 1.5);
blue_col = get(f_handle, 'Color');
hold on
plot(x, real(p_0), 'LineWidth', 1.5);
plot(x, real(p_2), 'LineWidth', 1.5);
plot(x, real(p_4), 'LineWidth', 1.5);
plot(x, real(p_11), 'LineWidth', 1.5);
legend({'f', 'p_0', 'p_2', 'p_4', 'p_{11}'}, 'AutoUpdate', 'off', 'Location', 'northwest')

plot([-3.1*pi, 3.1*pi], [0, 0], 'k')
plot([0 0], [-0.25, 3.5], 'k')
hold off
axis([-10, 10, -0.5, 3.75]);
