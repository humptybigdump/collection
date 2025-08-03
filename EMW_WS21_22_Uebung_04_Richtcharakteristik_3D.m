function richtcharakteristik3d()

theta = linspace(0, pi, 101);
phi = linspace(0, 2*pi, 101);
[phi,theta] = meshgrid(phi,theta);

S = @(h_over_l,theta) ((cos(pi*h_over_l .* cos(theta)) - cos(pi*h_over_l))./sin(theta)).^2;

h_2_over_lambda = [1/2; 1; 2];


for k = 1:length(h_2_over_lambda)
    figure();
    view(3)
   
    r = S(h_2_over_lambda(k), theta);
    x = r.*sin(theta).*cos(phi);
    y = r.*sin(theta).*sin(phi);
    z = r.*cos(theta);
    surface(x,y,z,r,'EdgeColor','none');
    light
    axis off
    axis equal
    colormap(winter)
end

end