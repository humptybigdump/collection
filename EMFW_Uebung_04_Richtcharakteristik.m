function EMFW_Uebung_04_Richtcharakteristik()

theta = linspace(0, pi, 1001);
S = @(h_over_l,theta) ((cos(pi*h_over_l .* cos(theta)) - cos(pi*h_over_l))./sin(theta)).^2;

h_over_lambda = [1/2; 1; 3/2];


for k = 1:length(h_over_lambda)
    figure;
    r = S(h_over_lambda(k), theta);
    
    polarplot(theta, r, 'LineWidth', 1);
    ax = gca();
    set(ax,'ThetaZeroLocation','top','ThetaDir','clockwise','RTick',[]);
    ax.ThetaAxis.Label.String = "ϑ";
    ax.ThetaLim = [0 180];
end

end