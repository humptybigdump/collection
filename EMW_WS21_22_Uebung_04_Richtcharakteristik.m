function richtcharakteristik()

theta = linspace(0, pi, 1001);
S = @(h_over_l,theta) ((cos(pi*h_over_l .* cos(theta)) - cos(pi*h_over_l))./sin(theta)).^2;

% Exam
h_2_over_lambda = [1/2; 1; 2];


for k = 1:length(h_2_over_lambda)
    h = figure;
    r = S(h_2_over_lambda(k), theta);
    
    polarplot(theta, r);
    ax = gca();
    set(ax,'ThetaZeroLocation','top','ThetaDir','clockwise','RTick',[]);
    ax.ThetaAxis.Label.String = "ϑ";
    ax.ThetaTickLabel = ax.ThetaTickLabel + "°";
    ax.ThetaLim = [0 180];
end

end