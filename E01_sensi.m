clear all; close all; clc

% exercise 1
% logistic map
% sensitivity to initial conditions

a=[3.8];
x0=[0.5];

for k=1:numel(a)
    a_chosen=a(k)
    for j = 1:numel(x0)
        x(1) = x0(j);
        x2(1) = x0(j)+0.01;
        for i = 2:100
            x(i)=a_chosen*x(i-1)*(1-x(i-1));
            x2(i)=a_chosen*x2(i-1)*(1-x2(i-1));
        end
        figure(1)
        plot(x,'-ob','markersize',2);hold on
        plot(x2,'--xr','markersize',2);hold on
        xlabel('n');
        ylabel('x')
        title(['Initial value x_0 = ' num2str(x0(j),'%1.2f') ', Parameter a =' num2str(a(k))])
        ylim([-0.5 1.5])
        legend('with initial x_o', 'with initial x_o + 0.01')
        drawnow
    end
    hold off
end