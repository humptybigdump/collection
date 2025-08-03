clear all; close all; clc

% exercise 1
% logistic map

a=[0:0.2:3 3.1:0.05:3.45 3.46:0.02:3.54 3.55:0.01:3.57 3:58:0.02:4];
x0=[0.4];

for k=1:numel(a)
    a_chosen=a(k)
    for j = 1:numel(x0)
        x(1) = x0(j);
        for i = 2:500
            x(i)=a_chosen*x(i-1)*(1-x(i-1));
        end
        plot(x,'ob','markersize',2);hold on
        xlabel('n');
        ylabel('x')
        title(['Initial value x_0 = ' num2str(x0(j),'%1.2f') ', Parameter a =' num2str(a(k),'%1.2f')])
        ylim([-0.5 1.5])
        drawnow
        pause(0.1)
    end
    hold off
end
