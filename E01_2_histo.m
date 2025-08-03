clear all; close all; clc

% exercise 1
% sensitivity to initial conditions and histogram

%a=[3 3.2 3.5];
a=[3.8]
x0=[0.05:0.1:0.95];

for k=1:numel(a)
    a_chosen=a(k)
    for j = 1:numel(x0)
        x(1) = x0(j);
        for i = 2:10000
            x(i)=a_chosen*x(i-1)*(1-x(i-1));
        end
        figure(1)
        subplot(2,1,2)
        histogram(x,[0:0.01:1]);
        xlabel('x');
        ylabel('n')
        title(['Initial value x_0 = ' num2str(x0(j),'%1.2f') ', Parameter a =' num2str(a(k))])
        xlim([0 1])
        ylim([0 1000])
        subplot(2,1,1)
        plot(x,'-o');
        xlabel('n');
        ylabel('x_n')
        title(['Initial value x_0 = ' num2str(x0(j),'%1.2f') ', Parameter a =' num2str(a(k))])
        ylim([0 1])
        xlim([1000 1100])
        drawnow
        pause(0.1)
    end
end