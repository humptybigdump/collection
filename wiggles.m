clear all
close all
clc

for n=1:2:15
    clf
    % Given (n+1) data points
    % equispaced between x0 and xn
    x0 = -1; xn = 1;
    x = linspace(x0,xn,n+1);  %generate the x-values of the sample points
    f=@(x)1./(1+25*x.^2);% we can generate a function handle directly here
                         % or we can define the function at the end of the script
                         % or save the function as a separate file.
    y=f(x); 
    % of course we can also directly compute y as below
    % y = (x)1./(1+25*x.^2); 
        
    % plot the known data points
    plot(x,y,'ok','LineWidth',1.5)
    hold on
    
    % first plot the true solution
    xtrue=linspace(x0,xn,100);
    ytrue=f(xtrue);
    plot(xtrue,ytrue,'k','LineWidth',1.2)
    
    % do interpolation at a bunch of x-points
    % to show fitted-curve
    xinterp=linspace(x0,xn,40);
    for kk=1:length(xinterp)
        yinterp=0;
        for i=1:n
            Ly=y(i);
            for j=1:n
                if j ~= i
                    Ly=Ly*(xinterp(kk)-x(j))/(x(i)-x(j));
                end
            end
            yinterp=yinterp+Ly;
        end
        ycurve(kk)=yinterp;
    end
    
    %show result
    %disp(yinterp);
    hold on
    plot(xinterp, ycurve,'--r','LineWidth',1.2);
    ylim([-0.5 2])
    grid on
    title(['degree of polynomial :' num2str(n)])
    legend('known data points', 'true solution', 'approximated function')
    drawnow
    pause
    %pause %if you want to pause and continue manually
    
end
% to mitigate the problem of polynomial wiggles, 
% we can use spline interpolation,
% cf. "spline" built-in function 
ysp=spline(x,y,xinterp);
plot(xinterp,ysp,':g','linewidth',2)

legend('known data points', 'true solution', 'approximated function','spline')


%function y=myfun(x)
%y=1./(1+25*x.^2);
%%y=sin(x)
%end
