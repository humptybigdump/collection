function bcMatrix = fun(location,state,y_or)
    N = length(y_or);
    x_or = linspace(-pi,pi-2*pi/N,N);
    bcMatrix = interp1([x_or- 2*pi, x_or,x_or + 2*pi],[y_or,y_or,y_or],angle(location.x+1i*location.y));
    if isnan(bcMatrix) == 1
        
        pause
    end
end