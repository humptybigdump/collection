%**************************************************************
%  Simulation of a 2d Random Walk Process (advection, diffusion)
% x(t+dt)=x(t)+ux*dt+R*sqrt(6 *D*dt)
% y(t+dt)=y(t)+uy*dt+R*sqrt(6 *D*dt)
%**************************************************************
% create grid and dimension arrays
dx=0.01;
dy=0.01;
% creates a 2 d grid
[X,Y] = meshgrid(-10:dx:10, -10:dy:10);
dV=dx.*dy*1; % volume of grid element
dim=1000; % number of particles
p_mass= 1/(1000*dim) %  particle mass in kg, total solute mass is 0.001 kg

Np=zeros(length(X(1,:)),1);% Integral number of particle as function of x to calculate C(x,t)
xn=zeros(dim,1);
yn=zeros(dim,1);


% set time step and parameters

dt=100000; % time step in secondes

ux=0.e-9 % x component of velocity in m/s
uy= 0.e-6 % y component of velocity in m/s

D=1.e-9; % diffusion coefficient m^2 /s


rand_step_x= sqrt(6*D*dt);% maximum random step in x
rand_step_y=sqrt(6*D*dt);
tmax=100*dt;
time=0.;

%**************************************************************
% time loop
%**************************************************************
n=1;
time=0;
while (time < tmax)
    time=time+dt
     sx= rand_step_x *(2*rand(dim,1)-ones(dim,1)); %random step in x
     sy= rand_step_y *(2*rand(dim,1)-ones(dim,1)); % %random step in y 
%    sx= rand_step_x *randn(dim,1) %random step in x
%    sy= rand_step_y *randn(dim,1); % %random step in y 
    xn=xn+sx+ux*dt; % new position in x xn=xn + u*dt + R*sqrt(2*D*dt)
    yn=yn+sy+uy*dt; % new position in y 
    Mbla(n)=getframe;
    subplot(2,1,1);
    plot(xn,yn,'r+')
    axis([-1 1 -1 1]);
    xlabel( 'x [m]','fontsize',14);
    ylabel( 'x [m]','fontsize',14);
    set(gca,'fontsize',14,'linewidth',2); 
    n=n+1;
    Np=zeros(length(X(1,:)),1);
    for i=1:length(X(1,:))-1;
        ip=find( xn > X(1,i) & xn <= X(1,i+1));
         Np(i)=length(ip);
    end
    subplot(2,1,2);
    h1=plot(X(1,:),Np/(dim*dx),'b-','linewidth',2);
    %plot(X(1,:),50*p_mass*Np/dV(1,1),'b-','linewidth',2);
    hold on;
    c_analytical=1/sqrt(4*pi*D*time)*exp(-X(1,:).*X(1,:)/(4*D*time));
    h2=plot(X(1,:),c_analytical,'r-','linewidth',2);
    hold off;
    legend('particle density','Gaussian distr.');
    xlabel( 'x [m]','fontsize',14);
    ylabel( 'C [kg/m^3]','fontsize',14);
    xlim([-1 1])
    ylim([0 25]);
    set(gca,'fontsize',14,'linewidth',2); 

end;
 
