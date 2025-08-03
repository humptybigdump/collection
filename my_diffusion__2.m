% Routine to solve linear diffusion problems

% Initialisation
% Define grid as vector with node coordinats
dx=0.1; %grid size
x=[-5:dx:5]; % meters
dx_vec=diff(x);

% Define variables & parameters, Diffusion Coefficient D 
D=1e-6; % diffusion in sqm per second

% Define initial state based on analytical solution of the linear diffusion
% problem

t0=1000 % initial time
c=1/sqrt(4*pi*D*t0)*exp(-x.*x/(4*D*t0));
c_old=c;

% Process part
tmax=20*86400; % maximum computation
time=t0; % start time for simulation
itime=1;
RMSE=[0];
mean_error=[0];
RMSE_rel=[0];
mean_error_rel=[0];

sim_time=[time];
time_step=[itime];
figure;
while time <tmax
    %assure stability, i.e. von Neumann condition is fullfilled
    dt=0.1*1/2*dx^2/D;
    % solve equation at boundary nodes 
    c(length(x))=c_old(length(x)-1);
    c(1)=c_old(2); 
    for k=2:length(x)-1
        % calculate new c at time i+1 from c_old at time i 
        c(k)=c_old(k)+2*D*dt*(c(k+1)-c(k))/((x(k+1)-x(k-1))*(x(k+1)-x(k)))...
        -2*D*dt*(c(k)-c(k-1))/((x(k+1)-x(k-1))*(x(k)-x(k-1)));    
        % Assure that solution is realistic, avoid negative c
        if c(k) < 0
            c(k) =0;
        end
    end
    time=time+dt;
    % Update c_old
    c_analytical=1/sqrt(4*pi*D*time)*exp(-x.*x/(4*D*time));
    c_old=c;
    plot(x,c,'linewidth',2);
    hold on;
    plot(x,c_analytical,'r-','linewidth',2);
    %Compute RMSE and Mean error
    res=c_analytical-c; % residual between analyitcal and numerical solution
    %  mean error 
    mean_error=[mean_error; mean(res)];
    mean_error_rel=[mean_error_rel; mean(res)/mean(c_analytical)];
        %  normalised mean error, normalised with mean of analytical solutions
    RMSE=[RMSE;  sqrt(sum(res.*res))]; %root mean square error
    RMSE_rel=[RMSE_rel;  sqrt(sum(res.*res))/mean(c_analytical)];
%    blabla=['time' num2str(time) ' s, Errors' num2str(mean_error) ' ' num2str(RMSE)];
    title(['time' num2str(time/3600) ' h, Mean error  ' num2str(mean_error(itime)) ' ; RMSE  '...
        num2str(RMSE(itime))],'fontsize',14);
    hold off;
    xlabel('x [m]','fontsize',14);
    ylabel('Concentration  or Temperature [kg/m^3 or C]', 'fontsize',14);
    set(gca,'fontsize',14,'linewidth',2);
    M(itime)=getframe;
    itime=itime+1;
    sim_time=[sim_time;time];
    time_step=[time_step; itime];
end
    
figure
subplot(2,2,1);
plot( sim_time/3600,RMSE,'r-','linewidth',2);
xlabel('simulation time [h]','fontsize',14);
ylabel('RMSE absolute', 'fontsize',14);
set(gca,'fontsize',14,'linewidth',2);
subplot(2,2,3);
plot( sim_time/3600,mean_error,'r-','linewidth',2);
xlabel('simulation time [h]','fontsize',14);
ylabel('Mean error absolute', 'fontsize',14);
set(gca,'fontsize',14,'linewidth',2);
subplot(2,2,2);
plot( sim_time/3600,RMSE_rel,'r-','linewidth',2);
xlabel('simulation time [h]','fontsize',14);
ylabel('RMSE normalised', 'fontsize',14);
set(gca,'fontsize',14,'linewidth',2);
subplot(2,2,4);
plot( sim_time/3600,mean_error_rel,'r-','linewidth',2);
xlabel('simulation time [h]','fontsize',14);
ylabel('Mean error normalised', 'fontsize',14);
set(gca,'fontsize',14,'linewidth',2);

