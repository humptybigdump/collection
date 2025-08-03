% ========================================================================================
%   Karlsruher Institute of Technology - Institute for Technical Physics (ITEP)
%
%   Simulation of the transient behaviour of a superconducting fault current limiter (FCL)
%
% ========================================================================================

clc; 
close all;
clear

% Parameter ===============================================================================
um = 0.000001;                                      % Converting units          |   um to m
mm = 0.001;                                         % Converting units          |   mm to m
cm = 0.01;                                          % Converting units          |   cm to m

h     = 1.0e-5;                                     % Time Step                
time  = 0:h:0.10;                                   % Time Vector                                  
ft    = length(time);                               % Size of time Vector  

fq  = 50.0;                                         % Frequency                   >      Hz                         
w   = 2.0*pi*fq;                                    % Omega        
cic = 1;                                            % Number of cycles (Normal operation)

% Set matrices to zero 
Ibg  = zeros(1,ft);             
Icc  = zeros(1,ft);                                
Vq   = zeros(1,ft);            
Vssb = zeros(1,ft); 
Rag  = zeros(1,ft); 
Rsp  = zeros(1,ft); 
Rssb = zeros(1,ft); 
Rg   = zeros(1,ft); 
Tssb = zeros(1,ft); 

% Per unit System =========================================================================
Vb = 200.0;                                         % Base voltage               >        V                       
Pb = 60000.0;                                       % Base power                 >       VA                                                                   
Zb = (Vb)^2/Pb;                                     % Base impedance             >      Ohm                  
Ib = Pb/Vb;                                         % Base current               >        A

% Parameter der Schaltung ================================================================
A   = sqrt(2);                           
phi = 0*pi/180;                                     % Phase shift                >     Grad
   
% 40 kA
Lkz = 2.0e-05 / Zb;                                 % Short-circuit inductance   >     p.u.
RKz = 0.0006  / Zb;                                 % Short-circuit resistance   >     p.u.

% Parameters of the HTS tape ==============================================================
Ll    = 850 * cm;                                   % Length                     >        m                       
breit = 1.2 * cm;                                   % width                      >        m  
nb    = 10;                                         % Number of tapes

% Parameter of Silver =====================================================================
cag   = 225.2;                                      % Specific heat              >   J/kg.K               
espag = 4 * um * nb;                                % Thickness                  >        m                                 
aag   = espag*breit;                                % Cross section area         >       m2                                     
dag   = 1049;                                       % Density                    >    kg/m3                           
Vag   = Ll*aag;                                     % Volume                     >       m3                                          
Cvag  = Vag*dag*cag;                                % Heat Capacity              >      J/K                
mag   = 4e-11;                                      % cte. resistivity                
zag   = -2.0E-9;                                    % cte. resistivity           
cteag = (Ll/aag);                                   % Constant                   >      1/m                       

% Parameters of YBCO ======================================================================                
csp     = 237.5;                                    % Specific heat              >    J/kgK  			
espsp   = 1 * um * nb;                              % Thickness                  >        m
asp     = espsp*breit;                              % Cross section area         >       m2         		
dsp     = 6390;                                     % Density                    >    kg/m3                                              
Vsp     = Ll*asp;                                   % Volume                     >       m3                       
Cvsp    = Vsp*dsp*csp;                              % Heat Capacity              >      J/K                    
rho_TC  = 2e-9;                                     % resistivity at Tc    
rhozero = 1e-13;                                    % resistivity under Tc 
Tc      = 92.0;                                     % Critical Temperature       >        K                                                                  
ctesp   = (Ll/asp);                                 % Constante                  >      1/m                                 
n1      = 15;                                       % Transition index           >     adm.    

% Parameters of Hastelloy =================================================================
chy   = 295.5;                                      % Specific heat >   J/kg.K                      
esphy = 100 * um * nb;                              % Thickness                  >        m                       
ahy   = esphy*breit;                                % Cross section area         >       m2                      
dhy   = 8890;                                       % Density                    >    kg/m3                                               
Vhy   = Ll*ahy;                                     % Volume                     >       m3                                 
Cvhy  = Vhy*dhy*chy;                                % Heat Capacity              >      J/K                   

% Initial Conditions ======================================================================
Tsti = 77.0; 
Tssb(:,1) = Tsti;     

Cb = Cvsp + Cvag + Cvhy;

Rag(1,1) = (cteag * (mag*Tsti  + zag)) / Zb;
Rsp(1,1) = (ctesp * rhozero) / Zb;

Rssb(1,1) = 1/ (1/Rag(1,1) + 1/Rsp(1,1));

% Loop of Time ============================================================================
for s = 2:ft 
    
    zn = time(s);
    zo = time(s-1);
    disp(['zeit = ' num2str(zn)])
     
    % Switching
    if zn <= cic/fq
		Rkz = 2 / Zb;
        
    elseif zn > cic/fq
		Rkz = RKz;
  
    end
    
    % Calculating the fault-current with runge kutta 4th order
    k1_cc = 1/Lkz * (A*sin(w*(zo+0.0*h) + phi) - Rkz*Icc(1,s-1));
    k2_cc = 1/Lkz * (A*sin(w*(zo+0.5*h) + phi) - Rkz*(Icc(1,s-1) + 0.5*h*k1_cc));
    k3_cc = 1/Lkz * (A*sin(w*(zo+0.5*h) + phi) - Rkz*(Icc(1,s-1) + 0.5*h*k2_cc));
    k4_cc = 1/Lkz * (A*sin(w*(zo+1.0*h) + phi) - Rkz*(Icc(1,s-1) + 1.0*h*k3_cc));
    
    Icc(1,s) = Icc(1,s-1) + h/6 * (k1_cc + 2*k2_cc + 2*k3_cc + k4_cc);
    
    % Calculating the limited-current with runge kutta 4th order
    Rg(1,s) = Rkz + Rssb(1,s-1);
    
    k1_bg = 1/Lkz * (A*sin(w*(zo+0.0*h) + phi) - Rg(1,s-1)*Ibg(1,s-1));
    k2_bg = 1/Lkz * (A*sin(w*(zo+0.5*h) + phi) - Rg(1,s-1)*(Ibg(1,s-1) + 0.5*h*k1_bg));
    k3_bg = 1/Lkz * (A*sin(w*(zo+0.5*h) + phi) - Rg(1,s-1)*(Ibg(1,s-1) + 0.5*h*k2_bg));
    k4_bg = 1/Lkz * (A*sin(w*(zo+1.0*h) + phi) - Rg(1,s-1)*(Ibg(1,s-1) + 1.0*h*k3_bg));
    
    Ibg(1,s) = Ibg(1,s-1) + h/6 * (k1_bg + 2*k2_bg + 2*k3_bg + k4_bg);
    
    I_ges    = Ibg(1,s) * Ib;


    % Joule Losses
    P = (Zb * Rssb(1,s-1)) * (I_ges)^2;
    
    % Temperature Calculation with Euler method
    Tssb(1,s) = Tssb(1,s-1) + h*P/Cb;
    
    Tw = Tssb(1,s);
    
    % Update Resistance values
    Rag(1,s) = (cteag * (mag*Tw  + zag)) / Zb;
    
    if Tw < Tc
        rho = rho_TC *(Tw/Tc)^(n1) - 1.3e-10;
    else
        rho = rho_TC *(Tw/Tc);
    end

    Rsp(1,s) = (ctesp * rho) / Zb;

    
    Rssb(1,s) = 1/ (1/Rag(1,s) + 1/Rsp(1,s));
    
    % Voltage Drop over RSFCL
    Vq(1,s)   = A * sin(w*zn + phi); 
    Vssb(1,s) = Rssb(1,s) * Ibg(1,s); 
                                                                                      
end    

% Plotting ===============================================================================
fig = figure('Name','Plots','NumberTitle','off','Color',[1 1 1]);  
set(fig,'units','normalized','outerposition',[0 0 1 1],'defaulttextinterpreter','latex',...
              'defaultAxesTickLabelInterpreter','latex','defaultLegendInterpreter','latex')
             
lin = 2;  col = 2;    
  
% Current
    subplot(lin,col,1) 
    plot(time,Icc*Ib*0.001,'r','LineStyle','-','LineWidth',2.5)  
    hold all
    plot(time,Ibg*Ib*0.001,'b','LineStyle','-','LineWidth',2.5)  
    xlabel('Time (s)')   
    xlim([0 time(ft)])
    ylabel('Current (kA)') 
    ylim([-45 80])
    set(gca,'fontsize',17,'linewidth',1,'FontName','Times New Roman')
    legend('Short-circuit current','Limited current','Location','best','orientation','vertical');        
    grid 
          
                
% Voltage
    subplot(lin,col,2) 
    plot(time,Vb*Vq,'r','LineStyle','-','LineWidth',2.5)  
    hold all
    plot(time,Vb*Vssb,'b','LineStyle','-','LineWidth',2.5)  
    xlabel('Time (s)')   
    xlim([0 time(ft)])
    ylabel('Voltage (V)') 
    set(gca,'fontsize',17,'linewidth',1,'FontName','Times New Roman')
    legend('Source Voltage','FCL Voltage','Location','best','orientation','vertical');        
    grid  
    
% Temperature  
    subplot(lin,col,3)
    plot(time,Tssb,'k','LineStyle','-','LineWidth',2.5)
    xlabel('Time (s)')
    xlim([0 time(ft)])
    ylabel('Temperature (K)')
    ylim([75 300])
    set(gca,'fontsize',17,'linewidth',1,'FontName','Times New Roman')
    legend('$T_{FCL}$','Location','best');
    grid
    
% Resistance  
    subplot(lin,col,4)
    plot(time,Rsp*Zb,'b','LineStyle','-','LineWidth',2.5)
    hold all
    plot(time,Rag*Zb,'r','LineStyle','-','LineWidth',2.5)
    hold all
    plot(time,Rssb*Zb,'m','LineStyle','-.','LineWidth',2.5)
    xlabel('Time (s)')
    xlim([0 time(ft)])
    ylabel('Resistance (Ohm)')
    set(gca,'fontsize',17,'linewidth',1,'FontName','Times New Roman')
    legend('$R_{YBCO}$','$R_{Silver}$','$R_{FCL}$','Location','best');
    grid