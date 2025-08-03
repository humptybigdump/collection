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
h      = 1.0e-5;                                    % Time step                
time   = 0:h:0.10;                                  % Time Vector                                  
ft     = length(time);                              % Size of time Vector  

fq  = 50.0;                                         % Frequency                   >      Hz                         
w   = 2.0*pi*fq;                                    % Omega        
cic = 1;                                            % Number of cycles (Normal operation)

% Set matrices to zero  
Icc_ana = zeros(1,ft);  
Icc     = zeros(1,ft);                                
Vq      = zeros(1,ft);     
Rg      = zeros(1,ft);  

% Per unit System =========================================================================
Vb = 200.0;                                         % Base voltage               >        V                      
Pb = 60000.0;                                       % Base power                 >       VA                                                                 
Zb = (Vb)^2/Pb;                                     % Base impedance             >      Ohm                  
Ib = Pb/Vb;                                         % Base current               >        A

% Parameter of the circuit ================================================================
A   = sqrt(2);                           
phi = 0*pi/180;                                     % Phase shift                >     Grad
   
% 40 kA
Lc      = 2.0e-05 / Zb;                             % Short-circuit inductance   >     p.u.
Rc      = 0.0006  / Zb;                             % Short-circuit resistance   >     p.u.
Rg(1,1) = 2/Zb;                                     % Start value for Rtotal     >     p.u.

% Begin of time loop  =====================================================================
for s = 2:ft 
    
    zn = time(s);
    zo = time(s-1);
    % disp(['zeit = ' num2str(zn)])
    
    % Fault state or normal state
    if zn <= cic/fq
		Rkz = 2 / Zb;
        Rg(1,s) = Rkz;
        
    elseif zn > cic/fq
		Rkz = Rc;
        Rg(1,s) = Rkz;
        
    end
    
    % Calculation RK parameters 
    k1_cc = 1/Lc * (A*sin(w*(zo+0.0*h) + phi) - Rkz*Icc(1,s-1));
    k2_cc = 1/Lc * (A*sin(w*(zo+0.5*h) + phi) - Rkz*(Icc(1,s-1) + 0.5*h*k1_cc));
    k3_cc = 1/Lc * (A*sin(w*(zo+0.5*h) + phi) - Rkz*(Icc(1,s-1) + 0.5*h*k2_cc));
    k4_cc = 1/Lc * (A*sin(w*(zo+1.0*h) + phi) - Rkz*(Icc(1,s-1) + 1.0*h*k3_cc));

    % Calculation of current 
    Icc(1,s) = Icc(1,s-1) + h/6 * (k1_cc + 2*k2_cc + 2*k3_cc + k4_cc);
                
    Vq(1,s)   = A * sin(w*zn + phi); 
                                                                                 
end    

% Plotting ================================================================================
fig = figure('Name','Plots','NumberTitle','off','Color',[1 1 1]);  
set(fig,'units','normalized','outerposition',[0 0 1 1],'defaulttextinterpreter','latex',...
              'defaultAxesTickLabelInterpreter','latex','defaultLegendInterpreter','latex')
             
lin = 2;  col = 2;    
  
% Current  
    subplot(lin,col,1) 
    plot(time,Icc*Ib*0.001,'r','LineStyle','-','LineWidth',2.5)  
    xlabel('Time /s')   
    xlim([0 time(ft)])
    ylabel('Current /kA') 
    ylim([-45 80])
    set(gca,'fontsize',17,'linewidth',1,'FontName','Times New Roman')
    legend('Short-circuit current','Location','best','orientation','vertical');        
    grid 
                          
% Voltage 
    subplot(lin,col,2) 
    plot(time,Vb*Vq,'b','LineStyle','-','LineWidth',2.5)  
    xlabel('Time /s')   
    xlim([0 time(ft)])
    ylabel('Voltage /V') 
    set(gca,'fontsize',17,'linewidth',1,'FontName','Times New Roman')
    legend('Source Voltage','Location','best','orientation','vertical');        
    grid  
    
% Resistance    
    subplot(lin,col,3)
    plot(time,Rg*Zb,'k','LineStyle','-','LineWidth',2.5)
    xlabel('Time /s')
    xlim([0 time(ft)])
    ylabel('Resistance /Ohm')
    ylim([-0.1 2.3])
    set(gca,'fontsize',17,'linewidth',1,'FontName','Times New Roman')
    legend('$R_{total}$','Location','best');
    grid
          