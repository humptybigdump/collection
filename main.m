clear all;

%% parameters
w = 1000;
x0 = [1; 0; 0; 1/w; 0; 0];
y0 = [1; 0; 0; 1; 0; 0];
t_end = 1;
% N_vec = 2.^(2:10);
N_vec = unique( round(2.^(2:0.0625:10)) );

%% preparations
tau_vec = t_end./N_vec;
% Index for Different methods:
% 1: Gautschi
% 2: Deuflhard
% 3: Garcia-Archilla
% 4: Hochbruck Lubich
% 5: Hairer Lubich
% 6: Hochbruck Grimm
method_names = {'A: Gautschi','B: Deuflhard','C: Garcia-Archilla',...
                'D: Hochbruck Lubich','E: Hairer Lubich','F: Hochbruck Grimm'};

%% exact solution (using ode45)
fprintf('Exact solution is computed\n')
[xex,yex] = FPUT_reference_solution(x0,y0,w,t_end);

%% numerical approximation
figure
tl = tiledlayout(2,3,"Padding","compact","TileSpacing","compact");

% structure for errors: 6 methods, multiple step numbers
err_mat = zeros(6,length(N_vec));

% loop over methods
for method_ind = 1:6
    nexttile(method_ind)
    fprintf('Method %i of 6 is applied\n',method_ind)

    % loop over step numbers
    for N_ind = 1:length(N_vec)
        N = N_vec(N_ind);
        tau = t_end/N;
        % compute numerical approximation
        x = x0; y = y0;
        for nn = 1:N
            [x,y] = general_method(x,y,tau,w,method_ind);
        end
        % compute error in positions
        err_mat(method_ind,N_ind) = norm(x - xex);
    end

    % Create error plot for current method
    if length(N_vec)<=10
        loglog(tau_vec,err_mat(method_ind,:),'*-','LineWidth',1.5)
    else
        loglog(tau_vec,err_mat(method_ind,:),'LineWidth',1.5)
    end
    title(method_names{method_ind})
    hold on
    % reference line second order
    C = 1e-1;
    loglog(tau_vec,C*tau_vec.^2,'k--')
    % Plot settings
    xlim([min(tau_vec),max(tau_vec)])
    ylim([1e-7,1e-2])
    for k = 1:10 % mark smallest 10 critical step sizes
        tau_crit = k*pi/w;
        loglog(tau_crit*[1;1],[1e-7,1e+3],'--','Color',[0.7,0.7,0.7])
    end
end
title(tl,['error in positions at $t=1$ vs. step size for $\omega = ',num2str(w),'$'],'Interpreter','latex')
subtitle(tl,'(Black line: second order reference line)','FontSize',12)
