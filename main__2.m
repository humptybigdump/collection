clear all;

%% set parameters
% w_vec = [5,20,50];      
w_vec = [5,20,50,200];

t_end = 1;

% N_vec = unique(round(2.^(1:1:11)));     % for coarse resolution of the plot
N_vec = unique(round(2.^(1:0.125:11))); % for fine resolution of the plot
tau_vec = t_end./N_vec;

%% computations
figure
% compute error for each step size in tau_vec and for each w in w_vec
err_mat = zeros(length(N_vec),length(w_vec));

% loop over all values of w
for w_ind = 1:length(w_vec)
    w = w_vec(w_ind);
    p0 = w; q0 = 1;
    
    % exact solution
    q_ex = q0*cos(w*t_end) + p0/w * sin(w*t_end);
    p_ex = -q0*w*sin(w*t_end) + p0 * cos(w*t_end);

    % loop over all values of tau (or N)
    for N_ind = 1:length(N_vec)
        % numerical approximation
        N = N_vec(N_ind);
        tau = t_end/N;

        p = p0; q = q0;
        for n = 1:N
            [p,q] = svHarmonicOscillator(p,q,tau,w);
        end

        err_mat(N_ind,w_ind) = norm( [p-p_ex;q-q_ex] );
    end

    % plot error for current value of w
    loglog(tau_vec,err_mat(:,w_ind),'*-','DisplayName',['\omega = ',num2str(w)])
    hold on

    % mark step size 2/w
    set(gca,'ColorOrderIndex',get(gca,'ColorOrderIndex')-1)
    tau = 2/w;
    loglog(tau*[1;1],[1e-10,1e+100],'-.','HandleVisibility','off')
end

% legend entry for markers of step sizes 2/w
loglog(0,0,'-.','Color',[0.7,0.7,0.7],'DisplayName','step size 2/\omega')

% second order reference line
C = 2;
loglog(tau_vec,C*tau_vec.^2,'k--','DisplayName','Reference C\tau^2')

% plot settings
xlim([min(tau_vec),max(tau_vec)])
ylim([1e-6,1e+5])
xlabel('step size $\tau$','Interpreter','latex','FontSize',14)
ylabel('error $| ({p_N \atop q_N}) - ({p(t_{end}) \atop q(t_{end})}) |$','Interpreter','latex','FontSize',16)
legend('Location','northwest')



