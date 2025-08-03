%% A simulation to demonstrate the BER of a repetition code for different N, where
%% represents the repetition factor.

%pkg load communications

clear all,
close all,


N = [1, 2, 3, 4, 100];
SNR = 10.^([-15:0.1:40]/10);
BER_AWGN = zeros(length(N), length(SNR));
BER_RC = zeros(length(N), length(SNR));


%% BER for the AWGN
for i = 1:length(N)
    BER_AWGN(i,:) = qfunc(sqrt(2 * N(i) * SNR));
end

%% BER for the repetition coding 
for i = 1:length(N)
    for j= 1:length(SNR)
        f = @(x) qfunc(sqrt(2  * x * SNR(j))) .* 1/factorial(N(i) - 1) .* x.^(N(i) - 1) .* exp(-x);
        BER_RC(i,j) = quadv(f, 0, 1000);
    end
end
figure;
semilogy(10*log10(SNR), BER_AWGN(1,:), 'k-', 'Linewidth', 2);
hold on,
semilogy(10*log10(SNR), BER_AWGN(2,:), 'k-.', 'Linewidth', 2);
hold on,
semilogy(10*log10(SNR), BER_AWGN(5,:), 'k--', 'Linewidth', 2);
hold on,
semilogy(10*log10(SNR), BER_RC(1,:), 'LineStyle', '-', 'Linewidth', 2);
hold on,
semilogy(10*log10(SNR), BER_RC(2,:), 'LineStyle', '-.', 'Linewidth', 2);
hold on,
semilogy(10*log10(SNR), BER_RC(3,:), 'LineStyle', '-.', 'Linewidth', 2);
hold on,
semilogy(10*log10(SNR), BER_RC(4,:), 'LineStyle', '-.', 'Linewidth', 2);
hold on,
semilogy(10*log10(SNR), BER_RC(5,:), 'LineStyle', '--', 'Linewidth', 2);

axis([min(10*log10(SNR)) max(10*log10(SNR)) 10^-6 1])
xlabel('SNR [dB]');
ylabel('BER');
%grid on;
legend('AWGN, N = 1', 'AWGN, N = 2', 'AWGN, N = Inf', 'RC, N = 1', 'RC, N = 2', ...
'RC, N = 3', 'RC, N = 4', 'RC, N = Inf');

