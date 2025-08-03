figure
c1 = csvread('evaluation.csv')
c2 = csvread('evaluation_old.csv')

subplot(2,1,1)
plot(c1(:,7),c1(:,2)./max(c1(:,2)),'b')
hold on
plot(c2(:,7),c2(:,2)./max(c2(:,2)),'r')
hold off
legend('Proposed Exploration', 'Simple Random Exploration')
ylabel('Normalized Avg. Lateral Error')
xlabel('RL Steps')
ylim(0:1)

subplot(2,1,2)
plot(c1(:,7),c1(:,4).*pi/(4*max(c1(:,4))),'b')
hold on
plot(c2(:,7),c2(:,4).*pi/(4*max(c2(:,4))),'r')
hold off
ylabel('Normalized Avg. |Steering|')
legend('Proposed Exploration', 'Simple Random Exploration')
xlabel('RL Steps')
ylim(0:1)













