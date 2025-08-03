% Make something that looks like data for different conditions

condCellArray = cell(1,3);

condCellArray{1} = randn(1,500);
condCellArray{2} = randn(1,400)-0.5;
condCellArray{3} = randn(1,550)+1.1;

numConds = numel(condCellArray);

%% --- Histogram

figure(1)

lineStyles = {'k-','r--','b:'};

subplot(3,1,1)
cla

for kk = 1:numConds

    [bin_counts,bin_centers] = hist(condCellArray{kk},20);
    plot(bin_centers,bin_counts,...
        lineStyles{kk},'LineWidth',1)
    hold on
    xlabel('Variable value')
    ylabel('Count')

end


%% --- Kernel-based density estimation

figure(1)

subplot(3,1,2)
cla

for kk = 1:numConds

    [density,density_support] = ksdensity(condCellArray{kk});
    plot(density_support,density,...
        lineStyles{kk},'LineWidth',1)
    hold on
    xlabel('Variable value')
    ylabel('Probability density')

end


%% --- Boxplots

figure(1)

subplot(3,1,3)
cla

valueVec = [];
groupVec = [];

for kk = 1:numConds
    
    valueVec = [valueVec,condCellArray{kk}];
    groupVec = [groupVec,kk.*ones(size(condCellArray{kk}))];

end

boxplot(valueVec,groupVec,'labels',{'Cond 1','Cond 2','Cond 3'})

ylabel('Variable value')