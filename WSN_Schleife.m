% VES - Zusatzmaterial
% Simulation eines schleifenbehafteten G/M/1-Warteschlangennetzes
% 14.07.2021

clear variables;
close all;
clc;
set(0,'DefaultaxesFontsize',15)
set(0,'DefaultlineLinewidth',2)

%% Beispiel aus den Vorlesungsfolien

% Parameter wie in den Vorlesungsfolien, Kapitel 5.4.5 (VL)
gamma =[0 1/7];
mu1 = [0 1/3];
mu2 = [0 1/35];
port_weights = [0.01,0.99];

lambda_stat_VL(1) = gamma(2)/port_weights(2);
lambda_stat_VL(2) = gamma(2) * port_weights(1)/port_weights(2);
lambda_stat_VL(3) = gamma(2);
lambda_stat_VL(4) = port_weights(2) * lambda_stat_VL(1);
sim('WSN_Schleife_Modell');
gamma_out_VL = gamma_out;
WSS1_VL = WSS1;
WSS2_VL = WSS2;
queue_out_VL = queue_out;
plot_hists(gamma_out_VL,WSS1_VL,WSS2_VL,queue_out_VL,lambda_stat_VL)


% Parametrisierung mit starker Rückkopplung (SR)
gamma =[0 5];
mu1 = [0 8];
mu2 = [0 30];
port_weights = [0.9,0.1];

lambda_stat_SR(1) = gamma(2)/port_weights(2);
lambda_stat_SR(2) = gamma(2) * port_weights(1)/port_weights(2);
lambda_stat_SR(3) = gamma(2);
lambda_stat_SR(4) = port_weights(2) * lambda_stat_SR(1);
sim('WSN_Schleife_Modell');
gamma_out_SR = gamma_out;
WSS1_SR = WSS1;
WSS2_SR = WSS2;
queue_out_SR = queue_out;
plot_hists(gamma_out_SR,WSS1_SR,WSS2_SR,queue_out_SR,lambda_stat_SR)


%% Schleifenbehaftetes Warteschlangennetz mit vier Warteschlangensystemen aus Aufgabe 5.2 (4W)

gamma = [0 1.5];
mu1 = [0 4];
mu2 = [0 0.4];
mu3 = [0 3.6];
mu4 = [0 5.2];
lambda_stat_4W(1) = gamma(2)*4/3;
lambda_stat_4W(2) = gamma(2)*2/15;
lambda_stat_4W(3) = gamma(2)*3/5;
lambda_stat_4W(4) = gamma(2)*26/15;
lambda_stat_4W(5) = gamma(2);
lambda_stat_4W(6) = lambda_stat_4W(2) + 0.5*lambda_stat_4W(4);

sim('WSN_Schleife_Modell_2');
gamma_out_4W = gamma_out;
WSS1_4W = WSS1;
WSS2_4W = WSS2;
WSS3_4W = WSS3;
WSS4_4W = WSS4;
queue_out_4W = queue_out;
plot_hists2(gamma_out_4W,WSS1_4W,WSS2_4W,WSS3_4W,WSS4_4W,queue_out_4W,lambda_stat_4W)


%% Funktionen

function plot_hists(gamma_out,WSS1,WSS2,queue_out,lambda_stat)
    figure;
    subplot(2,2,1);
    edges = linspace(0,max(diff(gamma_out.time)),51);
    edges_with_middle = linspace(0,max(diff(gamma_out.time)),101);
    histogram(diff(gamma_out.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),-exp(-lambda_stat(3).*edges_with_middle(3:2:101))+exp(-lambda_stat(3).*edges_with_middle(1:2:99)));
    hold off
    title('\gamma');
    
    subplot(2,2,2);
    edges = linspace(0,max(diff(WSS1.time)),51);
    edges_with_middle = linspace(0,max(diff(WSS1.time)),101);
    histogram(diff(WSS1.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(1).*edges_with_middle(3:2:101))+exp(-lambda_stat(1).*edges_with_middle(1:2:99))));
    hold off
    title('WSS 1');
    
    subplot(2,2,3);
    edges = linspace(0,max(diff(WSS2.time)),51);
    edges_with_middle = linspace(0,max(diff(WSS2.time)),101);
    histogram(diff(WSS2.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(2).*edges_with_middle(3:2:101))+exp(-lambda_stat(2).*edges_with_middle(1:2:99))));
    hold off
    title('WSS 2');
    
    subplot(2,2,4);
    edges = linspace(0,max(diff(queue_out.time)),51);
    edges_with_middle = linspace(0,max(diff(queue_out.time)),101);
    histogram(diff(queue_out.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(4).*edges_with_middle(3:2:101))+exp(-lambda_stat(4).*edges_with_middle(1:2:99))));
    hold off
    title('Ausgang');
end


function plot_hists2(gamma_out,WSS1,WSS2,WSS3,WSS4,queue_out,lambda_stat)
    figure;
    subplot(2,3,1);
    edges = linspace(0,max(diff(gamma_out.time)),51);
    edges_with_middle = linspace(0,max(diff(gamma_out.time)),101);
    histogram(diff(gamma_out.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(5).*edges_with_middle(3:2:101))+exp(-lambda_stat(5).*edges_with_middle(1:2:99))));
    hold off
    title('\gamma');
    
    subplot(2,3,2);
    edges = linspace(0,max(diff(WSS1.time)),51);
    edges_with_middle = linspace(0,max(diff(WSS1.time)),101);
    histogram(diff(WSS1.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(1).*edges_with_middle(3:2:101))+exp(-lambda_stat(1).*edges_with_middle(1:2:99))));
    hold off
    title('WSS 1');
    
    subplot(2,3,3);
    edges = linspace(0,max(diff(WSS2.time)),51);
    edges_with_middle = linspace(0,max(diff(WSS2.time)),101);
    histogram(diff(WSS2.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(2).*edges_with_middle(3:2:101))+exp(-lambda_stat(2).*edges_with_middle(1:2:99))));
    hold off
    title('WSS 2');
    
    
    subplot(2,3,4);
    edges = linspace(0,max(diff(WSS3.time)),51);
    edges_with_middle = linspace(0,max(diff(WSS3.time)),101);
    histogram(diff(WSS3.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(3).*edges_with_middle(3:2:101))+exp(-lambda_stat(3).*edges_with_middle(1:2:99))));
    hold off
    title('WSS 3');
    
    
    subplot(2,3,5);
    edges = linspace(0,max(diff(WSS4.time)),51);
    edges_with_middle = linspace(0,max(diff(WSS4.time)),101);
    histogram(diff(WSS4.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(4).*edges_with_middle(3:2:101))+exp(-lambda_stat(4).*edges_with_middle(1:2:99))));
    hold off
    title('WSS 4');
    
    subplot(2,3,6);
    edges = linspace(0,max(diff(queue_out.time)),51);
    edges_with_middle = linspace(0,max(diff(queue_out.time)),101);
    histogram(diff(queue_out.time),edges,'Normalization','probability');
    hold on
    plot(edges_with_middle(2:2:100),(-exp(-lambda_stat(6).*edges_with_middle(3:2:101))+exp(-lambda_stat(6).*edges_with_middle(1:2:99))));
    hold off
    title('Ausgang');
end

