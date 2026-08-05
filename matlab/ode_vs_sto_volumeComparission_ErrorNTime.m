% =========================================================================
% EXPERIMENTO 5: Convergência ao Limite Termodinâmico (Morris-Lecar)
% Comparação entre ODE Determinística e Simulação Estocástica Exata (RTC)
% =========================================================================

clear; clc; close all;

% Parâmetros do Experimento
tmax = 200; % Tempo de simulação em ms (reduzido para viabilidade com volumes altos)
IappValue = 100; % Regime Tonic Spiking
volumes_to_test = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 50, 100, 500, 750, 1000, 10000, 100000];

% Criação de pasta para salvar os frames individuais
if ~exist('plots_comparacao_matlab', 'dir')
    mkdir('plots_comparacao_matlab');
end

% =========================================================================
% 1. Padrão Ouro Determinístico (ODE Baseline)
% =========================================================================
fprintf('Rodando simulação ODE (Baseline)...\n');

% Condições iniciais (Alinhadas com mlexactboth)
V0 = -50; 
N0 = 0.5; % Fração inicial de canais de Potássio abertos

% Solução da ODE usando ode15s (ideal para sistemas stiff como neurônios)
[time_ode, y_ode] = ode15s(@(t,y) ml_ode_baseline(t, y, IappValue), [0, tmax], [V0; N0]);
V_ode = y_ode(:,1);

% =========================================================================
% 2. Varredura Estocástica (Número de Canais / Volume)
% =========================================================================
fprintf('Iniciando varredura de Volumes Estocásticos...\n');

% Variáveis para armazenar resultados
resultados_rmse = zeros(length(volumes_to_test), 1);
resultados_tempo = zeros(length(volumes_to_test), 1);

for i = 1:length(volumes_to_test)
    vol = volumes_to_test(i);
    fprintf('Simulando Volume (Canais) = %d... ', vol);
    
    % Mtot e Ntot assumem o valor do volume
    Mtot = vol;
    Ntot = vol;
    
    % Limpa globais para evitar vazamento de estado entre loops
    clear global tau1 T1 tau2 T2 tau3 T3 tau4 T4
    
    % Inicia cronômetro
    tic;
    
    % Executa o simulador estocástico exato
    % doPlot = false (4º argumento) para não poluir a tela
    [V_sto, M_sto, N_sto, time_sto] = mlexactboth(tmax, Mtot, Ntot, false, IappValue);
    
    % Para cronômetro
    sim_time = toc;
    resultados_tempo(i) = sim_time;
    fprintf('Feito em %.2f segundos.\n', sim_time);
    
    % O vetor de tempo de mlexactboth pode conter tempos duplicados 
    % nas bordas dos eventos. Removendo duplicatas para interpolar:
    [time_sto_uniq, idx_uniq] = unique(time_sto);
    V_sto_uniq = V_sto(idx_uniq);
    
    % Alinhamento: Interpolação linear da série temporal estocástica 
    % para os instantes exatos da grade ODE
    V_sto_aligned = interp1(time_sto_uniq, V_sto_uniq, time_ode, 'linear', 'extrap');
    
    % Cálculo do Root Mean Squared Error (RMSE)
    rmse_val = sqrt(mean((V_ode - V_sto_aligned).^2));
    resultados_rmse(i) = rmse_val;
    
    % =====================================================================
    % Plot Individual (Frame a Frame)
    % =====================================================================
    fig = figure('Visible', 'off', 'Position', [100 100 800 400]);
    plot(time_ode, V_ode, 'k', 'LineWidth', 1.5); hold on;
    plot(time_sto, V_sto, 'Color', [0.85 0.33 0.10], 'LineWidth', 0.5);
    title(sprintf('Volume (Canais) = %d | RMSE = %.2f', vol, rmse_val));
    xlabel('Tempo (ms)');
    ylabel('Voltagem (mV)');
    legend('ODE Determinística', 'Estocástico Exato');
    grid on;
    
    % Salva o plot na pasta
    filename = sprintf('plots_comparacao_matlab/plot_vol_%04d.png', vol);
    saveas(fig, filename);
    close(fig);
end

% =========================================================================
% 3. Visualização Dupla (Trade-off: Erro vs Custo)
% =========================================================================
fprintf('\nGerando gráfico final de Trade-off...\n');

fig_final = figure('Position', [100 100 1200 500]);

% Gráfico 1: RMSE vs Volume
subplot(1,2,1);
plot(volumes_to_test, resultados_rmse, '-o', 'Color', [0.85 0.33 0.10], 'LineWidth', 2, 'MarkerSize', 6, 'MarkerFaceColor', [0.85 0.33 0.10]);
set(gca, 'XScale', 'log');
title('Convergência ao Limite Termodinâmico');
xlabel('Volume / N° de Canais (Escala Log)');
ylabel('Erro RMSE (mV)');
grid on;

% Gráfico 2: Tempo vs Volume
subplot(1,2,2);
plot(volumes_to_test, resultados_tempo, '-o', 'Color', [0.47 0.67 0.19], 'LineWidth', 2, 'MarkerSize', 6, 'MarkerFaceColor', [0.47 0.67 0.19]);
set(gca, 'XScale', 'log');
title('Custo Computacional');
xlabel('Volume / N° de Canais (Escala Log)');
ylabel('Tempo de Execução (Segundos)');
grid on;

saveas(fig_final, 'experimento5_tradeoff_matlab.png');
fprintf('Tudo pronto! Gráfico salvo como "experimento5_tradeoff_matlab.png".\n');


% =========================================================================
% FUNÇÕES LOCAIS AUXILIARES
% =========================================================================

function dydt = ml_ode_baseline(t, y, IappValue)
    % Implementação do Morris-Lecar Determinístico Base
    V = y(1);
    N = y(2);

    % Parâmetros (Idênticos ao mlexactboth)
    vK = -84; vL = -60; vCa = 120;
    gK = 8; gL = 2; C = 20; gCa = 4.4;
    va = -1.2; vb = 18;
    vc = 2; vd = 30;
    phi_m = 0.4; phi_n = 0.04;

    % Funções de ativação
    xi_m = (V - va) / vb;
    minf = 0.5 * (1 + tanh(xi_m));
    
    xi_n = (V - vc) / vd;
    ninf = 0.5 * (1 + tanh(xi_n));
    tau_n = 1 ./ (phi_n * cosh(xi_n/2));

    % Equações Diferenciais
    dVdt = (IappValue - gCa * minf * (V - vCa) - gK * N * (V - vK) - gL * (V - vL)) / C;
    dNdt = (ninf - N) / tau_n;

    dydt = [dVdt; dNdt];
end

% -------------------------------------------------------------------------
% Abaixo está a função mlexactboth fornecida por você (inclusa como local)
% -------------------------------------------------------------------------
function [V,M,N,t,Mtot,Ntot]=mlexactboth(tmax,Mtot,Ntot,doPlot,IappValue)
    global Mcalcium_tot Mcalcium tau1 T1 tau2 T2
    global Npotassium_tot Npotassium tau3 T3 tau4 T4
    if nargin < 3, Ntot=40; end
    Npotassium_tot=Ntot;
    if nargin < 2, Mtot=40; end
    Mcalcium_tot=Mtot;
    if nargin < 1, tmax=4e3; end
    if nargin < 4, doPlot=true; end
    if nargin < 5, IappValue=100; end

    phi_m=0.4; va=-1.2; vb=18; vc=2; vd=30; phi_n=0.04;

    global Iapp; Iapp=@(t)IappValue; 
    global xi_m; xi_m=@(v)(v-va)/vb; 
    global minf; minf=@(v)0.5*(1+tanh(xi_m(v))); 
    global tau_m; tau_m=@(v)1./(phi_m*cosh(xi_m(v)/2)); 
    global alpha_m; alpha_m=@(v)(minf(v)./tau_m(v));
    global beta_m; beta_m=@(v)((1-minf(v))./tau_m(v));
    global xi_n; xi_n=@(v)(v-vc)/vd; 
    global ninf; ninf=@(v)0.5*(1+tanh(xi_n(v))); 
    global tau_n; tau_n=@(v)1./(phi_n*cosh(xi_n(v)/2)); 
    global alpha_n; alpha_n=@(v)(ninf(v)./tau_n(v));
    global beta_n; beta_n=@(v)((1-ninf(v))./tau_n(v));

    options=odeset('Events',@nextevent);
    t=0; 
    tau1=-log(rand); tau2=-log(rand); tau3=-log(rand); tau4=-log(rand);
    T1=0; T2=0; T3=0; T4=0;
    V0=-50; V=V0; 
    M0=0; Mcalcium=M0; M=M0; 
    N0=ceil(Ntot/2); Npotassium=N0; N=N0; 

    while t(end)<tmax
        U0=[V0;0;0;0;0;M0;N0];
        tspan=[t(end),tmax];
        [tout,Uout,~,~,event_idx]=ode23(@dudtfunc,tspan,U0,options);
        Vout=Uout(:,1); Mout=Uout(:,6); Nout=Uout(:,7);
        t=[t,tout']; V=[V,Vout']; M=[M,Mout']; N=[N,Nout'];
        
        if isempty(event_idx), break; end
        
        mu=event_idx(1); 
        if mu==1, M0=M0+1; tau1=tau1-log(rand);
        elseif mu==2, M0=M0-1; tau2=tau2-log(rand);
        elseif mu==3, N0=N0+1; tau3=tau3-log(rand);
        elseif mu==4, N0=N0-1; tau4=tau4-log(rand);
        end
        Mcalcium=M0; Npotassium=N0;
        
        T1=T1+Uout(end,2); T2=T2+Uout(end,3);
        T3=T3+Uout(end,4); T4=T4+Uout(end,5);
        V0=V(end);
    end

    if doPlot
        figure
        subplot(6,1,1),plot(t,M),ylabel('M'),set(gca,'FontSize',12)
        subplot(6,1,2),plot(t,N),ylabel('N'),set(gca,'FontSize',12)
        subplot(6,1,6),plot(t,V),xlabel('Time'),ylabel('V'),set(gca,'FontSize',12)
        subplot(6,1,4:6),plot3(V,M,N,'.-'),xlabel('V'),ylabel('M'),zlabel('N'),set(gca,'FontSize',12)
        grid on, rotate3d, shg
    end
end

function dudt=dudtfunc(t,u)
    global Iapp Mcalcium Mcalcium_tot Npotassium Npotassium_tot
    global alpha_m beta_m alpha_n beta_n
    vK =-84; vL =-60; vCa = 120;
    gK =8; gL =2; C=20; gCa = 4.4;
    v=u(1); 
    dudt=[
        (Iapp(t)-gCa*(Mcalcium/Mcalcium_tot)*(v-vCa)-gL*(v-vL)-gK*(Npotassium/Npotassium_tot)*(v-vK))/C;
        alpha_m(v)*(Mcalcium_tot-Mcalcium);
        beta_m(v)*Mcalcium;
        alpha_n(v)*(Npotassium_tot-Npotassium);
        beta_n(v)*Npotassium;
        0; 0];
end

function [value,isterminal,direction] = nextevent(~,u)
    global tau1 T1 tau2 T2 tau3 T3 tau4 T4
    value=[u(2)-(tau1-T1);u(3)-(tau2-T2);u(4)-(tau3-T3);u(5)-(tau4-T4)];
    isterminal=[1;1;1;1]; 
    direction=[1;1;1;1]; 
end