% =========================================================================
% EXPERIMENTO 5: Convergência Teórica ao Limite Termodinâmico
% Mock de Resultados (Baseado no Teorema de Kurtz e Tempos Reais)
% =========================================================================

clear; clc; close all;

% 1. Dados de Volume (Número de canais)
volumes = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...
           50, 100, 500, 750, 1000, 10000];

tempo_real = [0.71, 0.24, 0.23, 0.34, 0.31, 0.23, 0.26, 0.36, 0.35, 0.42, ...
              3.08, 4.24, 20.34, 32.00, 27.84, 351.63];

% A fórmula base é RMSE = Constante * (1 / sqrt(Volume)) + ruído residual
constante_erro = 50; % Define o erro inicial em V=1
ruido_fundo = 2.5;   % Um pequeno limite mínimo de erro que nunca some
rmse_mockado = constante_erro .* (1 ./ sqrt(volumes)) + ruido_fundo;

rng(42); % Semente para reproducibilidade
jitter = 1 + 0.05 * randn(1, length(volumes));
rmse_mockado = rmse_mockado .* jitter;

% =========================================================================
% PLOTAGEM DO TRADE-OFF: ERRO VS CUSTO
% =========================================================================

fig_final = figure('Position', [100 100 1200 500], 'Color', 'w');

% Gráfico 1: Decaimento Teórico do Erro (RMSE vs Volume)
subplot(1,2,1);
plot(volumes, rmse_mockado, '-o', ...
    'Color', [0.85 0.33 0.10], ...
    'LineWidth', 2, ...
    'MarkerSize', 7, ...
    'MarkerFaceColor', [0.85 0.33 0.10]);
set(gca, 'XScale', 'log'); % Eixo X em log para ver a varredura
title('Convergência ao Limite Termodinâmico');
subtitle('Erro RMSE simulado decaindo a \approx 1/\surd{V}');
xlabel('Volume / N° de Canais (Escala Log)');
ylabel('Erro RMSE (mV)');
grid on;

% Gráfico 2: Custo Computacional (Seus dados reais)
subplot(1,2,2);
plot(volumes, tempo_real, '-o', ...
    'Color', [0.47 0.67 0.19], ...
    'LineWidth', 2, ...
    'MarkerSize', 7, ...
    'MarkerFaceColor', [0.47 0.67 0.19]);
set(gca, 'XScale', 'log', 'YScale', 'log'); % Y em log mostra bem a explosão
title('Custo Computacional');
subtitle('Aumento de tempo do Algoritmo de Gillespie/RTC');
xlabel('Volume / N° de Canais (Escala Log)');
ylabel('Tempo de Execução (Segundos) - Log');
grid on;

% Adicionando uma anotação visual para destacar o valor extremo
hold on;
plot(volumes(end), tempo_real(end), 'rp', 'MarkerSize', 12, 'MarkerFaceColor', 'r');
text(volumes(end)*0.6, tempo_real(end), sprintf(' %.1f s', tempo_real(end)), ...
    'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right', 'FontSize', 10);

% Salva a imagem com alta resolução
exportgraphics(fig_final, 'experimento5_mock_teorico.png', 'Resolution', 300);
fprintf('Gráfico gerado com sucesso: experimento5_mock_teorico.png\n');