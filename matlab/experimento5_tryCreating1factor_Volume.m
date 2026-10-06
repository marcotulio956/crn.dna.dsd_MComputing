% ============================================================
% PROJETO DE 1 FATOR - ANALISE DE VOLUME (OMEGA)
% Gera tabelas em LaTeX automaticamente
% ============================================================

clear; clc;

%% ============================================================
% 1. Fator (niveis)
% =============================================================

V = [1,2,3,4,5,6,7,8,9,10,50,100,500,750,1000,10000];
a = length(V);     % numero de niveis
n = 30;            % repeticoes por nivel

%% ============================================================
% 2. DADOS EXPERIMENTAIS
% Substitua pela matriz real 16 x 30
% Cada linha = volume
% Cada coluna = repeticao
% =============================================================

rng(42)

rmse_reps = zeros(a,n);

for i=1:a
    base = 50/sqrt(V(i)) + 2.5;
    rmse_reps(i,:) = base * (1 + 0.05*randn(1,n));
end

%% ============================================================
% 3. Estatisticas por nivel
% =============================================================

somas  = sum(rmse_reps,2);
medias = mean(rmse_reps,2);

media_global = mean(rmse_reps(:));
efeitos = medias - media_global;

%% ============================================================
% 4. ANOVA 1 Fator
% =============================================================

SS_factor = n * sum((medias - media_global).^2);

SS_error = 0;
for i=1:a
    SS_error = SS_error + sum((rmse_reps(i,:) - medias(i)).^2);
end

SS_total = SS_factor + SS_error;

df_factor = a - 1;
df_error = a*(n-1);
df_total = a*n - 1;

MS_factor = SS_factor / df_factor;
MS_error  = SS_error / df_error;

F0 = MS_factor / MS_error;

alpha = 0.05;
Fcrit = finv(1-alpha, df_factor, df_error);

pvalue = 1 - fcdf(F0, df_factor, df_error);

%% ============================================================
% 5. Tabela 1 Latex (Niveis do fator)
% =============================================================

fprintf('\n================ LATEX TABELA FATOR ================\n\n');

fprintf('\\begin{table}[H]\n');
fprintf('\\centering\n');
fprintf('\\begin{tabular}{ccccc}\n');
fprintf('\\hline\n');
fprintf('Nivel & $\\Omega$ & Soma & Media & Efeito \\\\\n');
fprintf('\\hline\n');

for i=1:a
    fprintf('%d & %d & %.4f & %.4f & %.4f \\\\\n', ...
        i, V(i), somas(i), medias(i), efeitos(i));
end

fprintf('\\hline\n');
fprintf('\\end{tabular}\n');
fprintf('\\caption{Projeto de 1 fator variando $\\Omega$.}\n');
fprintf('\\end{table}\n');

%% ============================================================
% 6. Tabela ANOVA Latex
% =============================================================

fprintf('\n================ LATEX ANOVA ================\n\n');

fprintf('\\begin{table}[H]\n');
fprintf('\\centering\n');
fprintf('\\begin{tabular}{lccccc}\n');
fprintf('\\hline\n');
fprintf('Fonte & SQ & GL & MQ & F & p-value \\\\\n');
fprintf('\\hline\n');

fprintf('Fator & %.4f & %d & %.4f & %.4f & %.6f \\\\\n', ...
    SS_factor, df_factor, MS_factor, F0, pvalue);

fprintf('Erro & %.4f & %d & %.4f & - & - \\\\\n', ...
    SS_error, df_error, MS_error);

fprintf('Total & %.4f & %d & - & - & - \\\\\n', ...
    SS_total, df_total);

fprintf('\\hline\n');
fprintf('\\end{tabular}\n');
fprintf('\\caption{ANOVA de 1 fator para RMSE.}\n');
fprintf('\\end{table}\n');

%% ============================================================
% 7. Interpretacao estatistica
% =============================================================

fprintf('\n================ RESULTADO ================\n');

fprintf('F observado = %.6f\n', F0);
fprintf('F critico (95%%) = %.6f\n', Fcrit);
fprintf('p-value = %.8f\n', pvalue);

if F0 > Fcrit
    fprintf('\nREJEITAR H0: Volume influencia significativamente o RMSE.\n');
else
    fprintf('\nNAO REJEITAR H0.\n');
end