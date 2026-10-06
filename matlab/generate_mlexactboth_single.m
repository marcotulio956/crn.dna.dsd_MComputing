% generate_mlexactboth_single.m
%
% Generates a single realization of the stochastic Morris-Lecar model.
% Plots Voltage with raster-style spike indicators, and plots M/N channels. 
% Saves both the data and the plot to dynamically named files.

clear;
clc;
close all;

%% ------------------------------------------------------------------------
% 1. Parameters
%% ------------------------------------------------------------------------
% Tweak these values for your specific single run

regimes = {
    % 'low_drive_quiescent'      ,  60 , 40 , 40
    'near_threshold_irregular' ,  80 , 40 , 40
    % 'tonic_spiking'            , 100 , 40 , 40
    % 'high_drive_fast_spiking'  , 130 , 40 , 40
    % 'channel_noise_dominant'   , 100 , 20 , 20
};


tmax = 800;
dt   = 1;
Iapp = 80;
Mtot = 1000;
Ntot = 1000;
seed = 12345; % Set a specific seed for reproducibility

% Output directory
outDir = fullfile(pwd, 'data');
if ~exist(outDir, 'dir')
    mkdir(outDir);
end

%% ------------------------------------------------------------------------
% 2. Run Simulation
%% ------------------------------------------------------------------------
fprintf('Running single ML realization...\n');
fprintf('Parameters: Iapp=%g, Mtot=%g, Ntot=%g\n', Iapp, Mtot, Ntot);

rng(seed);
tUniform = (0:dt:tmax)';

% Call the stochastic solver
[V, M, N, t] = mlexactboth(tmax, Mtot, Ntot, false, Iapp);

%% ------------------------------------------------------------------------
% 3. Clean & Resample Simulator Output
%% ------------------------------------------------------------------------
t = t(:); V = V(:); M = M(:); N = N(:);

% Sort and remove duplicate timestamps (keep last observed state)
[t, order] = sort(t);
V = V(order); M = M(order); N = N(order);

[tUnique, idx] = unique(t, 'last');
V = V(idx); M = M(idx); N = N(idx);

% Resample onto a fixed grid
Vgrid = interp1(tUnique, V, tUniform, 'linear', 'extrap');
Mgrid = interp1(tUnique, M, tUniform, 'previous', 'extrap');
Ngrid = interp1(tUnique, N, tUniform, 'previous', 'extrap');

% Calculate normalized voltage and spikes
vmin = min(Vgrid);
vmax = max(Vgrid);
if vmax > vmin
    Vnorm = (Vgrid - vmin) ./ (vmax - vmin);
else
    Vnorm = zeros(size(Vgrid));
end

threshold = vmin + 0.70*(vmax-vmin);
isAbove = Vgrid >= threshold + 10;
spikeTrain = [0; diff(isAbove)==1];

% Extract specific times where spikes occurred for the raster plot
spikeIndices = find(spikeTrain == 1);
spikeTimes = tUniform(spikeIndices);

%% ------------------------------------------------------------------------
% 4. Plotting
%% ------------------------------------------------------------------------
% Create a stacked figure for V, M, and N
fig = figure('Name', 'Morris-Lecar Single Realization', 'Position', [100, 100, 800, 700]);

% Voltage Plot with Raster Lines
subplot(3, 1, 1);
hold on;
% Draw the raster-style vertical lines first so they sit behind the voltage trace
if ~isempty(spikeTimes)
    % xline is standard in modern MATLAB for clean vertical lines
    xline(spikeTimes, 'k--', 'LineWidth', 0.8, 'Alpha', 0.5, 'HandleVisibility', 'off');
end
% Plot the actual voltage trace on top
plot(tUniform, Vgrid, 'b', 'LineWidth', 1.2, 'DisplayName', 'Voltage');
hold off;
ylabel('Voltage (mV)');
title(sprintf('Stochastic Morris-Lecar (I_{app}=%g, M_{tot}=%g, N_{tot}=%g, seed=%d)', Iapp, Mtot, Ntot, seed));
legend('Location', 'best');
grid on;

% Open M-Channels Plot (No spike markers)
subplot(3, 1, 2);
plot(tUniform, Mgrid, 'r', 'LineWidth', 1);
ylabel('Open M Channels');
grid on;

% Open N-Channels Plot (No spike markers)
subplot(3, 1, 3);
plot(tUniform, Ngrid, 'g', 'LineWidth', 1);
ylabel('Open N Channels');
xlabel('Time (ms)');
grid on;

%% ------------------------------------------------------------------------
% 5. Save Output Files (Data and Plot)
%% ------------------------------------------------------------------------
% Base standardized filename
baseName = sprintf('ml_single_Iapp%g_Mtot%g_Ntot%g_seed%d_tmax%d', Iapp, Mtot, Ntot, seed,tmax);

% Save the CSV Data
csvFilePath = fullfile(outDir, [baseName, '.csv']);
T = table(tUniform, Vgrid, Vnorm, Mgrid, Ngrid, spikeTrain, ...
    'VariableNames', {'time', 'V', 'V_norm', 'M', 'N', 'spike'});
writetable(T, csvFilePath);

% Save the Plot as a PNG image
figFilePath = fullfile(outDir, [baseName, '.png']);
exportgraphics(fig, figFilePath, 'Resolution', 300);

fprintf('\nSaved data to:\n  %s\n', csvFilePath);
fprintf('Saved plot to:\n  %s\n', figFilePath);
fprintf('Done.\n');