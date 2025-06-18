% plot_RLC_comparison()
datafile = "C:\Users\Mark\Documents\time_vc_i.csv"
drivefile = "C:\Users\Mark\Documents\time_v1p.csv"

%[r,l,c] = estimate_RLC_parameters(datafile)

%plot_RLC_comparison(datafile, drivefile)

[r,l,c] = estimate_RLC_sundaresan(datafile, drivefile)

function [R_est,L_est,C_est] = estimate_RLC_parameters(filename)
% estimate_RLC  Estimate R, L, C of a series RLC from measured v_C and i_L.
%
%   [R_est,L_est,C_est] = estimate_RLC(filename)
%
%   - filename: path to CSV file with columns [time, v_C, i_L].
%               May have a header row (auto-skipped by readmatrix).
%
%   Returns the best-fit values (in SI: Ohm, Henry, Farad).

  %--- load data -----------------------------------------------------------
  M = readmatrix(filename);
  t = M(:,1);
  v_data = M(:,2);
  i_data = M(:,3);

  %--- initial guess and bounds --------------------------------------------
  x0 = [  1.0,    1e-3,   1e-6   ];  % [R0 (Ω), L0 (H),  C0 (F)]
  lb = [  0,      0,      0      ];  % physical lower bounds
  ub = [ Inf,    Inf,    Inf    ];  

  %--- optimization options -----------------------------------------------
  opts = optimoptions(@lsqnonlin,...
                      'Display','iter', ...
                      'TolFun',1e-8, ...
                      'TolX',  1e-8);

  %--- run the fit ---------------------------------------------------------
  x_est = lsqnonlin(@(x) residuals(x,t,v_data,i_data), ...
                    x0, lb, ub, opts);

  R_est = x_est(1);
  L_est = x_est(2);
  C_est = x_est(3);

  fprintf('Estimated: R = %.4f Ω,  L = %.4e H,  C = %.4e F\n', ...
           R_est, L_est, C_est);
end


function e = residuals(x,t,v_meas,i_meas)
% residuals  Compute stacked v and i errors for given [R,L,C]
  R = x(1);
  L = x(2);
  C = x(3);

  % state: y = [v_C; i_L]
  %  dv_C/dt = i_L / C
  %  di_L/dt = -(R/L)*i_L - (1/L)*v_C
  sys = @(tt,y) [ y(2)/C;
                 -(R/L)*y(2) - (1/L)*y(1) ];

  y0 = [ v_meas(1);
         i_meas(1) ];

  % simulate at the same time points
  [~, Y] = ode45(sys, t, y0);

  v_sim = Y(:,1);
  i_sim = Y(:,2);

  % stack errors for least squares
  e = [ v_sim - v_meas;
        i_sim - i_meas ];
end

function plot_RLC_comparison(dataFile, driveFile)
% plot_RLC_comparison - Compare measured RLC outputs to simulation
%   using the same input drive.
%
%   plot_RLC_comparison_with_drive(dataFile, driveFile)
%
%   - dataFile : CSV with [time, v_C_meas, i_L_meas]
%   - driveFile: CSV with [time, v_in]

  %--- 1) Estimate R, L, C from your measured data ------------------------
  [R_est,L_est,C_est] = estimate_RLC_parameters(dataFile);

  %--- 2) Load measured outputs ------------------------------------------
  M      = readmatrix(dataFile);
  t      = M(:,1);
  v_meas = M(:,2);
  i_meas = M(:,3);

  %--- 3) Load input drive -----------------------------------------------
  U        = readmatrix(driveFile);
  t_drive  = U(:,1);
  v_drive  = U(:,2);
  % build an input-interpolant
  u = @(tt) interp1(t_drive, v_drive, tt, 'linear', 'extrap');

  %--- 4) Simulate the estimated RLC under that drive --------------------
  % state y = [v_C; i_L]
  sys_in = @(tt,y) [ ...
               y(2)/C_est; ...
               ( u(tt) - R_est*y(2) - y(1) )/L_est ];
  y0 = [ v_meas(1); i_meas(1) ];
  [~, Y] = ode45(sys_in, t, y0);
  v_sim = Y(:,1);
  i_sim = Y(:,2);

  %--- 5) Plot comparison ------------------------------------------------
  figure('Position',[100 100 800 600]);

  subplot(2,1,1);
  plot(t, v_meas,  'b-', 'LineWidth',1.5); hold on;
  plot(t, v_sim,  'r--','LineWidth',1.5);
  ylabel('Capacitor Voltage (V)');
  legend('Measured','Simulated','Location','Best');
  title(sprintf('v_C  —  R=%.2fΩ, L=%.2eH, C=%.2eF',R_est,L_est,C_est));
  grid on;

  subplot(2,1,2);
  plot(t, i_meas,  'b-', 'LineWidth',1.5); hold on;
  plot(t, i_sim,  'r--','LineWidth',1.5);
  xlabel('Time (s)');
  ylabel('Inductor Current (A)');
  legend('Measured','Simulated','Location','Best');
  grid on;
end

function [R_est, L_est, C_est] = estimate_RLC_sundaresan(dataFile, driveFile)
% Estimate R, L, C using Sundaresan–Krishnaswamy method from step response.
%
% Inputs:
%   - dataFile : CSV with columns [time, output (e.g., v_C)]
%   - driveFile: CSV with columns [time, input (e.g., v_in)]
%
% Assumes step input. Returns estimated R, L, and C.

  %--- Load input and output data ----------------------------------------
  data  = readmatrix(dataFile);
  t     = data(:,1);
  y     = data(:,2);  % measured output: v_C or i_L

  drive = readmatrix(driveFile);
  u     = drive(:,2);  % input voltage
  t_u   = drive(:,1);

  %--- Interpolate input to match t -------------------------------------
  u_interp = interp1(t_u, u, t, 'linear', 'extrap');

  %--- Step detection ---------------------------------------------------
  u0 = u_interp(1);
  uf = u_interp(end);
  delta_u = uf - u0;

  y0 = y(1);
  yf = y(end);
  delta_y = yf - y0;

  % Normalize step response
  y_norm = (y - y0) / delta_y;

  % Sundaresan–Krishnaswamy time points
  t1_target = 0.353;
  t2_target = 0.853;

  % Find times corresponding to 35.3% and 85.3%
  t1_idx = find(y_norm >= t1_target, 1, 'first');
  t2_idx = find(y_norm >= t2_target, 1, 'first');

  t1 = t(t1_idx);
  t2 = t(t2_idx);

  % Apply S-K formulas
  tau1 = 0.67 * (t2 - t1);
  tau2 = 0.33 * (t2 - t1);
  td   = t1 - 0.5 * (t2 - t1);
  K    = delta_y / delta_u;

  % Form SOPDT transfer function:
  %   G(s) = K / (τ1 τ2 s^2 + (τ1+τ2)s + 1)
  % Map to RLC model: Assume series RLC, output = v_C, input = v_in
  % From transfer function theory:
  %   L = τ1 * τ2
  %   R = (τ1 + τ2)
  %   C = 1 / (K)

  L_est = tau1 * tau2;
  R_est = tau1 + tau2;
  C_est = 1 / K;

  % Output results
  fprintf('\n[Sundaresan–Krishnaswamy Estimate]\n');
  fprintf('Estimated R = %.4f Ω\n', R_est);
  fprintf('Estimated L = %.4e H\n', L_est);
  fprintf('Estimated C = %.4e F\n', C_est);
end

