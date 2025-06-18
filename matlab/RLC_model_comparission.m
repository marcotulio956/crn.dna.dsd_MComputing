function [R_est,L_est,C_est] = estimate_RLC(filename)
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
  v_data = M(:,2);s
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

[R_est,L_est,C_est] = estimate_RLC('my_rlc_data.csv');

% 2) Load measured data
M       = readmatrix('my_rlc_data.csv');
t       = M(:,1);
v_meas  = M(:,2);
i_meas  = M(:,3);

% 3) Simulate the fitted model
sys = @(tt,y) [ y(2)/C_est;
               -(R_est/L_est)*y(2) - (1/L_est)*y(1) ];
y0  = [ v_meas(1); i_meas(1) ];
[~, Y] = ode45(sys, t, y0);
v_sim = Y(:,1);
i_sim = Y(:,2);

% 4) Plot comparison
figure('Position',[100 100 800 600]);

% Voltage comparison
subplot(2,1,1);
plot(t, v_meas,  'b-', 'LineWidth',1.5); hold on;
plot(t, v_sim,  'r--','LineWidth',1.5);
ylabel('Capacitor Voltage (V)');
legend('Measured','Simulated','Location','Best');
title(sprintf('Voltage: R=%.2fΩ, L=%.2eH, C=%.2eF', R_est,L_est,C_est));
grid on;

% Current comparison
subplot(2,1,2);
plot(t, i_meas,  'b-', 'LineWidth',1.5); hold on;
plot(t, i_sim,  'r--','LineWidth',1.5);
xlabel('Time (s)');
ylabel('Inductor Current (A)');
legend('Measured','Simulated','Location','Best');
grid on;