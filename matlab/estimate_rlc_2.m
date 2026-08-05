% Read CSV file with columns time, v1p, vc, i
T     = readtable('C:\Users\Mark\Documents\time_v1p_vc_i.csv');                                      % uses readtable to import text data :contentReference[oaicite:0]{index=0}
t     = T.time; u = T.v1p;                                          % time vector and input signal
y     = [T.vc, T.i];                                                % assemble two-output matrix
Ts    = t(2) - t(1);                                                % estimate (uniform) sample time
data  = iddata(y, u, Ts);                                           % multivariable iddata object :contentReference[oaicite:1]{index=1}

plotyy(t, u, t, y); legend('v1p','vc','i');
title('Experimental Input v1p and Outputs vc,i');
xlabel('Time (s)');

np = 2;                                                            % choose second-order denominator
opt = tfestOptions('InitializeMethod','n4sid');                    % initialize via subspace :contentReference[oaicite:3]{index=3}
sys_tf = tfest(data, np, opt);                                     % MIMO TF, same poles for both outputs :contentReference[oaicite:4]{index=4}

nx    = 2;                                                         % two states for RLC dynamics
sys_ss = ssest(data, nx, 'Ts', 0);                                 % continuous-time MIMO state-space :contentReference[oaicite:5]{index=5}

na = 1:5; nb = 1:5; nk = 0;                                        % search ranges
for k = 1:2s
    dk = iddata(data.OutputData(:,k), u, Ts);                     % single-output iddata :contentReference[oaicite:6]{index=6}
    NN = struc(na, nb, nk);                                        % generate order combinations :contentReference[oaicite:7]{index=7}
    V  = arxstruc(dk, dk, NN);                                     % evaluate fits across orders :contentReference[oaicite:8]{index=8}
    sel = selstruc(V, 0);                                          % pick by AIC
    sys_arx{k} = arx(dk, sel);                                     % fit ARX model
end

% Simulate MIMO TF response
y_sim = lsim(sys_tf, u, t);
figure;
plot(t, y, 'b', t, y_sim, 'r--','LineWidth',1.2);
legend('Measured vc','Measured i','Sim vc','Sim i');
title('Time-Domain Validation');
xlabel('Time (s)');


[num, den] = tfdata(sys_tf, 'v');
den = den{1};                                                      % [1, a1, a0]
a1 = den(2); a0 = den(3);                                          % identify R/L and 1/(LC)
C_guess = 1e-6;                                                    % initial or measured C
L_est   = 1/(a1*C_guess);
R_est   = a1 * L_est;
fprintf('Estimated R=%.2f Ω, L=%.2e H, C assumed=%.2e F\n', R_est, L_est, C_guess);

