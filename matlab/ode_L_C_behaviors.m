% MATLAB Code for Isolated Capacitor and Inductor with Square Input

% Parameters
R = 1;    % Resistance in ohms
C = 1;    % Capacitance in farads
L = 1;    % Inductance in henries

% Simulation parameters
num_points = 1000; % Number of simulation points
T = 10;           % Simulation time in seconds
t = linspace(0, T, num_points); % Time vector

% Square wave input
V_in = square(2 * pi * 0.1 * t); % Square wave with 0.5 Hz frequency

% Initialize variables
V_C = zeros(1, num_points); % Voltage across capacitor
I_C = zeros(1, num_points); % Current through capacitor
E_C = zeros(1, num_points); % Electric field in capacitor

I_L = zeros(1, num_points); % Current through inductor
V_L = zeros(1, num_points); % Voltage across inductor
B_L = zeros(1, num_points); % Magnetic field in inductor

% Initial conditions
Q_C = 0; % Initial charge on the capacitor
phi_L = 0; % Initial magnetic flux in the inductor

% Capacitance electric field calculation
d_C = 0.01; % Distance between plates (meters)

% Inductance magnetic field calculation
A_L = 0.01; % Cross-sectional area (m^2)

% Simulation loop
for k = 2:num_points
    % Time step
    dt = t(k) - t(k-1);

    % Capacitor calculations
    I_C(k) = (V_in(k) - V_C(k-1)) / R; % Current through capacitor
    Q_C = Q_C + I_C(k) * dt;           % Update charge on capacitor
    V_C(k) = Q_C / C;                 % Voltage across capacitor
    E_C(k) = V_C(k) / d_C;           % Electric field in capacitor

    % Inductor calculations
    V_L(k) = V_in(k);                 % Voltage across inductor
    dI_L = (V_L(k) - R * I_L(k-1)) * dt / L; % Change in current through inductor
    I_L(k) = I_L(k-1) + dI_L;         % Update inductor current
    phi_L = L * I_L(k);              % Magnetic flux in inductor
    B_L(k) = phi_L / A_L;            % Magnetic field in inductor
end

% Plot results
figure;
subplot(2, 2, 1);
plot(t, V_C, 'b', 'LineWidth', 1.5);
xlabel('Time (s)'); ylabel('Voltage (V)');
title('Capacitor Voltage'); grid on;

subplot(2, 2, 2);
plot(t, I_C, 'r', 'LineWidth', 1.5);
xlabel('Time (s)'); ylabel('Current (A)');
title('Capacitor Current'); grid on;

subplot(2, 2, 3);
plot(t, E_C, 'g', 'LineWidth', 1.5);
xlabel('Time (s)'); ylabel('Electric Field (V/m)');
title('Capacitor Electric Field'); grid on;

subplot(2, 2, 4);
plot(t, V_in, 'k', 'LineWidth', 1.5);
xlabel('Time (s)'); ylabel('Input Voltage (V)');
title('Input Voltage'); grid on;

% figure;
% subplot(2, 2, 1);
% plot(t, V_L, 'b', 'LineWidth', 1.5);
% xlabel('Time (s)'); ylabel('Voltage (V)');
% title('Inductor Voltage'); grid on;
% 
% subplot(2, 2, 2);
% plot(t, I_L, 'r', 'LineWidth', 1.5);
% xlabel('Time (s)'); ylabel('Current (A)');
% title('Inductor Current'); grid on;
% 
% subplot(2, 2, 3);
% plot(t, B_L, 'g', 'LineWidth', 1.5);
% xlabel('Time (s)'); ylabel('Magnetic Field (T)');
% title('Inductor Magnetic Field'); grid on;
% 
% subplot(2, 2, 4);
% plot(t, V_in, 'k', 'LineWidth', 1.5);
% xlabel('Time (s)'); ylabel('Input Voltage (V)');
% title('Input Voltage'); grid on;

% Save results to CSV
csvwrite('capacitor_results.csv', [t' V_C' I_C' E_C']);
% csvwrite('inductor_results.csv', [t' V_L' I_L' B_L']);
