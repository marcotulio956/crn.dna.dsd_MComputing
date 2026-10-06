import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt

# Circuit parameters
R = 1.0         # Resistance (Ohm)
C = 1.0         # Capacitance (Farad)
k_leak = 0.01   # Leakage rate for Q+ and Q-
k_corr = 1000.0 # Correction gain for I_cap

# Time settings for simulation
t_start, t_end = 0, 10
t_eval = np.linspace(t_start, t_end, 1000)

# Define a square wave that oscillates between +1 and -1.
def square_wave(t, freq=0.5):
    # At t=0, sin(0)=0 so we define >=0 to map t=0 to +1.
    return 1.0 if np.sin(2 * np.pi * freq * t) >= 0 else -1.0

# Dual-rail input functions:
def V_in_pos(t):
    return max(square_wave(t), 0)

def V_in_neg(t):
    return max(-square_wave(t), 0)

def V_in(t):
    return V_in_pos(t) - V_in_neg(t)

# Define the ODE system.
# State vector: y[0]=Q^+, y[1]=Q^-, y[2]=V_cap, y[3]=I_cap.
def capacitor_system(t, y):
    Qp, Qm, Vcap, Icap = y

    # Net input voltage (from the dual-rail encoding)
    Vin = V_in(t)

    # Compute dQ+/dt and dQ-/dt from the positive and negative charging channels.
    dQp_dt = (1.0/R) * max(Vin - Vcap, 0) - k_leak * Qp
    dQm_dt = (1.0/R) * max(Vcap - Vin, 0) - k_leak * Qm

    # Enforce capacitor voltage dynamics:
    dVcap_dt = (1.0/C) * (dQp_dt - dQm_dt)

    # Correct I_cap to track (Vin - Vcap)/R.
    # We assume dVin/dt ~ 0, so the ideal relation is dIcap/dt = -dVcap_dt/R.
    dIcap_dt = -(1.0/R) * dVcap_dt + k_corr * (((Vin - Vcap)/R) - Icap)

    return [dQp_dt, dQm_dt, dVcap_dt, dIcap_dt]

# Initial conditions.
# Let Qp and Qm be equal initially so that Vcap = 0.
# For a square wave, at t=0 square_wave(0) = 1 so Vin(0)=1.
# Then, ideally, I_cap(0) should be (1-0)/R = 1.
Qp0 = 0.1
Qm0 = 0.1
Vcap0 = (Qp0 - Qm0) / C  # = 0
Icap0 = (V_in(0) - Vcap0) / R  # = 1.0 given Vin(0)=1
y0 = [Qp0, Qm0, Vcap0, Icap0]

# Solve the ODE system.
sol = solve_ivp(capacitor_system, (t_start, t_end), y0, t_eval=t_eval, method='RK45')

# Retrieve solutions:
Qp = sol.y[0]
Qm = sol.y[1]
V_cap = sol.y[2]
I_cap = sol.y[3]
time = sol.t

# Also compute the net input voltage over time:
V_in_vals = np.array([V_in(t) for t in time])

# Plot the results.
plt.figure(figsize=(12, 10))

# Plot input voltage.
plt.subplot(3, 1, 1)
plt.plot(time, V_in_vals, 'k-', label='Input Voltage $V_{in}$ (square wave)')
plt.ylabel('Voltage (V)')
plt.legend()
plt.title('Dual-Rail Capacitor Module with Integrated V_cap and I_cap')

# Plot capacitor voltage.
plt.subplot(3, 1, 2)
plt.plot(time, V_cap, 'b-', label='Capacitor Voltage $V_{cap}$')
plt.ylabel('Voltage (V)')
plt.legend()

# Plot capacitor current.
plt.subplot(3, 1, 3)
plt.plot(time, I_cap, 'g-', label='Capacitor Current $I_{cap}$')
plt.xlabel('Time (s)')
plt.ylabel('Current (A)')
plt.legend()

plt.tight_layout()
plt.show()
