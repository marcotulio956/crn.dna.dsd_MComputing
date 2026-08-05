# Generate plots for the three simulation methods

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Re-define the simulations in Python for plotting

def simulate_gillespie(Tmax, params, V0, s0):
    t = 0
    V = V0
    s = s0
    out = [(t, V, s)]
    
    while t < Tmax:
        a1 = params['k_rest']
        a2 = params['k_leak'] * V
        a3 = params['beta'] * V
        a4 = params['gamma'] * s
        a5 = params['alpha'] * s * V
        
        a0 = a1 + a2 + a3 + a4 + a5
        if a0 <= 0:
            break
        
        tau = np.random.exponential(1/a0)
        t += tau
        
        r = np.random.choice(5, p=np.array([a1,a2,a3,a4,a5])/a0)
        
        if r == 0: V += 1
        elif r == 1: V = max(V - 1, 0)
        elif r == 2: s += 1
        elif r == 3: s = max(s - 1, 0)
        elif r == 4: V = max(V - 1, 0)
        
        out.append((t, V, s))
    
    return pd.DataFrame(out, columns=["t","V","s"])


def simulate_tau(Tmax, dt, params, V0, s0):
    t = np.arange(0, Tmax+dt, dt)
    V = np.zeros(len(t))
    s = np.zeros(len(t))
    
    V[0] = V0
    s[0] = s0
    
    for i in range(len(t)-1):
        a1 = params['k_rest']
        a2 = params['k_leak'] * V[i]
        a3 = params['beta'] * V[i]
        a4 = params['gamma'] * s[i]
        a5 = params['alpha'] * s[i] * V[i]
        
        K1 = np.random.poisson(a1 * dt)
        K2 = np.random.poisson(a2 * dt)
        K3 = np.random.poisson(a3 * dt)
        K4 = np.random.poisson(a4 * dt)
        K5 = np.random.poisson(a5 * dt)
        
        V[i+1] = max(V[i] + K1 - K2 - K5, 0)
        s[i+1] = max(s[i] + K3 - K4, 0)
    
    return pd.DataFrame({"t":t, "V":V, "s":s})


def simulate_ou(Tmax, dt, params, V0, s0):
    t = np.arange(0, Tmax+dt, dt)
    V = np.zeros(len(t))
    s = np.zeros(len(t))
    spikes = []
    
    V[0] = V0
    s[0] = s0
    
    for i in range(len(t)-1):
        dV_det = (-(V[i] - params['V_rest']) + params['R'] * params['I'])/params['tau_m'] \
                 - params['alpha'] * s[i] * (V[i] - params['V_reset'])
        
        # Keep adaptation drive positive-only so it behaves like activity-triggered adaptation.
        ds_det = params['beta'] * max(V[i] - params['V_rest'], 0) - params['gamma'] * s[i]
        
        dV_stoch = params['sigma_V'] * np.random.normal(0, np.sqrt(dt))
        ds_stoch = params['sigma_s'] * np.random.normal(0, np.sqrt(dt))
        
        V_next = V[i] + dV_det * dt + dV_stoch
        s_next = max(s[i] + ds_det * dt + ds_stoch, 0)

        # LIF spike: threshold crossing triggers reset and an adaptation jump.
        if V_next >= params['V_th']:
            spikes.append(t[i+1])
            V_next = params['V_reset']
            s_next += params['s_jump']

        V[i+1] = max(V_next, params['V_floor'])
        s[i+1] = s_next
    
    return pd.DataFrame({"t":t, "V":V, "s":s}), np.array(spikes)

# Deterministic ODE (Euler)
def simulate_ode(Tmax, dt, params, V0, s0):
    t = np.arange(0, Tmax+dt, dt)
    V = np.zeros(len(t))
    s = np.zeros(len(t))
    spikes = []
    
    V[0] = V0
    s[0] = s0
    
    for i in range(len(t)-1):
        dV = (-(V[i] - params['V_rest']) + params['R'] * params['I'])/params['tau_m'] \
             - params['alpha'] * s[i] * (V[i] - params['V_reset'])
        
        ds = params['beta'] * max(V[i] - params['V_rest'], 0) - params['gamma'] * s[i]
        
        V_next = V[i] + dV * dt
        s_next = max(s[i] + ds * dt, 0)

        if V_next >= params['V_th']:
            spikes.append(t[i+1])
            V_next = params['V_reset']
            s_next += params['s_jump']

        V[i+1] = max(V_next, params['V_floor'])
        s[i+1] = s_next
    
    return pd.DataFrame({"t":t, "V":V, "s":s}), np.array(spikes)

# Parameters tuned for: (1) low stochastic noise around ODE and (2) clear LIF spiking.
np.random.seed(0)
params = {
    "k_rest": 0.20,
    "k_leak": 0.08,
    "beta": 0.015,
    "gamma": 0.10,
    "alpha": 0.02,
    "V_rest": 0,
    "V_reset": 0,
    "V_th": 1.0,
    "V_floor": -1.0,
    "s_jump": 0.01,
    "R": 1,
    "I": 1.40,
    "tau_m": 8,
    "sigma_V": 2e-4,
    "sigma_s": 1e-4
}

# Run simulations
g = simulate_gillespie(50, params, 0, 0)
tau = simulate_tau(50, 0.1, params, 0, 0)
ou, spikes_ou = simulate_ou(50, 0.01, params, 0, 0)
ode, spikes_ode = simulate_ode(50, 0.01, params, 0, 0)

rmse_v = np.sqrt(np.mean((ou["V"] - ode["V"])**2))
print(
    f"OU spikes: {len(spikes_ou)} | ODE spikes: {len(spikes_ode)} | "
    f"V RMSE(OU vs ODE): {rmse_v:.5f}"
)
if len(spikes_ou) > 0:
    print(f"First OU spike times: {spikes_ou[:5]}")

# Plot V comparison including ODE
plt.figure()
plt.plot(g["t"], g["V"], label="Gillespie")
plt.plot(tau["t"], tau["V"], label="Tau-leaping")
plt.plot(ou["t"], ou["V"], label="OU")
plt.plot(ode["t"], ode["V"], label="ODE")
plt.axhline(params["V_th"], color="k", linestyle="--", alpha=0.4, label="V_th")
if len(spikes_ou) > 0:
    plt.scatter(spikes_ou, np.full_like(spikes_ou, params["V_th"]), s=9, c="red", alpha=0.7, label="OU spikes")
plt.xlabel("Time")
plt.ylabel("V")
plt.title("V trajectories: stochastic vs deterministic")
plt.legend()
plt.show()

# Plot s comparison including ODE
plt.figure()
plt.plot(g["t"], g["s"], label="Gillespie")
plt.plot(tau["t"], tau["s"], label="Tau-leaping")
plt.plot(ou["t"], ou["s"], label="OU")
plt.plot(ode["t"], ode["s"], label="ODE")
plt.xlabel("Time")
plt.ylabel("s")
plt.title("s trajectories: stochastic vs deterministic")
plt.legend()
plt.show()