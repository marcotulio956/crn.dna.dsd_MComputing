# Stochastic Simulation Methods for Chemical Reaction Networks

This document describes three fundamental approaches for simulating
Chemical Reaction Networks (CRNs):

1.  Gillespie Stochastic Simulation Algorithm (SSA)
2.  Tau-Leaping Method
3.  Ornstein–Uhlenbeck Approximation

Each method corresponds to a different level of approximation of the
underlying stochastic dynamics governed by the Chemical Master Equation
(CME).

------------------------------------------------------------------------

# 1. Chemical Master Equation (CME)

Consider a system with: - $N$ species - $M$ reactions

State vector:
$$\mathbf{X}(t) \in \mathbb{Z}_{\ge 0}^N$$

Stoichiometry matrix:
$$\nu \in \mathbb{Z}^{M \times N}$$

Propensity functions:
$$a_i(\mathbf{x}), \quad i = 1, \dots, M$$

The Chemical Master Equation is:
$$\frac{dP(\mathbf{x}, t)}{dt} = \sum_{i=1}^{M} \left[
a_i(\mathbf{x} - \nu_i) P(\mathbf{x} - \nu_i, t)
- a_i(\mathbf{x}) P(\mathbf{x}, t)
\right]$$

This equation is generally intractable, motivating simulation methods.

------------------------------------------------------------------------

# 2. Gillespie Stochastic Simulation Algorithm (SSA)

## Description

The Gillespie SSA generates exact sample paths of the CME as a
continuous-time Markov chain. It simulates one reaction event at a time.

## Equations

Total propensity:
$$a_0(\mathbf{x}) = \sum_{i=1}^{M} a_i(\mathbf{x})$$

Time to next reaction:
$$\tau \sim \text{Exponential}(a_0(\mathbf{x}))$$

Reaction selection:
$$\text{Choose } \mu \text{ such that } 
\sum_{i=1}^{\mu} a_i(\mathbf{x}) \ge r \cdot a_0(\mathbf{x}), \quad r \sim \mathcal{U}(0,1)$$

State update:
$$\mathbf{X} \leftarrow \mathbf{X} + \nu_{\mu}$$

------------------------------------------------------------------------

## Pseudocode

```         
Initialize X = X0, t = t0

while t < tf:
compute propensities a_i(X)
compute a0 = sum(a_i)

if a0 == 0:
    terminate

sample tau ~ Exp(a0)
sample reaction index mu

X = X + nu[mu]
t = t + tau
```

------------------------------------------------------------------------

## Parameters

- $\mathbf{X}_0$: initial molecule counts
- $a_i(\mathbf{x})$: propensity functions
- $\nu$: stoichiometry matrix
- $t_f$: final time

---
## Properties

editor_options: 
  markdown: 
    wrap: 72
---

# 3. Tau-Leaping Method

## Description

Tau-leaping accelerates simulation by allowing multiple reactions to
occur in a fixed time interval $\tau$, assuming propensities remain
approximately constant.

## Equations

Number of firings for reaction $i$:
$$K_i \sim \text{Poisson}(a_i(\mathbf{x}) \, \tau)$$

State update:
$$\mathbf{X}(t + \tau) = \mathbf{X}(t) + \sum_{i=1}^{M} K_i \nu_i$$

------------------------------------------------------------------------

## Pseudocode

```         
Initialize X = X0, t = t0

while t < tf:
compute propensities a_i(X)

choose tau

for each reaction i:
    sample Ki ~ Poisson(a_i(X) * tau)

X = X + sum_i Ki * nu[i]

if any X < 0:
    fallback to SSA step

t = t + tau
```

------------------------------------------------------------------------

## Parameters

- $\tau$: time step
- $a_i(\mathbf{x})$: propensities
- $\nu$: stoichiometry
- Thresholds for switching to SSA (optional)

------------------------------------------------------------------------

## Properties

- Approximate method
- Much faster than SSA for large populations
- Can become unstable if $\tau$ is too large
- Requires safeguards to avoid negative populations

------------------------------------------------------------------------

# 4. Ornstein–Uhlenbeck Approximation

## Description

The Ornstein–Uhlenbeck (OU) process arises as a linear noise
approximation (LNA) of the CME near a stable equilibrium. It models
fluctuations around the deterministic trajectory.

This corresponds to a Gaussian approximation of the stochastic dynamics.

------------------------------------------------------------------------

## Deterministic Limit

Mean-field dynamics:
$$\frac{d\mathbf{x}}{dt} = \sum_{i=1}^{M} \nu_i a_i(\mathbf{x})$$

Let $\mathbf{x}^*$ be a fixed point.

------------------------------------------------------------------------

## Linearization

Define fluctuation:
$$\boldsymbol{\eta}(t) = \mathbf{X}(t) - \mathbf{x}^*$$

Linearized dynamics:
$$\frac{d\boldsymbol{\eta}}{dt} = J \boldsymbol{\eta} + \Gamma \boldsymbol{\xi}(t)$$

where:

- $J$: Jacobian of deterministic system
  $$J = \frac{\partial}{\partial \mathbf{x}} \left( \sum_i \nu_i a_i(\mathbf{x}) \right)\bigg|_{\mathbf{x}^*}$$

- $\Gamma$: noise matrix

- $\boldsymbol{\xi}(t)$: Gaussian white noise

------------------------------------------------------------------------

## Ornstein–Uhlenbeck SDE

$$d\boldsymbol{\eta} = J \boldsymbol{\eta} \, dt + B \, d\mathbf{W}(t)$$

where: - $\mathbf{W}(t)$: Wiener process -
$B B^T = \sum_{i=1}^{M} \nu_i \nu_i^T a_i(\mathbf{x}^*)$

------------------------------------------------------------------------

## Pseudocode (Euler–Maruyama)

```         
Initialize eta = eta0, t = t0

while t < tf:
sample dW ~ Normal(0, dt)

eta = eta + J * eta * dt + B * dW

t = t + dt
```

------------------------------------------------------------------------

## Parameters

- $J$: Jacobian matrix
- $B$: diffusion matrix
- $dt$: time step
- Initial fluctuation $\eta_0$

------------------------------------------------------------------------

## Properties

- Gaussian approximation
- Valid near equilibrium
- Much faster than SSA
- Cannot capture large deviations or extinction events

------------------------------------------------------------------------

# 5. Comparison of Methods

| Method | Type | Accuracy | Speed | Regime |
|--------------|--------------|--------------|--------------|------------------|
| Gillespie (SSA) | Exact | Exact (CME) | Slow | Small populations |
| Tau-Leaping | Approximate | High (if stable) | Fast | Medium/large populations |
| Ornstein–Uhlenbeck | Approximate | Local (Gaussian) | Very fast | Near equilibrium |

------------------------------------------------------------------------

# 6. Summary

- SSA provides exact stochastic trajectories but is computationally
  expensive.
- Tau-leaping offers a trade-off between accuracy and efficiency.
- Ornstein–Uhlenbeck approximation provides a continuous stochastic
  model for fluctuations near steady states.

These methods form a hierarchy of approximations to the Chemical Master
Equation and are chosen depending on system size, required accuracy, and
computational constraints.
