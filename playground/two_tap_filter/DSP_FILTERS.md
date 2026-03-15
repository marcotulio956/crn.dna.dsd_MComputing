# Digital Signal Processing Filters for Molecular Computing

This document describes the implementation of Digital Signal Processing (DSP) filters using Chemical Reaction Networks (CRNs), based on the paper "Digital Signal Processing with Molecular Reactions" by Jiang, Riedel, and Parhi (2013).

## Overview

The DNAr library now includes comprehensive support for implementing digital filters using molecular reactions. This enables signal processing capabilities within molecular computing systems, allowing for applications such as noise filtering, frequency analysis, and signal conditioning.

## Implemented Filter Functions

### 1. Make_Phase_Generator()

Generates two alternating phases to coordinate sequential filter operations using a Dalchau-style oscillator.

**Parameters:**
- `name`: Module name
- `period`: Oscillation period for phase switching
- `crange`: Concentration range for phase signals
- `rate`: Base reaction rate

**Returns:** Phase generator module with `phase1` and `phase2` species

**Example:**
```r
phase_gen <- Make_Phase_Generator(
  name = 'phase_gen',
  period = 100,
  crange = 10,
  rate = 1e-3
)
```

### 2. Make_Delay_Unit()

Implements the z^-1 delay operation essential for digital filters. Stores input value during one phase and outputs it in the next phase.

**Parameters:**
- `name`: Module name
- `nameInput`: Input species name
- `nameOutput`: Output species name (delayed signal)
- `phase1`: Phase 1 species name (sampling phase)
- `phase2`: Phase 2 species name (output phase)
- `cinput`: Initial input concentration
- `crange`: Concentration range
- `rate`: Reaction rate

**Example:**
```r
delay <- Make_Delay_Unit(
  name = 'delay1',
  nameInput = 'X_in',
  nameOutput = 'X_delayed',
  phase1 = phase_gen$species$phase1,
  phase2 = phase_gen$species$phase2,
  cinput = 0,
  crange = 10,
  rate = 1e-3
)
```

### 3. Make_Coefficient_Multiplier()

Multiplies input signal by a fixed coefficient using catalyst concentration to represent the coefficient value.

**Parameters:**
- `name`: Module name
- `nameInput`: Input species name
- `nameOutput`: Output species name
- `coefficient`: Multiplication coefficient (0 to 1)
- `cinput`: Initial input concentration
- `crange`: Concentration range
- `rate`: Reaction rate

**Example:**
```r
mult <- Make_Coefficient_Multiplier(
  name = 'mult1',
  nameInput = 'X',
  nameOutput = 'X_scaled',
  coefficient = 0.5,
  cinput = 0,
  crange = 10,
  rate = 1e-3
)
```

### 4. Make_TwoTap_Filter()

Implements a two-tap Finite Impulse Response (FIR) filter: y[n] = a0*x[n] + a1*x[n-1]

This is a first-order FIR filter that can implement:
- Moving average (low-pass): a0=0.5, a1=0.5
- Differencing (high-pass): a0=1.0, a1=-1.0
- Weighted average: any positive coefficients

**Parameters:**
- `name`: Filter name
- `nameInput`: Input signal species name
- `nameOutput`: Output signal species name
- `coeff0`: Coefficient for current sample (a0)
- `coeff1`: Coefficient for delayed sample (a1)
- `crange`: Concentration range
- `rate`: Base reaction rate

**Returns:** Complete filter module with all components

**Example:**
```r
# Moving average filter (low-pass)
fir_filter <- Make_TwoTap_Filter(
  name = 'fir1',
  nameInput = 'X_in',
  nameOutput = 'Y_out',
  coeff0 = 0.5,
  coeff1 = 0.5,
  crange = 10,
  rate = 1e-3
)

# Differencing filter (high-pass)
fir_diff <- Make_TwoTap_Filter(
  name = 'fir2',
  nameInput = 'X_in',
  nameOutput = 'Y_out',
  coeff0 = 1.0,
  coeff1 = -1.0,
  crange = 10,
  rate = 1e-3
)
```

### 5. Make_Biquad_Filter()

Implements a second-order Infinite Impulse Response (IIR) filter with transfer function:

H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)

The biquad structure can implement various filter types:
- Low-pass filter: attenuates high frequencies
- High-pass filter: attenuates low frequencies
- Band-pass filter: passes specific frequency range
- Notch filter: rejects specific frequency

**Parameters:**
- `name`: Filter name
- `nameInput`: Input signal species name
- `nameOutput`: Output signal species name
- `b0, b1, b2`: Numerator (feedforward) coefficients
- `a1, a2`: Denominator (feedback) coefficients (a0 normalized to 1)
- `crange`: Concentration range
- `rate`: Base reaction rate

**Returns:** Complete biquad filter module

**Examples:**

```r
# Low-pass Butterworth filter (fc = 0.1*fs)
lpf <- Make_Biquad_Filter(
  name = 'lpf',
  nameInput = 'X',
  nameOutput = 'Y',
  b0 = 0.0201, b1 = 0.0402, b2 = 0.0201,
  a1 = -1.5610, a2 = 0.6414,
  crange = 10,
  rate = 1e-3
)

# High-pass Butterworth filter
hpf <- Make_Biquad_Filter(
  name = 'hpf',
  nameInput = 'X',
  nameOutput = 'Y',
  b0 = 0.8008, b1 = -1.6016, b2 = 0.8008,
  a1 = -1.5610, a2 = 0.6414,
  crange = 10,
  rate = 1e-3
)

# Band-pass filter (center freq ≈ 0.15*fs)
bpf <- Make_Biquad_Filter(
  name = 'bpf',
  nameInput = 'X',
  nameOutput = 'Y',
  b0 = 0.0318, b1 = 0, b2 = -0.0318,
  a1 = -1.5610, a2 = 0.6364,
  crange = 10,
  rate = 1e-3
)

# Notch filter (rejects freq ≈ 0.15*fs)
notch <- Make_Biquad_Filter(
  name = 'notch',
  nameInput = 'X',
  nameOutput = 'Y',
  b0 = 0.9682, b1 = -1.5610, b2 = 0.9682,
  a1 = -1.5610, a2 = 0.9364,
  crange = 10,
  rate = 1e-3
)
```

## Testing with Forced Input Functions

The filter test benches demonstrate how to use forced concentrations to inject time-varying input signals into the CRN system.

### Forced Concentration Pattern

```r
# Define forcing function (returns dC/dt, not absolute concentration)
sinusoidal_forcing <- function(t, amplitude = 1.5e-6, frequency = 0.05) {
  omega <- 2 * pi * frequency
  return(amplitude * omega * cos(omega * t))
}

# Apply to CRN simulation
forced_conc <- list(
  Input = function(t) sinusoidal_forcing(t, amplitude = 2e-6, frequency = 0.1)
)

behavior <- react(
  species = filter$species,
  ci = filter$ci,
  reactions = filter$reactions,
  ki = filter$ki,
  t = t,
  forced_concentrations = forced_conc
)
```

### Available Test Benches

#### 1. Two-Tap Filter Test Bench
**File:** `examples/two_tap_filter_forced_test.R`

Tests the two-tap FIR filter with:
- Square wave inputs
- Sinusoidal inputs (various frequencies)
- Step functions
- Pulse inputs
- Mixed signals (multiple frequencies)
- Frequency response analysis

**To run:**
```r
source('examples/two_tap_filter_forced_test.R')
```


**To run:**
```r
source('examples/biquad_filter_test.R')
```


## References

1. Jiang, H., Riedel, M. D., & Parhi, K. K. (2013). "Digital Signal Processing with Molecular Reactions." *IEEE Design & Test*, 30(3), 21-31.

2. Oppenheim, A. V., & Schafer, R. W. (2009). "Discrete-Time Signal Processing" (3rd ed.). Prentice Hall.

3. Dalchau, N., et al. (2011). "A peptide molecular oscillator." *Journal of The Royal Society Interface*, 8(55), 185-196.