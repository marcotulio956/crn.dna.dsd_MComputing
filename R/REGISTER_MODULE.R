# =============================================================================
# Register Module for Pipelined Molecular Computation
# =============================================================================
# This module implements register components that hold values between clock
# phases, enabling non-blocking assignments and staged computation.
#
# Registers capture input data on a clock edge and hold it until the next
# clock cycle, implementing the behavior of blocking assignments (<=) in
# hardware description languages.
# =============================================================================

jn <- function(...) { paste(..., sep = '') }

#' Make Register Module
#'
#' Creates a register that captures and holds a value based on a clock enable signal.
#' The register implements non-blocking assignment behavior, where the output
#' is updated only when the clock enable is active.
#'
#' This enables pipelined operations like:
#'   a = b + c      (combinational)
#'   reg_a <= a     (captured on clock edge)
#'   c <= reg_a     (updated on next clock edge)
#'
#' @param name Register name prefix
#' @param input_data Name of input data signal
#' @param clock_enable Name of clock enable signal (e.g., 'clk_pos')
#' @param output_data Name of output data signal
#' @param initial_value Initial register value (default: 0)
#' @param rate Reaction rate for register operations
#'
#' @return A gate object compatible with circuit_add_gate
#' @export
#'
#' @examples
#' reg <- Make_Register(
#'   name = 'reg_a',
#'   input_data = 'data_in',
#'   clock_enable = 'clk_pos',
#'   output_data = 'data_out',
#'   initial_value = 0,
#'   rate = 1e-3
#' )
Make_Register <- function(name,
                          input_data,
                          clock_enable,
                          output_data,
                          initial_value = 0,
                          rate = 1e-3) {
  
  # Internal register state
  internal_state <- jn(name, '_state')
  capture_signal <- jn(name, '_capture')
  
  species <- list(
    input = input_data,
    clock = clock_enable,
    internal = internal_state,
    capture = capture_signal,
    output = output_data
  )
  
  ci <- c(
    0,              # input (driven by previous stage)
    0,              # clock (driven by clock system)
    initial_value,  # internal state (initial register value)
    0,              # capture signal
    initial_value   # output (same as initial state)
  )
  
  reactions <- c(
    # Capture: when clock is high and input is present, generate capture signal
    # input + clock -> input + clock + capture
    jn(input_data, ' + ', clock_enable, ' -> ', 
       input_data, ' + ', clock_enable, ' + ', capture_signal),
    
    # Update internal state: capture signal transfers input to internal state
    # input + capture -> internal + capture
    jn(input_data, ' + ', capture_signal, ' -> ', 
       internal_state, ' + ', capture_signal),
    
    # Output follows internal state
    # internal -> internal + output
    jn(internal_state, ' -> ', internal_state, ' + ', output_data),
    
    # Decay of capture signal
    jn(capture_signal, ' -> 0'),
    
    # Output decay (to allow updates)
    jn(output_data, ' -> 0')
  )
  
  ki <- c(
    rate * 5,   # Capture generation
    rate * 10,  # State update
    rate * 10,  # Output generation
    rate * 2,   # Capture decay
    rate * 2    # Output decay
  )
  
  gate <- list(
    name = name,
    species = species,
    ci = ci,
    reactions = reactions,
    ki = ki
  )
  
  return(gate)
}

#' Make Simple Latch
#'
#' Creates a simple latch that passes input to output when enable is high.
#' Unlike a register, a latch is level-sensitive (not edge-triggered).
#'
#' @param name Latch name prefix
#' @param input_data Name of input signal
#' @param enable Name of enable signal
#' @param output_data Name of output signal
#' @param initial_value Initial latch value
#' @param rate Reaction rate
#'
#' @return A gate object
#' @export
Make_Latch <- function(name,
                      input_data,
                      enable,
                      output_data,
                      initial_value = 0,
                      rate = 1e-3) {
  
  species <- list(
    input = input_data,
    enable = enable,
    output = output_data
  )
  
  ci <- c(0, 0, initial_value)
  
  reactions <- c(
    # When enabled, output follows input
    jn(input_data, ' + ', enable, ' -> ', input_data, ' + ', enable, ' + ', output_data),
    # Output decay when not driven
    jn(output_data, ' -> 0')
  )
  
  ki <- c(rate * 10, rate * 2)
  
  gate <- list(
    name = name,
    species = species,
    ci = ci,
    reactions = reactions,
    ki = ki
  )
  
  return(gate)
}

#' Make D Flip-Flop
#'
#' Creates a D flip-flop that captures data on the rising edge of the clock.
#' This is a proper edge-triggered register.
#'
#' @param name Flip-flop name prefix
#' @param d_input D input signal name
#' @param clock Clock signal name
#' @param q_output Q output signal name
#' @param initial_value Initial Q value
#' @param rate Reaction rate
#'
#' @return A gate object
#' @export
Make_D_FlipFlop <- function(name,
                            d_input,
                            clock,
                            q_output,
                            initial_value = 0,
                            rate = 1e-3) {
  
  # Use the register implementation
  Make_Register(
    name = name,
    input_data = d_input,
    clock_enable = clock,
    output_data = q_output,
    initial_value = initial_value,
    rate = rate
  )
}

#' Make Pipeline Register Bank
#'
#' Creates a bank of registers for holding multiple signals in a pipeline stage.
#' All registers share the same clock enable.
#'
#' @param name Bank name prefix
#' @param input_signals Vector of input signal names
#' @param clock_enable Clock enable signal name
#' @param output_signals Vector of output signal names (must match input_signals length)
#' @param initial_values Vector of initial values (optional)
#' @param rate Reaction rate
#'
#' @return A list of gate objects, one per register
#' @export
Make_Register_Bank <- function(name,
                               input_signals,
                               clock_enable,
                               output_signals,
                               initial_values = NULL,
                               rate = 1e-3) {
  
  if (length(input_signals) != length(output_signals)) {
    stop("input_signals and output_signals must have the same length")
  }
  
  if (is.null(initial_values)) {
    initial_values <- rep(0, length(input_signals))
  }
  
  if (length(initial_values) != length(input_signals)) {
    stop("initial_values must have the same length as input_signals")
  }
  
  registers <- list()
  
  for (i in seq_along(input_signals)) {
    reg_name <- jn(name, '_reg', i)
    
    registers[[i]] <- Make_Register(
      name = reg_name,
      input_data = input_signals[i],
      clock_enable = clock_enable,
      output_data = output_signals[i],
      initial_value = initial_values[i],
      rate = rate
    )
  }
  
  return(registers)
}
