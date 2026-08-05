# # Hjelmfelt neuron behaving as a bounded 0-1 Schmitt trigger
#   hje <- list()
#   hje$species <- c(
#     'I',     # input
#     'XH',    # HIGH state
#     'XL'     # LOW state
#   )
#   hje$ci <- c(
#     0,       # input
#     0,       # XH
#     1        # XL
#   )
#   reactions <- list()
#   reactions[[length(reactions)+1]] <- 'I + XL -> I + XH'
#   reactions[[length(reactions)+1]] <- 'XH -> XL'
#   reactions[[length(reactions)+1]] <- 'XH + XL -> 2 XH'
#   reactions[[length(reactions)+1]] <- 'XL + XH -> 2 XL'
#   hje$reactions <- reactions
#   # Rates
#   hje$ki <- c(
#     40,   # input activation
#     5,    # decay
#     50,   # XH reinforcement
#     45    # XL reinforcement
#   )



  rate <- 1e3
  
  hebb_dual_rail_neuron <- list()
  
  hebb_dual_rail_neuron$species <- c(
    'V_plus','V_minus',
    'U_plus','U_minus',
    'X1',
    'A',
    'B',
    
    'S1_plus','S1_minus',
    'S2_plus','S2_minus',
    'S3_plus','S3_minus',
    
    'W1_plus','W1_minus',
    'W2_plus','W2_minus',
    'W3_plus','W3_minus',
    
    'T_pre1','T_pre2','T_pre3',
    'T_post1',
    
    'W1_sink',
    'W2_sink',
    'W3_sink'
  )
  
  hebb_dual_rail_neuron$ci <- c(
    0,0,
    0,0,
    0,
    0,
    100,
    
    4,0,
    5,0,
    9,0,
    
    1,0,#1,,0,
    1,0,#,1,0,
    1,0,#1,0,
    
    0,0,0,
    0,
    
    0,
    0,
    0
  )
  
  reactions <- list()
  
  # Infrastructure
  reactions[[length(reactions)+1]] <- 'V_plus + V_minus -> 0'
  reactions[[length(reactions)+1]] <- 'U_plus + U_minus -> 0'
  reactions[[length(reactions)+1]] <- 'V_plus -> 0'
  reactions[[length(reactions)+1]] <- 'V_minus -> 0'
  
  # Hjelmfelt engine
  reactions[[length(reactions)+1]] <- 'V_plus -> X1 + V_plus'
  reactions[[length(reactions)+1]] <- 'X1 -> 2X1'
  reactions[[length(reactions)+1]] <- '2X1 -> 0'
  reactions[[length(reactions)+1]] <- 'U_plus + X1 -> U_plus'
  
  # Spike
  reactions[[length(reactions)+1]] <- 'X1 + B -> A + X1'
  reactions[[length(reactions)+1]] <- 'A -> B'
  
  # Recovery/reset
  reactions[[length(reactions)+1]] <- 'V_plus -> U_plus + V_plus'
  reactions[[length(reactions)+1]] <- 'V_minus -> U_minus + V_minus'
  reactions[[length(reactions)+1]] <- 'U_plus -> 0'
  reactions[[length(reactions)+1]] <- 'U_minus -> 0'
  reactions[[length(reactions)+1]] <- 'U_minus -> V_plus + U_minus'
  reactions[[length(reactions)+1]] <- 'A -> U_plus + A'
  reactions[[length(reactions)+1]] <- 'A + V_plus -> A'
  reactions[[length(reactions)+1]] <- 'A + X1 -> A'
  
  # Synapse 1
  reactions[[length(reactions)+1]] <- 'S1_plus + W1_plus -> V_plus + S1_plus + W1_plus'
  reactions[[length(reactions)+1]] <- 'S1_plus + W1_minus -> V_minus + S1_plus + W1_minus'
  reactions[[length(reactions)+1]] <- 'S1_minus + W1_plus -> V_minus + S1_minus + W1_plus'
  reactions[[length(reactions)+1]] <- 'S1_minus + W1_minus -> V_plus + S1_minus + W1_minus'
  
  reactions[[length(reactions)+1]] <- 'S1_plus -> T_pre1 + S1_plus'
  reactions[[length(reactions)+1]] <- 'A -> T_post1 + A'
  
  reactions[[length(reactions)+1]] <- 'T_pre1 -> 0'
  reactions[[length(reactions)+1]] <- 'T_post1 -> 0'
  
  reactions[[length(reactions)+1]] <- 'T_pre1 + A -> W1_plus + T_pre1 + A'
  reactions[[length(reactions)+1]] <- 'T_post1 + S1_plus -> W1_sink + T_post1 + S1_plus'
  
  reactions[[length(reactions)+1]] <- 'W1_sink + W1_plus -> 0'
  reactions[[length(reactions)+1]] <- 'W1_plus -> 0'
  
  # Synapse 2
  reactions[[length(reactions)+1]] <- 'S2_plus + W2_plus -> V_plus + S2_plus + W2_plus'
  reactions[[length(reactions)+1]] <- 'S2_plus + W2_minus -> V_minus + S2_plus + W2_minus'
  reactions[[length(reactions)+1]] <- 'S2_minus + W2_plus -> V_minus + S2_minus + W2_plus'
  reactions[[length(reactions)+1]] <- 'S2_minus + W2_minus -> V_plus + S2_minus + W2_minus'
  
  reactions[[length(reactions)+1]] <- 'S2_plus -> T_pre2 + S2_plus'
  reactions[[length(reactions)+1]] <- 'T_pre2 -> 0'
  
  reactions[[length(reactions)+1]] <- 'T_pre2 + A -> W2_plus + T_pre2 + A'
  reactions[[length(reactions)+1]] <- 'T_post1 + S2_plus -> W2_sink + T_post1 + S2_plus'
  
  reactions[[length(reactions)+1]] <- 'W2_sink + W2_plus -> 0'
  reactions[[length(reactions)+1]] <- 'W2_plus -> 0'
  
  # Synapse 3
  reactions[[length(reactions)+1]] <- 'S3_plus + W3_plus -> V_plus + S3_plus + W3_plus'
  reactions[[length(reactions)+1]] <- 'S3_plus + W3_minus -> V_minus + S3_plus + W3_minus'
  reactions[[length(reactions)+1]] <- 'S3_minus + W3_plus -> V_minus + S3_minus + W3_plus'
  reactions[[length(reactions)+1]] <- 'S3_minus + W3_minus -> V_plus + S3_minus + W3_minus'
  
  reactions[[length(reactions)+1]] <- 'S3_plus -> T_pre3 + S3_plus'
  reactions[[length(reactions)+1]] <- 'T_pre3 -> 0'
  
  reactions[[length(reactions)+1]] <- 'T_pre3 + A -> W3_plus + T_pre3 + A'
  reactions[[length(reactions)+1]] <- 'T_post1 + S3_plus -> W3_sink + T_post1 + S3_plus'
  
  reactions[[length(reactions)+1]] <- 'W3_sink + W3_plus -> 0'
  reactions[[length(reactions)+1]] <- 'W3_plus -> 0'
  
  hebb_dual_rail_neuron$reactions <- reactions
  
  hebb_dual_rail_neuron$ki <- rate * c(
    
    # Infrastructure
    1e4,
    1e4,
    1.0,
    1.0,
    
    # Hjelmfelt engine
    50.0,
    120.0,
    2.0,
    15.0,
    
    # Spike
    100.0,
    10.0,
    
    # Recovery/reset
    0.5,
    0.5,
    0.1,
    0.1,
    5.0,
    4.0,
    500.0,
    500.0,
    
    # Synapse 1
    10.0,
    10.0,
    10.0,
    10.0,
    
    1.0,
    1.0,
    
    5.0,
    5.0,
    
    0.8,
    1.0,
    
    1e3,
    0.005,
    
    # Synapse 2
    10.0,
    10.0,
    10.0,
    10.0,
    
    1.0,
    
    5.0,
    
    0.8,
    1.0,
    
    1e3,
    0.005,
    
    # Synapse 3
    10.0,
    10.0,
    10.0,
    10.0,
    
    1.0,
    
    5.0,
    
    0.8,
    1.0,
    
    1e3,
    0.005
  )
  