# -------------------------------
# 1. Função “factory” que compila o modelo fixo
# -------------------------------
make_reactor_optimized <- function(species, ci, reactions, ki_init, 
                         t, verbose = FALSE, ...) {
  # species : vetor de nomes das espécies (caracteres)
  # ci      : vetor de concentrações iniciais (nome em mesma ordem de 'species')
  # reactions: lista de reações (cada elemento, estrutura de reação)
  # ki_init  : vetor inicial de constantes de taxa (usado apenas para checar uq)
  # t        : vetor de tempos para simulação (mesmo de antes)
  # verbose  : se TRUE, exibe diagnósticos de deSolve
  # ...      : quaisquer argumentos extras que queira passar a deSolve[1]
  #
  # Dentro, só fazemos estequiometria e mapeamentos UMA vez:
  
  # 1.1 Verifica consistência estrutural
  reactions_checked <- check_crn(species, ci, reactions, ki_init, t)
  
  # 1.2 Monta a matriz de estequiometria (M) e as matrizes de reagentes
  sto_info   <- get_M(reactions_checked, species)
  sto_react  <- t(sto_info$react)
  Mt         <- t(sto_info$M)


  
  # 1.3 Constrói 'reactant_map': para cada reação, quais índices de espécie aparecem
  reactant_map <- lapply(reactions_checked, function(reaction) {
    r_map <- reactants_in_reaction(species, reaction)
    if (is.null(r_map)) {
      # Para o caso sem reagentes, devolve NA (assim evitamos prod(NA ^ v_exp) errado)
      return(NA)
    } else {
      return(r_map)
    }
  })
  
  cat(reactant_map, "\n")

  # 1.4 Calcula diretamente os expoentes: v_exp_reactants[[i]] será um vetor de expoentes
  v_exp_reactants <- lapply(seq_along(reactant_map), function(i) {
    if (length(reactant_map[[i]]) > 1 || !is.na(reactant_map[[i]])) {
      return(sto_react[reactant_map[[i]], i])
    } else {
      return(0)
    }
  })
  
  # Guarda tudo em um ambiente (ou lista) para uso futuro sem ter que recomputar:
  data_compiled <- list(
    species         = species,
    ci              = ci,
    reactions       = reactions_checked,
    reactant_map    = reactant_map,
    v_exp_reactants = v_exp_reactants,
    Mt              = Mt,
    t               = t,
    verbose         = verbose,
    extra_args      = list(...)  # argumentos extras a repassar a deSolve
  )
  
  # 1.5 Define a função interna 'run' que só recebe um vetor ki e simula
  run_fn <- function(ki_vec) {
    # ki_vec deve ter comprimento igual a length(reactions_checked)
    if (length(ki_vec) != length(data_compiled$reactions)) {
      stop("Comprimento de ki_vec não corresponde ao número de reações.")
    }
    
    # Função dx/dt para o deSolve, que usa apenas o mapeamento precomputado
    fx <- function(t, y, parms) {
      # Calcula para cada reação: k_i * prod(y[react_map]^expoente)
      v <- matrix(
        mapply(
          function(react_idx, k_val, v_exp) {
            if (is.na(react_idx)[1]) {
              return(0)  # Caso não haja reagentes, taxa zero ou vazia
            }
            prod_y_expo <- prod(y[react_idx]^v_exp)
            return(k_val * prod_y_expo)
          },
          data_compiled$reactant_map,
          ki_vec,
          data_compiled$v_exp_reactants
        )
      )
      # Multiplica Mᵀ %*% v para obter derivadas de cada espécie
      dy <- data_compiled$Mt %*% v
      list(as.vector(dy))
    }
    
    # Chama o solver deSolve apenas com o fx e o mesmo y0 e t da compilação
    sol <- deSolve::ode(
      times = data_compiled$t,
      y     = data_compiled$ci,
      func  = fx,
      parms = NULL,
      # repassa os argumentos extras de data_compiled$extra_args
      atol  = data_compiled$extra_args$atol %||% 1e-6,
      rtol  = data_compiled$extra_args$rtol %||% 1e-6,
      verbose = data_compiled$verbose
    )
    
    # Converte para data.frame igual à sua função original
    df_res <- data.frame(sol)
    names(df_res) <- c('time', data_compiled$species)
    return(df_res)
  }
  
  # 1.6 Retorna lista com tudo: o modelo compilado e a função run
  list(
    compiled_data = data_compiled,
    run           = run_fn
  )
}

# -------------------------------
# 2. Exemplo de uso dentro de um loop de otimização
# -------------------------------
#
# Suponha que você tenha:
#   species   = vetor de nomes de todas as espécies,
#   ci        = vetor numérico de concentrações iniciais,
#   reactions = lista de definições de cada reação,
#   ki_init   = vetor inicial de constantes de taxa,
#   t_vector  = vetor de tempos (mesmo que você usava antes).
#
# Então, primeiro você compila o modelo:
#
#   reactor_obj <- make_reactor(species, ci, reactions, ki_init, t_vector, verbose = FALSE)
#
# Isso demora aproximadamente o mesmo que uma única chamada a React(...) antiga,
# mas agora todas as estruturas fixas estão salvas em reactor_obj$compiled_data.
#
# Para simular com um novo vetor ki_novo, basta chamar:
#
#   resultado_ki_novo <- reactor_obj$run(ki_novo)
#
# Isso invoca somente o deSolve com uma função 'fx()' que já sabe quais colunas usar,
# qual Mᵀ usar e qual mapeamento de reagentes usar. Não há nenhum novo cálculo de stoic.
#
# Dentro de uma rotina de calibração (e.g. dentro de optim()), fica assim:
#
#   obj_fun <- function(ki_vec) {
#     # Simula CRN rápido:
#     df_crn <- reactor_obj$run(ki_vec)
#     # Extrai as saídas desejadas (mesmo processo que antes):
#     vc_crn <- 0.1 * (df_crn[['rlcol_vcp']] - df_crn[['rlcol_vcn']])
#     il_crn <- 0.1 * (df_crn[['rlcol_ip']]  - df_crn[['rlcol_in']])
#     # Simula o circuito “analógico” com a mesma v1p:
#     simRLC <- simulate_sRLC_voltage_source(
#       t_vector,
#       df_crn[['v1p']],
#       R_val, L_val, C_val
#     )
#     vc_true <- simRLC$capacitor_voltage
#     il_true <- simRLC$inductor_current
#     # Calcula erro quadrático somado:
#     err <- sum((vc_crn - vc_true)^2) + sum((il_crn - il_true)^2)
#     return(err)
#   }
#
#   # E então:
#   otim_res <- optim(
#     par    = ki_init,
#     fn     = obj_fun,
#     method = "L-BFGS-B",
#     lower  = rep(0, length(ki_init)),
#     upper  = rep(Inf, length(ki_init))
#   )
#
# Note que, em cada iteração de optim(), apenas `obj_fun()` será reexecutado. 
# Ele chama `reactor_obj$run(ki_vec)`, mas tudo que envolve estequiometria, `get_M()`, 
# `reactants_in_reaction()` e montagem de matrizes já foi feito uma única vez.  
#
# Dessa forma, a parte repetitiva e pesada (montagem das matrizes) é removida e 
# só sobra a integração numérica por deSolve. Isso, na prática, costuma reduzir o 
# tempo total do ajuste em dezenas de vezes, especialmente se o número de reações 
# e espécies for grande.
