
source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/neuron_hjelmfelt.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')
source('R/forced_concentrations.R')

timing <- seq(0, 5, by = 0.05)
rate <- 1

build_compiled_circuit <- function(gates, timing) {
	circuit <- make_circuit(timing)
	circuit <- circuit_add_compile_gates(circuit, gates)
	return(circuit)
}

# Instantiate every component-constructor available in ELECTRO_LIB.R
cap0 <- Make_Capacitor_Component(id = 0, capacitance = 1, resistance = 1)
ind0 <- Make_Inductor_Component(id = 0, inductance = 1, resistance = 1)
rlc0 <- Make_RLC_Component(resistance = 1, inductance = 1, capacitance = 1)

circuits <- list(
	capacitor_rc = build_compiled_circuit(
		Make_Circuit_RC(cap0$name, cap0$il, cap0$ol, cap0$ic, rate),
		timing
	),
	capacitor_pure = build_compiled_circuit(
		Make_Circuit_Pure_Capacitor(cap0$name, cap0$il, cap0$ol, cap0$ic, rate),
		timing
	),
	capacitor_derivative = build_compiled_circuit(
		Make_Circuit_Pure_Capacitor_Derivative(cap0$name, cap0$il, cap0$ol, cap0$ic, rate),
		timing
	),
	inductor_rl = build_compiled_circuit(
		Make_Circuit_RL(ind0$name, ind0$il, ind0$ol, ind0$ic, rate),
		timing
	),
	inductor_rl2 = build_compiled_circuit(
		Make_Circuit_RL2(ind0$name, ind0$il, ind0$ol, ind0$ic, rate),
		timing
	),
	inductor_resistive = build_compiled_circuit(
		Make_Circuit_Resistive_Inductor(ind0$name, ind0$il, ind0$ol, ind0$ic, rate),
		timing
	),
	inductor_pure = build_compiled_circuit(
		Make_Circuit_Pure_Inductor(ind0$name, ind0$il, ind0$ol, ind0$ic, rate),
		timing
	),
	inductor_integrator = build_compiled_circuit(
		Make_Circuit_Pure_Inductor_Integrator(ind0$name, ind0$il, ind0$ol, ind0$ic, rate),
		timing
	),
	rlc = build_compiled_circuit(
		Make_Circuit_RLC(rlc0$name, rlc0$il, rlc0$ol, rlc0$ic, c(1, 1, 1, 1, 1, 1, 1, 1)),
		timing
	),
	rlc_composited = Make_Circuit_RLC_Composited(timing,'U')
)

cat('Circuit sizes (CRN and 4-domain translation)\n')

for (name in names(circuits)) {
	circuit <- circuits[[name]]
	cat(sprintf(
		'\n[%s] CRN: reactions=%d species=%d\n',
		name,
		length(circuit$reactions),
		length(circuit$species)
	))

	dsd <- Translate_4domain(circuit)
	cat(sprintf(
		'[%s] DSD: reactions=%d species=%d\n',
		name,
		length(dsd$reactions),
		length(dsd$species)
	))
}


