// collectData.js

export const collectSimuladorData = () => {
  const simulador1Data =
    JSON.parse(localStorage.getItem("simulador1Data")) || {};
  const simulador2Data =
    JSON.parse(localStorage.getItem("simulador2Data")) || {};
  const simulador3Data =
    JSON.parse(localStorage.getItem("simulador3Data")) || {};
  const simulador4Data =
    JSON.parse(localStorage.getItem("simulador4Data")) || {};

  const combinedData = {
    simulador1: simulador1Data,
    simulador2: simulador2Data,
    simulador3: simulador3Data,
    simulador4: simulador4Data,
  };

  return combinedData;
};

export function convertSimuladorData(data) {
  // Extract inputs from simulador1
  const inputs = data.simulador1.inputs;
  const inputMap = {};
  inputs.forEach((name) => {
    inputMap[name] = "0";
  });

  // Extract reactions from simulador2
  // Extract reactions from simulador2
  // Extract reactions from simulador2
  const reactions = data.simulador2.inputGroups.map((group) => {
    // The rate constant is the last element
    const rate = parseFloat(group[group.length - 1]);

    // Extract number of reactants and products
    const numReactants = data.simulador2.numReactants;
    const numProducts = data.simulador2.numProducts;

    // Reactants are the first numReactants elements
    const reactants = group.slice(0, numReactants);

    // Products are the next numProducts elements
    const products = group.slice(numReactants, numReactants + numProducts);

    // Construct the reaction string
    const reactantsString = reactants.join(" + ");
    const productsString = products.join(" + ");
    const reactionString = `${reactantsString} -> ${productsString}`;

    return {
      reacao: reactionString,
      constanteTaxa: rate,
    };
  });

  // Extract goticulas from simulador3
  // Extract goticulas from simulador3
  const goticulas = data.simulador3.inputGroups.map((group, idx) => {
    const goticula = {
      id: idx,
      nome: `${group.text}`,
      especies: group.extraInputs.map((inp) => ({
        nome: inp.selectValue,
        concentracaoInicial: inp.textValue,
      })),
    };

    // Add default values for species not mentioned
    inputs.forEach((species) => {
      if (!goticula.especies.some((e) => e.nome === species)) {
        goticula.especies.push({ nome: species, concentracaoInicial: "0" });
      }
    });

    return goticula;
  });

  // Extract goticulas misturadas from simulador4
  const misturadas1 = data.simulador4.inputGroups.map((group, idx) => {
    // Get the names of the goticulas to be mixed from the select values
    const goticulasToMix = group.extraInputs.map((inp) => inp.selectValue);

    // Find the ids of the goticulas and misturadas whose names match the select values
    const goticulasMisturadas = goticulas
      .filter((g) => goticulasToMix.includes(g.nome))
      .map((g) => g.id);

    return {
      id: goticulas.length + idx,
      nome: `${group.text}`,
      goticulasMisturadas: goticulasMisturadas,
      tempoMistura: group.extraInputs.map((inp) => parseFloat(inp.textValue)),
    };
  });

  // Extract goticulas misturadas from simulador4
  const misturadas = data.simulador4.inputGroups.map((group, idx) => {
    // Get the names of the goticulas to be mixed from the select values
    const goticulasToMix = group.extraInputs.map((inp) => inp.selectValue);

    const goticulasMisturadas = goticulas
      .filter((g) => goticulasToMix.includes(g.nome))
      .map((g) => ({ id: g.id, nome: g.nome }));

    const goticulasMisturadasM = misturadas1
      .filter((g) => goticulasToMix.includes(g.nome))
      .map((g) => ({ id: g.id, nome: g.nome }));

    const combinedGoticulasMisturadas = [
      ...goticulasMisturadas,
      ...goticulasMisturadasM,
    ]
      .sort(
        (a, b) =>
          goticulasToMix.indexOf(a.nome) - goticulasToMix.indexOf(b.nome)
      )
      .map((g) => g.id);

    return {
      id: goticulas.length + idx,
      nome: `${group.text}`,
      goticulasMisturadas: combinedGoticulasMisturadas,
      tempoMistura: group.extraInputs.map((inp) => parseFloat(inp.textValue)),
    };
  });

  // Prepare the final output
  const output = {
    goticulasIniciais: goticulas,
    goticulasMisturadas: misturadas,
    reacoes: reactions,
  };

  console.log(JSON.stringify(output));

  return output;
}
