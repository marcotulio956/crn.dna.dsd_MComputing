import AddIcon from "@mui/icons-material/Add";
import DeleteIcon from "@mui/icons-material/Delete";
import { Box, Button, Drawer, IconButton, List } from "@mui/material";
import SaveIcon from "@mui/icons-material/Save";
import UploadIcon from "@mui/icons-material/Upload";
import ScienceIcon from "@mui/icons-material/Science";
import AutorenewIcon from "@mui/icons-material/Autorenew";
import React, { useEffect, useState } from "react";
import MenuSection from "./MenuSection";
import ReactionPopup from "./ReactionPopup";
import SpeciesConcentrationDialog from "./SpeciesConcentrationDialog";
import SimulationSettings from "./SimulationSettings";

const DEFAULT_DROPLET_VOLUME = 2.25e-13;

const withDefaultDropletVolume = (droplet = {}) => ({
  ...droplet,
  volume: DEFAULT_DROPLET_VOLUME,
});

const applyDefaultDropletVolume = (droplets = []) =>
  (droplets || []).map(withDefaultDropletVolume);

const LeftSidebar = ({
  selectedNode,
  setSelectedNode,
  selectedEdge,
  setSelectedEdge,
  nodes,
  setNodes,
  speciesData,
  setSpeciesData,
  edges,
  setEdges,
  setLoading,
  setResultImages,
  setResultTables,
  resultTables,
  setTabIndex,
  setAnimationData,
  setTableData,
  tableData
}) => {
  const initializeConcentrations = (speciesOptions) => {
    return speciesOptions.map((species) => ({
      speciesId: species.id,
      droplets: {},
    }));
  };

  const reindexSpeciesList = (speciesList) =>
    speciesList.map((species, index) => ({
      ...species,
      id: index + 1,
    }));

  const buildSpeciesIdMapping = (speciesList) => {
    const mapping = {};
    speciesList.forEach((species, index) => {
      mapping[species.id] = index + 1;
    });
    return mapping;
  };

  const remapConcentrationArray = (concentration = [], idMapping = {}) => {
    const updated = [];
    Object.entries(idMapping).forEach(([oldId, newId]) => {
      const oldIndex = Number(oldId);
      updated[newId] = concentration?.[oldIndex] ?? 0;
    });
    return updated;
  };

  const remapDropletConcentrations = (droplets, idMapping) =>
    droplets.map((droplet) => ({
      ...droplet,
      concentration: remapConcentrationArray(droplet.concentration || [], idMapping),
    }));

  const normalizeConcentrations = (concentration = [], speciesList = []) => {
    if (!Array.isArray(speciesList) || speciesList.length === 0) {
      return [];
    }

    const normalized = [];
    speciesList.forEach(({ id }) => {
      normalized[id] = concentration?.[id] ?? 0;
    });

    return normalized;
  };

  const [inputs, setInputs] = useState([]);
  const [selectedRow, setSelectedRow] = useState(null);
  const [selectedSpecies, setSelectedSpecies] = useState(null);
  const [reactionData, setReactionData] = useState([]);
  const [selectedReaction, setSelectedReaction] = useState(null);
  const [reactionPopupOpen, setReactionPopupOpen] = useState(false);
  const [reactants, setReactants] = useState([]);
  const [products, setProducts] = useState([]);
  const [rates, setRates] = useState([]);
  const [numReactants, setNumReactants] = useState(1);
  const [numProducts, setNumProducts] = useState(1);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [concentrations, setConcentrations] = useState(
    initializeConcentrations(speciesData)
  );
  const [simulation, setSimulation] = useState({
    engine: "desolve",
    stochastic: false,
    dna: false,
    volume: 10,
    seed: null,
  });
  const [forcing, setForcing] = useState({ name: null, params: {} });
  const [timing, setTiming] = useState({
    enabled: false,
    tolerancePercent: 1,
    stableSamples: 3,
    step: 0.1,
    maxTime: 100,
  });

  useEffect(() => {
    setTableData((prevRows) => applyDefaultDropletVolume(prevRows || []));
  }, [setTableData]);

  const handleDialogClose = () => setDialogOpen(false);

  const formatReaction = (reactants, products) => {
    if (
      !reactants ||
      !products ||
      reactants.length === 0 ||
      products.length === 0
    )
      return "";
    const reactantsStr = reactants.filter(Boolean).join(" + ");
    const productsStr = products.filter(Boolean).join(" + ");
    return `${reactantsStr} -> ${productsStr}`;
  };

  const handleSaveData = () => {
    const jsonData = {
      tableData,
      speciesData,
      reactionData: reactionData.map((reaction) => ({
        ...reaction,
        formattedReaction: formatReaction(
          reaction.reactants,
          reaction.products
        ),
      })),
      concentrations: concentrations.map((concentration) => ({
        ...concentration,
      })),
      reactants,
      products,
      numReactants,
      numProducts,
      nodes,
      edges,
      simulation,
      forcing,
      timing,
      mixing: { concentrationUnit: "nmol/L", volumeUnit: "m3" },
    };

    const jsonString = JSON.stringify(jsonData, null, 2);
    const blob = new Blob([jsonString], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = "data.json";
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  };

  const handleImportData = (event) => {
    const file = event.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (e) => {
        const importedData = JSON.parse(e.target.result);
        const adjustedSpeciesData = (importedData.speciesData || []).map((species) => ({
          ...species,
          showInResults: species.showInResults !== undefined ? species.showInResults : true, // Faz com que comecem checked
        }));
        const normalizedTableData = (importedData.tableData || []).map((row) => ({
          ...row,
          concentration: row.concentration || [],
          isStartingDroplet: Boolean(row.isStartingDroplet),
        }));
        const firstStartingIndex = normalizedTableData.findIndex((row) => row.isStartingDroplet);
        const sanitizedTableData =
          firstStartingIndex === -1
            ? normalizedTableData
            : normalizedTableData.map((row, index) => ({
                ...row,
                isStartingDroplet: index === firstStartingIndex,
              }));
        const reindexedSpeciesData = reindexSpeciesList(adjustedSpeciesData);
        const speciesIdMapping = buildSpeciesIdMapping(adjustedSpeciesData);
        const remappedDroplets = remapDropletConcentrations(
          sanitizedTableData,
          speciesIdMapping
        );
        setTableData(applyDefaultDropletVolume(remappedDroplets));
        setSpeciesData(reindexedSpeciesData);
        setReactionData(importedData.reactionData || []);
        setConcentrations(importedData.concentrations || []);
        setReactants(importedData.reactants || []);
        setProducts(importedData.products || []);
        setNumReactants(importedData.numReactants || []);
        setNumProducts(importedData.numProducts || []);
        setNodes(importedData.nodes || []);
        setEdges(importedData.edges || []);
        setSimulation({
          engine: "desolve",
          stochastic: false,
          dna: false,
          volume: 10,
          seed: null,
          ...(importedData.simulation || {}),
        });
        setForcing(importedData.forcing || { name: null, params: {} });
        setTiming({
          enabled: false,
          tolerancePercent: 1,
          stableSamples: 3,
          step: 0.1,
          maxTime: 100,
          ...(importedData.timing || {}),
        });
        setProducts(importedData.products || []);
      };
      reader.readAsText(file);
    }
  };

  const handleImportReactions = (event) => {
    const file = event.target.files[0];
    if (!file) {
      return;
    }

    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        const importedData = JSON.parse(e.target.result);
        const importedSpecies = Array.isArray(importedData.speciesData)
          ? importedData.speciesData
          : [];
        const normalizedSpecies = reindexSpeciesList(
          importedSpecies.map((species) => ({
            ...species,
            showInResults:
              species.showInResults !== undefined ? species.showInResults : true,
          }))
        );

        const importedReactions = Array.isArray(importedData.reactionData)
          ? importedData.reactionData
          : [];
        const normalizedReactions = importedReactions.map((reaction, index) => {
          const reactants = reaction.reactants || [];
          const products = reaction.products || [];
          return {
            ...reaction,
            id: index + 1,
            reactants,
            products,
            rates:
              reaction.rates !== undefined
                ? reaction.rates
                : reaction.rate !== undefined
                ? reaction.rate
                : null,
            formattedReaction:
              reaction.formattedReaction || formatReaction(reactants, products),
          };
        });

        setSpeciesData(normalizedSpecies);
        setReactionData(normalizedReactions);
        setSelectedSpecies(null);
        setSelectedReaction(null);
        setTableData((prevTableData) => {
          const updatedTable = applyDefaultDropletVolume(
            (prevTableData || []).map((row) => ({
              ...row,
              concentration: normalizeConcentrations([], normalizedSpecies),
            }))
          );

          if (selectedRow) {
            const updatedSelected = updatedTable.find(
              (row) => row.id === selectedRow.id
            );
            if (updatedSelected) {
              setSelectedRow(updatedSelected);
            }
          }

          return updatedTable;
        });
      } catch (error) {
        console.error("Failed to import reactions:", error);
        alert(
          "Failed to import reactions. Please ensure the JSON format is correct."
        );
      } finally {
        if (importReactionsInputRef.current) {
          importReactionsInputRef.current.value = "";
        }
      }
    };

    reader.readAsText(file);
  };

  function convertPosition(positionPercent, channel) {
    const fromX = Number(edges[channel].from.x);
    const fromY = Number(edges[channel].from.y);
    const toX = Number(edges[channel].to.x);
    const toY = Number(edges[channel].to.y);
    const percent = Number(positionPercent);
    

    const x = fromX + percent * (toX - fromX);
    const y = fromY + percent * (toY - fromY);


    return { x, y };
  }

  function transformData(data) {
    const dropletsMap = new Map();

    data.network.forEach((networkItem) => {
      const time = networkItem.time;

      networkItem.bigDroplets.forEach((droplet) => {
        const { id, boundaries } = droplet;

        if (!dropletsMap.has(id)) {
          dropletsMap.set(id, { id, positions: [] });
        }

        if (boundaries.length > 0) {
          const { channel, position } = boundaries[boundaries.length - 1].position;
          const { x, y } = convertPosition(position, channel);
          dropletsMap.get(id).positions.push({ x, y, time });
        }
      });
    });

    return Array.from(dropletsMap.values());
  }

  const handleFormat = () => {
    const reactionD = reactionData.map((reaction) => ({
      ...reaction,
      formattedReaction: formatReaction(reaction.reactants, reaction.products),
    }));

    // Create "goticulasIniciais" array
    const goticulasIniciais = tableData.map((droplet) => {
      const especies = speciesData.map((species, index) => {
        return {
          nome: species.name,
          concentracaoInicial: droplet.concentration[index + 1] ?? 0, // Fallback to 0 if undefined or null
        };
      });

      // Example: Adjust concentrations based on specific droplet conditions
      // You can add logic here to populate `concentracaoInicial` from your inputData

      return {
        id: droplet.id - 1, // Adjusting id to start from 0
        nome: droplet.name,
        pump: droplet.pump,
        volume: Number(droplet.volume),
        especies: especies,
      };
    });

    // Create "reacoes" array
    const reacoes = reactionD.map((reaction) => {
      return {
        reacao: reaction.formattedReaction,
        rate: reaction.rates
      };
    });

    // Create final JSON object
    const data = {
      goticulasIniciais: goticulasIniciais,
      nodes: nodes,
      reacoes: reacoes,
      edges: edges,
      simulation,
      forcing: forcing.name ? { ...forcing, enabled: true } : null,
      timing,
      mixing: { concentrationUnit: "nmol/L", volumeUnit: "m3" },
    };

    console.log(data)

    setLoading(true);
    fetch("/submit_MMFT_New", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(data),
    })
      .then(async (response) => {
        if (!response.ok) {
          const errorData = await response.json();
          // Create error object with structured data
          const error = new Error(errorData.message || errorData.error || `Server error: ${response.status}`);
          error.errorType = errorData.error;
          error.suggestion = errorData.suggestion;
          throw error;
        }
        return response.json();
      })
      .then((data) => {
        //setPlotImage(`data:image/png;base64,${data.plot_image}`);
        if (data.pdf_images && Array.isArray(data.pdf_images)) {
          setResultImages(
            data.pdf_images.map(
              (imageBase64) => `data:image/png;base64,${imageBase64}`
            )
          );
        } else {
          setResultImages([]);
        }
        setResultTables(data.tabelas_DNA || []); // ONDE TA OS DADOS
        setTabIndex(1);
        //setValue(1)
        if (data.simulation_result) {
          setAnimationData(transformData(data.simulation_result));
        }
        setLoading(false);
      })
      .catch((error) => {
        console.error("Error:", error);
        
        // Extract structured error information
        const errorTitle = error.errorType || "Simulation Failed";
        const errorMessage = error.message || "An unknown error occurred";
        const suggestion = error.suggestion || "Please check your input and try again";
        
        // Display user-friendly error with icon and formatting
        alert(
          `❌ ${errorTitle}\n\n` +
          `${errorMessage}\n\n` +
          `💡 ${suggestion}`
        );
        setLoading(false);
      });
  };

  useEffect(() => {
    if (selectedEdge) {
      setInputs([{ label: "Height", value: selectedEdge.height || "" }]);
    } else if (selectedNode) {
      setInputs([
        { label: "Name", value: selectedNode.name || "", readOnly: false },
        { label: "X position", value: selectedNode.x || "" },
        { label: "Y position", value: selectedNode.y || "" },
        {
          label: "isSink",
          value: selectedNode.isSink || false,
          type: "checkbox",
        },
      ]);
    } else {
      setInputs([]);
    }
  }, [selectedNode, selectedEdge]);

  useEffect(() => {
    const hasMissingShowInResults = speciesData.some(
      (sp) => sp.showInResults === undefined
    );
  
    if (hasMissingShowInResults) {
      const updatedSpeciesData = speciesData.map((sp) =>
        sp.showInResults !== undefined ? sp : { ...sp, showInResults: true }
      );
      setSpeciesData(updatedSpeciesData);
    }
  }, []);  

  useEffect(() => {
    if (selectedReaction) {
      setReactants(selectedReaction.reactants || []);
      setProducts(selectedReaction.products || []);
      setNumReactants((selectedReaction.reactants || []).length);
      setNumProducts((selectedReaction.products || []).length);
      setRates(selectedReaction.rates || []);
    }
  }, [selectedReaction]);

  useEffect(() => {
    if (selectedRow) {
      setConcentrations(
        normalizeConcentrations(selectedRow.concentration || [], speciesData)
      );
    } else {
      setConcentrations([]);
    }
  }, [selectedRow, speciesData]);

  const handleInputChange = (index, newValue) => {
    if (selectedEdge) {
      setSelectedEdge({ ...selectedEdge, height: newValue });
    } else if (selectedNode) {
      // Determine whether to update x, y, or isSink based on index
      if (inputs[index].label === "isSink") {
        setSelectedNode({
          ...selectedNode,
          isSink: newValue,
        });
      } else {
        setSelectedNode({
          ...selectedNode,
          [index === 1 ? "x" : "y"]: newValue,
        });
      }
    } else if (selectedRow) {
      // Update selectedRow based on input label
      const updatedRow = {
        ...selectedRow,
        [inputs[index].label.toLowerCase()]: newValue,
      };
      setSelectedRow(updatedRow);
    }
  };

  const handleRowInputChange = (field, value) => {
    if (selectedRow) {
      const updatedValue =
        field === "volume" ? DEFAULT_DROPLET_VOLUME : value;
      const updatedRow = { ...selectedRow, [field]: updatedValue };
      setSelectedRow(updatedRow);
      setTableData(
        tableData.map((row) => (row.id === selectedRow.id ? updatedRow : row))
      );
    }
  };

  const handleRowSelect = (row) => {
    setSelectedRow(row);
  };

  const addNewRow = () => {
    const newRow = {
      id: tableData.length + 1,
      name: `Droplet ${tableData.length + 1}`,
      volume: DEFAULT_DROPLET_VOLUME,
      pump: "",
      concentration: [],
      isStartingDroplet: false,
    };
    setTableData([...tableData, newRow]);
  };

  const deleteSelectedRow = () => {
    if (selectedRow) {
      setTableData(tableData.filter((row) => row.id !== selectedRow.id));
      setSelectedRow(null);
    }
  };

  const handleSpeciesSelect = (species) => {
    setSelectedSpecies(species);
  };

  const handleSpeciesInputChange = (field, value) => {
    if (selectedSpecies) {
      const updatedSpecies = { ...selectedSpecies, [field]: value };
      setSelectedSpecies(updatedSpecies);
      setSpeciesData(
        speciesData.map((species) =>
          species.id === selectedSpecies.id ? updatedSpecies : species
        )
      );
    }
  };

  const addNewSpecies = () => {
    const newSpecies = {
      id: speciesData.length + 1,
      name: `Species ${speciesData.length + 1}`,
      showInResults: true,
    };
    setSpeciesData([...speciesData, newSpecies]);
  };

  const deleteSelectedSpecies = () => {
    if (!selectedSpecies) {
      return;
    }

    const speciesName = selectedSpecies.name || "this species";
    const confirmMessage = `Deleting "${speciesName}" will also remove all reactions that reference it. Continue?`;

    if (typeof window !== "undefined" && !window.confirm(confirmMessage)) {
      return;
    }

    let shouldClearSelectedReaction = false;
    setReactionData((prevReactions) => {
      const filteredReactions = prevReactions.filter((reaction) => {
        const reactants = reaction.reactants || [];
        const products = reaction.products || [];
        const usesSpecies =
          reactants.some((name) => name === speciesName) ||
          products.some((name) => name === speciesName);
        return !usesSpecies;
      });

      if (
        selectedReaction &&
        !filteredReactions.some((reaction) => reaction.id === selectedReaction.id)
      ) {
        shouldClearSelectedReaction = true;
      }

      return filteredReactions;
    });

    if (shouldClearSelectedReaction) {
      setSelectedReaction(null);
    }

    setSpeciesData((prevSpecies) => {
      const filteredSpecies = prevSpecies.filter(
        (species) => species.id !== selectedSpecies.id
      );

      const idMapping = buildSpeciesIdMapping(filteredSpecies);

      setTableData((prevDroplets) => {
        const updatedDroplets = remapDropletConcentrations(prevDroplets, idMapping);

        if (selectedRow) {
          const updatedSelectedDroplet = updatedDroplets.find(
            (row) => row.id === selectedRow.id
          );
          if (updatedSelectedDroplet) {
            setSelectedRow(updatedSelectedDroplet);
            setConcentrations(updatedSelectedDroplet.concentration || []);
          }
        }

        return updatedDroplets;
      });

      return reindexSpeciesList(filteredSpecies);
    });

    setSelectedSpecies(null);
  };

  const handleReactionSelect = (reaction) => {
    console.log(reaction)
    setSelectedReaction(reaction);
    setReactants(reaction.reactants || []);
    setProducts(reaction.products || []);
    setNumReactants((reaction.reactants || []).length);
    setNumProducts((reaction.products || []).length);
    setRates(reaction.rate || []);
  };

  const handleNumReactantsChange = (event) => {
    const value = parseInt(event.target.value, 10);
    setNumReactants(value);
    if (value > reactants.length) {
      setReactants([...reactants, ...Array(value - reactants.length).fill("")]);
    } else {
      setReactants(reactants.slice(0, value));
    }
  };

  const handleNumProductsChange = (event) => {
    const value = parseInt(event.target.value, 10);
    setNumProducts(value);
    if (value > products.length) {
      setProducts([...products, ...Array(value - products.length).fill("")]);
    } else {
      setProducts(products.slice(0, value));
    }
  };

  const handleReactantChange = (index, value) => {
    const updatedReactants = [...reactants];
    updatedReactants[index] = value;
    setReactants(updatedReactants);
  };

  const enforceSingleStartingDroplet = (data) => {
    const firstStartingIndex = data.findIndex((row) => row.isStartingDroplet);
    if (firstStartingIndex === -1) {
      return data;
    }
    return data.map((row, index) => ({
      ...row,
      isStartingDroplet: index === firstStartingIndex,
    }));
  };

  const handleStartingDropletToggle = (dropletId, isChecked) => {
    setTableData((prevRows) => {
      const updatedRows = prevRows.map((row) => ({
        ...row,
        isStartingDroplet: row.id === dropletId ? isChecked : false,
      }));
      return enforceSingleStartingDroplet(updatedRows);
    });
    if (selectedRow && selectedRow.id === dropletId) {
      setSelectedRow({ ...selectedRow, isStartingDroplet: isChecked });
    }
  };

  const getFinalConcentrations = () => {
    if (!resultTables || resultTables.length === 0) {
      return null;
    }
    const finalTable = resultTables[resultTables.length - 1];
    if (!finalTable || finalTable.length === 0) {
      return null;
    }
    const finalSnapshot = finalTable[finalTable.length - 1];
    if (!finalSnapshot) {
      return null;
    }
    const concentrationsBySpecies = [];
    speciesData.forEach((species) => {
      const value = parseFloat(finalSnapshot?.[species.name]);
      concentrationsBySpecies[species.id] =
        Number.isFinite(value) && !Number.isNaN(value) ? value : 0;
    });
    return concentrationsBySpecies;
  };

  const handleRecycleDroplet = () => {
    const startingDroplet = tableData.find((row) => row.isStartingDroplet);
    if (!startingDroplet) {
      alert("Select a starting droplet before recycling.");
      return;
    }

    const finalConcentrations = getFinalConcentrations();
    if (!finalConcentrations) {
      alert("No simulation data available to recycle.");
      return;
    }

    setTableData((prevRows) =>
      prevRows.map((row) =>
        row.id === startingDroplet.id
          ? { ...row, concentration: [...finalConcentrations] }
          : row
      )
    );

    if (selectedRow && selectedRow.id === startingDroplet.id) {
      setConcentrations([...finalConcentrations]);
      setSelectedRow({
        ...startingDroplet,
        concentration: [...finalConcentrations],
      });
    }

    alert("Final droplet concentrations recycled to the starting droplet.");
  };

  const handleConcentrationChange = (index, value) => {
    const updatedConcentrations = normalizeConcentrations(
      concentrations,
      speciesData
    );
    const parsedValue =
      value === "" || value === null || value === undefined
        ? 0
        : Number(value);
    updatedConcentrations[index] = parsedValue;
    setConcentrations(updatedConcentrations);
    handleRowInputChange("concentration", updatedConcentrations);
  };

  const handleProductChange = (index, value) => {
    const updatedProducts = [...products];
    updatedProducts[index] = value;
    setProducts(updatedProducts);
  };

  const handleRateChange = (value) => {
    console.log(value)
    setRates(parseFloat(value));
  };

  const addNewReaction = () => {
    const newReaction = {
      id: reactionData.length + 1,
      reaction: `Reaction ${reactionData.length + 1}`,
      reactants: [],
      products: [],
      rates: 2.8e-3, 
    };
    setReactionData([...reactionData, newReaction]);
    setSelectedReaction(newReaction);
    setReactionPopupOpen(true);
  };

  const deleteSelectedReaction = () => {
    if (selectedReaction) {
      setReactionData(
        reactionData.filter((reaction) => reaction.id !== selectedReaction.id)
      );
      setSelectedReaction(null);
    }
  };

  const handleReactionPopupClose = () => {
    setReactionPopupOpen(false);
    if (selectedReaction) {
      const updatedReaction = {
        ...selectedReaction,
        reactants,
        products,
        rates,
      };
      setReactionData(
        reactionData.map((reaction) =>
          reaction.id === selectedReaction.id ? updatedReaction : reaction
        )
      );
    }
  };

  const sections = [
    {
      title: selectedEdge ? "Edge Properties" : "Node Properties",
      inputs,
      expandWhenSelect: true,
    },
    {
      title: "Droplet Properties",
      tableData,
      selectedRow,
      onRowSelect: handleRowSelect,
      onRowInputChange: handleRowInputChange,
      nodes,
      actions: (
        <div
          style={{
            display: "flex",
            justifyContent: "space-between",
            marginTop: 16,
          }}
        >
          <IconButton onClick={addNewRow}>
            <AddIcon />
          </IconButton>
          <IconButton onClick={deleteSelectedRow} disabled={!selectedRow}>
            <DeleteIcon />
          </IconButton>
        </div>
      ),
    },
    {
      title: "Species",
      tableData: speciesData,
      selectedRow: selectedSpecies,
      onRowSelect: handleSpeciesSelect,
      onRowInputChange: handleSpeciesInputChange,
      nodes,
      speciesData,
      setSpeciesData,

      actions: (
        <div
          style={{
            display: "flex",
            justifyContent: "space-between",
            marginTop: 16,
          }}
        >
          <IconButton onClick={addNewSpecies}>
            <AddIcon />
          </IconButton>
          <IconButton
            onClick={deleteSelectedSpecies}
            disabled={!selectedSpecies}
          >
            <DeleteIcon />
          </IconButton>
        </div>
      ),
    },
    {
      title: "Reactions",
      tableData: reactionData,
      selectedRow: selectedReaction,
      onRowSelect: handleReactionSelect,
      onRowInputChange: () => {}, // Not used for reactions
      nodes,
      speciesData,
      actions: (
        <div
          style={{
            display: "flex",
            justifyContent: "space-between",
            marginTop: 16,
          }}
        >
          <IconButton onClick={addNewReaction}>
            <AddIcon />
          </IconButton>
          <IconButton
            onClick={deleteSelectedReaction}
            disabled={!selectedReaction}
          >
            <DeleteIcon />
          </IconButton>
        </div>
      ),
    },
  ];

  const importInputRef = React.useRef();
  const importReactionsInputRef = React.useRef();

  return (
    <Drawer variant="permanent" anchor="left">
      <Box display="flex" flexDirection="column" height="100%">
        <List style={{ width: "450px", padding: "16px", flexGrow: 1 }}>
          <SimulationSettings
            simulation={simulation}
            setSimulation={setSimulation}
            forcing={forcing}
            setForcing={setForcing}
            timing={timing}
            setTiming={setTiming}
          />
          {sections.map((section, index) => (
            <MenuSection
              key={index}
              title={section.title}
              inputs={section.inputs}
              expandWhenSelect={section.expandWhenSelect}
              selectedEdge={selectedEdge}
              selectedNode={selectedNode}
              tableData={section.tableData}
              selectedRow={section.selectedRow}
              onRowSelect={section.onRowSelect}
              onRowInputChange={section.onRowInputChange}
              nodes={section.nodes}
              speciesData={section.speciesData}
              setSpeciesData={section.setSpeciesData}
              onInputChange={handleInputChange}
              actions={section.actions}
              handleEditReaction={() => setReactionPopupOpen(true)}
              handleOpenSpeciesConcentrationDialog={() => setDialogOpen(true)}
              onToggleStartingDroplet={handleStartingDropletToggle}
            />
          ))}
        </List>
        <Box display="flex" flexDirection="column" p={2} gap={1.5}>
        <Box display="flex" justifyContent="center" gap={1}>
          <Button
            variant="contained"
            startIcon={<SaveIcon />}
            onClick={handleSaveData}
          >
            Save
          </Button>
          <input
            type="file"
            style={{ display: "none" }}
            ref={importInputRef}
            onChange={handleImportData}
          />
          <Button
            variant="contained"
            startIcon={<UploadIcon />}
            onClick={() => importInputRef.current.click()}
          >
            Import
          </Button>
          <input
            type="file"
            style={{ display: "none" }}
            ref={importReactionsInputRef}
            onChange={handleImportReactions}
          />
          <Button
            variant="contained"
            startIcon={<UploadIcon />}
            onClick={() => importReactionsInputRef.current.click()}
          >
            Import Reactions
          </Button>
        </Box>
          <Box display="flex" justifyContent="center">
            <Button
              variant="contained"
              startIcon={<ScienceIcon />}
              onClick={handleFormat}
              style={{ marginRight: "8px" }}
            >
              Simulate
            </Button>
            <Button
              variant="contained"
              color="secondary"
              startIcon={<AutorenewIcon />}
              onClick={handleRecycleDroplet}
              disabled={!resultTables || resultTables.length === 0}
            >
              Recycle
            </Button>
          </Box>
        </Box>
      </Box>
      <ReactionPopup
        open={reactionPopupOpen}
        onClose={handleReactionPopupClose}
        reactants={reactants}
        products={products}
        numReactants={numReactants}
        numProducts={numProducts}
        onReactantChange={handleReactantChange}
        onProductChange={handleProductChange}
        onRateChange={handleRateChange}
        rates={rates}
        onNumReactantsChange={handleNumReactantsChange}
        onNumProductsChange={handleNumProductsChange}
        speciesOptions={speciesData}
      />
      <SpeciesConcentrationDialog
        open={dialogOpen}
        onClose={handleDialogClose}
        concentrations={concentrations}
        selectedDropletId={selectedRow?.id}
        handleConcentrationChange={handleConcentrationChange}
        speciesOptions={speciesData}
      />
    </Drawer>
  );
};

export default LeftSidebar;
