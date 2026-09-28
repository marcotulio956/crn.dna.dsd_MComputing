import AddIcon from "@mui/icons-material/Add";
import DeleteIcon from "@mui/icons-material/Delete";
import { Box, Button, Drawer, IconButton, List } from "@mui/material";
import SaveIcon from "@mui/icons-material/Save";
import UploadIcon from "@mui/icons-material/Upload";
import ScienceIcon from "@mui/icons-material/Science";
import React, { useEffect, useState } from "react";
import MenuSection from "./MenuSection";
import ReactionPopup from "./ReactionPopup";
import SpeciesConcentrationDialog from "./SpeciesConcentrationDialog";

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
        setTableData(importedData.tableData || []);
        // setSpeciesData(importedData.speciesData || []);
        setSpeciesData(adjustedSpeciesData);
        setReactionData(importedData.reactionData || []);
        setConcentrations(importedData.concentrations || []);
        setReactants(importedData.reactants || []);
        setProducts(importedData.products || []);
        setNumReactants(importedData.numReactants || []);
        setNumProducts(importedData.numProducts || []);
        setNodes(importedData.nodes || []);
        setEdges(importedData.edges || []);
        setProducts(importedData.products || []);
      };
      reader.readAsText(file);
    }
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
    };

    console.log(data)

    setLoading(true);
    fetch("http://localhost:8001/submit_MMFT_New", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(data),
    })
      .then((response) => response.json())
      .then((data) => {
        //setPlotImage(`data:image/png;base64,${data.plot_image}`);
        setResultImages(
          data.pdf_images.map(
            (imageBase64) => `data:image/png;base64,${imageBase64}`
          )
        );
        setResultTables(data.tabelas_DNA); // ONDE TA OS DADOS
        setTabIndex(1);
        //setValue(1)
        setAnimationData(transformData(data.simulation_result));
        setLoading(false);
      })
      .catch((error) => console.error("Error:", error));
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
      setConcentrations(selectedRow.concentration || []);
    }
  }, [selectedRow]);

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
      const updatedRow = { ...selectedRow, [field]: value };
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
      volume: 0,
      pump: "",
      concentration: [],
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
    if (selectedSpecies) {
      setSpeciesData(
        speciesData.filter((species) => species.id !== selectedSpecies.id)
      );
      setSelectedSpecies(null);
    }
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

  const handleConcentrationChange = (index, value) => {
    const updatedConcentrations = [...concentrations];
    updatedConcentrations[index] = value;
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

  return (
    <Drawer variant="permanent" anchor="left">
      <Box display="flex" flexDirection="column" height="100%">
        <List style={{ width: "450px", padding: "16px", flexGrow: 1 }}>
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
            />
          ))}
        </List>
        <Box display="flex" justifyContent="flex-end" p={2}>
          <Button
            variant="contained"
            startIcon={<SaveIcon />}
            onClick={handleSaveData}
            style={{ marginRight: "8px" }}
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
            style={{ marginRight: "8px" }}
          >
            Import
          </Button>
          <Button
            variant="contained"
            startIcon={<ScienceIcon />}
            onClick={handleFormat}
          >
            Simulate
          </Button>
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
