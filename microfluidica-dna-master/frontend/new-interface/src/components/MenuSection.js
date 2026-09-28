import React, { useEffect, useState } from "react";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
  Button,
  Checkbox,
  FormControl,
  FormControlLabel,
  InputAdornment,
  InputLabel,
  MenuItem,
  Select,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from "@mui/material";

const MenuSection = ({
  title,
  inputs,
  onInputChange,
  tableData,
  selectedRow,
  onRowSelect,
  onRowInputChange,
  nodes,
  speciesData,
  setSpeciesData,
  expandWhenSelect,
  selectedEdge,
  selectedNode,
  actions,
  handleEditReaction,
  handleOpenSpeciesConcentrationDialog, // Add this prop
}) => {
  const [expanded, setExpanded] = useState(false);

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

  useEffect(() => {
    if ((selectedEdge || selectedNode) && expandWhenSelect) {
      setExpanded(true);
    }
  }, [selectedNode, selectedEdge, expandWhenSelect]);

  const handleChange = (event, isExpanded) => {
    setExpanded(isExpanded);
  };

  return (
    <Accordion expanded={expanded} onChange={handleChange}>
      <AccordionSummary expandIcon={<ExpandMoreIcon />}>
        <Typography>{title}</Typography>
      </AccordionSummary>
      <AccordionDetails>
        {inputs &&
          inputs.map((input, index) => {
            if (input.type === "checkbox") {
              return (
                <FormControlLabel
                  key={index}
                  control={
                    <Checkbox
                      checked={input.value}
                      onChange={(e) => onInputChange(index, e.target.checked)}
                    />
                  }
                  label="Is Sink"
                />
              );
            }
            return (
              <TextField
                key={index}
                label={input.label}
                disabled={input.readOnly}
                value={input.value || ""}
                fullWidth
                margin="normal"
                onChange={(e) => onInputChange(index, e.target.value)}
                InputProps={{
                  endAdornment:
                    input.label === "Height" ? (
                      <Typography variant="body2">100µm</Typography>
                    ) : input.label === "X position" ||
                      input.label === "Y position" ? (
                      <Typography variant="body2">µm</Typography>
                    ) : null,
                }}
              />
            );
          })}

        {title === "Droplet Properties" && (
          <>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell>ID</TableCell>
                  <TableCell>Name</TableCell>
                  <TableCell>Volume (m³)</TableCell>
                  <TableCell>Pump</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {tableData.map((row) => (
                  <TableRow
                    key={row.id}
                    hover
                    selected={selectedRow && selectedRow.id === row.id}
                    onClick={() => onRowSelect(row)}
                  >
                    <TableCell>{row.id}</TableCell>
                    <TableCell>{row.name}</TableCell>
                    <TableCell>{row.volume}</TableCell>
                    <TableCell>{row.pump}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
            {selectedRow && (
              <>
                <TextField
                  label="Name"
                  value={selectedRow.name}
                  onChange={(e) => onRowInputChange("name", e.target.value)}
                  fullWidth
                  margin="normal"
                />
                <TextField
                  label="Volume"
                  value={selectedRow.volume}
                  onChange={(e) => onRowInputChange("volume", e.target.value)}
                  fullWidth
                  margin="normal"
                  InputProps={{
                    endAdornment: (
                      <InputAdornment position="end">m³</InputAdornment>
                    ),
                  }}
                />
                <FormControl fullWidth margin="normal">
                  <InputLabel id="pump-select-label">Pump</InputLabel>
                  <Select
                    labelId="pump-select-label"
                    id="pump-select"
                    label="Pump"
                    value={selectedRow.pump}
                    onChange={(e) => onRowInputChange("pump", e.target.value)}
                    fullWidth
                    margin="normal"
                  >
                    {nodes.map((node, index) => (
                      <MenuItem key={index} value={node.name}>
                        {node.name}
                      </MenuItem>
                    ))}
                  </Select>
                </FormControl>
                <Button
                  variant="contained"
                  color="primary"
                  onClick={handleOpenSpeciesConcentrationDialog} // Add button click handler
                  style={{ marginTop: 16 }}
                >
                  Species Concentration
                </Button>
              </>
            )}

            {actions}
          </>
        )}

        {title === "Species" && (
          <>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell>ID</TableCell>
                  <TableCell>Species Name</TableCell>
                  <TableCell>Show in Results</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {tableData.map((species) => (
                  <TableRow
                    key={species.id}
                    hover
                    selected={selectedRow && selectedRow.id === species.id}
                    onClick={() => onRowSelect(species)}
                  >
                    <TableCell>{species.id}</TableCell>
                    <TableCell>{species.name}</TableCell>
                    <TableCell>
                    <Checkbox
                      checked={species.showInResults}
                      onChange={(e) => {
                        const updatedSpeciesData = speciesData.map((sp) =>
                          sp.id === species.id
                            ? { ...sp, showInResults: e.target.checked }
                            : sp
                        );
                        setSpeciesData(updatedSpeciesData);
                      }}
                    />
                  </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
            {selectedRow && (
              <TextField
                label="Species Name"
                value={selectedRow.name}
                onChange={(e) => onRowInputChange("name", e.target.value)}
                fullWidth
                margin="normal"
              />
            )}
            {actions}
          </>
        )}

        {title === "Reactions" && (
          <>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell style={{ display: "none" }}>ID</TableCell>
                  <TableCell>Reaction</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {tableData.map((row) => (
                  <TableRow
                    key={row.id}
                    hover
                    selected={selectedRow && selectedRow.id === row.id}
                    onClick={() => onRowSelect(row)}
                  >
                    <TableCell style={{ display: "none" }}>{row.id}</TableCell>
                    <TableCell>
                      {formatReaction(row.reactants, row.products)}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
            {selectedRow && (
              <>
                <Box mt={2} />
                <Box
                  display="flex"
                  justifyContent="space-between"
                  alignItems="center"
                  marginBottom={2}
                >
                  <Button
                    variant="contained"
                    color="primary"
                    onClick={handleEditReaction}
                  >
                    Edit Reaction
                  </Button>
                </Box>
              </>
            )}
            {actions}
          </>
        )}
      </AccordionDetails>
    </Accordion>
  );
};

export default MenuSection;
