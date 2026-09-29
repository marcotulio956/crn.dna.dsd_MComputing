import React from "react";
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  Switch,
  TextField,
  Typography,
} from "@mui/material";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";

const SimulationSettings = ({
  simulation,
  setSimulation,
  forcing,
  setForcing,
  timing,
  setTiming,
}) => {
  const updateSimulation = (field, value) =>
    setSimulation((current) => ({ ...current, [field]: value }));
  const updateForcing = (field, value) =>
    setForcing((current) => ({ ...current, [field]: value }));
  const updateForcingParam = (field, value) =>
    setForcing((current) => ({
      ...current,
      params: { ...current.params, [field]: value },
    }));
  const updateTiming = (field, value) =>
    setTiming((current) => ({ ...current, [field]: value }));

  return (
    <Accordion>
      <AccordionSummary expandIcon={<ExpandMoreIcon />}>
        <Typography>Simulation Settings</Typography>
      </AccordionSummary>
      <AccordionDetails>
        <FormControl fullWidth margin="normal">
          <InputLabel id="simulation-engine-label">Engine</InputLabel>
          <Select
            labelId="simulation-engine-label"
            label="Engine"
            value={simulation.engine}
            onChange={(event) => updateSimulation("engine", event.target.value)}
            disabled={simulation.stochastic}
          >
            <MenuItem value="desolve">deSolve</MenuItem>
            <MenuItem value="diffeqr">diffeqR</MenuItem>
          </Select>
        </FormControl>
        <label>
          Stochastic simulation
          <Switch
            checked={simulation.stochastic}
            onChange={(event) => updateSimulation("stochastic", event.target.checked)}
          />
        </label>
        <label>
          DNA 4-domain simulation
          <Switch
            checked={simulation.dna}
            onChange={(event) => updateSimulation("dna", event.target.checked)}
          />
        </label>
        <TextField
          fullWidth
          margin="normal"
          type="number"
          label="Simulation volume"
          value={simulation.volume}
          onChange={(event) => updateSimulation("volume", event.target.value)}
          inputProps={{ min: 0 }}
        />
        <TextField
          fullWidth
          margin="normal"
          type="number"
          label="Random seed"
          value={simulation.seed ?? ""}
          onChange={(event) => updateSimulation("seed", event.target.value === "" ? null : event.target.value)}
        />

        <FormControl fullWidth margin="normal">
          <InputLabel id="forcing-label">Forced concentration</InputLabel>
          <Select
            labelId="forcing-label"
            label="Forced concentration"
            value={forcing.name || "none"}
            onChange={(event) => updateForcing("name", event.target.value === "none" ? null : event.target.value)}
          >
            <MenuItem value="none">None</MenuItem>
            <MenuItem value="step_input">Step</MenuItem>
            <MenuItem value="pulse_input">Pulse</MenuItem>
            <MenuItem value="sinusoidal_input">Sinusoidal</MenuItem>
            <MenuItem value="saw_wave_input">Saw wave</MenuItem>
            <MenuItem value="square_input">Square wave</MenuItem>
          </Select>
        </FormControl>
        {forcing.name && (
          <TextField
            fullWidth
            margin="normal"
            label="Forced species"
            value={forcing.species || ""}
            onChange={(event) => updateForcing("species", event.target.value)}
          />
        )}
        {forcing.name && (
          <TextField
            fullWidth
            margin="normal"
            type="number"
            label="Forcing amplitude"
            value={forcing.params.amplitude ?? ""}
            onChange={(event) => updateForcingParam("amplitude", event.target.value)}
          />
        )}
        {forcing.name && ["step_input", "pulse_input"].includes(forcing.name) && (
          <TextField
            fullWidth
            margin="normal"
            type="number"
            label="Forcing start time"
            value={forcing.params.time ?? ""}
            onChange={(event) => updateForcingParam("time", event.target.value)}
          />
        )}
        {forcing.name === "pulse_input" && (
          <TextField
            fullWidth
            margin="normal"
            type="number"
            label="Pulse width"
            value={forcing.params.width ?? ""}
            onChange={(event) => updateForcingParam("width", event.target.value)}
          />
        )}
        {forcing.name === "sinusoidal_input" && (
          <>
            <TextField
              fullWidth
              margin="normal"
              type="number"
              label="Frequency"
              value={forcing.params.frequency ?? ""}
              onChange={(event) => updateForcingParam("frequency", event.target.value)}
            />
            <TextField
              fullWidth
              margin="normal"
              type="number"
              label="Offset"
              value={forcing.params.offset ?? ""}
              onChange={(event) => updateForcingParam("offset", event.target.value)}
            />
          </>
        )}
        {forcing.name === "square_input" && (
          <>
            <TextField
              fullWidth
              margin="normal"
              type="number"
              label="Period"
              value={forcing.params.period ?? ""}
              onChange={(event) => updateForcingParam("period", event.target.value)}
            />
            <TextField
              fullWidth
              margin="normal"
              type="number"
              label="Pulse width"
              value={forcing.params.pulse_width ?? ""}
              onChange={(event) => updateForcingParam("pulse_width", event.target.value)}
            />
            <TextField
              fullWidth
              margin="normal"
              type="number"
              label="Delay"
              value={forcing.params.delay ?? ""}
              onChange={(event) => updateForcingParam("delay", event.target.value)}
            />
          </>
        )}

        <label>
          Automatic settling timing
          <Switch
            checked={timing.enabled}
            onChange={(event) => updateTiming("enabled", event.target.checked)}
          />
        </label>
        <TextField
          fullWidth
          margin="normal"
          type="number"
          label="Settling tolerance (%)"
          value={timing.tolerancePercent}
          onChange={(event) => updateTiming("tolerancePercent", event.target.value)}
          inputProps={{ min: 0 }}
        />
        <TextField
          fullWidth
          margin="normal"
          type="number"
          label="Stable samples"
          value={timing.stableSamples}
          onChange={(event) => updateTiming("stableSamples", event.target.value)}
          inputProps={{ min: 1 }}
        />
      </AccordionDetails>
    </Accordion>
  );
};

export default SimulationSettings;
