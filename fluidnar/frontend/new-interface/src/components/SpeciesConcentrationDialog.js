import React from "react";
import {
  Dialog,
  DialogContent,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
  Box,
} from "@mui/material";

const SpeciesConcentrationDialog = ({
  open,
  onClose,
  concentrations,
  handleConcentrationChange,
  speciesOptions,
}) => {

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="md">
      <DialogContent>
        <Typography variant="h6" gutterBottom>
          Species Concentrations
        </Typography>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Species Name</TableCell>
              <TableCell>Concentration (nmol/L)</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {speciesOptions.map((species) => (
              <TableRow key={species.id}>
                <TableCell>{species.name}</TableCell>
                <TableCell>
                  <TextField
                    type="number"
                    value={
                      concentrations?.[species.id] !== undefined
                        ? concentrations[species.id]
                        : 0
                    }
                    onChange={(e) =>
                      handleConcentrationChange(species.id, e.target.value)
                    }
                    fullWidth
                    margin="dense"
                    InputProps={{
                      endAdornment: (
                        <Typography variant="body2">nmol/L</Typography>
                      ),
                    }}
                  />
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
        <Box mt={2}>
          <Typography variant="caption">
            Update the concentrations for each species in the selected droplet.
          </Typography>
        </Box>
      </DialogContent>
    </Dialog>
  );
};

export default SpeciesConcentrationDialog;
