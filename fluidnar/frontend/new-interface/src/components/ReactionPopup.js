import AddIcon from '@mui/icons-material/Add';
import ArrowForwardIcon from '@mui/icons-material/ArrowForward';
import {
    Box,
    Dialog,
    DialogContent,
    FormControl,
    MenuItem,
    Select,
    TextField,
    Typography
} from "@mui/material";
import React from "react";

const ReactionPopup = ({
  open,
  onClose,
  reactants,
  products,
  rates,
  numReactants,
  numProducts,
  onReactantChange,
  onProductChange,
  onRateChange,
  onNumReactantsChange,
  onNumProductsChange,
  speciesOptions
}) => {

  const hasReactants = reactants.length > 0;
  const hasProducts = products.length > 0;

  return (
    <Dialog 
      open={open} 
      onClose={onClose}
      fullWidth
      maxWidth="md"
    >
      <DialogContent>
        <Box 
          display="flex" 
          flexDirection="column" 
          alignItems="center"
          mb={2}
        >
          <Box display="flex" mb={2} gap={2} flexWrap="wrap" justifyContent="center">
            <TextField
              label="Number of Reactants"
              type="number"
              value={numReactants}
              onChange={onNumReactantsChange}
              style={{ width: '160px' }}
              margin="normal"
            />
            <TextField
              label="Number of Products"
              type="number"
              value={numProducts}
              onChange={onNumProductsChange}
              style={{ width: '160px' }}
              margin="normal"
            />
          </Box>
          {hasReactants && hasProducts && (
            <>
              <Typography variant="h6" gutterBottom align="center">Reaction Details</Typography>
              <Box display="flex" flexDirection="row" alignItems="center" mb={2} flexWrap="wrap" justifyContent="center">
                <Box display="flex" flexDirection="row" alignItems="center" mr={2} flexWrap="wrap" justifyContent="center">
                  {reactants.map((reactant, index) => (
                    <Box display="flex" alignItems="center" key={index} mb={1}>
                      <FormControl style={{ minWidth: '120px'}}>
                        <Select
                          value={reactant}
                          onChange={(e) => onReactantChange(index, e.target.value)}
                        >
                          {speciesOptions.map((species) => (
                            <MenuItem key={species.id} value={species.name}>
                              {species.name}
                            </MenuItem>
                          ))}
                        </Select>
                      </FormControl>
                      {index < reactants.length - 1 && <AddIcon style={{ margin: '0 8px' }}/>}
                    </Box>
                  ))}
                </Box>
                <ArrowForwardIcon style={{ margin: '0 8px' }} />
                <Box display="flex" flexDirection="row" alignItems="center" mr={3} ml={2} flexWrap="wrap" justifyContent="center">
                  {products.map((product, index) => (
                    <Box display="flex" alignItems="center" key={index} mb={1}>
                      <FormControl style={{ minWidth: '120px', marginRight: '8px' }}>
                        <Select
                          value={product}
                          onChange={(e) => onProductChange(index, e.target.value)}
                        >
                          {speciesOptions.map((species) => (
                            <MenuItem key={species.id} value={species.name}>
                              {species.name}
                            </MenuItem>
                          ))}
                        </Select>
                      </FormControl>
                      {index < products.length - 1 && <AddIcon />}
                    </Box>
                  ))}
                </Box>

                <TextField
                  label="Reaction Rate"
                  type="number"
                  value={rates}
                  onChange={(e) => onRateChange(e.target.value)}
                  style={{ width: '160px', marginBottom: '10'  }}
                  margin="normal"
                />
              </Box>
            </>
          )}
        </Box>
      </DialogContent>
    </Dialog>
  );
};

export default ReactionPopup;
