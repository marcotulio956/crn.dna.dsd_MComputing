import './Simulador2.css';
import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import Popover from '@mui/material/Popover';
import Typography from '@mui/material/Typography';
import MenuItem from '@mui/material/MenuItem';
import Select from '@mui/material/Select';
import Stepper from '@mui/material/Stepper';
import Step from '@mui/material/Step';
import StepLabel from '@mui/material/StepLabel';
import Button from '@mui/material/Button';
import StepIcon from '@mui/material/StepIcon';
import { styled } from '@mui/material/styles';

const steps = ['Species', 'Reactions', 'Initial Droplet', 'Mixtures', 'Results'];
const stepDescriptions = [
  'In this step, you must enter the inputs, outputs, and products required to perform the proposed reaction.',
  'In this step, you must enter the chemical reactions to be simulated, including the inputs to be reacted, the output to be generated, and the reaction rate.',
  'In this step, you must enter additional details and parameters required for the simulation.',
  'In this step, you must review and finalize the details for the simulation.',
  'In this step, you can view the results of the simulation and analyze the outcomes.'
];

const CustomStepIconRoot = styled('div')(({ theme, ownerState }) => ({
  color: ownerState.active || ownerState.completed ? '#9ec39e' : '#e0e0e0',
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  '& .MuiStepIcon-text': {
    fill: '#fff',
  },
}));

function CustomStepIcon(props) {
  const { active, completed, className } = props;
  return (
    <CustomStepIconRoot ownerState={{ active, completed }} className={className}>
      <StepIcon {...props} />
    </CustomStepIconRoot>
  );
}

const CustomButton = styled(Button)({
  color: 'black',
});

export default function Simulador2() {
  const [inputGroups, setInputGroups] = useState([]);
  const [anchorElStep, setAnchorElStep] = useState(null);
  const [anchorElInput, setAnchorElInput] = useState(null);
  const [numProducts, setNumProducts] = useState(1);
  const [numReactants, setNumReactants] = useState(1);
  const [selectedInputs, setSelectedInputs] = useState([]);
  const [simulador1Inputs, setSimulador1Inputs] = useState([]);
  const [reactionRate, setReactionRate] = useState('');
  const [activeStep, setActiveStep] = useState(1);
  const navigate = useNavigate();
  const [anchorEls, setAnchorEls] = useState([null, null, null, null, null]);

  useEffect(() => {
    const storedInputs = JSON.parse(localStorage.getItem('simulador1Data')) || [];
    setSimulador1Inputs(storedInputs.inputs || []);
  }, []);

  const handleAddInputClick = (event) => {
    setAnchorElInput(event.currentTarget);
  };

  const handlePopoverClose = () => {
    setAnchorElStep(null);
    setAnchorElInput(null);
  };

  const handleClick = (index) => (event) => {
    const newAnchorEls = [...anchorEls];
    newAnchorEls[index] = event.currentTarget;
    setAnchorEls(newAnchorEls);
  };

  const handleClose = (index) => () => {
    const newAnchorEls = [...anchorEls];
    newAnchorEls[index] = null;
    setAnchorEls(newAnchorEls);
  };

  const handleNumProductsChange = (event) => {
    const value = parseInt(event.target.value, 10);
    setNumProducts(value);
    setSelectedInputs(Array(value + numReactants).fill(''));
  };

  const handleNumReactantsChange = (event) => {
    const value = parseInt(event.target.value, 10);
    setNumReactants(value);
    setSelectedInputs(Array(value + numProducts).fill(''));
  };

  const handleSelectChange = (index, event) => {
    const newSelectedInputs = [...selectedInputs];
    newSelectedInputs[index] = event.target.value;
    setSelectedInputs(newSelectedInputs);
  };

  const handleReactionRateChange = (event) => {
    setReactionRate(event.target.value);
  };

  const handleAddSelectedInputs = () => {
    let newInputGroup = selectedInputs.filter(input => input !== '');
    newInputGroup.numProducts = numProducts
    newInputGroup.numReactants = numReactants
    if (newInputGroup.length > 0) {
      setInputGroups([...inputGroups, [...newInputGroup, reactionRate]]);
      setSelectedInputs(Array(numProducts + numReactants).fill(''));
      setReactionRate('');
    }
    setAnchorElInput(null);
  };

  const storeSimulador2Data = (data) => {
    localStorage.setItem('simulador2Data', JSON.stringify(data));
  };

  const handleSubmit = () => {
    const data = {
      inputGroups,
      numReactants,
      numProducts
    };
    storeSimulador2Data(data);
    navigate('/simulador3');
  };

  const openStepPopover = Boolean(anchorElStep);
  const openInputPopover = Boolean(anchorElInput);
  const stepPopoverId = openStepPopover ? 'step-popover' : undefined;
  const inputPopoverId = openInputPopover ? 'input-popover' : undefined;
  const open = (index) => Boolean(anchorEls[index]);
  const id = (index) => (open(index) ? `simple-popover-${index}` : undefined);

  return (
    <div className="simulador">
      <Stepper activeStep={activeStep} alternativeLabel>
        {steps.map((label, index) => (
          <Step key={label}>
            <StepLabel StepIconComponent={CustomStepIcon}>
              <CustomButton onClick={handleClick(index)}>
                {label}
                <i className="fas fa-info-circle"></i>
              </CustomButton>
              <Popover
                id={id(index)}
                open={open(index)}
                anchorEl={anchorEls[index]}
                onClose={handleClose(index)}
                anchorOrigin={{
                  vertical: 'center',
                  horizontal: 'center',
                }}
                transformOrigin={{
                  vertical: 'center',
                  horizontal: 'center',
                }}
              >
                <Typography sx={{ p: 2 }}>
                  {stepDescriptions[index]}
                </Typography>
              </Popover>
            </StepLabel>
          </Step>
        ))}
      </Stepper>
      <div className="header-container">
        {/* <div className="reactions">
          <h3>Reactions</h3>
        </div> */}
      </div>
      <div className="input-container">
        <div className="input-list">
          {inputGroups.length > 0 && inputGroups.map((inputGroup, groupIndex) => (
            <div key={groupIndex} className="input-group">
              {inputGroup.slice(0, numReactants).map((input, index) => (
                <input
                  key={index}
                  type="text"
                  value={input}
                  readOnly
                />
              ))}
              <i className="fas fa-arrow-right arrow-icon"></i>
              {inputGroup.slice(numReactants, numReactants + numProducts).map((input, index) => (
                <input
                  key={index + numReactants}
                  type="text"
                  value={input}
                  readOnly
                />
              ))}
              <input
                type="text"
                value={inputGroup[inputGroup.length - 1]}
                readOnly
                placeholder="Velocity"
              />
            </div>
          ))}
          <button className="add-input-button" onClick={handleAddInputClick}>
            Add Input
          </button>
        </div>
        <div className="footer">
          <button className="footer-button" onClick={handleSubmit}>
            Step 3 <i className="fas fa-arrow-right"></i>
          </button>
        </div>
      </div>
      <Popover
        id={inputPopoverId}
        open={openInputPopover}
        anchorEl={anchorElInput}
        onClose={handlePopoverClose}
        anchorOrigin={{
          vertical: 'center',
          horizontal: 'center',
        }}
        transformOrigin={{
          vertical: 'center',
          horizontal: 'center',
        }}
      >
        <div className="popover-content">
          <div className="num-selects-inputs">
            <label>
              Number of Reactants:
              <input
                type="number"
                min="1"
                value={numReactants}
                onChange={handleNumReactantsChange}
              />
            </label>
            <br />
            <label>
              Number of Products:
              <input
                type="number"
                min="1"
                value={numProducts}
                onChange={handleNumProductsChange}
              />
            </label>
          </div>
          <Typography sx={{ p: 2 }}>
            Select the entrances:
          </Typography>
          <div className="select-and-rate-group">
            <div className="select-group">
              {Array.from({ length: numReactants }).map((_, index) => (
                <Select
                  key={index}
                  value={selectedInputs[index] || ''}
                  onChange={(event) => handleSelectChange(index, event)}
                  displayEmpty
                  inputProps={{ 'aria-label': 'Without label' }}
                >
                  {simulador1Inputs.map((input, inputIndex) => (
                    <MenuItem key={inputIndex} value={input}>
                      {input || `Input ${inputIndex + 1}`}
                    </MenuItem>
                  ))}
                </Select>
              ))}
            </div>
            <i className="fas fa-arrow-right arrow-icon"></i>
            <div className="select-group">
              {Array.from({ length: numProducts }).map((_, index) => (
                <Select
                  key={index + numReactants}
                  value={selectedInputs[index + numReactants] || ''}
                  onChange={(event) => handleSelectChange(index + numReactants, event)}
                  displayEmpty
                  inputProps={{ 'aria-label': 'Without label' }}
                >
                  {simulador1Inputs.map((input, inputIndex) => (
                    <MenuItem key={inputIndex} value={input}>
                      {input || `Input ${inputIndex + 1}`}
                    </MenuItem>
                  ))}
                </Select>
              ))}
            </div>
            <div className="reaction-rate-input">
              <label>Insert the velocity:</label>
              <input
                type="text"
                value={reactionRate}
                onChange={handleReactionRateChange}
                placeholder="Velocity"
              />
            </div>
          </div>
          <button className="pop-button" onClick={handleAddSelectedInputs}>
            Add
          </button>
        </div>
      </Popover>
    </div>
  );
}
