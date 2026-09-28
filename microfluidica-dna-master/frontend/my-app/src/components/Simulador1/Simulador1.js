import './Simulador1.css';
import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import Popover from '@mui/material/Popover';
import Typography from '@mui/material/Typography';
import Stepper from '@mui/material/Stepper';
import Step from '@mui/material/Step';
import StepLabel from '@mui/material/StepLabel';
import Button from '@mui/material/Button';
import StepIcon from '@mui/material/StepIcon';
import { styled } from '@mui/material/styles';
import { makeStyles } from '@mui/styles';

const steps = ['Species', 'Reactions', 'Initial Droplet', 'Mixtures', 'Results'];
const stepDescriptions = [
  'In this step, you must enter the inputs, outputs, and products required to perform the proposed reaction.',
  'In this step, you must enter the chemical reactions to be simulated, including the inputs to be reacted, the output to be generated, and the reaction rate.',
  'In this step, you must enter additional details and parameters required for the simulation.',
  'In this step, you must review and finalize the details for the simulation.',
  'In this step, you can view the results of the simulation and analyze the outcomes.'
];

const CustomButton = styled(Button)({
  color: 'black',
});

const useStepIconStyles = makeStyles({
  root: {
    color: '#e0e0e0',
    '&.MuiStepIcon-active': {
      color: '#9ec39e',
    },
    '&.MuiStepIcon-completed': {
      color: '#4CAF50',
    },
  },
  text: {
    fill: '#fff',
  },
});

function CustomStepIcon(props) {
  const classes = useStepIconStyles();
  return <StepIcon {...props} classes={{ root: classes.root, text: classes.text }} />;
}

export default function Simulador1() {
  const [inputs, setInputs] = useState(['']);
  const navigate = useNavigate();
  const [activeStep, setActiveStep] = useState(0);
  const [anchorEls, setAnchorEls] = useState([null, null, null, null, null]);

  useEffect(() => {
    // Limpa o localStorage quando a página é carregada
    localStorage.removeItem('simulador1Inputs');
  }, []);

  const handleAddInput = () => {
    const newInputs = [...inputs, ''];
    setInputs(newInputs);
  };

  const handleChange = (index, event) => {
    const newInputs = [...inputs];
    newInputs[index] = event.target.value;
    setInputs(newInputs);
  };

  const storeSimulador1Data = (data) => {
    localStorage.setItem('simulador1Data', JSON.stringify(data));
  };

  const handleSubmit = () => {
    const data = {
      inputs,
    };
    storeSimulador1Data(data);
    navigate('/simulador2');
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
                  vertical: 'top',
                  horizontal: 'right',
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
        {/* <div className="species">
          <h3>Species</h3>
        </div> */}
      </div>
      <div className="input-container">
        <div className="input-list">
          {inputs.map((input, index) => (
            <input
              className="input_but"
              key={index}
              type="text"
              value={input}
              onChange={(event) => handleChange(index, event)}
              placeholder={`Input ${index + 1}`}
            />
          ))}
          <button className="add-input-button" onClick={handleAddInput}>
            +
          </button>
        </div>
        <div className="footer">
          <button className="footer-button" onClick={handleSubmit}>
            Step 2 <i className="fas fa-arrow-right"></i>
          </button>
        </div>
      </div>
    </div>
  );
}
