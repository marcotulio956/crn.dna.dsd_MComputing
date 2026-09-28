import './Simulador3.css';
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
import WaterDropIcon from '@mui/icons-material/WaterDrop';

const steps = ['Step 1', 'Step 2', 'Step 3', 'Step 4', 'Results'];
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

const getRandomColor = () => {
  const letters = '0123456789ABCDEF';
  let color = '#';
  for (let i = 0; i < 6; i++) {
    color += letters[Math.floor(Math.random() * 16)];
  }
  return color;
};

export default function Simulador3() {
  const [inputGroups, setInputGroups] = useState([]);
  const [simulador1Inputs, setSimulador1Inputs] = useState([]);
  const navigate = useNavigate();
  const [anchorEl, setAnchorEl] = useState(null);
  const [activeStep, setActiveStep] = useState(2);
  const [anchorEls, setAnchorEls] = useState([null, null, null, null, null]);

  useEffect(() => {
    const storedInputs = JSON.parse(localStorage.getItem('simulador1Data')) || [];
    setSimulador1Inputs(storedInputs.inputs || []);
  }, []);

  const handleAddInputGroup = () => {
    setInputGroups([...inputGroups, { color: getRandomColor(), text: '', extraInputs: [] }]);
  };

  const handleTextChange = (index, event) => {
    const newInputGroups = [...inputGroups];
    newInputGroups[index].text = event.target.value;
    setInputGroups(newInputGroups);
  };

  const handleAddExtraInput = (index) => {
    const newInputGroups = [...inputGroups];
    newInputGroups[index].extraInputs.push({ selectValue: '', textValue: '' });
    setInputGroups(newInputGroups);
  };

  const handleSelectChange = (groupIndex, extraIndex, event) => {
    const newInputGroups = [...inputGroups];
    newInputGroups[groupIndex].extraInputs[extraIndex].selectValue = event.target.value;
    setInputGroups(newInputGroups);
  };

  const handleExtraTextChange = (groupIndex, extraIndex, event) => {
    const newInputGroups = [...inputGroups];
    newInputGroups[groupIndex].extraInputs[extraIndex].textValue = event.target.value;
    setInputGroups(newInputGroups);
  };

  const storeSimulador3Data = (data) => {
    localStorage.setItem('simulador3Data', JSON.stringify(data));
  };
  
  // Function to handle form submission
  const handleSubmit = () => {
    const data = {
      inputGroups,
    };
    storeSimulador3Data(data);
    navigate('/simulador4');
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
        <div className="droplet">
          <h3>Initial Droplet</h3>
        </div>
      </div>
      <div className="input-container-three">
        <div className="input-list-three">
          {inputGroups.map((group, groupIndex) => (
            <div key={groupIndex} className="input-group-three">
              <WaterDropIcon style={{ color: group.color, fontSize: 100 }} />
              <div className="text-and-button">
                <input
                  type="text"
                  value={group.text}
                  onChange={(event) => handleTextChange(groupIndex, event)}
                  placeholder="Title"
                  className="main-input"
                />

                {group.extraInputs.map((extraInput, extraIndex) => (
                  <div key={extraIndex} className="extra-input-group">
                    <div className="extra-input-title">Species
                    <div className="select-container">
                    <select
                      value={extraInput.selectValue}
                      onChange={(event) => handleSelectChange(groupIndex, extraIndex, event)}
                      className="extra-select"
                    >
                      <option value="">Select</option>
                      {simulador1Inputs.map((input, inputIndex) => (
                        <option key={inputIndex} value={input}>
                          {input}
                        </option>
                      ))}
                    </select>
                    </div>
                    </div>
                    <div className="extra-input-title">Concentration
                    <div className="select-container">
                    <input
                      type="text"
                      value={extraInput.textValue}
                      onChange={(event) => handleExtraTextChange(groupIndex, extraIndex, event)}
                      placeholder="Enter number"
                      className="extra-text"
                    />
                    </div>
                    </div>
                  </div>
                ))}
                <button className="extra-input-button" onClick={() => handleAddExtraInput(groupIndex)}>
                  Add Extra Input
                </button>
              </div>
            </div>
          ))}
        </div>
        <div className="add-input-container">
          <button className="add-input-button" onClick={handleAddInputGroup}>
            Add Input
          </button>
        </div>
        <div className="footer">
          <button className="footer-button" onClick={handleSubmit}>
            Step 4 <i className="fas fa-arrow-right"></i>
          </button>
        </div>
      </div>
    </div>
  );
}
