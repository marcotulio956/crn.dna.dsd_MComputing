import './Simulador5.css';
import React, { useState } from 'react';
import Popover from '@mui/material/Popover';
import Typography from '@mui/material/Typography';
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import Stepper from '@mui/material/Stepper';
import Step from '@mui/material/Step';
import StepLabel from '@mui/material/StepLabel';
import { useNavigate } from 'react-router-dom';
import { collectSimuladorData, convertSimuladorData } from './collectData';
import Carousel from '../Carousel/Carousel';
import Loading from '../Loading/Loading';

const steps = ['Species', 'Reactions', 'Initial Droplet', 'Mixtures', 'Results'];

function TabPanel(props) {
  const { children, value, index, ...other } = props;

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`simple-tabpanel-${index}`}
      aria-labelledby={`simple-tab-${index}`}
      {...other}
    >
      {value === index && (
        <Box sx={{ p: 3 }}>
          <Typography>{children}</Typography>
        </Box>
      )}
    </div>
  );
}

export default function Simulador5() {
  const [value, setValue] = useState(0);
  const [loading, setLoading] = useState(false);
  const [activeStep, setActiveStep] = useState(4);
  const [plotImage, setPlotImage] = useState(null);
  const [pdfImages, setPdfImages] = useState([]);

  const navigate = useNavigate();

  const handleChange = (event, newValue) => {
    setValue(newValue);
  };

  const handleSubmit = () => {
    setLoading(true)
    const data = convertSimuladorData(collectSimuladorData());
    fetch('http://localhost:8001/submit_MMFT', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(data)
    })
      .then(response => response.json())
      .then(data => {
        setPlotImage(`data:image/png;base64,${data.plot_image}`);
        setPdfImages(data.pdf_images.map(imageBase64 => `data:image/png;base64,${imageBase64}`));
        setValue(1)
        setLoading(false)
      })
      .catch(error => console.error('Error:', error));
  };

  return (
    <div className="simulador">
      <Loading loading={loading} />
      <Stepper activeStep={activeStep} alternativeLabel>
        {steps.map((label) => (
          <Step key={label}>
            <StepLabel>{label}</StepLabel>
          </Step>
        ))}
      </Stepper>
      <Box sx={{ borderBottom: 1, borderColor: 'divider' }} className="tabs-container">
        <Tabs value={value} onChange={handleChange} aria-label="basic tabs example" centered>
          <Tab label="Resume" />
          <Tab label="CRN" />
          <Tab label="Table" />
          <Tab label="Microfluidics Scheme" />
        </Tabs>
      </Box>
      <TabPanel value={value} index={0}>
        Resultados da Simulação de CRNs com Microfluidica
      </TabPanel>
      <TabPanel value={value} index={1}>
        <div className="centered-container">
          {pdfImages.length > 0 ? (
            <Carousel images={pdfImages} />
          ) : (
            <Typography>No images to display</Typography>
          )}
        </div>
      </TabPanel>
      <TabPanel value={value} index={2}>
        Conteúdo da Aba 3
      </TabPanel>
      <TabPanel value={value} index={3}>
        <div className="centered-container">
          {plotImage ? (
            <img src={plotImage} alt="Plot" className="centered-image" />
          ) : (
            <Typography>No plot image to display</Typography>
          )}
        </div>
      </TabPanel>
      <div className="footer">
        <button className="footer-button" onClick={handleSubmit}>
          Run Simulation
        </button>
      </div>
    </div>
  );
}
