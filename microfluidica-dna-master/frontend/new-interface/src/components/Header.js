// src/components/Header.js
import React from 'react';
import AppBar from '@mui/material/AppBar';
import Toolbar from '@mui/material/Toolbar';
import Typography from '@mui/material/Typography';

const Header = () => (
  <AppBar position="static">
    <Toolbar>
      <Typography variant="h5" component="div" sx={{ flexGrow: 1, textAlign: 'center', fontWeight: 'bold' }}>
        DNAr Microfluidics Droplet Simulator
      </Typography>
    </Toolbar>
  </AppBar>
);

export default Header;
