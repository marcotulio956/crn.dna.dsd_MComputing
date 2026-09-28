import React from 'react';
import { Drawer, Typography, Box } from '@mui/material';
import { Image } from 'antd'; // Import the Image component from antd

const RightSidebar = ({ resultImages }) => {

  return (
    <Drawer variant="permanent" anchor="right">
      <div style={{ width: '200px', padding: '16px' }}>
        <Typography style={{ textAlign: 'center' }} variant="h6">Imagens DNAr</Typography>
        <Box mt={2}>
          <Box>
            {resultImages.length > 0 ? (
              resultImages.map((src, index) => (
                <Box key={index} mb={2}>
                  <Image src={src} alt={`Result Image ${index + 1}`} width="100%" />
                </Box>
              ))
            ) : (
              <Typography variant="body1">No images available</Typography>
            )}
          </Box>
        </Box>
      </div>
    </Drawer>
  );
};

export default RightSidebar;
