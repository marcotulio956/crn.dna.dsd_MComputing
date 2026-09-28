import './Home.css';
import React from 'react';
import SearchIcon from '@mui/icons-material/Search';


const images = require.context('../../assets/simulacoes_recentes', false, /\.(png|jpe?g|svg)$/);
const handleClick = (index) => {
    console.log(`Image ${index + 1} clicked`);
    // Adicione aqui a funcionalidade desejada
  };
  
export default function Home() {
    const simulationImages = images.keys().map(images);

    return (
        <div className="recent-simulations">
            <div className="header-container">
                <h2>Recent Simulations</h2>
                <div className="search-bar">
                    <input type="text" placeholder="Search" />
                    <SearchIcon sx={{ fontSize: 40, color: '#000000' }} />
                </div>
            </div>
            <div className="simulation-container">
                {simulationImages.map((image, index) => (
                    <button className="simulation-card" key={index} onClick={() => handleClick(index)}>
                    <img src={image} alt={`Simulation ${index + 1}`} />
                    </button>
                ))}
            </div>
        </div>
    );
}