import React, { useState } from 'react';import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Header from './components/Header/Header';
import Sidebar from './components/Atoms/Sidebar';
import Home from './components/Home/Home'; // Importar Home
import Simulador1 from './components/Simulador1/Simulador1'
import Simulador2 from './components/Simulador2/Simulador2'
import Simulador3 from './components/Simulador3/Simulador3'
import Simulador4 from './components/Simulador4/Simulador4'
import Simulador5 from './components/Simulador5/Simulador5'
import './App.css';

function App() {
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  const toggleMenu = () => {
    setIsMenuOpen(!isMenuOpen);
  };

  return (
    <Router>
      <div className="App">
        <Header toggleMenu={toggleMenu}/>
        <div className="container">
          <Sidebar isOpen={isMenuOpen} />
          <main>
          <Routes>
            <Route path="/" element={<Home />} />
            <Route path="/simulador" element={<Simulador1 />} />
            <Route path="/simulador2" element={<Simulador2 />} />
            <Route path="/simulador3" element={<Simulador3 />} />
            <Route path="/simulador4" element={<Simulador4 />} />
            <Route path="/simulador5" element={<Simulador5 />} />
          </Routes>
          </main>
        </div>
      </div>
    </Router>
  );
}

export default App;