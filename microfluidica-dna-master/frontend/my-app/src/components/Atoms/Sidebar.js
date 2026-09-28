import React from 'react';
import { Link } from 'react-router-dom';
import './Sidebar.css';

function Sidebar({ isOpen }) {
  return (
    <aside className={`sidebar ${isOpen ? 'open' : ''}`}>
    <nav>
      <ul>
        <li><Link to="/">Recent Simulations</Link></li>
        <li><Link to="/simulador">Simulator</Link></li>
      </ul>
    </nav>
  </aside>
  );
}

export default Sidebar;