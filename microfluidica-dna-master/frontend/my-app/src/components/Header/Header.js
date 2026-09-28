import "./Header.css";
import React from 'react';
import logo from '../../assets/logo_nanocomp.png';



export default function Header({ toggleMenu }) {

    return (
        <header className="header">
            <div className="header-left">
                <img src={logo} alt="Nanocomp Logo" className="logo" />
                <button className="menu-button" onClick={toggleMenu}>☰</button>
            </div>
        </header>
    );
}