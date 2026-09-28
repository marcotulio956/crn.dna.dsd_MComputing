import { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import Simulacao from "../Simulador5/Simulador5";
import './Subject_miniature.css';

export default function Miniatura({ id, title, image, description }) {
    const [simulacao, setSimulacao] = useState(null);

    useEffect(() => {
        async function fetchSimulacao() {
            const response = await fetch(`http://localhost:3001/simulacoes/${id}`);
            const data = await response.json();
            setSimulacao(data);
        }

        fetchSimulacao();
    }, [id]);

    return (
        <Link to={`/simulador5/${id}`} className="miniatura">
            <img src={image} alt={title} />
            <h3>{title}</h3>
            <p>{description}</p>
            {simulacao && <Simulacao {...simulacao} />}
        </Link>
    );
}