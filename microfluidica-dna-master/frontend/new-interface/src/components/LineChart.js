import React, { useRef } from 'react';
import {
  Chart as ChartJS,
  LineElement,
  PointElement,
  LinearScale,
  CategoryScale,
  Tooltip,
  Legend,
  Title,
} from 'chart.js';
import { Line } from 'react-chartjs-2';

ChartJS.register(
  LineElement,
  PointElement,
  LinearScale,
  CategoryScale,
  Tooltip,
  Legend,
  Title
);

const LineChart = ({ data, speciesData, onPointClick }) => {
  const chartRef = useRef(null);
  const maxPoints = 50;

  let filteredData;
  
  if (data.length <= maxPoints) {
    // If there are fewer points than maxPoints, use all data
    filteredData = data;
  } else {
    // Always include first and last points
    const firstPoint = data[0];
    const lastPoint = data[data.length - 1];

    const timeMin = firstPoint.time;
    const timeMax = lastPoint.time;
    const interval = (timeMax - timeMin) / (maxPoints - 1);

    let selectedTimes = [timeMin]; // Start with first point

    for (let i = 1; i < maxPoints - 1; i++) {
      selectedTimes.push(timeMin + i * interval);
    }

    selectedTimes.push(timeMax); // End with last point

    // Find the closest data point for each selected time
    filteredData = selectedTimes.map(targetTime =>
      data.reduce((prev, curr) =>
        Math.abs(curr.time - targetTime) < Math.abs(prev.time - targetTime) ? curr : prev
      )
    );
  }

  // Extract visible species
  const visibleSpecies = speciesData.filter(species => species.showInResults);
  const visibleSpeciesNames = visibleSpecies.map(sp => sp.name);

  const timeValues = filteredData.map(entry => entry.time.toFixed(0));
  const parameters = Object.keys(data[0]).filter(key => key !== 'time' && visibleSpeciesNames.includes(key));

  // Create datasets
  const datasets = parameters.map(param => ({
    label: param,
    data: filteredData.map(entry => entry[param]),
    fill: false,
    borderColor: getRandomColor(),
    pointRadius: 2,
    pointHoverRadius: 5,
  }));

  const chartData = {
    labels: timeValues,
    datasets: datasets,
  };

  const options = {
    responsive: true,
    onClick: (event, elements) => {
      if (elements.length > 0) {
        const clickedIndex = elements[0].index;
        if (onPointClick) {
          onPointClick(clickedIndex);
        }
      }
    },
    scales: {
      x: { title: { display: true, text: 'Time (s)' } },
      y: { title: { display: true, text: 'Value (nmol/L)' } },
    },
    plugins: {
      tooltip: {
        mode: 'nearest',
        intersect: false,
        callbacks: {
          label: context => `${context.dataset.label}: ${context.parsed.y.toFixed(2)} nmol/L`,
          title: tooltipItems => `Time: ${tooltipItems[0].label} s`,
        },
      },
    },
  };

  const handleDownload = () => {
    if (chartRef.current) {
      const chartImage = chartRef.current.toBase64Image();
      const link = document.createElement('a');
      link.href = chartImage;
      link.download = 'line-chart.png';
      link.click();
    }
  };

  return (
    <div>
      <Line ref={chartRef} data={chartData} options={options} width={850} height={400} />
      <button onClick={handleDownload}>Download Chart</button>
    </div>
  );
};

// Utility function to generate random colors
function getRandomColor() {
  const letters = '0123456789ABCDEF';
  let color = '#';
  for (let i = 0; i < 6; i++) {
    color += letters[Math.floor(Math.random() * 16)];
  }
  return color;
}

export default LineChart;
