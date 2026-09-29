import zIndex from "@mui/material/styles/zIndex";
import React, { useState, useEffect, useRef } from "react";
import { Stage, Layer, Circle } from "react-konva";


// Function to normalize the time of positions
const normalizeTime = (positions, maxTime) => {
  return positions.map((pos) => ({
    ...pos,
    time: (pos.time / maxTime) * 5000, // Normalize to 5 seconds (5000ms)
  }));
};

const Droplet = ({ positions, simulation, timestamp, onComplete }) => {
  const [currentPosition, setCurrentPosition] = useState(positions[0]);
  const [isVisible, setIsVisible] = useState(false);
  const [startAnimation, setStartAnimation] = useState(false);
  const startTimeRef = useRef(Date.now());
  const animationFrameIdRef = useRef(null);

  useEffect(() => {
    if (simulation) {
      const startTimeout = setTimeout(() => {
        setIsVisible(true);
        setStartAnimation(true);
      }, positions[0].time);

      return () => {
        clearTimeout(startTimeout);
      };
    }
  }, [simulation, positions]);

  useEffect(() => {
    if (timestamp !== undefined) {
      // Find the position corresponding to the given timestamp
      const posIndex = positions.findIndex((pos) => pos.time > timestamp);
      if (posIndex > 0) {
        const prevPos = positions[posIndex - 1];
        const nextPos = positions[posIndex];
        const progress = (timestamp - prevPos.time) / (nextPos.time - prevPos.time);
        setCurrentPosition({
          x: prevPos.x + (nextPos.x - prevPos.x) * progress,
          y: prevPos.y + (nextPos.y - prevPos.y) * progress,
        });
      } else {
        setCurrentPosition(positions[positions.length - 1]);
      }
    } else if (startAnimation) {
      const animate = (startIndex, endIndex) => {
        const startTime = positions[startIndex].time;
        const endTime = positions[endIndex].time;
        const startX = positions[startIndex].x;
        const startY = positions[startIndex].y;
        const endX = positions[endIndex].x;
        const endY = positions[endIndex].y;
        const duration = endTime - startTime;

        const animateStep = () => {
          const currentTime = Date.now() - startTimeRef.current;
          const progress = Math.min(currentTime / duration, 1);

          const x = startX + (endX - startX) * progress;
          const y = startY + (endY - startY) * progress;

          setCurrentPosition({ x, y });

          if (progress < 1) {
            animationFrameIdRef.current = requestAnimationFrame(animateStep);
          } else if (endIndex < positions.length - 1) {
            startTimeRef.current = Date.now(); // Reset start time for the next segment
            animate(endIndex, endIndex + 1);
          } else {
            onComplete();
          }
        };

        animateStep();
      };

      startTimeRef.current = Date.now();
      animate(0, 1);

      return () => {
        cancelAnimationFrame(animationFrameIdRef.current);
      };
    }
  }, [startAnimation, positions, timestamp, onComplete]);

  useEffect(() => {
    if (!simulation) {
      setCurrentPosition(positions[0]);
      setIsVisible(false);
      setStartAnimation(false);
      cancelAnimationFrame(animationFrameIdRef.current); // Clean up animation frame
    }
  }, [simulation, positions]);

  if (!isVisible) return null;

  return (
    <Circle
      x={currentPosition.x}
      y={currentPosition.y}
      radius={10}
      fill="blue"
    />
  );
};

const DropletAnimation = ({ droplets, simulation, setSimulation, timestamp }) => {
  const maxTime = Math.max(
    ...droplets.flatMap((d) => d.positions.map((p) => p.time))
  );

  const normalizedDroplets = droplets.map((droplet) => ({
    ...droplet,
    positions: normalizeTime(droplet.positions, maxTime),
  }));

  const handleComplete = () => {
    setSimulation(false);
  };

  return (
    <>
      {simulation &&
        normalizedDroplets.map((droplet) => (
          <Droplet
            key={droplet.id}
            positions={droplet.positions}
            simulation={simulation}
            timestamp={timestamp}
            onComplete={handleComplete}
          />
        ))}
    </>
  );
};

export default DropletAnimation;
