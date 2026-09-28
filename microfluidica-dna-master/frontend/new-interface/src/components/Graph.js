import React, { useState, useEffect } from "react";
import { Stage, Layer, Circle, Line, Text, Rect, Group } from "react-konva";
import { useSpring, animated } from "react-spring";
import {
  Fab,
  Dialog,
  DialogContent,
  DialogTitle,
  TextField,
  Button,
  Slider, // Import Slider
} from "@mui/material";
import AddIcon from "@mui/icons-material/Add";
import PlayArrowIcon from "@mui/icons-material/PlayArrow";
import DropletAnimation from "./CircleAnimation";

const AnimatedCircle = animated(Circle);

const Graph = ({
  selectedNode,
  setSelectedNode,
  selectedEdge,
  setSelectedEdge,
  nodes,
  setNodes,
  edges,
  setEdges,
  animationData,
}) => {
  const [currentNode, setCurrentNode] = useState(null);
  const [isDragging, setIsDragging] = useState(false);
  const [previewLine, setPreviewLine] = useState(null);
  const [addingNode, setAddingNode] = useState(false);
  const [showDialog, setShowDialog] = useState(false);
  const [simulate, setSimulate] = useState(false);
  const [enableSlider, setEnableSlider] = useState(false);
  const [nodeName, setNodeName] = useState("");
  const [nodePosition, setNodePosition] = useState({ x: 0, y: 0 });
  const [ctrlPressed, setCtrlPressed] = useState(false);
  const [timestamp, setTimestamp] = useState(100); // State for the timestamp
  const defaultHeight = 5;

  useEffect(() => {
    if (selectedNode) {
      setNodes(
        nodes.map((node) =>
          node.name === selectedNode.name
            ? { ...node, x: selectedNode.x, y: selectedNode.y, isSink: selectedNode.isSink }
            : node
        )
      );
      setEdges(
        edges.map((edge) =>
          edge.from.name === selectedNode.name
            ? {
                ...edge,
                from: { ...edge.from, x: selectedNode.x, y: selectedNode.y },
              }
            : edge.to.name === selectedNode.name
            ? {
                ...edge,
                to: { ...edge.to, x: selectedNode.x, y: selectedNode.y },
              }
            : edge
        )
      );
    }
    if (selectedEdge) {
      setEdges(
        edges.map((edge) =>
          edge.from.name === selectedEdge.from.name &&
          edge.to.name === selectedEdge.to.name
            ? { ...edge, height: selectedEdge.height }
            : edge
        )
      );
    }
  }, [selectedNode, selectedEdge]);

  useEffect(() => {
    const handleKeyDown = (e) => {
      if (e.key === "Control") {
        setCtrlPressed(true);
      }
    };

    const handleKeyUp = (e) => {
      if (e.key === "Control") {
        setCtrlPressed(false);
      }
    };

    window.addEventListener("keydown", handleKeyDown);
    window.addEventListener("keyup", handleKeyUp);

    return () => {
      window.removeEventListener("keydown", handleKeyDown);
      window.removeEventListener("keyup", handleKeyUp);
    };
  }, []);

  const addNode = () => {
    if (nodeName.trim() === "") {
      alert("Node name cannot be empty");
      return;
    }
    setNodes([
      ...nodes,
      {
        x: Math.round(nodePosition.x),
        y: Math.round(nodePosition.y),
        name: nodeName,
      },
    ]);
    setNodeName("");
    setShowDialog(false);
    setAddingNode(false);
  };

  const getNodeAtPosition = (x, y) => {
    return nodes.find((node) => Math.hypot(node.x - x, node.y - y) < 15);
  };

  const getEdgeAtPosition = (x, y) => {
    return edges.find((edge) => {
      const { from, to } = edge;
      const distance = Math.hypot(to.x - from.x, to.y - from.y);
      const distanceToLine =
        Math.abs(
          (to.y - from.y) * x -
            (to.x - from.x) * y +
            to.x * from.y -
            to.y * from.x
        ) / distance;
      return distanceToLine < 10;
    });
  };

  const addEdge = (node1, node2) => {
    setEdges([...edges, { from: node1, to: node2, height: defaultHeight }]);
  };

  const handleMouseDown = (e) => {
    if (addingNode) {
      const { x, y } = e.target.getStage().getPointerPosition();
      setNodePosition({ x: Math.round(x), y: Math.round(y) });
      setShowDialog(true);
    } else {
      const { x, y } = e.target.getStage().getPointerPosition();
      const node = getNodeAtPosition(x, y);
      const edge = getEdgeAtPosition(x, y);
      if (node) {
        setSelectedNode(node);
        setCurrentNode(node);
        setSelectedEdge(null);
        setIsDragging(true);
      } else if (edge) {
        setSelectedEdge(edge);
        setSelectedNode(null);
      } else {
        setSelectedNode(null);
        setSelectedEdge(null);
      }
    }
  };

  const handleMouseMove = (e) => {
    if (isDragging && currentNode) {
      const { x, y } = e.target.getStage().getPointerPosition();
      if (ctrlPressed) {
        setNodes(
          nodes.map((node) =>
            node.name === currentNode.name
              ? { ...node, x: Math.round(+x), y: Math.round(+y) }
              : node
          )
        );
        setCurrentNode({ ...currentNode, x: Math.round(+x), y: Math.round(+y) });
      } else {
        setPreviewLine({
          x1: currentNode.x,
          y1: currentNode.y,
          x2: Math.round(x),
          y2: Math.round(y),
        });
      }
    }
  };

  const handleMouseUp = (e) => {
    if (isDragging && currentNode) {
      const { x, y } = e.target.getStage().getPointerPosition();
      const targetNode = getNodeAtPosition(Math.round(x), Math.round(y));

      if (!ctrlPressed) {
        const areNodesEqual = (node1, node2) => {
          return (
            node1 &&
            node2 &&
            node1.x === node2.x &&
            node1.y === node2.y &&
            node1.name === node2.name
          );
        };

        if (targetNode && !areNodesEqual(targetNode, currentNode)) {
          addEdge(currentNode, targetNode);
        }
      }

      setIsDragging(false);
      setPreviewLine(null);
    }
  };

  const handleSimulate = () => {
    setSimulate(true);
    setEnableSlider(false);
    setTimeout(() => {
      //setTimestamp(0)
      //setEnableSlider(true);
    }, 5000);
  };

  const handleSliderChange = (event, newValue) => {
    setTimestamp(newValue);
  };

  return (
    <div style={{ position: "relative", width: "100%", height: "100%" }}>
      <Stage
        width={1100}
        height={820}
        onMouseDown={handleMouseDown}
        onMouseMove={handleMouseMove}
        onMouseUp={handleMouseUp}
      >
        <Layer>
        {edges.map((edge, index) => (
            <Line
              key={index}
              zIndex={1}
              points={[edge.from.x, edge.from.y, edge.to.x, edge.to.y]}
              stroke="black"
              strokeWidth={edge.height/2}
              onClick={() => setSelectedEdge(edge)}
            />
          ))}
          {previewLine && (
            <Line
              points={[
                previewLine.x1,
                previewLine.y1,
                previewLine.x2,
                previewLine.y2,
              ]}
              stroke="grey"
              dash={[4, 4]}
            />
          )}
        </Layer>
        <Layer>
        {enableSlider?
            <DropletAnimation
              droplets={animationData}
              simulation={simulate}
              setSimulation={setSimulate}
              timestamp={timestamp} // Pass timestamp to the animation
            />:
            <DropletAnimation
              droplets={animationData}
              simulation={simulate}
              setSimulation={setSimulate}
            />
          }
        </Layer>
        <Layer>
          {nodes.map((node, index) => (
            <React.Fragment key={index}>
              <Circle
                x={node.x}
                y={node.y}
                zIndex={3}
                radius={15}
                fill="black"
                onClick={() => setSelectedNode(node)}
              />
              <Group
                draggable
                x={node.labelX !== undefined ? node.labelX : parseInt(node.x) + 20} 
                y={node.labelY !== undefined ? node.labelY : parseInt(node.y) - 20}
                onDragMove={(e) => {
                  // Atualizar a posição do rótulo ao arrastar
                  const newLabelX = e.target.x();
                  const newLabelY = e.target.y();
                  setNodes((prevNodes) =>
                    prevNodes.map((n, i) =>
                      i === index
                        ? { ...n, labelX: newLabelX, labelY: newLabelY }
                        : n
                    )
                  );
                }}
              >
              <Rect
                width={node.name.length * 10 + 10} // Largura dinâmica baseada no tamanho do texto
                height={20} // Altura fixa
                fill="#f8f8f8" // Cor de fundo (off white)
                stroke="black" // Cor da borda
                strokeWidth={1} // Espessura da borda
                cornerRadius={5} // Bordas arredondadas
              />
              <Text
                text={node.name || `Node ${index}`} // Text to display
                fontSize={14} // Font size for the label
                fill="black" // Text color
                offsetX={-5} // Ajuste de posicionamento horizontal
                offsetY={-5}
              />
              </Group>
            </React.Fragment>
          ))}
        </Layer>
      </Stage>
      <Fab
        color="primary"
        aria-label="add"
        style={{
          position: "absolute",
          bottom: "16px",
          right: "16px",
          zIndex: 1300,
        }}
        onClick={() => setAddingNode(true)}
      >
        <AddIcon />
      </Fab>
      <Fab
        color="secondary"
        aria-label="simulate"
        style={{
          position: "absolute",
          bottom: "16px",
          right: "80px",
          zIndex: 1300,
        }}
        onClick={handleSimulate}
      >
        <PlayArrowIcon />
      </Fab>
      <Dialog open={showDialog} onClose={() => setShowDialog(false)}>
        <DialogTitle>Add Node</DialogTitle>
        <DialogContent>
          <TextField
            label="Node Name"
            value={nodeName}
            onChange={(e) => setNodeName(e.target.value)}
            fullWidth
          />
          <Button onClick={addNode} color="primary">
            Add
          </Button>
        </DialogContent>
      </Dialog>
      <div
        style={{
          position: "absolute",
          bottom: "70px", // Adjust this value as needed
          left: "50%",
          transform: "translateX(-50%)",
          width: "80%", // Adjust width as needed
          zIndex: 1300,
        }}
      >
      {enableSlider &&
        <Slider
          value={timestamp}
          onChange={handleSliderChange}
          aria-labelledby="timestamp-slider"
          min={0}
          max={5000} // Assuming the length of animationData is the max timestamp
        />
      }
      </div>
    </div>
  );
};

export default Graph;
