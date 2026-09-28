import React, { useEffect, useState } from "react";
import Header from "../components/Header";
import LeftSidebar from "../components/LeftSidebar";
import RightSidebar from "../components/RightSidebar";
import { Backdrop, Box, CircularProgress, Tab, Tabs } from "@mui/material";
import Graph from "../components/Graph";
import LineChart from "../components/LineChart";
import PaginatedTable from "../components/PaginatedTable";

const MainPage = () => {
  const [loading, setLoading] = useState(false);
  const [selectedNode, setSelectedNode] = useState(null);
  const [selectedEdge, setSelectedEdge] = useState(null);
  const [resultImages, setResultImages] = useState([]);
  const [resultTables, setResultTables] = useState([]); 
  const [nodes, setNodes] = useState([]);
  const [edges, setEdges] = useState([]);
  const [tabIndex, setTabIndex] = useState(0);
  const [animationData, setAnimationData] = useState([]);
  const [tableData, setTableData] = useState([]); 
  const [speciesData, setSpeciesData] = useState([]);
  const [selectedTableIndex, setSelectedTableIndex] = useState(0);
  const [highlightedRowIndex, setHighlightedRowIndex] = useState(null);
  const numberTables = resultTables.length

  const handleTableChange = (event) => {
    setSelectedTableIndex(event.target.value);
    setHighlightedRowIndex(null);
  };

  const handlePointClick = (index) => {
    setHighlightedRowIndex(index); // Set the highlighted row index
    setTabIndex(2); // Switch to the table tab
  };

  const handleTabChange = (event, newValue) => {
    setTabIndex(newValue);
  };

  useEffect(() => {
    setSelectedTableIndex(numberTables-1)
  }, [numberTables]);

  let dropletCounter = 1;
  let tableCounter = 1;

  return (
    <Box sx={{ display: "flex" }}>
      <Backdrop
        sx={{ color: "#fff", zIndex: (theme) => theme.zIndex.drawer + 1 }}
        open={loading}
      >
        <CircularProgress color="inherit" />
      </Backdrop>
      <LeftSidebar
        selectedNode={selectedNode}
        setSelectedNode={setSelectedNode}
        selectedEdge={selectedEdge}
        setSelectedEdge={setSelectedEdge}
        nodes={nodes}
        setNodes={setNodes}
        edges={edges}
        setEdges={setEdges}
        setLoading={setLoading}
        setResultImages={setResultImages}
        setResultTables={setResultTables}
        setTabIndex={setTabIndex}
        speciesData={speciesData}
        setSpeciesData={setSpeciesData}
        setAnimationData={setAnimationData}
        setTableData={setTableData}
        tableData={tableData}
      />
      <Box
        component="main"
        sx={{
          flexGrow: 1,
          padding: 3,
          marginLeft: "450px",
        }}
      >
        <Header />
        <Box
          sx={{
            marginTop: 2,
            height: "calc(100vh - 64px - 32px - 50px)",
            border: "1px solid #ddd",
          }}
        >
          <Tabs value={tabIndex} onChange={handleTabChange}>
            <Tab label="Microfluidic Prototype" />
            <Tab label="DNAr Results" disabled={resultTables.length === 0} />
            <Tab label="Tables" disabled={resultTables.length === 0} />
          </Tabs>
          {tabIndex === 0 && (
            <Graph
              selectedNode={selectedNode}
              setSelectedNode={setSelectedNode}
              selectedEdge={selectedEdge}
              setSelectedEdge={setSelectedEdge}
              nodes={nodes}
              setNodes={setNodes}
              edges={edges}
              setEdges={setEdges}
              animationData={animationData}
            />
          )}
          {tabIndex === 1 && resultTables.length > 0 && (
            <div>
              <select
                onChange={handleTableChange}
                value={selectedTableIndex}
                style={{
                  fontSize: "16px",
                  padding: "10px",
                  borderRadius: "5px",
                  marginLeft: "30px",
                  marginTop: "30px",
                  marginRight: "30px",
                  border: "1px solid #ccc",
                  backgroundColor: "#f0f0f0",
                  marginBottom: "20px",
                }}
              >
                <option value="" disabled>
                  Select a results table
                </option>
                {resultTables.map((_, index) => {
                  const dropletName = tableData[index]?.name || `Mixture ${
                    tableData[index]
                      ? index + 1
                      : (() => {
                          // Increment the counter when a new "Table X" is generated
                          const currentCounter = dropletCounter;
                          dropletCounter++;
                          return currentCounter;
                        })()
                  }`;
                  return (
                    <option key={index} value={index}>
                      {dropletName}
                    </option>
                  );
                })}
              </select>
              <LineChart 
                data={resultTables[selectedTableIndex]}
                speciesData={speciesData} 
                onPointClick={handlePointClick}
              />
            </div>
          )}
          {tabIndex === 2 && resultTables.length > 0 && (
            <div>
              <select
                onChange={handleTableChange}
                value={selectedTableIndex}
                style={{
                  fontSize: "16px",
                  padding: "10px",
                  borderRadius: "5px",
                  marginLeft: "30px",
                  marginTop: "30px",
                  marginRight: "30px",
                  border: "1px solid #ccc",
                  backgroundColor: "#f0f0f0",
                  marginBottom: "20px",
                }}
              >
                <option value="" disabled>
                  Select a simulation table
                </option>
                {resultTables.map((_, index) => {
                  const dropletName = tableData[index]?.name || `Mixture ${
                    tableData[index]
                      ? index + 1
                      : (() => {
                          const currentCounter = tableCounter;
                          tableCounter++;
                          return currentCounter;
                        })()
                  }`;
                  return (
                    <option key={index} value={index}>
                      {dropletName}
                    </option>
                  );
                })}
              </select>
              <PaginatedTable 
                data={resultTables[selectedTableIndex]} 
                speciesData={speciesData}
                rowsPerPage={10} 
                highlightedRowIndex={highlightedRowIndex}
              />
              
            </div>
          )}
        </Box>
      </Box>
    </Box>
  );
};

export default MainPage;
