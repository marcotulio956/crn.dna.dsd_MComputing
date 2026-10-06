import React, { useState, useEffect } from "react";

const PaginatedTable = ({ data, rowsPerPage, highlightedRowIndex, speciesData }) => {
  const [currentPage, setCurrentPage] = useState(0);
  const filteredColumns = speciesData
    .filter(species => species.showInResults) // Keep only species with showInResults = true
    .map(species => species.name); // Extract the names

  const filteredData = data.map(row => {
    return Object.fromEntries(
      Object.entries(row).filter(([key]) => filteredColumns.includes(key) || key === "time")
    );
  });
  data = filteredData

  console.log(filteredData);

  const totalPages = Math.ceil(data.length / rowsPerPage);

  const handleNextPage = () => {
    if (currentPage < totalPages - 1) setCurrentPage(currentPage + 1);
  };

  const handlePreviousPage = () => {
    if (currentPage > 0) setCurrentPage(currentPage - 1);
  };

  const startRow = currentPage * rowsPerPage;
  const endRow = startRow + rowsPerPage;
  const displayedData  = data.slice(startRow, endRow);

  useEffect(() => {
    if (highlightedRowIndex !== null) {
      const targetPage = Math.floor(highlightedRowIndex / rowsPerPage);
      setCurrentPage(targetPage);
    }
  }, [highlightedRowIndex, rowsPerPage]);

  const handlePageChange = (newPage) => {
    setCurrentPage(newPage);
  };

  return (
    <div>
      <table
        style={{
          width: "100%",
          borderCollapse: "collapse",
          marginTop: "20px",
        }}
      >
        <thead>
          <tr>
            {Object.keys(data[0]).map((key) => (
              <th
                key={key}
                style={{
                  border: "1px solid #ddd",
                  padding: "8px",
                  backgroundColor: "#f4f4f4",
                }}
              >
                {key}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {displayedData.map((row, index) => (
            <tr
            key={index}
            style={{
              backgroundColor:
                highlightedRowIndex === startRow + index
                  ? "lightblue"
                  : "transparent",
            }}
            >
              {Object.values(row).map((value, cellIndex) => (
                <td
                  key={cellIndex}
                  style={{
                    border: "1px solid #ddd",
                    padding: "8px",
                  }}
                >
                  {value}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
      <div style={{ marginTop: "10px", display: "flex", justifyContent: "center" }}>
      <button
          onClick={handlePreviousPage}
          disabled={currentPage === 0}
          style={{
            padding: "5px 10px",
            marginRight: "10px",
            cursor: currentPage > 0 ? "pointer" : "not-allowed",
          }}
        >
          Previous
        </button>
      {Array.from({ length: totalPages }, (_, pageIndex) => (
          <button
            key={pageIndex}
            onClick={() => handlePageChange(pageIndex)}
            style={{
              margin: "0 5px",
              backgroundColor:
                pageIndex === currentPage ? "lightblue" : "transparent",
            }}
          >
            {pageIndex + 1}
          </button>
          )
        )}
        <button
          onClick={handleNextPage}
          disabled={currentPage === totalPages - 1}
          style={{
            padding: "5px 10px",
            marginLeft: "10px",
            cursor: currentPage < totalPages - 1 ? "pointer" : "not-allowed",
          }}
        >
          Next
        </button>
        <span
          style={{
            marginLeft: "20px", // Adiciona espaço entre o botão Next e o texto
          }}
        >
          Page {currentPage + 1} of {totalPages}
        </span>
      </div>
    </div>
  );
};

export default PaginatedTable;
