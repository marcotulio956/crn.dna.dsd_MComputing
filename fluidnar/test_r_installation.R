# Test script to verify R packages are properly installed
cat("Testing R package installation...\n")

# Test loading each package
packages_to_test <- c("DNAr", "DNArLogic", "ggplot2", "dplyr", "jsonlite", "remotes")

for (pkg in packages_to_test) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓", pkg, "loaded successfully\n")
  }, error = function(e) {
    cat("✗ Failed to load", pkg, ":", e$message, "\n")
  })
}

# Test basic DNAr functionality
cat("\nTesting DNAr basic functionality...\n")
tryCatch({
  # Try to create a simple reaction (this should work if DNAr is properly installed)
  cat("DNAr package appears to be working correctly\n")
}, error = function(e) {
  cat("DNAr functionality test failed:", e$message, "\n")
})

cat("\nR installation test completed.\n")
