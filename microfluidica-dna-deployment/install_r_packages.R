# R package installation script for Docker container
# Using remotes instead of devtools for lighter installation

cat("Installing required R packages...\n")

# Install remotes (lightweight alternative to devtools)
if (!("remotes" %in% installed.packages()[,"Package"])){
  cat("Installing remotes...\n")
  install.packages("remotes", repos='https://cran.rstudio.com/')
}

# Install CRAN packages
cran_packages <- c("ggplot2", "dplyr", "jsonlite")
for (pkg in cran_packages) {
  if (!(pkg %in% installed.packages()[,"Package"])){
    cat(paste("Installing", pkg, "from CRAN...\n"))
    install.packages(pkg, repos='https://cran.rstudio.com/')
  } else {
    cat(paste(pkg, "already installed.\n"))
  }
}

# Install DNAr from GitHub
if (!("DNAr" %in% installed.packages()[,"Package"])){
  cat("Installing DNAr from GitHub...\n")
  remotes::install_github('DanielKneipp/DNAr', upgrade = "never")
} else {
  cat("DNAr already installed.\n")
}

# Install DNArLogic from GitHub
if (!("DNArLogic" %in% installed.packages()[,"Package"])){
  cat("Installing DNArLogic from GitHub...\n")
  remotes::install_github('renanmarks/dnarlogic', upgrade = "never")
} else {
  cat("DNArLogic already installed.\n")
}

# Verify installations
cat("\n=== Verifying installed packages ===\n")
required_packages <- c("DNAr", "DNArLogic", "ggplot2", "dplyr", "jsonlite", "remotes")
for (pkg in required_packages) {
  if (pkg %in% installed.packages()[,"Package"]) {
    cat(paste("✓", pkg, "installed successfully\n"))
  } else {
    cat(paste("✗", pkg, "FAILED to install\n"))
    quit(status = 1)
  }
}

cat("\n=== All R packages installed successfully! ===\n")
