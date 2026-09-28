# Multi-stage build for optimization
# Stage 1: Build React frontend
FROM node:18-alpine AS frontend-builder
WORKDIR /app
COPY frontend/new-interface/package*.json ./
RUN npm ci
COPY frontend/new-interface/ ./
RUN npm run build

# Stage 2: Build R packages (separate stage for caching)
FROM ubuntu:22.04 AS r-builder
ENV DEBIAN_FRONTEND=noninteractive
ENV R_HOME=/usr/lib/R
ENV R_LIBS_USER=/usr/local/lib/R/site-library

# Install only R and essential build dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    build-essential \
    curl \
    wget \
    git \
    libgit2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libgsl-dev \
    libxt-dev \
    libcairo2-dev \
    libpango1.0-dev \
    cmake \
    ninja-build \
    python3-dev \
    python3-pip \
    python3-setuptools \
    python3-wheel \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
COPY install_r_packages.R /tmp/install_r_packages.R
RUN Rscript /tmp/install_r_packages.R

# Install Python packages with build tools available
WORKDIR /app
COPY requirements.txt .
# Copy the pre-built wheel file
COPY mmft_simulator-0.2.2.dev54+gf6e155442-cp310-cp310-linux_x86_64.whl ./
# Install wheel first, then other requirements in single layer
RUN pip3 install --no-cache-dir --upgrade pip setuptools wheel \
    && pip3 install --no-cache-dir mmft_simulator-0.2.2.dev54+gf6e155442-cp310-cp310-linux_x86_64.whl \
    && pip3 install --no-cache-dir -r requirements.txt \
    && rm mmft_simulator-0.2.2.dev54+gf6e155442-cp310-cp310-linux_x86_64.whl

# Stage 3: Final production image - use slim base for smaller size
FROM ubuntu:22.04
ENV DEBIAN_FRONTEND=noninteractive
ENV R_HOME=/usr/lib/R
ENV R_LIBS_USER=/usr/local/lib/R/site-library

# Install minimal runtime dependencies only
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        python3 \
        python3-pip \
        r-base \
        curl \
        libcurl4 \
        libssl3 \
        libxml2 \
        libfreetype6 \
        libpng16-16 \
        libtiff5 \
        libjpeg8 \
        libfontconfig1 \
        libharfbuzz0b \
        libfribidi0 \
        libgdal30 \
        libproj22 \
        libgeos3.10.2 \
        libudunits2-0 \
        libgsl27 \
        libxt6 \
        libcairo2 \
        libpango-1.0-0 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Copy R packages from builder stage
COPY --from=r-builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy Python packages from builder stage
COPY --from=r-builder /usr/local/lib/python3.10/dist-packages /usr/local/lib/python3.10/dist-packages
COPY --from=r-builder /usr/local/bin /usr/local/bin

# Set working directory
WORKDIR /app

# Copy application files and create user in single layer
COPY Python/ ./Python/
COPY DNAr/ ./DNAr/
# Copy built frontend from builder stage
COPY --from=frontend-builder /app/build ./frontend/new-interface/build

# Create non-root user and startup script
RUN useradd -m -u 1000 appuser \
    && echo '#!/bin/bash\n\
cd /app/Python\n\
PORT=${PORT:-8000}\n\
# Use single worker and limit threads for lower memory usage\n\
uvicorn subprocess_R:app --host 0.0.0.0 --port $PORT --workers 1 --limit-concurrency 10\n\
' > /app/start.sh \
    && chmod +x /app/start.sh \
    && chown -R appuser:appuser /app

USER appuser

# Expose port (Railway will use PORT env var)
EXPOSE 8000

# Start the application
CMD ["/app/start.sh"]
