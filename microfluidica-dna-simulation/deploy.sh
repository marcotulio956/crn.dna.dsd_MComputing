#!/bin/bash

# Deployment script for Microfluidic DNA Droplets Simulation Tool

echo "🚀 Starting deployment process..."

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo "❌ Docker is not installed. Please install Docker first."
    exit 1
fi

# Check if Docker Compose is installed
if ! docker compose version &> /dev/null && ! command -v docker-compose &> /dev/null; then
    echo "❌ Docker Compose is not installed. Please install Docker Compose first."
    exit 1
fi

if docker compose version &> /dev/null; then
    COMPOSE=(docker compose)
else
    COMPOSE=(docker-compose)
fi

# Build the React frontend
echo "📦 Building React frontend..."
cd frontend/studio
if ! npm install; then
    echo "❌ Failed to install npm dependencies"
    exit 1
fi

if ! npm run build; then
    echo "❌ Failed to build React app"
    exit 1
fi

cd ../..

# Build Docker image
echo "🐳 Building Docker image..."
if ! "${COMPOSE[@]}" build dnar-dev fluidna-engine microfluidica-app; then
    echo "❌ Failed to build Docker image"
    exit 1
fi

# Start the application
echo "🚀 Starting application..."
if ! "${COMPOSE[@]}" up -d; then
    echo "❌ Failed to start application"
    exit 1
fi

echo "✅ Deployment completed successfully!"
echo "🌐 Your application should be running at: http://localhost:8000"
echo "📊 API documentation available at: http://localhost:8000/docs"
echo ""
echo "To stop the application, run: docker-compose down"
echo "To view logs, run: docker-compose logs -f"
