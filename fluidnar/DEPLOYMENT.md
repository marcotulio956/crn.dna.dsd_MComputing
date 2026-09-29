# 🚀 Deployment Guide - Microfluidic DNA Droplets Simulation Tool

This guide will help you deploy your microfluidic DNA droplets simulation tool online.

## 📋 Prerequisites

- Docker and Docker Compose installed
- Git repository with your code
- Account on a cloud platform (Railway, Render, or similar)

## 🎯 Recommended Deployment Options

### Option 1: Railway (Recommended)
**Best for:** Easy deployment, good for scientific applications
**Cost:** Free tier available, then pay-per-use

### Option 2: Render
**Best for:** Free hosting, simple setup
**Cost:** Free tier with limitations

### Option 3: Google Cloud Run
**Best for:** Scalability, serverless
**Cost:** Pay-per-use, very cost-effective

## 🚀 Quick Deployment with Railway

### Step 1: Create GitHub Repository

1. **Go to [GitHub.com](https://github.com)** and create a new repository:
   - **Repository name:** `microfluidica-dna-simulation` (or your preferred name)
   - **Visibility:** Private
   - **Don't initialize** with README, .gitignore, or license (since you already have code)

2. **Add GitHub as a remote and push your code:**
   ```bash
   # Add GitHub remote (replace YOUR_USERNAME with your GitHub username)
   git remote add github https://github.com/YOUR_USERNAME/microfluidica-dna-simulation.git
   
   # Push your deployment branch to GitHub
   git push github deployment
   ```

### Step 2: Deploy to Railway

1. **Go to [Railway.app](https://railway.app)**
2. **Sign up/Login** with your GitHub account
3. **Click "New Project"** → **"Deploy from GitHub repo"**
4. **Select your private repository** (you may need to grant Railway access)
5. **Choose the `deployment` branch**
6. **Railway will automatically detect the Dockerfile and deploy**

### GitHub-Specific Notes:

- **Private Repository:** Railway can access private repos with proper permissions
- **Authentication:** Connect your GitHub account to Railway
- **Repository Access:** Railway will ask for access to your private repository
- **Branch Selection:** Choose the `deployment` branch for deployment

### Step 3: Configure Environment Variables (Optional)

In Railway dashboard:
- Go to your project → Variables
- Add any environment variables if needed

### Step 4: Access Your Application

- Railway will provide you with a URL (e.g., `https://your-app.railway.app`)
- Your app will be available at this URL

## 🐳 Local Testing with Docker

Before deploying, test locally:

```bash
# Make the deployment script executable
chmod +x deploy.sh

# Run the deployment script
./deploy.sh
```

Or manually:

```bash
# Build and start with Docker Compose
docker-compose up --build

# Access your application at http://localhost:8000
```

## 🔧 Alternative: Render Deployment

### Step 1: Create Render Account
1. Go to [render.com](https://render.com)
2. Sign up with GitHub

### Step 2: Deploy
1. **New** → **Web Service**
2. **Connect your repository**
3. **Configure:**
   - **Build Command:** `docker build -t microfluidica-app .`
   - **Start Command:** `/app/start.sh`
   - **Environment:** Docker

## ☁️ Alternative: Google Cloud Run

### Step 1: Setup Google Cloud
```bash
# Install Google Cloud CLI
# Then authenticate
gcloud auth login
gcloud config set project YOUR_PROJECT_ID
```

### Step 2: Build and Deploy
```bash
# Build the container
docker build -t gcr.io/YOUR_PROJECT_ID/microfluidica-app .

# Push to Google Container Registry
docker push gcr.io/YOUR_PROJECT_ID/microfluidica-app

# Deploy to Cloud Run
gcloud run deploy microfluidica-app \
  --image gcr.io/YOUR_PROJECT_ID/microfluidica-app \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated
```

## 🔍 Troubleshooting

### Common Issues:

1. **R Package Installation Fails:**
   - The Dockerfile now includes proper GitHub package installation via `devtools`
   - Packages `DNAr` and `DNArLogic` are installed from GitHub repositories
   - If installation fails, check the build logs for specific package errors
   - Ensure internet connectivity during build for GitHub package downloads

2. **Frontend Not Loading:**
   - Verify React build completed successfully
   - Check if static files are properly mounted

3. **API Endpoints Not Working:**
   - Check CORS configuration
   - Verify FastAPI server is running on correct port

4. **Memory Issues:**
   - Scientific simulations can be memory-intensive
   - Consider upgrading to a higher-tier plan

### Debug Commands:

```bash
# Check Docker logs
docker-compose logs -f

# Access container shell
docker-compose exec microfluidica-app bash

# Test R installation
docker-compose exec microfluidica-app R --version

# Test Python dependencies
docker-compose exec microfluidica-app python3 -c "import fastapi; print('FastAPI OK')"
```

## 🔒 Production Considerations

### Security:
1. **Update CORS origins** in `Python/subprocess_R.py`:
   ```python
   allow_origins=["https://yourdomain.com"]  # Replace with your domain
   ```

2. **Add authentication** if needed
3. **Use HTTPS** (most platforms provide this automatically)

### Performance:
1. **Monitor resource usage** - R simulations can be CPU/memory intensive
2. **Consider caching** for repeated simulations
3. **Optimize Docker image size** by using multi-stage builds

### Monitoring:
1. **Set up logging** for production
2. **Monitor application health** with health checks
3. **Set up alerts** for failures

## 📊 Cost Estimation

| Platform | Free Tier | Paid Plans |
|----------|-----------|------------|
| Railway | $5/month credit | Pay-per-use |
| Render | 750 hours/month | $7+/month |
| Google Cloud Run | 2M requests/month | $0.40/1M requests |

## 🎉 Success!

Once deployed, your microfluidic DNA droplets simulation tool will be accessible worldwide! 

**Next Steps:**
- Test all functionality in production
- Set up monitoring and alerts
- Consider adding user authentication
- Document the API for users

## 📞 Support

If you encounter issues:
1. Check the troubleshooting section above
2. Review platform-specific documentation
3. Check application logs for error messages
