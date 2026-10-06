FROM dnar-dev

ENV DEBIAN_FRONTEND=noninteractive
ENV PYTHONPATH=/app
ENV R_HOME=/usr/local/lib/R
ENV LD_LIBRARY_PATH=/usr/local/lib/R/lib

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 \
    python3-pip \
    docker.io \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY microfluidica-dna-simulation/backend/requirements.txt /tmp/requirements.txt
RUN python3 -m pip install --no-cache-dir -r /tmp/requirements.txt
COPY microfluidica-dna-simulation/backend /app/backend
COPY microfluidica-dna-simulation/frontend/studio/dist /app/frontend/studio/dist

RUN useradd -m -u 1000 appuser || true \
    && mkdir -p /app/.runs \
    && chown -R appuser:appuser /app
USER appuser

CMD ["python3", "-m", "uvicorn", "backend.api:app", "--host", "0.0.0.0", "--port", "8000"]
