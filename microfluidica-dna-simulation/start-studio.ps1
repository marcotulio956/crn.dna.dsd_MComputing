param([int]$Port = 8000, [switch]$Build)
$ErrorActionPreference = 'Stop'
$projectPath = $PSScriptRoot
Set-Location -LiteralPath $projectPath
if (!(Test-Path -LiteralPath '.venv/Scripts/python.exe')) {
    & py -3.11 -m venv .venv
    if ($LASTEXITCODE -ne 0) { throw 'Install Python 3.11 or create .venv manually.' }
    & .venv/Scripts/python.exe -m pip install -r backend/requirements.txt
    if ($LASTEXITCODE -ne 0) { throw 'Python dependency installation failed.' }
}
& docker info --format '{{.ServerVersion}}'
if ($LASTEXITCODE -ne 0) { throw 'Start Docker Desktop (Linux containers) first.' }
if ($Build) {
    & docker build -f Dockerfile.engine -t fluidna-engine:v2 .
    if ($LASTEXITCODE -ne 0) { throw 'MMFT/DNAr image build failed.' }
    Push-Location -LiteralPath 'frontend/studio'
    try {
        & npm.cmd ci
        if ($LASTEXITCODE -ne 0) { throw 'Frontend dependency installation failed. Use Node 22.12+.' }
        & npm.cmd run build
        if ($LASTEXITCODE -ne 0) { throw 'Frontend build failed.' }
    } finally { Pop-Location }
}
if (!(Test-Path -LiteralPath 'frontend/studio/dist/index.html')) { throw 'Build first: ./start-studio.ps1 -Build' }
& docker image inspect fluidna-engine:v2 --format '{{.Id}}'
if ($LASTEXITCODE -ne 0) { throw 'Engine image missing. Run ./start-studio.ps1 -Build' }
Write-Host "fluiDNA Studio: http://127.0.0.1:$Port"
Write-Host 'Keep this terminal open. Ctrl+C stops the service. Original interface is unchanged.'
& .venv/Scripts/python.exe -m uvicorn backend.api:app --host 127.0.0.1 --port $Port --workers 1
