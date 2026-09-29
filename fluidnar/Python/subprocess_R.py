import subprocess
import json
from MMFT import simulateMMFT, simulateMMFTWithNodeData
from rpy2.robjects import pandas2ri
from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse, FileResponse
from fastapi.staticfiles import StaticFiles
import base64
from collections import defaultdict
from fastapi.middleware.cors import CORSMiddleware
from pdf2image import convert_from_path
import os
import glob
import numpy as np
from collections import defaultdict
from simulation_config import SimulationConfigError, has_cycle, normalize_request


app = FastAPI()

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # In production, replace with your domain
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Mount static files for React build
try:
    app.mount("/static", StaticFiles(directory="/app/frontend/new-interface/build/static"), name="static")
except:
    pass  # Ignore if build directory doesn't exist yet




BASE_DIR = os.path.dirname(os.path.abspath(__file__))
DNAR_DIR = os.path.abspath(os.path.join(BASE_DIR, "..", "DNAr"))
RESULT_TABLES_PATH = os.path.join(BASE_DIR, "tabelas.json")


def build_timing_report(data):
    timing = data.get("timing") or {}
    nodes = [node.get("name") for node in data.get("nodes", []) if node.get("name")]
    cycle_detected = has_cycle(nodes, data.get("edges", []))
    return {
        "enabled": bool(timing.get("enabled", False)),
        "cycleDetected": cycle_detected,
        "mode": "explicit-fallback" if cycle_detected else "open-loop",
        "tolerancePercent": timing.get("tolerancePercent"),
        "stableSamples": timing.get("stableSamples"),
        "warnings": [
            "Feedback cycle detected; automatic settling timing was not applied."
        ] if cycle_detected else [],
    }


def run_R(data, new):
    # Assuming simulateMMFT and pandas2ri are defined and imported correctly
    data = normalize_request(data)
    normalized_data = data
    timing_report = build_timing_report(data)
    print("Chama")
    pandas2ri.activate()
    print("Executando MMFT")
    if new:
        result = simulateMMFTWithNodeData(data)
    else:
        result = simulateMMFT(data)
    print("MMFT finalizado")
    simulation_result = result['simulationResult']
    droplet_injection_times = json.dumps(result['dropletInjectionTimes'])
    data_payload = json.dumps(normalized_data)
    print(data_payload)
    simulation_time = str(result['simulation_time'])
    reactions = json.dumps(result['reactions'])
    r_script_path = os.path.join(DNAR_DIR, "process_inputs.R")
    work_dir = os.getenv("FLUIDNAR_WORKDIR", BASE_DIR)
    dnar_root = os.getenv("DNAR_ROOT", "/home/mark/dnar")
    print("Executando DNAr")
    try:
        completed = subprocess.run([
            "Rscript", r_script_path,
            '', droplet_injection_times, data_payload, simulation_time, reactions
        ], check=True, cwd=work_dir, env={**os.environ, "DNAR_ROOT": dnar_root},
           capture_output=True, text=True)
        if completed.stdout:
            print(completed.stdout)
        if completed.stderr:
            print(completed.stderr)
        print("DNAr finalizado")
        return simulation_result, droplet_injection_times, timing_report, normalized_data
    except subprocess.CalledProcessError as e:
        details = (e.stderr or e.stdout or "").strip()
        message = f"DNAr process failed with exit code {e.returncode}"
        if details:
            message = f"{message}: {details[-2000:]}"
        raise RuntimeError(message) from e


def encode_image_to_base64(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')


@app.post("/submit_MMFT_New")
async def run_python_new(request: Request):
    try:
        print("=" * 80)
        print("HTTP Request Received at /submit_MMFT_New")
        print("=" * 80)
        data = await request.json()
        print(f"Request data: {json.dumps(data, indent=2)}")
        print("Calling simulateMMFTWithNodeData...")
        
        # Wrap simulation in try-catch to prevent container crash
        try:
            simulationResult, droplet_injection_times, timing_report, normalized_data = run_R(data, True)
            print("simulateMMFTWithNodeData completed successfully")
        except ValueError as e:
            # Input validation errors - user's fault
            error_msg = str(e)
            print(f"Validation error: {error_msg}")
            return JSONResponse(
                content={
                    "error": "Invalid Input",
                    "message": error_msg,
                    "suggestion": "Please check your network design (nodes, channels, and droplets)."
                },
                status_code=400
            )
        except RuntimeError as e:
            # Simulation execution errors
            error_msg = str(e)
            print(f"Runtime error: {error_msg}")
            
            # Provide specific suggestions based on error
            suggestion = "Check your network topology and try again."
            if "validation" in error_msg.lower():
                suggestion = "Make sure all nodes are connected and the network forms valid paths to the sink."
            elif "inject" in error_msg.lower():
                suggestion = "Check that all droplet pump nodes are connected to channels."
            elif "simulation failed" in error_msg.lower():
                suggestion = "Try simplifying your network or reducing the number of droplets."
                
            return JSONResponse(
                content={
                    "error": "Simulation Error",
                    "message": error_msg,
                    "suggestion": suggestion
                },
                status_code=500
            )
        except subprocess.CalledProcessError as e:
            # R script execution failed
            print(f"R script execution failed: {e}")
            return JSONResponse(
                content={
                    "error": "Chemical Simulation Failed",
                    "message": "DNAr simulation encountered an error.",
                    "suggestion": "Check your reaction definitions and species concentrations."
                },
                status_code=500
            )
        except Exception as e:
            # Unexpected errors
            error_msg = str(e)
            print(f"Unexpected error: {error_msg}")
            import traceback
            traceback.print_exc()
            return JSONResponse(
                content={
                    "error": "Unexpected Error",
                    "message": error_msg,
                    "suggestion": "An unexpected error occurred. Please try again or contact support."
                },
                status_code=500
            )

        print("Processando imagens")
        # Convert RPlots.pdf to PNG images
        #images = convert_from_path('Rplots.pdf')
        images = []
        image_b64_list = []
        # Remove any existing PNG files
        for png_file in glob.glob("RPlots_page_*.png"):
            os.remove(png_file)

        for i, image in enumerate(images):
            image_filename = f"RPlots_page_{i + 1}.png"
            image.save(image_filename, 'PNG')
            image_b64_list.append(encode_image_to_base64(image_filename))
        tabelasDNA = []


        with open(RESULT_TABLES_PATH, 'r') as file:
            tabelasDNA = json.load(file)

        # Build master time axis
        all_times = np.array(sorted({row["time"] for table in tabelasDNA for row in table}), dtype=float)
        global_max_time = all_times.max()

        final_table = [{"time": t} for t in all_times]
        final_sums = defaultdict(lambda: np.zeros(len(all_times)))

        for table in tabelasDNA:
            times = np.array([row["time"] for row in table], dtype=float)
            keys = [k for k in table[0] if k != "time"]

            for key in keys:
                values = np.array([row.get(key, 0.0) for row in table], dtype=float)

                # Determine mask: last curve is inclusive at end, others exclusive
                if times.max() == global_max_time:
                    mask = (all_times >= times.min()) & (all_times <= times.max())
                else:
                    mask = (all_times >= times.min()) & (all_times < times.max())

                interp_values = np.zeros(len(all_times), dtype=float)
                interp_values[mask] = np.interp(all_times[mask], times, values)

                final_sums[key] += interp_values

        # Build final table
        for i, t in enumerate(all_times):
            for key, arr in final_sums.items():
                final_table[i][key] = arr[i]

        # Append the merged table as the last element
        tabelasDNA.append(final_table)


        # Return the images
        response_data = {
            "pdf_images": image_b64_list,
            "simulation_result": simulationResult,
            "tabelas_DNA": tabelasDNA,
            "droplet_injection_times": droplet_injection_times,
            "simulation_config": normalized_data.get("simulation", {}),
            "timing_report": timing_report,
        }

        print("=" * 80)
        print("Request completed successfully!")
        print("=" * 80)
        return JSONResponse(content=response_data)
    except Exception as e:
        print("=" * 80)
        print(f"ERROR occurred: {str(e)}")
        print(f"Error type: {type(e).__name__}")
        import traceback
        print(f"Traceback:\n{traceback.format_exc()}")
        print("=" * 80)
        return JSONResponse(content={"error": str(e)}, status_code=500)


@app.post("/submit_MMFT")
async def run_python(request: Request):
    try:
        print("HTTP Request Feito")
        data = await request.json()
        run_R(data)

        print("Processando imagens")
        # Convert RPlots.pdf to PNG images
        #images = convert_from_path('Rplots.pdf')
        images = []
        image_b64_list = []

        for i, image in enumerate(images):
            image_filename = f"RPlots_page_{i + 1}.png"
            image.save(image_filename, 'PNG')
            image_b64_list.append(encode_image_to_base64(image_filename))

        # Encode plot.png to base64
        plot_image_b64 = encode_image_to_base64("plot.png")

        # Return the images
        response_data = {
            "plot_image": plot_image_b64,
            "pdf_images": image_b64_list
        }

        return JSONResponse(content=response_data)

    except Exception as e:
        return JSONResponse(content={"error": str(e)}, status_code=500)


@app.get("/")
async def serve_frontend():
    """Serve the React frontend"""
    try:
        return FileResponse("/app/frontend/new-interface/build/index.html")
    except:
        return {"message": "Frontend not built yet. Please run 'npm run build' in the frontend/new-interface directory."}


@app.get("/health")
async def health_check():
    return {"status": "ok", "dnar_root": os.getenv("DNAR_ROOT", "/home/mark/dnar")}


@app.get("/health")
async def health_check():
    """Health check endpoint for monitoring"""
    return {"status": "healthy", "service": "microfluidica-dna-simulation"}
