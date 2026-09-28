import subprocess
import json
from MMFT import simulateMMFT, simulateMMFTWithNodeData
from rpy2.robjects import pandas2ri
from fastapi import FastAPI, Request
from fastapi.responses import FileResponse, JSONResponse
import base64
from collections import defaultdict
from fastapi.middleware.cors import CORSMiddleware
from pdf2image import convert_from_path
import os
import glob
import numpy as np
from collections import defaultdict


app = FastAPI()

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3005"],  # Adjust this to match
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)




def run_R(data, new):
    # Assuming simulateMMFT and pandas2ri are defined and imported correctly
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
    data = json.dumps(result['data'])
    print(data)
    simulation_time = str(result['simulation_time'])
    reactions = json.dumps(result['reactions'])
    r_script_path = '../DNAr/process_inputs.R'
    print("Executando DNAr")
    try:
        subprocess.run([
            "Rscript", r_script_path,
            '', droplet_injection_times, data, simulation_time, reactions
        ], check=True)
        print("DNAr finalizado")
        return simulation_result, droplet_injection_times
    except subprocess.CalledProcessError as e:
        print(f"An error occurred while running the R script: {e}")


def encode_image_to_base64(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')


@app.post("/submit_MMFT_New")
async def run_python_new(request: Request):
    try:
        print("HTTP Request Feito")
        data = await request.json()
        simulationResult, droplet_injection_times = run_R(data, True)

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


        with open('tabelas.json', 'r') as file:
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
        }

        return JSONResponse(content=response_data)
    except Exception as e:
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
