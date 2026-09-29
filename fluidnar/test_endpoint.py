import requests
import json

# Test data - minimal example
test_data = {
    "goticulasIniciais": [
        {
            "id": 0,
            "nome": "Droplet 1",
            "pump": "Node1",
            "especies": [
                {"nome": "Species 1", "concentracaoInicial": 1.0}
            ]
        }
    ],
    "nodes": [
        {"name": "Node1", "x": 0, "y": 0, "isSink": False},
        {"name": "Node2", "x": 100, "y": 0, "isSink": False},
        {"name": "Sink", "x": 200, "y": 0, "isSink": True}
    ],
    "reacoes": [
        {
            "reacao": "Species 1 -> Species 2",
            "rate": 0.0028
        }
    ],
    "edges": [
        {
            "from": {"name": "Node1", "x": 0, "y": 0},
            "to": {"name": "Node2", "x": 100, "y": 0},
            "height": 1.0
        },
        {
            "from": {"name": "Node2", "x": 100, "y": 0},
            "to": {"name": "Sink", "x": 200, "y": 0},
            "height": 1.0
        }
    ]
}

print("=" * 80)
print("Testing /submit_MMFT_New endpoint")
print("=" * 80)

# Test health endpoint first
try:
    print("\n1. Testing health endpoint...")
    response = requests.get("http://localhost:8000/health")
    print(f"Status Code: {response.status_code}")
    print(f"Response: {response.json()}")
    print("✓ Health check passed!")
except Exception as e:
    print(f"✗ Health check failed: {e}")
    exit(1)

# Test the main endpoint
try:
    print("\n2. Testing /submit_MMFT_New endpoint...")
    print(f"Sending data:\n{json.dumps(test_data, indent=2)}")
    
    response = requests.post(
        "http://localhost:8000/submit_MMFT_New",
        json=test_data,
        timeout=60
    )
    
    print(f"\nStatus Code: {response.status_code}")
    
    if response.status_code == 200:
        print("✓ Request successful!")
        result = response.json()
        print(f"Response keys: {list(result.keys())}")
    else:
        print("✗ Request failed!")
        print(f"Error: {response.text}")
        
except requests.exceptions.Timeout:
    print("✗ Request timed out (took longer than 60 seconds)")
except Exception as e:
    print(f"✗ Error: {e}")

print("\n" + "=" * 80)

