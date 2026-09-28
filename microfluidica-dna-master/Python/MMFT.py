import matplotlib.pyplot as plt
from mmft.simulator import Network,Simulation, Platform, Type, ChannelType
from mmft.simulator import *
import networkx as nx
import matplotlib.pyplot as plt
import json
from typing import List, Dict
from collections import defaultdict

def find_goticulas_formed(data, times):
    goticulas_misturadas = data['goticulasMisturadas']
    formation_times = []

    for goticula in goticulas_misturadas:
        mix_times = goticula['tempoMistura']
        cumulative_time = sum(mix_times)
        formation_times.append({
            'id': goticula['id'],
            'formation_time': cumulative_time,
            'components': goticula['goticulasMisturadas']
        })

    # Sort formation_times by 'formation_time'
    formation_times.sort(key=lambda x: x['formation_time'])

    result = []

    for time in times:
        for formation in formation_times:
            if time <= formation['formation_time']:
                result.append({
                    'time': time,
                    'goticula_id': formation['id'],
                    'components': formation['components']
                })
                break
    print(result)
    return result

def find_new_droplet_times(simulation_data):
    initial_ids = set(droplet['id'] for droplet in simulation_data['network'][0]['bigDroplets'])
    previous_ids = initial_ids
    new_droplet_times = []

    for network_instance in simulation_data['network'][1:]:
        current_time = network_instance['time']
        current_droplets = network_instance['bigDroplets']

        current_ids = set(droplet['id'] for droplet in current_droplets)

        # Find new IDs that were not present in the previous time step
        new_ids = current_ids - previous_ids

        if new_ids:
            new_droplet_times.append(current_time)

        previous_ids = current_ids

    return new_droplet_times

def track_new_droplet_appearance(current_time, new_droplet, existing_droplets, used_components, node_connections):
    # Extract the new droplet's ID and boundaries
    new_droplet_id = new_droplet['id']
    new_droplet_boundaries = new_droplet['boundaries']
    
    # Find droplets that formed the new droplet based on boundaries
    components = []
    #print("existing droplets")
    #print(existing_droplets)
    for boundary in new_droplet_boundaries[:-1]:  # Excludes the last element
        node_with_channel = next(
            (node for node, channels in reversed(node_connections.items()) if channels and channels[0] == boundary['position']['channel']),
            None
        )
        print(node_with_channel)
        components.append(node_connections[node_with_channel][1])
        used_components.add(node_connections[node_with_channel][1])  # Mark the droplet as used

    #node_connections[new_droplet_boundaries[-1]['position']['channel']].append(new_droplet_boundaries[-1]['position']['channel'])
    print("Componentes")
    print(components)
    # Return the information about the new droplet and its components
    if new_droplet_id not in node_connections:
        node_connections[new_droplet_id] = []

    node_connections[new_droplet_id].append(new_droplet_boundaries[-1]['position']['channel'])
    node_connections[new_droplet_id].append(new_droplet_id)
    existing_droplets.append(new_droplet)
    return {
        'time': current_time,
        'goticula_id': new_droplet_id,
        'components': components
    }

def filter_droplets_by_components(droplets):
    filtered = []
    for droplet in droplets:
        # Check if this droplet's components overlap with an existing one
        to_remove = None
        for existing in filtered:
            if set(droplet['components']) & set(existing['components']):  # Check for overlap
                if len(droplet['components']) > len(existing['components']):
                    to_remove = existing  # Mark the smaller one for removal
                else:
                    to_remove = droplet  # The new one is smaller, so mark it
                break  # Stop checking after first match 
        # If an entry was found to be removed, remove it
        if to_remove:
            filtered = [d for d in filtered if d != to_remove]  # Remove the weaker entry
            if to_remove != droplet:
                filtered.append(droplet)  # Add the new one if it's the larger    
        else:
            filtered.append(droplet)  # If no conflict, add normally
    return filtered

def find_new_droplet_formed(simulation_data, node_connections):
    # Initialize the set of IDs from the first network instance
    initial_ids = set(droplet['id'] for droplet in simulation_data['network'][0]['bigDroplets'])
    previous_ids = initial_ids
    
    # This set will track which droplets have been used as components
    used_components = set()
    droplets_formed = []

    # Iterate through the network instances, starting from the second one
    for network_instance in simulation_data['network'][1:]:
        current_time = network_instance['time']
        current_droplets = network_instance['bigDroplets']

        # Get the set of IDs for the current droplets
        current_ids = set(droplet['id'] for droplet in current_droplets)

        # Find new IDs that were not present in the previous time step
        new_ids = current_ids - previous_ids   
        if new_ids:
            for new_id in new_ids:
                # Find the new droplet by its ID and track its components
                droplets_formed.append(track_new_droplet_appearance(
                    current_time,
                    next(droplet for droplet in current_droplets if droplet['id'] == new_id),
                    [cd for cd in current_droplets if cd['id'] in previous_ids],
                    used_components, # Pass the set of used components
                    node_connections  
                ))
        print(node_connections)
        # Update the previous IDs for the next iteration
        previous_ids = current_ids
    print("formed")
    print(filter_droplets_by_components(droplets_formed))
    return filter_droplets_by_components(droplets_formed)


def scale_distance(distance):
  return distance*5.27e-3

def get_height(distance, time):
  baseHeight = 1e-4 
  baseDistance = 5.27e-3
  return (baseHeight*baseDistance*time)/(2000*distance*baseDistance)

def simulation_from_graph(edges_list, edges, pos, node_times, data):
  network = Network()
  flowRate = 3e-15
  baseDistance = 5.27e-3
  nodes = {}
  channels = {}
  droplets = {}
  heights = {}
  edges_info = {}

  for node, position in pos.items():
    if(node == "Sink"):
      nodes[node] = network.addNode(scale_distance(position[0]), scale_distance(position[1]), True, True)
    else:
      nodes[node] = network.addNode(scale_distance(position[0]), scale_distance(position[1]), False)

  for edge in edges_list: 
    if(edge["end"] == "Sink"):
      channels[edge["start"]] = network.addChannel(nodes[edge["start"]], nodes[edge["end"]], 1e-4, 3e-5, ChannelType.normal)
    else:
      channels[edge["start"]] = network.addChannel(nodes[edge["start"]], nodes[edge["end"]], get_height(edge["length"],node_times[edge["start"]]), 3e-5, ChannelType.normal)
      heights[edge["start"]] = get_height(edge["length"],node_times[edge["start"]])

  for goticula in data['goticulasIniciais']:
    network.addFlowRatePump(nodes["Sink"], nodes[goticula['id']], flowRate)  # Adding flow rate pump

  network.sort()
  network.valid()

  simulation = Simulation()

  # Simulation meta-data
  simulation.setType(Type.abstract)
  simulation.setPlatform(Platform.bigDroplet)
  simulation.setNetwork(network)

  # Fluid & Resistance Model
  water = simulation.addFluid(1e3, 1e-3, 1.0)
  oil = simulation.addFluid(1e3, 3e-3, 1.0)
  simulation.setContinuousPhase(water)
  simulation.setRectangularResistanceModel()
  for edge, edge_true in zip(edges_list, edges):
    if(edge["end"] == "Sink"):
       edges_info[edge_true] = {"channel height": 1e-4, "distance": edge["length"]*baseDistance}
    else:
       edges_info[edge_true] = {"channel height": heights[edge["start"]], "distance": edge["length"]*baseDistance}
  
  for goticula in data['goticulasIniciais']:
    droplets[goticula['id']] = simulation.addDroplet(oil, 4.5e-13)
    simulation.injectDroplet(droplets[goticula['id']], 0.0, channels[goticula['id']], 0.5)
  
  simulation.simulate()
  simulation.saveResult("dropletAbst.JSON")
  return edges_info


def simulateMMFTWithNodeData(data=None):
    # Input JSON
    # Read JSON data from a file
    if data is None:
        with open('exemplo_3.json', 'r') as file:
            data = json.load(file)
    # Initialize the network
    network = Network()

    # Initialize positions, channels, and flow rate pump dictionaries
    positions = {}
    node_names = {}
    node_positions = {}
    channel_results = {}
    flow_rate_pumps = {}

    def scale_distance(distance):
        # Print the type and value of distance for debugging
        print(f"Original distance: {distance} (type: {type(distance)})")
        # Ensure distance is a numeric type (float)
        if isinstance(distance, (int, float)):
            return distance * 10e-6
        elif isinstance(distance, str):
            try:
                # Attempt to convert distance to float
                distance = float(distance)
                return distance * 10e-6
            except ValueError:
                # Handle the case where conversion fails
                print(f"Error: Cannot convert distance '{distance}' to float.")
                return None
        else:
            # Handle unexpected types
            print(f"Error: Invalid type for distance '{distance}'. Expected int, float, or str.")
            return None
    def scale_height(height):
        return height * 0.00001

    # Function to add node position and name
    def add_node_position(node_id, x, y, name, is_sink=False):
        positions[node_id] = (x, y)
        node_names[node_id] = name
        if is_sink:
            node_position = network.addNode(scale_distance(x), scale_distance(y), True, True)
            node_positions[node_id] = node_position
        else:
          node_position = network.addNode(scale_distance(x), scale_distance(y), False)
          node_positions[node_id] = node_position
        return node_position
    # Add nodes to the network

    # First, collect nodes with droplets
    nodes_with_droplets = set(g["pump"] for g in data.get("goticulasIniciais", []))

    # Separate nodes into two lists
    priority_nodes = [node for node in data["nodes"] if node["name"] in nodes_with_droplets]
    remaining_nodes = [node for node in data["nodes"] if node["name"] not in nodes_with_droplets]

    # Function to add nodes
    def process_nodes(nodes, start):
        for i, node in enumerate(nodes):
            x = node["x"]
            y = node["y"]
            name = node["name"]
            is_sink = node.get("isSink", False)  # Default to False if not specified

            # Add node to the network and store its position
            print(start+i, name)
            add_node_position(start+i, x, y, name, is_sink)

    # Process nodes in order
    process_nodes(priority_nodes, 0)
    process_nodes(remaining_nodes, len(priority_nodes))

    # Function to add a channel between nodes
    def add_channel(channel_id, from_node, to_node, height):
        channel = network.addChannel(from_node, to_node, height, 3e-5, ChannelType.normal)
        channel_results[channel_id] = channel
        return channel

    print('addedNodes')
    # Add edges (channels) to the network
    # Initialize a dictionary to store connections for each node
    node_connections = {node_name: [] for node_name in node_names.values()}
    sink_node = next(node["name"] for node in data["nodes"] if node.get("isSink", False))

    for i, edge in enumerate(data["edges"]):
        from_name = edge["from"]["name"]
        to_name = edge["to"]["name"]
        height = scale_height(float(edge["height"]))
        
        # Find corresponding node positions
        node1 = node_positions[list(node_names.keys())[list(node_names.values()).index(from_name)]]
        node2 = node_positions[list(node_names.keys())[list(node_names.values()).index(to_name)]]
        
        # Add the edge (channel) to the network and store it in channel_results
        print(add_channel(i, node1, node2, height))
        droplet_id = next((goticula["id"] for goticula in data["goticulasIniciais"] if goticula["pump"] == from_name), None)
        
        # Update the connections dictionary
        node_connections[from_name].append(i)
        node_connections[from_name].append(node1)
    
    # Function to add a flow rate pump
    def add_flow_rate_pump(pump_name, sink_name, flow_rate):
        start_node = node_positions[list(node_names.keys())[list(node_names.values()).index(pump_name)]]
        end_node = node_positions[list(node_names.keys())[list(node_names.values()).index(sink_name)]]
        pump = network.addFlowRatePump(end_node, start_node, flow_rate)
        flow_rate_pumps[(pump_name, sink_name)] = pump
        return pump

    # Add flow rate pumps
    flow_rate = 3e-16
    for goticula in data["goticulasIniciais"]:
        pump_name = goticula["pump"]
        
        # Find the sink node
        sink_node = next(node["name"] for node in data["nodes"] if node.get("isSink", False))
        
        # Add the flow rate pump
        print(add_flow_rate_pump(pump_name, sink_node, flow_rate))

    #print(data["goticulasIniciais"])
    # The network now contains all the nodes, channels, and flow rate pumps added
    print("Validating Network")
    network.sort()
    network.valid()
    simulation = Simulation()

    # Simulation meta-data
    simulation.setType(Type.abstract)
    simulation.setPlatform(Platform.bigDroplet)
    simulation.setNetwork(network)

    # Fluid & Resistance Model
    water = simulation.addFluid(1e3, 1e-3, 1.0)
    oil = simulation.addFluid(1e3, 3e-3, 1.0)
    simulation.setContinuousPhase(water)
    simulation.setRectangularResistanceModel()
    print('Validated')
    goticulas = {}
    # Create and inject droplets

    def create_and_inject_droplets():
        volume = 4.5e-13
        print(data["goticulasIniciais"])
        for i, goticula in enumerate(data["goticulasIniciais"]):
            droplet_name = goticula["nome"]
            pump_node_name = goticula["pump"]
            print('PUMP_NODE')
            print(pump_node_name)
            print(node_names)

            if droplet_name not in goticulas:
                goticulas[droplet_name] = {"volume": []}
            goticulas[droplet_name]["volume"].append(volume)

            # Create a droplet
            droplet = simulation.addDroplet(oil, volume)

            # Find the pump node position
            pump_node = node_positions[list(node_names.keys())[list(node_names.values()).index(pump_node_name)]]
            print('nó')
            insert_channel = node_connections[pump_node_name][0]
            print("wtf")
            print(insert_channel)
            print(pump_node)
            # Inject the droplet into the pump node
            simulation.injectDroplet(droplet, 0.1, insert_channel, 0.25)
            print('endDebughaha')

    print('endDebug4')
    # Execute the droplet creation and injection
    create_and_inject_droplets()
    simulation.simulate()
    simulation.saveResult("dropletAbst.JSON")
    simulation_data = {}
    with open('dropletAbst.JSON', 'r') as file:
        simulation_data = json.load(file)
    new_droplet_times = find_new_droplet_times(simulation_data)

    # Print the times
    #print("Times when new bigDroplets appear (ignoring initial droplets):", new_droplet_times)
    reactions = {}
    return {'simulationResult':simulation_data, 'dropletInjectionTimes':find_new_droplet_formed(simulation_data, node_connections), 'data': data, 'simulation_time': simulation_data["network"][-1]["time"], 'reactions': reactions}

def simulateMMFT(data=None):
    # Input JSON
    # Read JSON data from a file
    if data is None:
        with open('exemplo_1.json', 'r') as file:
            data = json.load(file)

    # Initialize positions dictionary and list of nodes
    node_times = {}

    # Create a directed graph
    G = nx.DiGraph()

    # Add nodes for "goticulasIniciais"
    for goticula in data["goticulasIniciais"]:
        G.add_node(goticula["id"], label=goticula["nome"])

    # Add nodes and edges for "goticulasMisturadas"
    for goticula in data["goticulasMisturadas"]:
        G.add_node(goticula["id"], label=goticula["nome"])
        for i, mixed_id in enumerate(goticula["goticulasMisturadas"]):
            G.add_edge(mixed_id, goticula["id"])
            node_times[mixed_id] = goticula["tempoMistura"][i]

    # Add a final sink node
    sink_node = 'Sink'
    G.add_node(sink_node, label='Final Sink')

    # Connect nodes with no outgoing edges to the final sink node
    for node in G.nodes:
        if G.out_degree(node) == 0 and node != sink_node:
            G.add_edge(node, sink_node)

    # Draw the updated graph
    pos = nx.spring_layout(G)
    labels = nx.get_node_attributes(G, 'label')

    # Print node positions
    print("Node positions:")
    for node, position in pos.items():
        print(f"Node {node}: Position {position}")

    # Print edge lengths
    edges_list = []
    for edge in G.edges:
        start, end = edge
        length = ((pos[start][0] - pos[end][0]) ** 2 + (pos[start][1] - pos[end][1]) ** 2) ** 0.5
        edges_list.append({'start': start, 'end': end, 'length': length})

    # Print the list of edges with lengths

    edges_info = simulation_from_graph(edges_list, G.edges, pos, node_times, data)

    # Plot the graph
    plt.figure(figsize=(12, 6))
    nx.draw(G, pos, with_labels=True, labels=labels, node_size=2000, node_color='skyblue', font_size=10, font_weight='bold', arrowsize=20)

    # Annotate edges with distance and channel height
    edge_labels = {}
    for edge in edges_info:
        print(edges_info[edge])
        edge_labels[edge] = f"D: {edges_info[edge]['distance']:.5f} \n H: {edges_info[edge]['channel height']:.5f}"

    print(edge_labels)
    nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels, font_color='red', font_size=8)

    plt.title('Goticulas Graph with Final Sink')
    plt.show()
    plt.savefig('plot.png')

    # Load the JSON data from the file
    with open('dropletAbst.JSON', 'r') as file:
        simulation_data = json.load(file)

    # Find the times when new droplets appear, ignoring the initial droplets
    new_droplet_times = find_new_droplet_times(simulation_data)

    # Print the times
    print("Times when new bigDroplets appear (ignoring initial droplets):", new_droplet_times)
    reactions = {}
    return {'simulationResult':'dropletAbst.JSON', 'dropletInjectionTimes':find_goticulas_formed(data, new_droplet_times), 'data': data, 'simulation_time': simulation_data["network"][-1]["time"], 'reactions': reactions}

simulateMMFTWithNodeData()