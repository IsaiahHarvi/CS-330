# Author: Isaiah Harville
# Purpose: Implementation of the A* Pathfinding algorithm
# Date 11/9/2023

import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import numpy as np
from data import *
from queue import PriorityQueue

# Load the data from text files using functions in data.py
nodes = load_nodes('Programming Assignment_3/nodes.txt')
connections = load_connections('Programming Assignment_3/connections.txt')

# Define the graph from data.py
G = Graph()

# Add nodes to the graph
for node_id, node_info in nodes.items():
    G.add_node(Node(node_id, node_info.x, node_info.z))

# Add edges to the graph
for connection_id, connection_info in connections.items():
    G.add_edge(connection_info.from_node, connection_info.to_node, connection_info.cost)

source = 1
target = 66

# A* algorithm
def astar_path(graph, start, end, heuristic):
    open_set = PriorityQueue()
    open_set.put((0, start))
    came_from = {}
    g_score = {node: float("inf") for node in graph.nodes}
    g_score[start] = 0
    f_score = {node: float("inf") for node in graph.nodes}
    f_score[start] = heuristic(graph.nodes[start], graph.nodes[end])

    open_set_hash = {start}

    while not open_set.empty():
        current = open_set.get()[1]
        open_set_hash.remove(current)

        if current == end:
            # Reconstruct path
            path = []
            while current in came_from:
                current = came_from[current]
                path.append(current)
            path.append(start)
            return path[::-1]  # Return reversed path

        for neighbor in graph.edges[current]:
            temp_g_score = g_score[current] + graph.edges[current][neighbor]
            if temp_g_score < g_score[neighbor]:
                came_from[neighbor] = current
                g_score[neighbor] = temp_g_score
                f_score[neighbor] = temp_g_score + heuristic(graph.nodes[neighbor], graph.nodes[end])
                if neighbor not in open_set_hash:
                    open_set.put((f_score[neighbor], neighbor))
                    open_set_hash.add(neighbor)

    return False  # No path was found

# Euclidean distance heuristic
def euclidean_distance(a, b):
    # Extract the x and z attributes from the Nodes
    return np.sqrt((a.x - b.x) ** 2 + (a.z - b.z) ** 2)

path = astar_path(G, source, target, euclidean_distance)

# Plotting the graph
def plot_graph(nodes, connections, path=None):
    # Create a scatter plot for the nodes
    for node_id, node_info in nodes.items():
        plt.scatter(node_info.x, node_info.z, c='blue')
        plt.text(node_info.x, node_info.z, str(node_id), fontsize=9)

    # Create a line plot for the edges
    for connection_id, connection in connections.items():
        node_start = nodes[connection.from_node]
        node_end = nodes[connection.to_node]
        plt.plot([node_start.x, node_end.x], [node_start.z, node_end.z], c='black')

    # Highlight the path
    if path:
        for i in range(len(path) - 1):
            node_start = nodes[path[i]]
            node_end = nodes[path[i + 1]]
            plt.plot([node_start.x, node_end.x], [node_start.z, node_end.z], c='red', linewidth=2)

    legend = [mlines.Line2D([0], [0], color='blue', marker='o', markersize=5, label='Node'),
            mlines.Line2D([0], [0], color='black', lw=1, label='Connection'),
            mlines.Line2D([0], [0], color='red', lw=2, label='Path')]
   
    plt.legend(handles=legend)
    plt.axis('equal')
    plt.axis('off')
    plt.show()

# Call the function with the nodes, connections, and the path
plot_graph(nodes, connections, path=path)
