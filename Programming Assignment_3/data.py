# Provides classes to be used in the main.py file.

class Node: 
    def __init__(self, node_id, x, z, status='unvisited', cost_so_far=float('inf'), # Set default values because the graphed nodes will not have these attributes
                 estimated_heuristic=float('inf'), estimated_total=float('inf'), 
                 previous_node=None, number_plot_pos=(0, 0), name_plot_pos=(0, 0), name=''):
        self.node_id = node_id
        self.status = status
        self.cost_so_far = cost_so_far
        self.estimated_heuristic = estimated_heuristic
        self.estimated_total = estimated_total
        self.previous_node = previous_node
        self.x = x
        self.z = z
        self.number_plot_pos = number_plot_pos
        self.name_plot_pos = name_plot_pos
        self.name = name


class Connection:
    def __init__(self, connection_id, from_node, to_node, cost, cost_plot_pos, ctype):
        self.connection_id = connection_id
        self.from_node = from_node
        self.to_node = to_node
        self.cost = cost
        self.cost_plot_pos = cost_plot_pos
        self.ctype = ctype


class Graph:
    def __init__(self):
        self.nodes = {}
        self.edges = {}

    def add_node(self, node):
        self.nodes[node.node_id] = node

    def add_edge(self, from_node, to_node, weight):
        if from_node in self.nodes and to_node in self.nodes:
            if from_node not in self.edges:
                self.edges[from_node] = {}
            self.edges[from_node][to_node] = weight

            if to_node not in self.edges:
                self.edges[to_node] = {}
            self.edges[to_node][from_node] = weight

# Function to load nodes from a file
def load_nodes(file_path):
    nodes = {}
    with open(file_path, 'r') as file:
        for line in file:
            if line.startswith("#") or not line.strip():
                continue  # Skip comments and empty lines
            parts = line.strip().split(',')
            if parts[0].strip('" ') == "N":
                node = Node(
                    node_id = int(parts[1]),
                    status = int(parts[2]),
                    cost_so_far = float(parts[3]),
                    estimated_heuristic = float(parts[4]),
                    estimated_total = float(parts[5]),
                    previous_node = int(parts[6]),
                    x = float(parts[7]),
                    z = float(parts[8]),
                    number_plot_pos = int(parts[9]),
                    name_plot_pos = int(parts[10]),
                    name=parts[11].strip('" ').replace("\\n", "\n").strip()
                )
                nodes[node.node_id] = node
    return nodes

# Function to load connections from a file
def load_connections(file_path):
    connections = {}
    with open(file_path, 'r') as file:
        for line in file:
            if line.startswith("#") or not line.strip():
                continue  # Skip comments and empty lines
            parts = line.strip().split(',')
            if parts[0].strip('" ') == "C":
                connection = Connection(
                    connection_id = int(parts[1]),
                    from_node = int(parts[2]),
                    to_node = int(parts[3]),
                    cost = float(parts[4]),
                    cost_plot_pos = int(parts[5]),
                    ctype = int(parts[6])
                )
                connections[connection.connection_id] = connection
    return connections