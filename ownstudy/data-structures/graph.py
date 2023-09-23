# Graph
    # - stores data points called vertices (nodes) and edges (edges)
    # - an edge connects 2 nodes together
    # - references taken from here: https://www.geeksforgeeks.org/generate-graph-using-dictionary-python/ and from here: https://www.codingninjas.com/studio/library/implementation-of-graph-in-python

def generate_edge_array(graph_str_file_name:str):
    edge_array = []
    fhand = open(graph_str_file_name, "r")
    for line in fhand:
        element_array = line.split("->")
        temp_tuple = (element_array[0].strip(), element_array[1].strip())
        edge_array.append(temp_tuple)
    return edge_array

def generate_graph_dict(graph_str_file_name:str):
    graph_dict = {}
    fhand = open(graph_str_file_name, "r")
    for line in fhand:
        element_array = line.split("->")
        graph_dict[element_array[0].strip()] = graph_dict.get(element_array[0].strip(), []) + [element_array[1].strip()]
    return graph_dict

class Graph:

    def __init__(self, parsed_edge_array, parsed_graph_dict):
        self.edge_array:list(tuple) = parsed_edge_array
        self.graph_dict:dict() = parsed_graph_dict
        print("A graph has been instantiated.")

    def dfs_path_finding(self, start_node, end_node, path_array=[]):
    # determines what nodes need to be travelled through to reach the end node from the start node, appends those nodes to an array
    # uses recursion to handle the searching through of each subsequent node by implementing default behaviour for each node, a break condition, and the actual line that causes recursion

        # iterated upon method recursion
        path_array += [start_node]

        # break condition
        if start_node == end_node:
            return path_array

        # actual recursion model
        for current_node in self.graph_dict[start_node]:
            if current_node not in path_array:
                new_path_array = self.dfs_path_finding(current_node, end_node, path_array)
                if new_path_array:
                    return new_path_array

    def shortest_path_finding(self, start_node, end_node, path_array=[]):
        path_array += [start_node]

        # break condition
        if start_node == end_node:
            return path_array

        shortest_length = None
        for current_node in self.graph_dict[start_node]:
            if current_node not in path_array:
                new_path_array = self.shortest_path_finding(start_node, end_node, path_array)
                if new_path_array:
                    if not shortest_length or len(new_path_array) < len(shortest_length):
                        shortest_length = new_path_array
        return shortest_length

# ----------

# example graph with the following nodes (a,b,c,d,e) and edges (connecting the nodes) in the graph.txt file
    # a -> c
    # b -> c
    # b -> e
    # c -> a
    # c -> b
    # c -> d
    # c -> e
    # d -> c
    # e -> c
    # e -> b

# testing the graph

g1 = Graph(generate_edge_array("graph.txt"), generate_graph_dict("graph.txt"))
print(g1.edge_array)
print(g1.graph_dict)
print(f'Path finding: {g1.dfs_path_finding("d","c")}')
print(f'Shortest path finding: {g1.shortest_path_finding("d","c")}')
