from functools import reduce
from operator import itemgetter
import csv
import networkx as nx
import operator
import community

data_dir = '../data/'
ldir = data_dir + "lookup/"
ddir = data_dir + "2019-05-23-ascher-bee-data/"
fn_nodes = ddir + \
    '2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv'
fn_edges = ddir + \
    '2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_7.0-author-networks.csv' 
fn_auth = ddir + \
    '2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv'
fn_statoids = ldir + \
    '2019-05-29-statoid-country-codes.csv'


def test():
    print("Hello World!")


def load_coauthor_nx():
    # dataset includes valid and invalid species

    with open(fn_edges, 'r', encoding='utf-8') as edgecsv: # Open the file
        edgereader = csv.reader(edgecsv) # Read the csv
        edges = [(e[1], e[2], e[0]) for e in edgereader if e[2] != ""][1:]

    edges = [(e[0], e[1], float(e[2])) for e in edges]


    with open(fn_nodes, 'r', encoding='utf-8') as nodecsv: # Open the file                       
        nodereader = csv.reader(nodecsv) # Read the csv  
        # Retrieve the data (using Python list comprhension and 
        # list slicing to remove the header row, see footnote 3)
        nodes = [n for n in nodereader][1:]

    all_node_names = []
    all_node_names = [all_node_names + [e[0], e[1]] for e in edges]
    all_node_names = reduce(operator.add, all_node_names)
    all_node_names = set(all_node_names); len(all_node_names)              

    node_names = [n[1] for n in nodes if n[1] in all_node_names] # Get a list of only the node names
    not_node_names = [n[1] for n in nodes if not(n[1] in all_node_names)] # Get a list of only the node names
    print("N nodes", len(node_names), "; N not nodes:", len(not_node_names))
    print("Proportion who did not coauthor", len(not_node_names)/len(node_names+not_node_names)*100)

    G = nx.Graph()

    G.add_nodes_from(node_names)
    G.add_weighted_edges_from(edges)

    print(nx.info(G))

    res_dict = {}
    n_spp_dict = {}
    for node in nodes: # Loop through the list, one row at a time
        res_dict[node[1]] = str.split(node[7], "; ")[0]
        n_spp_dict[node[1]] = int(node[11])
 
    # Add attributes
    nx.set_node_attributes(G, res_dict, 'country_of_residence') # Country of residence
    nx.set_node_attributes(G, n_spp_dict, 'ns_spp_n') # Number of species (valid)

    degree_dict = dict(G.degree(G.nodes())) # Degree
    nx.set_node_attributes(G, degree_dict, 'degree')

    betweenness_dict = nx.betweenness_centrality(G) # Betweenness centrality
    nx.set_node_attributes(G, betweenness_dict, 'betweenness')
    eigenvector_dict = nx.eigenvector_centrality(G) # Eigenvector centrality
    nx.set_node_attributes(G, eigenvector_dict, 'eigenvector')

    return (G, nodes)