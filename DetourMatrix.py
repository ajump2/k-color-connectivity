import networkx as nx
import itertools
import numpy as np

def assign_to_matrix(matrix,pathLen,vertices):
    for (x,y) in vertices:
        matrix[x,y] = pathLen[(x,y)]
        matrix[y,x] = pathLen[(x,y)]
        matrix[x,x] = np.nan
        matrix[y,y] = np.nan
    return matrix
    
def detour_matrix(g):
    nodes = g.nodes()
    numOfNodes = len(list(nodes))
    
    detourMatrix = np.zeros(numOfNodes**2).reshape(numOfNodes,numOfNodes)
    #flatten = lambda l: [item for sublist in l for item in sublist]
    vertexPairs = list(itertools.combinations(nodes,2))

    paths = lambda (x,y) : list(nx.all_simple_paths(g,source=x,target=y))
    lengthOfPath = lambda a : len(a) - 1
    AllPaths = {x : paths(x) for x in vertexPairs}
    DictOfPaths = {x : map(lengthOfPath,y) for x,y in AllPaths.iteritems()}
    maxPathLength = lambda a : max(DictOfPaths[a])
    DictOfLen = {x : maxPathLength(x) for x in vertexPairs}
    detourMatrix = assign_to_matrix(detourMatrix,DictOfLen,vertexPairs)
    
    return int(np.nanmin(detourMatrix)),AllPaths
