import networkx as nx
import itertools
import numpy as np
import DetourMatrix as dm
import os
import sys
import time
from tqdm import *

def main():
    #First argument in the command line gives the number of vertices of the graph
    n = int(sys.argv[1:][0])
    #Second argument gives the number of chords
    ch = int(sys.argv[1:][1])

    #Change Directories to suit your needs
    wDir = str(os.getcwd())
    wFile = "-color-connected-graph-C_"+str(n)+"_"+str(ch)+"/"
    FileExt = ".graphml"
    
    #Now we generate the edges that we'll consider
    #The set difference of the edgelists for K_n and C_n
    edgesWeNeed = list(
        set(nx.complete_graph(n).edges()) - set(nx.cycle_graph(n).edges())
    )

    #we want collections of the considered edges, such that no edge
    #is repeated in the sublist
    chords = list(
        itertools.combinations(edgesWeNeed,ch)
    )

    #We'll make a cycle graph just for reference
    g = nx.cycle_graph(n)
    #and we'll find the cycle graphs shortest,longest path
    #see maximum topological distances matrix for details
    cycleLength,p = dm.detour_matrix(g)

    #So that we can use map over the nested lists in chords
    addEdge = lambda (a,b) : g.add_edge(a,b)

    #Variable for indexing output
    q = 0

    #tqdm to keep track of progress
    for i in tqdm(chords):
        #graph instance to be modified
        g = nx.cycle_graph(n)
        #adding the chords to the graph
        map(addEdge,i)
        #the minimum detour number, or k
        ChordedCycleLength,paths = dm.detour_matrix(g)
        #if ChordedCycleLength is larger than the value k for C_n
        #we write the graph to a graphml format
        if ChordedCycleLength > cycleLength:
            #we'll make the directory for the value k if it doesn't exist
            if not os.path.exists(wDir+str(ChordedCycleLength)+wFile):
                os.makedirs(wDir+str(ChordedCycleLength)+wFile)
            q = q + 1
            nx.write_graphml(g,wDir+str(ChordedCycleLength)+wFile+str(q)+FileExt)
        else:
            continue

if __name__ == "__main__":
    main()
