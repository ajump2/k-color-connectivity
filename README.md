library(devtools)
install_github("muschellij2/latexreadme")
library(latexreadme)

# Introduction
Contains some initial work on edge colorings for detour length paths. Thus far, we successfully check for $k$-color connectivity, determine minimum detour numbers, and view chord additions as permutations of distance between chord incident vertices. Thus far, and algorithmic approach to minimally $k$-color connecting a graph has not been implemented, and the proposed solution cannot be determined to halt unless the $k$-color connection number of any graph is less than $2k-1$.

A number of intractable problems arise as we begin exploring this topic. Rainbow-connection numbers (Li, Sun 2012) have been determined to be NP-Hard, likewise the computation of detour matrices (Weisstein, Eric W. "Detour Matrix." From MathWorld--A Wolfram Web Resource. http://mathworld.wolfram.com/DetourMatrix.html) has no efficient algorithm; the problem of $k$-color connecting a graph relies on both rainbow-connections, and minimum detour numbers. Regarding the programs, the wolfram package makes use of a graph isomorphism module which separates chorded cycles into equivalence classes; this is done using a function map, making it ideal for usage in parallel. The python files generate a set of all chorded cycles and separate them into classes by there detour number.

An approximation of the k-color connection number can be generated using DetourModule.wl, although that package has bugs. It uses all paths of length $k$ or greater to color the graph, depending on how frequently a particular edge occurs in a $k$ length path.
