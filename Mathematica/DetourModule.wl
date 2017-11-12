(* ::Package:: *)

BeginPackage["Detours`"]
DetourModule::usage="Computes minimum detour numbers";


Begin["`Private`"]
Needs["kColorConnectivity`"];
SetAttributes[UndirectedEdge,Orderless];



DetourModule[graph_]:=Module[
{g=graph},

Print[
Style[
Begin,20,Red
]
];

(*
We'll be using this output alot, so we'll save it to a list
*)
VertexPairs = 
Subsets[
VertexList[graph],
{2}
];

(*
We'll also need to know the vertex count without calling the command every time
*)
NumberOfVertices = VertexCount[g];

(*
Build an n\[Cross]n matrix from the number of vertices, populating every entry with 0
*)
DetourMatrix = Table[
0,
{i,1,NumberOfVertices},
{j,1,NumberOfVertices}
];

(*
To find all the paths between two vertices, and return the length of the largest path
*)

ListOfPaths=
List[];

AllComputedPaths=
Association[];

DistributeDefinitions[g,VertexPairs,DetourMatrix,ListOfPaths,AllComputedPaths];

PathFunction[x_,y_]:=
Module[{x1=x,y1=y},
(*Finds all paths between two vertices*)
AllPaths=FindPath[
g,
x,
y,
Infinity,
All
];

AllComputedPaths=Append[
AllComputedPaths,{x,y}->AllPaths
];

(*Finds length of the list of paths, note that true length is LengthOfPaths - 1 *)
LengthOfPaths=Map[
Length,
AllPaths
];

(*Returns the index of the longest path*)
IndexOfLongestPath=
FirstPosition[
LengthOfPaths,
Max[
LengthOfPaths
]
];

(*No need to compute everything twice, we append that path to a list for later*)

ListOfPaths=Append[
ListOfPaths,
AllPaths[[IndexOfLongestPath[[1]]]]
];

(*Print[ListOfPaths];*)

(*We return the length of the longest path for the detour matrix*)

Return[
Length[
AllPaths[
[
IndexOfLongestPath[
[
1
]
]
]
]
]-1
];

];

DistributeDefinitions[PathFunction]

(*
Assigns the longest path length to the vertices positions in the detour matrix
Need be modified for directed graphs
*)

Clear[r];
r=0;
Print["Computing Detour Matrix:"];
Print[ProgressIndicator[Dynamic[r],{0,Length[VertexPairs]}]];

MatrixMap[{x_,y_}]:=
DetourMatrix[
[x,y]
]
=PathFunction[x,y];

DistributeDefinitions[MatrixMap]

ParallelMap[(r++;
MatrixMap[#])&,
VertexPairs
];

(*Writes x,y length to y,x since we're considering undirected graphs*)

DetourFunction[{x_,y_}]:=DetourMatrix[
[y,x]
]=DetourMatrix[[x,y]];

Map[DetourFunction,VertexPairs];

Print["The Detour Matrix of this graph is:"];
Print[DetourMatrix//MatrixForm];

(*Possibly unneccesary, thought it would be interesting to check out*)

DetourPolynomial=CharacteristicPolynomial[DetourMatrix,k];

Print["The Detour Polynomial of this graph is:"];

Print[DetourPolynomial];

(*
Finds the minimum entry of the detour matrix (excluding 0) or as we know it, k
*)

MinimumDetourNumber=
TakeSmallest[
Flatten[
DetourMatrix
],
1,
ExcludedForms->0
][[1]];

(*
If the cardinality of the edge list is less than or equal to 2k-1, 
use a unique color on every edge and return the graph
*)

If[
Length[EdgeList[g]]<=2MinimumDetourNumber-1,
Print[
"This graph has "<>
ToString[EdgeCount[g]]<>
" edges, where k is "<>
ToString[MinimumDetourNumber]<> 
" and is therefore "<>
ToString[MinimumDetourNumber]<>
"-color connected."
];
];

(**)(**)(**)(**)(**)(**)
(**)(**)(**)(**)(**)(**)
(**)(**)(**)(**)(**)(**)
(*BEGIN unknown area*)
(**)(**)(**)(**)(**)(**)
(**)(**)(**)(**)(**)(**)
(**)(**)(**)(**)(**)(**)

(*ListOfPaths contains uv longest paths, the coloring does NOT work when we only consider paths equal to the Minimum Detour Number.
Likewise, if a shorter path between uv is chosen, not necessarily equal to the Minimum Detour Number, the coloring may fail*)

Print[
"Number of paths collected is "
<>
ToString[
Length[
ListOfPaths
]
]
];

Print[
"Average path length is "
<>
ToString[
Mean[
Map[
Length[#]-1&,
ListOfPaths
]
]//N
]
];

(*
From the ListOfPaths, creates subgraphs from each list and returns the edgelist of each
tallies how many times an edges was used, and sorts the tally in descending order, as a more frequently used edge should
be given precedence for a new color.
As we have 2k-2 colors left to use, we take the first 2k-2 edges from this list.
*)

OccuredEdges=Sort[
Tally[
Flatten[
Map[
EdgeList[
PathGraph[#]
]&,ListOfPaths
]
]
],#1[[2]]>#2[[2]]&
];

(*
Consider all cases from the Minimum Detour Number to 2k-1, 
subtract one from either because the default edge style counts as a color
*)

ConsideredEdges=
Map[
Flatten[
Map[
Drop[#,-1]&,
Take[
OccuredEdges
,#]
]
]&,Range[MinimumDetourNumber-1,2*MinimumDetourNumber-2]
];

(*
Assigns a color to each of the edge lists
*)

NewEdges=
Map[
Inner[
Style,
#,
RandomColor[Length[#]],
List
]&,ConsideredEdges
];

(*
Now we remove the ConsideredEdges from E(G) and add in the edge colored list
*)

InitialEdges=Map[
Complement[
EdgeList[g],
#
]&,ConsideredEdges];

AssignedEdges=
Inner[
Union,
InitialEdges,
NewEdges,
List
];

(*
We build a new graph using the new edges
*)

CCKGraph=
Map[
Graph[
Range[
NumberOfVertices
],
#,
GraphLayout->"CircularEmbedding"
]&,AssignedEdges
];

(*
We'll need define a Color Detour Matrix, 
or the number of colors that exist on the longest uv-path in the graph
*)

ColorDetourMatrix=Table[
Table[
0,
{i,1,NumberOfVertices},
{j,1,NumberOfVertices}
],
{i,1,Length[CCKGraph]}];

(*
Really the computationally heaviest part of this module, 
looking up property value seems to be really inefficient (double check), 
may want to consider analogues using edge weights
*)

PathsWithkColors=List[];

ColorTest[g1_,{x_,y_},i_]:=Module[

{gra=g1,x1=x,y1=y,i1=i},

(*Print["Begin ColorTest"];*)

PathEdges=Map[
EdgeList[
PathGraph[#]
]&,
AllComputedPaths[
{x,y}
]
];

ColorsOnEdges=Map[
#->PropertyValue[{g1,#},
EdgeStyle]&,
EdgeList[g1]
];

ColorsOnPaths=Map[
ColorsOnEdges[#]&,
PathEdges,
{2}
];

NumberOfColorsOnPath=ParallelMap[
Length[
DeleteDuplicates[#]
]&,
ColorsOnPaths
];

IndicesOfPaths=
Position[
NumberOfColorsOnPath,
a_/;a>=MinimumDetourNumber
];

ColorDetourMatrix
[
[
i
]
]
[[x,y]]=
Max[
NumberOfColorsOnPath
];

ColorDetourFunction[{x1_,y1_},i1_]:=
ColorDetourMatrix[
[i1]
]
[
[y1,x1]
]=ColorDetourMatrix[
[
i1
]
]
[
[
x1,y1
]
];

ColorDetourFunction[{x,y},i];

];


Print["Computing Color Detour Matrices"];

Clear[i,colorVar];
Print[
ProgressIndicator[
Dynamic[i],
{1,Length[CCKGraph]}
]
];

colorVar=0;

Print["Colors per vertex pair:"];

Print[
ProgressIndicator[
Dynamic[colorVar],
{0,Length[VertexPairs]}
]
];

For[i=1,i<=Length[CCKGraph],i++,
colorVar=0;
Map[
(colorVar++;
ColorTest[CCKGraph[[i]],#,i])&
,
VertexPairs
]
];

(*Print["The paths with k colors"];
Print[PathsWithkColors];*)

(*We'll probably want to add a backtrace to see if we can further reduce the color connection number
of the graph. 
This will honestly be the most computationally taxing portion.
We can have it so the matrix is searched for values less than the Minimum Detour Number,
followed by *)

KColorConnectedQ=Map[
TakeSmallest[
Flatten[
#
],
1,
ExcludedForms->0
][[1]]>=MinimumDetourNumber&,
ColorDetourMatrix
];


ListOfLongestPaths=Map[
EdgeList[
PathGraph[#]
]&,
ListOfPaths
];

Clear[ReColoringModule];

ReColoringModule:=Module[
{},

FirstTrueCCKIndex=
FirstPosition[
KColorConnectedQ,
True][[1]];

FirstTrueCCKGraph=
CCKGraph[
[
FirstTrueCCKIndex
]
];

FirstTrueCCKMatrix=
ColorDetourMatrix[
[
FirstTrueCCKIndex
]
];

If[
FirstTrueCCKIndex!=1,
VerticesWithNoKColors=Position[
ColorDetourMatrix
[
[
FirstTrueCCKIndex-1
]
],
x_/;LessThan[MinimumDetourNumber][x]&&x!=0
]
];

GraphColors=
DeleteDuplicates[
Flatten[
Map[
PropertyValue[
{CCKGraph[[FirstTrueCCKIndex-1]],#},
EdgeStyle
]&,
EdgeList[
CCKGraph[[FirstTrueCCKIndex-1]]
]
]
]
];

(*Print[GraphColors];*)

ConsideredVertexPairs=
Take[
VerticesWithNoKColors,
Length[
VerticesWithNoKColors
]/2
];

NewlyConsideredFunction[{x_,y_}]:=AllComputedPaths[{x,y}];

AllConsideredPaths=
Flatten[
DeleteCases[
Map[
NewlyConsideredFunction[#]&,
VerticesWithNoKColors
],
_Missing],
1
];

PathsGreaterOrEqualToK=
Cases[
AllConsideredPaths,
x_/;
Length[x]
>=
MinimumDetourNumber
];

ComparisonPaths=
Map[
EdgeList[
PathGraph[#]
]&,
PathsGreaterOrEqualToK
];

test=Map[
Cases[
ListOfLongestPaths,
#/;MemberQ[#]
]&,
ComparisonPaths
];


];

ReColoringModule;

(*Formatting outputs*)
OutputModule:=
Module[
{},
MapIndexed[
If[
#1,
Print["This graph is "<>
ToString[MinimumDetourNumber]<>
"-color connected using "<>
ToString[Length[ConsideredEdges[[#2[[1]]]]]+1]<>
" colors"];
Print[CCKGraph[[#2[[1]]]]];
Print["Its Color Detour Matrix is:"];
Print[ColorDetourMatrix[
[
#2[
[
1
]
]
]
]//MatrixForm
];

Print["Its Chromatic Detour Polynomial is:"];
Print[CharacteristicPolynomial[ColorDetourMatrix[
[
#2[
[
1
]
]
]
],
k]
];
Return[
CCKGraph[
[
#2
[
[
1
]
]
]
]
];
Print[N[Eigenvalues[ColorDetourMatrix[
[
#2[
[
1
]
]
]
]
]]
];
,
Print["This graph is NOT "<>
ToString[MinimumDetourNumber]<>
"-color connected given the coloring and "<>
ToString[Length[
ConsideredEdges[[#2[[1]]]]
]+1]<>
" colors"];
Print[
ColorDetourMatrix[[#2[[1]]]]//MatrixForm
];
Print["Its Chromatic Detour Polynomial is:"];
Print[CharacteristicPolynomial[ColorDetourMatrix[
[
#2[
[
1
]
]
]
],
k]
];
Print[Eigenvalues[ColorDetourMatrix[
[
#2[
[
1
]
]
]
]]];
]
&,
KColorConnectedQ
];
];

OutputModule;

Print[
GroebnerBasis[
Map[
CharacteristicPolynomial[#,x]&,
ColorDetourMatrix
],
{x}
]
];

];


Clear[DetourPolynomialCalc]
DetourPolynomialCalc[graph_]:=Module[
{g=graph},

Print[
Style[
Begin,20,Red
]
];

(*
We'll be using this output alot, so we'll save it to a list
*)
VertexPairs = 
Subsets[
VertexList[graph],
{2}
];

(*
We'll also need to know the vertex count without calling the command every time
*)
NumberOfVertices = VertexCount[g];

(*
Build an n\[Cross]n matrix from the number of vertices, populating every entry with 0
*)
DetourMatrix = Table[
0,
{i,1,NumberOfVertices},
{j,1,NumberOfVertices}
];

(*
To find all the paths between two vertices, and return the length of the largest path
*)

ListOfPaths=
List[];

AllComputedPaths=
Association[];

PathFunction[x_,y_]:=
Module[{x1=x,y1=y},
(*Finds all paths between two vertices*)
AllPaths=FindPath[
g,
x,
y,
Infinity,
All
];

AllComputedPaths=Append[
AllComputedPaths,{x,y}->AllPaths
];

(*Finds length of the list of paths, note that true length is LengthOfPaths - 1 *)
LengthOfPaths=Map[
Length,
AllPaths
];

(*Returns the index of the longest path*)
IndexOfLongestPath=
FirstPosition[
LengthOfPaths,
Max[
LengthOfPaths
]
];

(*No need to compute everything twice, we append that path to a list for later*)

ListOfPaths=Append[
ListOfPaths,
AllPaths[[IndexOfLongestPath[[1]]]]
];

(*Print[ListOfPaths];*)

(*We return the length of the longest path for the detour matrix*)

Return[
Length[
AllPaths[[IndexOfLongestPath[[1]]]]
]-1
];

];

(*
Assigns the longest path length to the vertices positions in the detour matrix
Need be modified for directed graphs
*)

r1=0;
Print["Computing Detour Matrix:"];
Print[ProgressIndicator[Dynamic[r1],{0,Length[VertexPairs]}]];

MatrixMap[{x_,y_}]:=
DetourMatrix[
[x,y]
]
=PathFunction[x,y];

Map[(r1++;
MatrixMap[#])&,
VertexPairs
];

(*Writes x,y length to y,x since we're considering undirected graphs*)

DetourFunction[{x_,y_}]:=DetourMatrix[
[y,x]
]=DetourMatrix[[x,y]];

Map[DetourFunction,VertexPairs];

Print["The Detour Matrix of this graph is:"];
Print[DetourMatrix//MatrixForm];

(*Possibly unneccesary, thought it would be interesting to check out*)

DetourPolynomial=CharacteristicPolynomial[DetourMatrix,k];
evalues=Eigenvalues[DetourMatrix];
Print["The Detour Polynomial and Detour Index of this graph are:"];
detourIndex=Total[Flatten[DetourMatrix]]/2;
Return[{DetourPolynomial,detourIndex,evalues}];
];

DistributeDefinitions[DetourModule];

End[]
EndPackage[]



