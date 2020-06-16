########################################################################################################################
###                                    Thermokarst Analysis Flow chart                                               ###
###                                         Code by HGR 6/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(DiagrammeR)
########################################################################################################################

### Create Flow Chart of Thermokarst Model #############################################################################
# with details and subgraphs
grViz("digraph {

# top section (single Column)
subgraph cluster_0 {
style = invisible
node [shape = rectangle, style = filled, fillcolor = White, penwidth = 2]
a [label = < <b>LiDAR</b> >, shape = rectangle]
b [label = < <b>Digital Terrain Model</b>\n<b>[Floating Point]</b> >']
1 [label =  'Pre-Processing', shape = oval, penwidth = 1]
a  -> 1 [arrowhead = none]
1 -> b

}

# Microtopography column
subgraph cluster_1 {
label = 'Main Thermokarst Classification'
node [shape = rectangle, style = filled, fillcolor = White, penwidth = 2]

# datasets
c1 [label = 'Median Elevation\n[Floating Point]']
d1 [label = 'Microtopography\n[Floating Point]']
e1 [label = 'Local Elevation Minima\n[0,1]']
i [label = 'Raw Thermokarst Classification\n[0,1]']
j [label = 'Filled Thermokarst\n[0,1]']
k [label = 'Filled Thermokarst Combinations\n[0,1]']
l [label = 'Final Thermokarst Classification\n[0,1]']

# processes
2.1 [label = 'Circular Focal Mean*\nRadius = 15 m or\nRadius = 25 m or\nRadius = 35 m', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
3.1 [label = 'DTM - Median Elevation', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
4.1 [label = 'Reclassify\n0: >=0, 1: <0', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
5.1 [label = 'Elevation Minima - Filter', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
7 [label = 'Fill Holes\n2x(Dilate then Erode)', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
8 [label = 'Combine Filled Thermokarst Layers*\nAdd Layers Derived From Different Radii', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
9 [label = 'Validation', shape = oval, fillcolor = SteelBlue2, penwidth = 1]

# edges
2.1 -> c1
2.1 -> c1
2.1 -> c1
c1 -> 3.1 [arrowhead = none]
c1 -> 3.1 [arrowhead = none]
c1 -> 3.1 [arrowhead = none]
3.1 -> d1
3.1 -> d1
3.1 -> d1
d1 -> 4.1 [arrowhead = none]
d1 -> 4.1 [arrowhead = none]
d1 -> 4.1 [arrowhead = none]
4.1 -> e1
4.1 -> e1
4.1 -> e1
e1 -> 5.1 [arrowhead = none]
e1 -> 5.1 [arrowhead = none]
e1 -> 5.1 [arrowhead = none]
5.1 -> i
5.1 -> i
5.1 -> i
i -> 7 [arrowhead = none]
i -> 7 [arrowhead = none]
i -> 7 [arrowhead = none]
7 -> j
7 -> j
7 -> j
j -> 8 [arrowhead = none]
j -> 8 [arrowhead = none]
j -> 8 [arrowhead = none]
8 -> k
8 -> k
8 -> k
k -> 9 [arrowhead = none]
k -> 9 [arrowhead = none]
k -> 9 [arrowhead = none]
9 -> l

}

subgraph cluster_2 {
label = 'Landscape Feature Filter'
node [shape = rectangle, style = filled, fillcolor = White, penwidth = 2]

###### nodes ######
### datasets
# Slope Column
c2 [label = 'Slope\n[&deg;]']
d2 [label = 'Hillslopes\n[0,1]']

# Stream Column
c3 [label = 'Flow Direction\n[Integer]']
d3 [label = 'Flow Accumulation\n[Integer]']
e3 [label = 'Stream\n[0,1]']
f3 [label = 'Stream Buffer Layers\n[0,1]']
g3 [label = 'Stream Buffer\n[0,1]']

# Filter Column (Slope and Stream Columns join)
g [label = 'Landscape Features\n[0,1,2]']
h [label = 'Filter\n[0,1]']

### processes
# Slope Column
2.2 [label = 'Terrain Function', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
3.2 [label = 'Reclassify\n0: <25&deg;, 1: >=25&deg;', shape = oval, fillcolor = SteelBlue2, penwidth = 1]

# Stream Column
2.3 [label = 'Flow Direction Tool', shape = oval, fillcolor = DarkOliveGreen3, penwidth = 1]
3.3 [label = 'Flow Accumulation Tool', shape = oval, fillcolor = DarkOliveGreen3, penwidth = 1]
4.3 [label = 'Reclassify*\n0: <7,000,000, 1: >7,000,000 or\n0: <8,000,000, 1: >8,000,000 or\n0: <20,000,000, 1: >20,000,000', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
5.3 [label = 'Buffer*\nWidth = 50 m or\nWidth = 100 m or\nWidth = 250 m', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
6.3 [label = 'Combine Stream Buffer Layers*\n0: all(Stream Buffer Layers == 0),\n1: !all(Stream Buffer Layers == 0)', shape = oval, fillcolor = SteelBlue2, penwidth = 1]

# Filter Column (Slope and Stream Columns join)
4.2 [label = 'Hillslopes + Stream Buffer', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
5.2 [label = 'Reclassify\n0: <1, 1: >=1', shape = oval, fillcolor = SteelBlue2, penwidth = 1]


###### edges ######
# Slope Column
2.2 -> c2
c2 -> 3.2 [arrowhead = none]
3.2 -> d2

# Stream Column
2.3 -> c3
c3 -> 3.3 [arrowhead = none]
3.3 -> d3
d3 -> 4.3 [arrowhead = none]
d3 -> 4.3 [arrowhead = none]
d3 -> 4.3 [arrowhead = none]
4.3 -> e3
4.3 -> e3
4.3 -> e3
e3 -> 5.3 [arrowhead = none]
e3 -> 5.3 [arrowhead = none]
e3 -> 5.3 [arrowhead = none]
5.3 -> f3
5.3 -> f3
5.3 -> f3
f3 -> 6.3 [arrowhead = none]
f3 -> 6.3 [arrowhead = none]
f3 -> 6.3 [arrowhead = none]
6.3 -> g3

# Filter Column (Slope and Stream Columns join)
{d2 g3} -> 4.2 [arrowhead = none]
4.2 -> g
g -> 5.2 [arrowhead = none]
5.2 -> h

}

b -> {2.1 2.2 2.3 3.1} [arrowhead = none]
h -> 5.1 [arrowhead = none]

}")

# with details
grViz("digraph {

graph [layout = dot, rankdir = TB]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = White]



###### Nodes ######
### data products
# top section (single column)
a [label = 'Raw LiDAR', shape = rectangle]
b [label = 'Digital Terrain Model\n[Floating Point]']

# Microtopography column
c1 [label = 'Median Elevation\n[Floating Point]']
d1 [label = 'Microtopography\n[Floating Point]']
e1 [label = 'Local Elevation Minima\n[0,1]']

# Slope Column
c2 [label = 'Slope\n[&deg;]']
d2 [label = 'Hillslopes\n[0,1]']

# Stream Column
c3 [label = 'Flow Direction\n[Integer]']
d3 [label = 'Flow Accumulation\n[Integer]']
e3 [label = 'Stream\n[0,1]']
f3 [label = 'Stream Buffer Layers\n[0,1]']
g3 [label = 'Stream Buffer\n[0,1]']

# Filter Column (Slope and Stream Columns join)
g [label = 'Landscape Features\n[0,1,2]']
h [label = 'Filter\n[0,1]']

# Bottom Single Column
i [label = 'Raw Thermokarst Classification\n[0,1]']
j [label = 'Filled Thermokarst\n[0,1]']
k [label = 'Filled Thermokarst Combinations\n[0,1]']
l [label = 'Final Thermokarst Classification\n[0,1]']


### processes
# top section (single column)
1 [label =  'Pre-Processing', shape = oval]

# Microtopography column
2.1 [label = 'Circular Focal Mean*\nRadius = 15 m or\nRadius = 25 m or\nRadius = 35 m', shape = oval, fillcolor = SteelBlue2]
3.1 [label = 'DTM - Median Elevation', shape = oval, fillcolor = SteelBlue2]
4.1 [label = 'Reclassify\n0: >=0, 1: <0', shape = oval, fillcolor = SteelBlue2]
5.1 [label = 'Elevation Minima - Filter', shape = oval, fillcolor = SteelBlue2]

# Slope Column
2.2 [label = 'Terrain Function', shape = oval, fillcolor = SteelBlue2]
3.2 [label = 'Reclassify\n0: <25&deg;, 1: >=25&deg;', shape = oval, fillcolor = SteelBlue2]

# Stream Column
2.3 [label = 'Flow Direction Tool', shape = oval, fillcolor = DarkOliveGreen3]
3.3 [label = 'Flow Accumulation Tool', shape = oval, fillcolor = DarkOliveGreen3]
4.3 [label = 'Reclassify*\n0: <7,000,000, 1: >7,000,000 or\n0: <8,000,000, 1: >8,000,000 or\n0: <20,000,000, 1: >20,000,000', shape = oval, fillcolor = SteelBlue2]
5.3 [label = 'Buffer*\nWidth = 50 m or\nWidth = 100 m or\nWidth = 250 m', shape = oval, fillcolor = SteelBlue2]
6.3 [label = 'Combine Stream Buffer Layers*\n0: all(Stream Buffer Layers == 0),\n1: !all(Stream Buffer Layers == 0)', shape = oval, fillcolor = SteelBlue2]

# Filter Column (Slope and Stream Columns join)
4.2 [label = 'Hillslopes + Stream Buffer', shape = oval, fillcolor = SteelBlue2]
5.2 [label = 'Reclassify\n0: <1, 1: >=1', shape = oval, fillcolor = SteelBlue2]

# Bottom Single Column
7 [label = 'Fill Holes\n2x(Dilate then Erode)', shape = oval, fillcolor = SteelBlue2]
8 [label = 'Combine Filled Thermokarst Layers*\nAdd Layers Derived From Different Radii', shape = oval, fillcolor = SteelBlue2]
9 [label = 'Validation', shape = oval, fillcolor = SteelBlue2]



####### Edges ######
# top section (single column)
a  -> 1 [arrowhead = none]
1 -> b
b -> {2.1 2.2 2.3 3.1} [arrowhead = none]

# Microtopography column
2.1 -> c1
2.1 -> c1
2.1 -> c1
c1 -> 3.1 [arrowhead = none]
c1 -> 3.1 [arrowhead = none]
c1 -> 3.1 [arrowhead = none]
3.1 -> d1
3.1 -> d1
3.1 -> d1
d1 -> 4.1 [arrowhead = none]
d1 -> 4.1 [arrowhead = none]
d1 -> 4.1 [arrowhead = none]
4.1 -> e1
4.1 -> e1
4.1 -> e1

# Slope Column
2.2 -> c2
c2 -> 3.2 [arrowhead = none]
3.2 -> d2

# Stream Column
2.3 -> c3
c3 -> 3.3 [arrowhead = none]
3.3 -> d3
d3 -> 4.3 [arrowhead = none]
d3 -> 4.3 [arrowhead = none]
d3 -> 4.3 [arrowhead = none]
4.3 -> e3
4.3 -> e3
4.3 -> e3
e3 -> 5.3 [arrowhead = none]
e3 -> 5.3 [arrowhead = none]
e3 -> 5.3 [arrowhead = none]
5.3 -> f3
5.3 -> f3
5.3 -> f3
f3 -> 6.3 [arrowhead = none]
f3 -> 6.3 [arrowhead = none]
f3 -> 6.3 [arrowhead = none]
6.3 -> g3

# Filter Column (Slope and Stream Columns join)
{d2 g3} -> 4.2 [arrowhead = none]
4.2 -> g
g -> 5.2 [arrowhead = none]
5.2 -> h

# Bottom Single Column
h -> 5.1 [arrowhead = none]
e1 -> 5.1 [arrowhead = none]
e1 -> 5.1 [arrowhead = none]
e1 -> 5.1 [arrowhead = none]
5.1 -> i
5.1 -> i
5.1 -> i
i -> 7 [arrowhead = none]
i -> 7 [arrowhead = none]
i -> 7 [arrowhead = none]
7 -> j
7 -> j
7 -> j
j -> 8 [arrowhead = none]
j -> 8 [arrowhead = none]
j -> 8 [arrowhead = none]
8 -> k
8 -> k
8 -> k
k -> 9 [arrowhead = none]
k -> 9 [arrowhead = none]
k -> 9 [arrowhead = none]
9 -> l
}")


# stripped down
grViz("digraph {

graph [layout = dot, rankdir = TB]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = White]

# data products
a [label = 'LiDAR', shape = rectangle]
b [label = 'Digital Terrain Model']
c1 [label = 'Median Elevation']
d1 [label = 'Microtopography']
e1 [label = 'Local Elevation Minima']
c2 [label = 'Slope']
d2 [label = 'Hillslopes']
c3 [label = 'Flow Direction']
d3 [label = 'Flow Accumulation']
e3 [label = 'Stream']
f3 [label = 'Stream Buffer']
g [label = 'Raw Thermokarst Classification']
h [label = 'Filled Thermokarst']
i [label = 'Filled Thermokarst Combinations']
j [label = 'Final Thermokarst Classification']

# processes
1 [label =  'Pre-Processing', shape = oval]
2.1 [label = 'Circular Focal Mean*', shape = oval, fillcolor = SteelBlue2]
2.2 [label = 'Terrain Function', shape = oval, fillcolor = SteelBlue2]
2.3 [label = 'Flow Direction Tool', shape = oval, fillcolor = DarkOliveGreen3]
3.1 [label = 'DTM - Median Elevation', shape = oval, fillcolor = SteelBlue2]
3.2 [label = 'Reclassify;', shape = oval, fillcolor = SteelBlue2]
3.3 [label = 'Flow Accumulation Tool', shape = oval, fillcolor = DarkOliveGreen3]
4.1 [label = 'Reclassify, shape = oval, fillcolor = SteelBlue2]
4.3 [label = 'Reclassify, shape = oval, fillcolor = SteelBlue2]
5.1 [label = 'Elevation Minima -\nHillslopes - Stream Buffer', shape = oval, fillcolor = SteelBlue2]
5.3 [label = 'Buffer', shape = oval, fillcolor = SteelBlue2]
6 [label = 'Fill Holes\nDilate/Erode', shape = oval, fillcolor = SteelBlue2]
7 [label = 'Overlay/Reclassify', shape = oval, fillcolor = SteelBlue2]
8 [label = 'Validation', shape = oval, fillcolor = SteelBlue2]

# edge definitions with the node IDs
a  -> 1 [arrowhead = none]
1 -> b
b -> {2.1 2.2 2.3 3.1} [arrowhead = none]
2.1 -> c1
2.2 -> c2
2.3 -> c3
c1 -> 3.1 [arrowhead = none]
c2 -> 3.2 [arrowhead = none]
c3 -> 3.3 [arrowhead = none]
3.1 -> d1
3.2 -> d2
3.3 -> d3
d1 -> 4.1 [arrowhead = none]
d3 -> 4.3 [arrowhead = none]
4.1 -> e1
4.3 -> e3
{d2 e1 f3} -> 5.1 [arrowhead = none]
e3 -> 5.3
5.3 -> f3
5.1 -> g
g -> 6 [arrowhead = none]
6 -> h
h -> 7 [arrowhead = none]
7 -> i
i -> 8 [arrowhead = none]
8 -> j
}")
