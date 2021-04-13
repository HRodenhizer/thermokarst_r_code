########################################################################################################################
###                                    Thermokarst Analysis Flow chart                                               ###
###                                         Code by HGR 6/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
########################################################################################################################

### Create Flow Chart of Thermokarst Model #############################################################################
### list of node labels
# node labels for the main classification flow
main_labels <- c('LiDAR',
                 'Digital Terrain Model\n[Floating Point]',
                 'Median Elevation\n[Floating Point]',
                 'Microtopography\n[0,1]',
                 'Local Elevation Minima\n[0,1]',
                 'Filled Elevation Minima\n[0,1]',
                 'Raw Thermokarst Classification\n[0,1]',
                 'Thermokarst Combinations\n[0,1]',
                 'Best Thermokarst Combination\n[0,1]',
                 'Thermokarst Polygons\n[Vector]',
                 'Final Thermokarst Polygons\n[Vector]',
                 'Final Thermokarst Raster\n[0,1]')
# node labels for the landscape features filter
filter_labels <- c('Slope\n[&deg;]',
                   'Steep Slopes\n[0,1]',
                   'Slope Buffer\n[0,1]',
                   'Sink Fill\n[Integer]',
                   'Flow Direction\n[Integer]',
                   'Flow Accumulation\n[Integer]',
                   'Stream\n[0,1]',
                   'Stream Buffer Layers\n[0,1]',
                   'Stream Buffer\n[0,1]',
                   'Sink Depth\n[Integer]',
                   'Sinks\n[0,1]',
                   'Sink Polygons\n[Vector]',
                   'Stream Polygons\n[Vector]',
                   'Non-Thermokarst Lake Polygons\n[Vector]',
                   'Non-Thermokarst Lakes\n[0,1]',
                   'Landscape Features\n[0,1,2]',
                   'Filter\n[0,1]')
# combine all node labels into one
node_labels <- c(main_labels, filter_labels)

# types to distinguish between the main classification and landscape features filter
node_types <- c(rep('main', length(main_labels)),
                rep('filter', length(filter_labels)))
# shapes - all rectangle currently
node_shapes <- c(rep('rectangle', length(node_labels)))
# node outline colors - black for main model, gray for filter
node_colors <- c(rep('black', length(main_labels)), rep('DimGray', length(filter_labels)))
# node fill color - all white currently
node_fill <- c(rep('white', length(node_labels)))
# node width - set to a fixed width currently. I don't think there is a way to automatically adjust.
node_widths <- c(2.25)

### create node dataframe using input variables from previous section
nodes <- create_node_df(n = length(node_labels),
                        type = node_types,
                        label = node_labels,
                        shape = node_shapes,
                        fontname = 'Arial',
                        fontcolor = node_colors,
                        color = node_colors,
                        fillcolor = node_fill,
                        width = node_widths)

### Edge Definitions
# from and to nodes
edges_from <- c(nodes$id[which(nodes$label == 'LiDAR')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Median Elevation\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Microtopography\n[0,1]')],
                nodes$id[which(nodes$label == 'Local Elevation Minima\n[0,1]')],
                nodes$id[which(nodes$label == 'Filled Elevation Minima\n[0,1]')],
                nodes$id[which(nodes$label == 'Raw Thermokarst Classification\n[0,1]')],
                nodes$id[which(nodes$label == 'Thermokarst Combinations\n[0,1]')],
                nodes$id[which(nodes$label == 'Best Thermokarst Combination\n[0,1]')],
                nodes$id[which(nodes$label == 'Thermokarst Polygons\n[Vector]')],
                nodes$id[which(nodes$label == 'Final Thermokarst Polygons\n[Vector]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Slope\n[&deg;]')],
                nodes$id[which(nodes$label == 'Steep Slopes\n[0,1]')],
                nodes$id[which(nodes$label == 'Slope Buffer\n[0,1]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Sink Fill\n[Integer]')],
                nodes$id[which(nodes$label == 'Flow Direction\n[Integer]')],
                nodes$id[which(nodes$label == 'Flow Accumulation\n[Integer]')],
                nodes$id[which(nodes$label == 'Stream\n[0,1]')],
                nodes$id[which(nodes$label == 'Stream Buffer Layers\n[0,1]')],
                nodes$id[which(nodes$label == 'Stream Buffer\n[0,1]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Sink Fill\n[Integer]')],
                nodes$id[which(nodes$label == 'Sink Depth\n[Integer]')],
                nodes$id[which(nodes$label == 'Sinks\n[0,1]')],
                nodes$id[which(nodes$label == 'Stream\n[0,1]')],
                nodes$id[which(nodes$label == 'Sink Polygons\n[Vector]')],
                nodes$id[which(nodes$label == 'Stream Polygons\n[Vector]')],
                nodes$id[which(nodes$label == 'Non-Thermokarst Lake Polygons\n[Vector]')],
                nodes$id[which(nodes$label == 'Non-Thermokarst Lakes\n[0,1]')],
                nodes$id[which(nodes$label == 'Landscape Features\n[0,1,2]')],
                nodes$id[which(nodes$label == 'Filter\n[0,1]')])

edges_to <- c(nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
              nodes$id[which(nodes$label == 'Microtopography\n[0,1]')],
              nodes$id[which(nodes$label == 'Median Elevation\n[Floating Point]')],
              nodes$id[which(nodes$label == 'Microtopography\n[0,1]')],
              nodes$id[which(nodes$label == 'Local Elevation Minima\n[0,1]')],
              nodes$id[which(nodes$label == 'Filled Elevation Minima\n[0,1]')],
              nodes$id[which(nodes$label == 'Raw Thermokarst Classification\n[0,1]')],
              nodes$id[which(nodes$label == 'Thermokarst Combinations\n[0,1]')],
              nodes$id[which(nodes$label == 'Best Thermokarst Combination\n[0,1]')],
              nodes$id[which(nodes$label == 'Thermokarst Polygons\n[Vector]')],
              nodes$id[which(nodes$label == 'Final Thermokarst Polygons\n[Vector]')],
              nodes$id[which(nodes$label == 'Final Thermokarst Raster\n[0,1]')],
              nodes$id[which(nodes$label == 'Slope\n[&deg;]')],
              nodes$id[which(nodes$label == 'Steep Slopes\n[0,1]')],
              nodes$id[which(nodes$label == 'Slope Buffer\n[0,1]')],
              nodes$id[which(nodes$label == 'Landscape Features\n[0,1,2]')],
              nodes$id[which(nodes$label == 'Sink Fill\n[Integer]')],
              nodes$id[which(nodes$label == 'Flow Direction\n[Integer]')],
              nodes$id[which(nodes$label == 'Flow Accumulation\n[Integer]')],
              nodes$id[which(nodes$label == 'Stream\n[0,1]')],
              nodes$id[which(nodes$label == 'Stream Buffer Layers\n[0,1]')],
              nodes$id[which(nodes$label == 'Stream Buffer\n[0,1]')],
              nodes$id[which(nodes$label == 'Landscape Features\n[0,1,2]')],
              nodes$id[which(nodes$label == 'Sink Depth\n[Integer]')],
              nodes$id[which(nodes$label == 'Sink Depth\n[Integer]')],
              nodes$id[which(nodes$label == 'Sinks\n[0,1]')],
              nodes$id[which(nodes$label == 'Sink Polygons\n[Vector]')],
              nodes$id[which(nodes$label == 'Stream Polygons\n[Vector]')],
              nodes$id[which(nodes$label == 'Non-Thermokarst Lake Polygons\n[Vector]')],
              nodes$id[which(nodes$label == 'Non-Thermokarst Lake Polygons\n[Vector]')],
              nodes$id[which(nodes$label == 'Non-Thermokarst Lakes\n[0,1]')],
              nodes$id[which(nodes$label == 'Landscape Features\n[0,1,2]')],
              nodes$id[which(nodes$label == 'Filter\n[0,1]')],
              nodes$id[which(nodes$label == 'Raw Thermokarst Classification\n[0,1]')])

# edge labels
arrow_labels <- c('Pre-Processing',
                  '',
                  'Circular Focal Mean\nRadius = [15 m, 25 m, 35 m]',
                  'DTM - Median Elevation',
                  'Reclassify\n0: >=0, 1: <0',
                  'Fill Holes With\nDilate and Erode',
                  '',
                  'Combine Filled Thermokarst Layers',
                  'Validation',
                  'Raster to Polygon',
                  'Fill Holes\nthreshold = 25 m&#x2072;',
                  'Polygon to Raster',
                  'Terrain Function',
                  'Reclassify\n0: <25&#176;, 1: >=25&deg;',
                  'Buffer\nWidth = 25 m',
                  '',
                  'Fill Tool',
                  'Flow Direction Tool',
                  'Flow Accumulation Tool',
                  'Reclassify*\n0: <7,000,000, 1: >7,000,000 or\n0: <8,000,000, 1: >8,000,000 or\n0: <20,000,000, 1: >20,000,000',
                  'Buffer*\nWidth = 50 m or\nWidth = 100 m or\nWidth = 250 m',
                  'Combine Stream Buffer Layers*\n0: all(Stream Buffer Layers == 0),\n1: !all(Stream Buffer Layers == 0)',
                  'Steep Slopes + Stream Buffer + Non-Thermokarst Lakes',
                  '',
                  'Sink Fill - Digital Terrain Model',
                  'Reclassify\n0: <=0, 1: >0',
                  'Raster to Polygon',
                  'Raster to Polygon',
                  '',
                  'Intersect',
                  'Rasterize',
                  '',
                  'Reclassify\n0: <1, 1: >=1',
                  'Elevation Minima - Filter')
# edge arrow colors - currently set by software used for that step
# black = NEON
# blue = R
# green = ArcMap
arrow_colors <- c('black',
                  rep('SteelBlue2',15),
                  rep('DarkOliveGreen3', 3),
                  rep('SteelBlue2', 15))
# edge arrow width - currently set by number of times the step is run
arrow_width <- c(rep(1, 1),
                 rep(2, 7),
                 rep(1, 11),
                 rep(2, 2),
                 rep(1, 12),
                 2)

### create edges dataframe from variables in previous section
edges <- create_edge_df(from = edges_from,
                        to = edges_to,
                        fontname = 'Arial',
                        fontcolor = c('black'),
                        color = arrow_colors,
                        label = arrow_labels,
                        headport = 'n',
                        arrowhead = c('vee'),
                        penwidth = arrow_width)

### create graph
graph <- create_graph(nodes_df = nodes,
                      edges_df = edges,
                      attr_theme = 'tb')
### visualize graph
graph %>% render_graph()

# # save file
graph %>% export_graph(file_name = 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_flow_chart_r.pdf',
             file_type = "pdf")
##########################################################################################################

### Create Simplified Flow Chart of Thermokarst Model ####################################################
### list of node labels
# node labels for the main classification flow
main_labels <- c('Digital Terrain Model\n[Floating Point]',
                 'Median Elevation\n[Floating Point]',
                 'Microtopography\n[0,1]',
                 'Elevation Minima\n[0,1]',
                 'Thermokarst Classification\n[0,1]',
                 'Thermokarst Polygons\n[Vector]')
# node labels for the landscape features filter
filter_labels <- c('Slope Filter\n[0,1]',
                   'Stream Filter\n[0,1]',
                   'Non-Thermokarst Lakes\n[0,1]',
                   'Filter\n[0,1]')
# combine all node labels into one
node_labels <- c(main_labels, filter_labels)

# types to distinguish between the main classification and landscape features filter
node_types <- c(rep('main', length(main_labels)),
                rep('filter', length(filter_labels)))
# shapes - all rectangle currently
node_shapes <- c(rep('rectangle', length(node_labels)))
# node outline colors - black for main model, gray for filter
node_colors <- c(rep('black', length(main_labels)), rep('DimGray', length(filter_labels)))
# node fill color - all white currently
node_fill <- c(rep('white', length(node_labels)))
# node width - set to a fixed width currently. I don't think there is a way to automatically adjust.
node_widths <- c(2.25)

### create node dataframe using input variables from previous section
nodes <- create_node_df(n = length(node_labels),
                        type = node_types,
                        label = node_labels,
                        shape = node_shapes,
                        fontname = 'Arial',
                        fontcolor = node_colors,
                        color = node_colors,
                        fillcolor = node_fill,
                        width = node_widths)

### Edge Definitions
# from and to nodes
edges_from <- c(nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Median Elevation\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Microtopography\n[0,1]')],
                nodes$id[which(nodes$label == 'Elevation Minima\n[0,1]')],
                nodes$id[which(nodes$label == 'Thermokarst Classification\n[0,1]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Slope Filter\n[0,1]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Stream Filter\n[0,1]')],
                nodes$id[which(nodes$label == 'Digital Terrain Model\n[Floating Point]')],
                nodes$id[which(nodes$label == 'Non-Thermokarst Lakes\n[0,1]')],
                nodes$id[which(nodes$label == 'Filter\n[0,1]')])

edges_to <- c(nodes$id[which(nodes$label == 'Microtopography\n[0,1]')],
              nodes$id[which(nodes$label == 'Median Elevation\n[Floating Point]')],
              nodes$id[which(nodes$label == 'Microtopography\n[0,1]')],
              nodes$id[which(nodes$label == 'Elevation Minima\n[0,1]')],
              nodes$id[which(nodes$label == 'Thermokarst Classification\n[0,1]')],
              nodes$id[which(nodes$label == 'Thermokarst Polygons\n[Vector]')],
              nodes$id[which(nodes$label == 'Slope Filter\n[0,1]')],
              nodes$id[which(nodes$label == 'Filter\n[0,1]')],
              nodes$id[which(nodes$label == 'Stream Filter\n[0,1]')],
              nodes$id[which(nodes$label == 'Filter\n[0,1]')],
              nodes$id[which(nodes$label == 'Non-Thermokarst Lakes\n[0,1]')],
              nodes$id[which(nodes$label == 'Filter\n[0,1]')],
              nodes$id[which(nodes$label == 'Thermokarst Classification\n[0,1]')])

# edge labels
# arrow_labels <- c('',
#                   'Circular Focal Mean',
#                   'DTM - Median Elevation',
#                   'Reclassify\n0: >=0, 1: <0',
#                   '',
#                   'Raster to Polygon',
#                   'Find Steep Slopes',
#                   '',
#                   'Find Big Streams',
#                   'Steep Slopes + Stream Buffer + Non-Thermokarst Lakes',
#                   '',
#                   'Sink Fill - Digital Terrain Model',
#                   'Reclassify\n0: <=0, 1: >0',
#                   'Raster to Polygon',
#                   'Raster to Polygon',
#                   '',
#                   'Intersect',
#                   'Rasterize',
#                   '',
#                   'Reclassify\n0: <1, 1: >=1',
#                   'Elevation Minima - Filter')
# edge arrow colors - currently set by software used for that step
# black = NEON
# blue = R
# green = ArcMap
arrow_colors <- c('black',
                  rep('SteelBlue2',15),
                  rep('DarkOliveGreen3', 3),
                  rep('SteelBlue2', 15))
# edge arrow width - currently set by number of times the step is run
arrow_width <- c(rep(1, 1),
                 rep(2, 7),
                 rep(1, 11),
                 rep(2, 2),
                 rep(1, 12),
                 2)

### create edges dataframe from variables in previous section
edges <- create_edge_df(from = edges_from,
                        to = edges_to,
                        fontname = 'Arial',
                        fontcolor = c('black'),
                        # color = arrow_colors,
                        # label = arrow_labels,
                        headport = 'n',
                        arrowhead = c('vee'),
                        penwidth = arrow_width)

### create graph
graph <- create_graph(nodes_df = nodes,
                      edges_df = edges,
                      attr_theme = 'tb')
### visualize graph
graph %>% render_graph()

# # save file
# graph %>% export_graph(file_name = '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_flow_chart_r_simple.pdf',
#                        file_type = "pdf")
# graph %>% export_graph(file_name = '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_flow_chart_r_simple.png',
#                        file_type = "png")
##########################################################################################################

### Focal Function Diagram ###############################################################################
elev.example <- raster(matrix(rnorm(2500, mean = 670), ncol = 50))
extent(elev.example) <- extent(0, 50, 0, 50)
elev.example.df <- as.data.frame(elev.example, xy = TRUE)

# create a circlular neighborhood simple feature
x <- seq(15.5, by = 1, length.out = 6)
y <- rep(15.5, 6)
color <- c(alpha('#000000', 0.1),
           alpha('#000000', 0.2),
           alpha('#000000', 0.4),
           alpha('#000000', 0.6),
           alpha('#000000', 0.8),
           alpha('#000000', 1))
points <- data.frame(x = x,
                     y = y,
                     color = I(color))
points <- st_as_sf(points, coords = c('x', 'y'))
circles <- st_buffer(points, dist = 15.5)
center.x <- c(15, 15, 16, 16, 15)
center.y <- c(15, 16, 16, 15, 15)
centers <- data.frame()
for (i in 1:6) {
  center.x.new <- center.x + (i - 1)
  centers <- rbind.data.frame(centers,
                              st_sf(geometry = st_sfc(st_polygon(list(matrix(c(center.x.new, center.y),
                                                                             ncol = 2,
                                                                             byrow = FALSE))))))
}
centers <- centers %>%
  mutate(color = I(color)) %>%
  st_as_sf()
arrows <- data.frame(x = seq(15, 19),
                     xend = seq(16, 20),
                     y = rep(15.5, 5),
                     yend = rep(15.5, 5),
                     color = I(color[1:5]))

ggplot(data = elev.example.df, aes(x = x, y = y, fill = layer)) +
  geom_raster(alpha = 0.4) +
  geom_sf(data = circles, aes(color = color), inherit.aes = FALSE, fill = NA) +
  geom_sf(data = centers, aes(color = color), inherit.aes = FALSE, fill = NA) +
  geom_segment(data = arrows, aes(x = x, y = y, xend = xend, yend = yend, color = color),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.01, "npc"))) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis(name = 'Elevation') +
  theme_few() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

plot1 <- ggplot(data = elev.example.df, aes(x = x, y = y, fill = layer)) +
  geom_raster(alpha = 0.4) +
  geom_sf(data = circles[1,], color = 'black', inherit.aes = FALSE, fill = NA) +
  geom_sf(data = centers[1,], color = 'black', inherit.aes = FALSE, fill = NA) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis(name = 'Elevation') +
  theme_few() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
plot1
# This could be useful to make a gif, if I can install magick
# # start by plotting in 3D
# subsidence.3D <- list()
# for (i in 1:length(Subsidence.matrix)){
#   img <- image_graph(1000, 1000, res = 96)
#   nlong <- (dim(Subsidence.matrix[[i]][[1]])[2]-1)*2
#   nlat <- (dim(Subsidence.matrix[[i]][[1]])[1]-1)*2
#   x <- matrix(rep(seq(0, nlong, by = 2), (nlat/2)+1), ncol = (nlong/2)+1, byrow = TRUE)
#   y <- matrix(rep(seq(nlat, 0, by = -2), (nlong/2)+1), ncol = (nlong/2)+1)
#   for(k in 1:length(Subsidence.matrix[[i]])){
#     surf3D(x = x, 
#            y = y, 
#            z = Subsidence.matrix[[i]][[k]], 
#            colvar = Subsidence.matrix[[i]][[k]], 
#            col = viridis(1024, begin = 0, end = 1), 
#            phi = 60, 
#            theta = -70, 
#            clim = c(-1, 0.5), 
#            zlim = c(-1, 0.5), 
#            colkey = FALSE)
#   }
#   dev.off()
#   subsidence.3D[[i]] <- image_animate(img, fps = 2)
# }
# 
# print(subsidence.3D[[1]])
# print(subsidence.3D[[2]])
# print(subsidence.3D[[3]])



ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/neighborhood_diagram.jpg',
       height = 4,
       width = 4.5)
ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/neighborhood_diagram.pdf',
       height = 4,
       width = 4.5)
##########################################################################################################

#### Old Code ############

# # with details and subgraphs
# graph <- "digraph {
# 
# # top section (single Column)
# subgraph cluster_0 {
# style = invisible
# node [shape = rectangle, style = filled, fillcolor = White, penwidth = 2]
# a [label = <<b>LiDAR</b>>, shape = rectangle]
# b [label = <<b>Digital Terrain Model</b>  <br/>  [Floating Point]>]
# 1 [label =  'Pre-Processing', shape = oval, penwidth = 1]
# a  -> 1 [arrowhead = none]
# 1 -> b
# 
# }
# 
# # Microtopography column
# subgraph cluster_1 {
# label = <<b>Main Thermokarst Classification</b>>
# node [shape = rectangle, style = filled, fillcolor = White, penwidth = 2]
# 
# # datasets
# c1 [label = <<b>Median Elevation</b>  <br/>  [Floating Point]>]
# d1 [label = <<b>Microtopography</b>  <br/>  [Floating Point]>]
# e1 [label = <<b>Local Elevation Minima</b> <br/>[0,1]>]
# i [label = <<b>Raw Thermokarst Classification</b>   <br/>   [0,1]>]
# j [label = <<b>Filled Thermokarst</b>  <br/>  [0,1]>]
# k [label = <<b>Filled Thermokarst Combinations</b>   <br/> [0,1]>]
# l [label = <<b>Final Thermokarst Classification</b>   <br/>  [0,1]>]
# 
# # processes
# 2.1 [label = 'Circular Focal Mean*\nRadius = 15 m or\nRadius = 25 m or\nRadius = 35 m', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 3.1 [label = 'DTM - Median Elevation', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 4.1 [label = 'Reclassify\n0: >=0, 1: <0', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 5.1 [label = 'Elevation Minima - Filter', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 7 [label = 'Fill Holes\n2x(Dilate then Erode)', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 8 [label = 'Combine Filled Thermokarst Layers*\nAdd Layers Derived From Different Radii', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 9 [label = 'Validation', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 
# # edges
# 2.1 -> c1
# 2.1 -> c1
# 2.1 -> c1
# c1 -> 3.1 [arrowhead = none]
# c1 -> 3.1 [arrowhead = none]
# c1 -> 3.1 [arrowhead = none]
# 3.1 -> d1
# 3.1 -> d1
# 3.1 -> d1
# d1 -> 4.1 [arrowhead = none]
# d1 -> 4.1 [arrowhead = none]
# d1 -> 4.1 [arrowhead = none]
# 4.1 -> e1
# 4.1 -> e1
# 4.1 -> e1
# e1 -> 5.1 [arrowhead = none]
# e1 -> 5.1 [arrowhead = none]
# e1 -> 5.1 [arrowhead = none]
# 5.1 -> i
# 5.1 -> i
# 5.1 -> i
# i -> 7 [arrowhead = none]
# i -> 7 [arrowhead = none]
# i -> 7 [arrowhead = none]
# 7 -> j
# 7 -> j
# 7 -> j
# j -> 8 [arrowhead = none]
# j -> 8 [arrowhead = none]
# j -> 8 [arrowhead = none]
# 8 -> k
# 8 -> k
# 8 -> k
# k -> 9 [arrowhead = none]
# k -> 9 [arrowhead = none]
# k -> 9 [arrowhead = none]
# 9 -> l
# 
# }
# 
# subgraph cluster_2 {
# label = <<b>Landscape Feature Filter</b>>
# node [shape = rectangle, style = filled, fillcolor = White, penwidth = 2]
# 
# ###### nodes ######
# ### datasets
# # Slope Column
# c2 [label = <<b>Slope</b> <br/>  [&deg;]>]
# d2 [label = <<b>Hillslopes</b>  <br/>  [0,1]>]
# 
# # Stream Column
# c3 [label = <<b>Flow Direction</b>  <br/>  [Integer]>]
# d3 [label = <<b>Flow Accumulation</b>  <br/>  [Integer]>]
# e3 [label = <<b>Stream</b> <br/>  [0,1]>]
# f3 [label = <<b>Stream Buffer Layers</b>  <br/>  [0,1]>]
# g3 [label = <<b>Stream Buffer</b>  <br/>  [0,1]>]
# 
# # Filter Column (Slope and Stream Columns join)
# g [label = <<b>Landscape Features</b>  <br/>  [0,1,2]>]
# h [label = <<b>Filter</b> <br/>  [0,1]>]
# 
# ### processes
# # Slope Column
# 2.2 [label = 'Terrain Function', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 3.2 [label = 'Reclassify\n0: <25&deg;, 1: >=25&deg;', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 
# # Stream Column
# 2.3 [label = 'Flow Direction Tool', shape = oval, fillcolor = DarkOliveGreen3, penwidth = 1]
# 3.3 [label = 'Flow Accumulation Tool', shape = oval, fillcolor = DarkOliveGreen3, penwidth = 1]
# 4.3 [label = 'Reclassify*\n0: <7,000,000, 1: >7,000,000 or\n0: <8,000,000, 1: >8,000,000 or\n0: <20,000,000, 1: >20,000,000', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 5.3 [label = 'Buffer*\nWidth = 50 m or\nWidth = 100 m or\nWidth = 250 m', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 6.3 [label = 'Combine Stream Buffer Layers*\n0: all(Stream Buffer Layers == 0),\n1: !all(Stream Buffer Layers == 0)', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 
# # Filter Column (Slope and Stream Columns join)
# 4.2 [label = 'Hillslopes + Stream Buffer', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 5.2 [label = 'Reclassify\n0: <1, 1: >=1', shape = oval, fillcolor = SteelBlue2, penwidth = 1]
# 
# 
# ###### edges ######
# # Slope Column
# 2.2 -> c2
# c2 -> 3.2 [arrowhead = none]
# 3.2 -> d2
# 
# # Stream Column
# 2.3 -> c3
# c3 -> 3.3 [arrowhead = none]
# 3.3 -> d3
# d3 -> 4.3 [arrowhead = none]
# d3 -> 4.3 [arrowhead = none]
# d3 -> 4.3 [arrowhead = none]
# 4.3 -> e3
# 4.3 -> e3
# 4.3 -> e3
# e3 -> 5.3 [arrowhead = none]
# e3 -> 5.3 [arrowhead = none]
# e3 -> 5.3 [arrowhead = none]
# 5.3 -> f3
# 5.3 -> f3
# 5.3 -> f3
# f3 -> 6.3 [arrowhead = none]
# f3 -> 6.3 [arrowhead = none]
# f3 -> 6.3 [arrowhead = none]
# 6.3 -> g3
# 
# # Filter Column (Slope and Stream Columns join)
# {d2 g3} -> 4.2 [arrowhead = none]
# 4.2 -> g
# g -> 5.2 [arrowhead = none]
# 5.2 -> h
# 
# }
# 
# b -> {2.1 2.2 2.3 3.1} [arrowhead = none]
# h -> 5.1 [arrowhead = none]
# 
# }"
# 
# grViz(graph)
# setwd('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing')
# 
# grViz(graph) %>%
#   export_svg %>%
#   charToRaw %>%
#   rsvg_pdf('test.pdf')
# 
# # with details
# grViz("digraph {
# 
# graph [layout = dot, rankdir = TB]
# 
# # define the global styles of the nodes. We can override these in box if we wish
# node [shape = rectangle, style = filled, fillcolor = White]
# 
# 
# 
# ###### Nodes ######
# ### data products
# # top section (single column)
# a [label = 'Raw LiDAR', shape = rectangle]
# b [label = 'Digital Terrain Model\n[Floating Point]']
# 
# # Microtopography column
# c1 [label = 'Median Elevation\n[Floating Point]']
# d1 [label = 'Microtopography\n[Floating Point]']
# e1 [label = 'Local Elevation Minima\n[0,1]']
# 
# # Slope Column
# c2 [label = 'Slope\n[&deg;]']
# d2 [label = 'Hillslopes\n[0,1]']
# 
# # Stream Column
# c3 [label = 'Flow Direction\n[Integer]']
# d3 [label = 'Flow Accumulation\n[Integer]']
# e3 [label = 'Stream\n[0,1]']
# f3 [label = 'Stream Buffer Layers\n[0,1]']
# g3 [label = 'Stream Buffer\n[0,1]']
# 
# # Filter Column (Slope and Stream Columns join)
# g [label = 'Landscape Features\n[0,1,2]']
# h [label = 'Filter\n[0,1]']
# 
# # Bottom Single Column
# i [label = 'Raw Thermokarst Classification\n[0,1]']
# j [label = 'Filled Thermokarst\n[0,1]']
# k [label = 'Filled Thermokarst Combinations\n[0,1]']
# l [label = 'Final Thermokarst Classification\n[0,1]']
# 
# 
# ### processes
# # top section (single column)
# 1 [label =  'Pre-Processing', shape = oval]
# 
# # Microtopography column
# 2.1 [label = 'Circular Focal Mean*\nRadius = 15 m or\nRadius = 25 m or\nRadius = 35 m', shape = oval, fillcolor = SteelBlue2]
# 3.1 [label = 'DTM - Median Elevation', shape = oval, fillcolor = SteelBlue2]
# 4.1 [label = 'Reclassify\n0: >=0, 1: <0', shape = oval, fillcolor = SteelBlue2]
# 5.1 [label = 'Elevation Minima - Filter', shape = oval, fillcolor = SteelBlue2]
# 
# # Slope Column
# 2.2 [label = 'Terrain Function', shape = oval, fillcolor = SteelBlue2]
# 3.2 [label = 'Reclassify\n0: <25&deg;, 1: >=25&deg;', shape = oval, fillcolor = SteelBlue2]
# 
# # Stream Column
# 2.3 [label = 'Flow Direction Tool', shape = oval, fillcolor = DarkOliveGreen3]
# 3.3 [label = 'Flow Accumulation Tool', shape = oval, fillcolor = DarkOliveGreen3]
# 4.3 [label = 'Reclassify*\n0: <7,000,000, 1: >7,000,000 or\n0: <8,000,000, 1: >8,000,000 or\n0: <20,000,000, 1: >20,000,000', shape = oval, fillcolor = SteelBlue2]
# 5.3 [label = 'Buffer*\nWidth = 50 m or\nWidth = 100 m or\nWidth = 250 m', shape = oval, fillcolor = SteelBlue2]
# 6.3 [label = 'Combine Stream Buffer Layers*\n0: all(Stream Buffer Layers == 0),\n1: !all(Stream Buffer Layers == 0)', shape = oval, fillcolor = SteelBlue2]
# 
# # Filter Column (Slope and Stream Columns join)
# 4.2 [label = 'Hillslopes + Stream Buffer', shape = oval, fillcolor = SteelBlue2]
# 5.2 [label = 'Reclassify\n0: <1, 1: >=1', shape = oval, fillcolor = SteelBlue2]
# 
# # Bottom Single Column
# 7 [label = 'Fill Holes\n2x(Dilate then Erode)', shape = oval, fillcolor = SteelBlue2]
# 8 [label = 'Combine Filled Thermokarst Layers*\nAdd Layers Derived From Different Radii', shape = oval, fillcolor = SteelBlue2]
# 9 [label = 'Validation', shape = oval, fillcolor = SteelBlue2]
# 
# 
# 
# ####### Edges ######
# # top section (single column)
# a  -> 1 [arrowhead = none]
# 1 -> b
# b -> {2.1 2.2 2.3 3.1} [arrowhead = none]
# 
# # Microtopography column
# 2.1 -> c1
# 2.1 -> c1
# 2.1 -> c1
# c1 -> 3.1 [arrowhead = none]
# c1 -> 3.1 [arrowhead = none]
# c1 -> 3.1 [arrowhead = none]
# 3.1 -> d1
# 3.1 -> d1
# 3.1 -> d1
# d1 -> 4.1 [arrowhead = none]
# d1 -> 4.1 [arrowhead = none]
# d1 -> 4.1 [arrowhead = none]
# 4.1 -> e1
# 4.1 -> e1
# 4.1 -> e1
# 
# # Slope Column
# 2.2 -> c2
# c2 -> 3.2 [arrowhead = none]
# 3.2 -> d2
# 
# # Stream Column
# 2.3 -> c3
# c3 -> 3.3 [arrowhead = none]
# 3.3 -> d3
# d3 -> 4.3 [arrowhead = none]
# d3 -> 4.3 [arrowhead = none]
# d3 -> 4.3 [arrowhead = none]
# 4.3 -> e3
# 4.3 -> e3
# 4.3 -> e3
# e3 -> 5.3 [arrowhead = none]
# e3 -> 5.3 [arrowhead = none]
# e3 -> 5.3 [arrowhead = none]
# 5.3 -> f3
# 5.3 -> f3
# 5.3 -> f3
# f3 -> 6.3 [arrowhead = none]
# f3 -> 6.3 [arrowhead = none]
# f3 -> 6.3 [arrowhead = none]
# 6.3 -> g3
# 
# # Filter Column (Slope and Stream Columns join)
# {d2 g3} -> 4.2 [arrowhead = none]
# 4.2 -> g
# g -> 5.2 [arrowhead = none]
# 5.2 -> h
# 
# # Bottom Single Column
# h -> 5.1 [arrowhead = none]
# e1 -> 5.1 [arrowhead = none]
# e1 -> 5.1 [arrowhead = none]
# e1 -> 5.1 [arrowhead = none]
# 5.1 -> i
# 5.1 -> i
# 5.1 -> i
# i -> 7 [arrowhead = none]
# i -> 7 [arrowhead = none]
# i -> 7 [arrowhead = none]
# 7 -> j
# 7 -> j
# 7 -> j
# j -> 8 [arrowhead = none]
# j -> 8 [arrowhead = none]
# j -> 8 [arrowhead = none]
# 8 -> k
# 8 -> k
# 8 -> k
# k -> 9 [arrowhead = none]
# k -> 9 [arrowhead = none]
# k -> 9 [arrowhead = none]
# 9 -> l
# }")
# 
# 
# # stripped down
# grViz("digraph {
# 
# graph [layout = dot, rankdir = TB]
# 
# # define the global styles of the nodes. We can override these in box if we wish
# node [shape = rectangle, style = filled, fillcolor = White]
# 
# # data products
# a [label = 'LiDAR', shape = rectangle]
# b [label = 'Digital Terrain Model']
# c1 [label = 'Median Elevation']
# d1 [label = 'Microtopography']
# e1 [label = 'Local Elevation Minima']
# c2 [label = 'Slope']
# d2 [label = 'Hillslopes']
# c3 [label = 'Flow Direction']
# d3 [label = 'Flow Accumulation']
# e3 [label = 'Stream']
# f3 [label = 'Stream Buffer']
# g [label = 'Raw Thermokarst Classification']
# h [label = 'Filled Thermokarst']
# i [label = 'Filled Thermokarst Combinations']
# j [label = 'Final Thermokarst Classification']
# 
# # processes
# 1 [label =  'Pre-Processing', shape = oval]
# 2.1 [label = 'Circular Focal Mean*', shape = oval, fillcolor = SteelBlue2]
# 2.2 [label = 'Terrain Function', shape = oval, fillcolor = SteelBlue2]
# 2.3 [label = 'Flow Direction Tool', shape = oval, fillcolor = DarkOliveGreen3]
# 3.1 [label = 'DTM - Median Elevation', shape = oval, fillcolor = SteelBlue2]
# 3.2 [label = 'Reclassify;', shape = oval, fillcolor = SteelBlue2]
# 3.3 [label = 'Flow Accumulation Tool', shape = oval, fillcolor = DarkOliveGreen3]
# 4.1 [label = 'Reclassify, shape = oval, fillcolor = SteelBlue2]
# 4.3 [label = 'Reclassify, shape = oval, fillcolor = SteelBlue2]
# 5.1 [label = 'Elevation Minima -\nHillslopes - Stream Buffer', shape = oval, fillcolor = SteelBlue2]
# 5.3 [label = 'Buffer', shape = oval, fillcolor = SteelBlue2]
# 6 [label = 'Fill Holes\nDilate/Erode', shape = oval, fillcolor = SteelBlue2]
# 7 [label = 'Overlay/Reclassify', shape = oval, fillcolor = SteelBlue2]
# 8 [label = 'Validation', shape = oval, fillcolor = SteelBlue2]
# 
# # edge definitions with the node IDs
# a  -> 1 [arrowhead = none]
# 1 -> b
# b -> {2.1 2.2 2.3 3.1} [arrowhead = none]
# 2.1 -> c1
# 2.2 -> c2
# 2.3 -> c3
# c1 -> 3.1 [arrowhead = none]
# c2 -> 3.2 [arrowhead = none]
# c3 -> 3.3 [arrowhead = none]
# 3.1 -> d1
# 3.2 -> d2
# 3.3 -> d3
# d1 -> 4.1 [arrowhead = none]
# d3 -> 4.3 [arrowhead = none]
# 4.1 -> e1
# 4.3 -> e3
# {d2 e1 f3} -> 5.1 [arrowhead = none]
# e3 -> 5.3
# 5.3 -> f3
# 5.1 -> g
# g -> 6 [arrowhead = none]
# 6 -> h
# h -> 7 [arrowhead = none]
# 7 -> i
# i -> 8 [arrowhead = none]
# 8 -> j
# }")
