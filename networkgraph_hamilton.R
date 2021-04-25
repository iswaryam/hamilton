

######## Author: Iswarya Murali ################################################################################
#######  Purpose: Transforming ham_lyrics.csv into an 'Edge List' dataframe with source, destination, and weight
####### Source ham_lyrics.csv file comes from  https://www.kaggle.com/lbalter/hamilton-lyrics# #####################

par(mar=c(0,0,0,0))
dev.off()
library(dplyr)
library(igraph)
library(ggplot2)
library(visNetwork)
library(gridExtra)

dataset <- read.csv('ham_lyrics.csv')

# EDA
summary(dataset)
head(dataset)
table(dataset$speaker) 

# Cleaning and pruning the dataset to remove outliers
dataset <- unique(dataset[1:2]) # get DISTINCT combinations of song and speaker by removing lines column and doing a unique()

dataset$speaker=str_replace(dataset$speaker, ' & MEN', '')
dataset$speaker=str_replace(dataset$speaker, ' & ENSEMBLE', '')
dataset$speaker=str_replace(dataset$speaker, ' & COMPANY', '')
dataset$speaker=str_replace(dataset$speaker, ' & WOMEN', '')
dataset$speaker=str_replace(dataset$speaker, ' & ALL WOMEN', '')
dataset$speaker=str_replace(dataset$speaker, ' & ALL MEN', '')


dataset <- dataset[dataset$speaker %in% c('HAMILTON','BURR','ELIZA', 
                                          'WASHINGTON', 'PHILIP', 'LAFAYETTE', 
                                          'SEABURY', 'PEGGY','ANGELICA',
                                          'JEFFERSON', 'LAURENS', 'KING GEORGE', 
                                          'MADISON', 'MULLIGAN',
                                          'MARIA'),]
dataset <- unique(dataset)

# lets use dplyr to build the speaker relationships
speaker_pair_relationships <- 
  full_join(dataset, dataset, c('title' = 'title'))%>% ##### full outer join dataset
  filter('speaker.x' != 'speaker.y')%>%                ##### remove self loops
  group_by(speaker.x, speaker.y)                 %>%   ##### group each pair of speakers
  summarise(weight = n())                              ##### how frequently they appear together


adj_mat <- as_adjacency_matrix(
  graph_from_data_frame(speaker_pair_relationships, 
                        directed = FALSE), 
  attr = "weight")
View(as.matrix(adj_mat))


# Creating an igraph object
graph_obj <- graph.adjacency(adj_mat,mode="undirected",weighted=TRUE,diag=FALSE)

# Lets explore the graph object
vcount(graph_obj) # number of vertices
ecount(graph_obj) #number of edges
V(graph_obj) # list all vertices

# Centrality Measures
View(degree(graph_obj))
View(sort(betweenness(graph_obj)))
View(eigen_centrality(graph_obj)$vector)
View(sort(closeness(graph_obj)))


# Plot the igraph using star layout
plot.igraph(graph_obj,
            layout=layout.circle(graph_obj),
            edge.width = 1,
            vertex.label.cex = 1,
            vertex.color=NA, vertex.frame.color = NA,
            )


# Not bad! But the old graph doesn't show anSything about relative importance of speakers or relationships
# This is where we can make good use of degree and edge_weight

plot.igraph(graph_obj,
            layout=layout.star(graph_obj, center='HAMILTON'),
            #layout=layout_on_sphere(graph_obj),
            edge.width= E(graph_obj)$weight/5, 
            # edge thickness is relative to weight
            vertex.label.cex = degree(graph_obj)/7, 
            # vertex font is relative to degree
            edge.curved=0.3, vertex.color=NA, vertex.frame.color = NA
    )


## Visigraph
vis_nodes <- data.frame(id=unique(speaker_pair_relationships$speaker.x), 
                        label=unique(speaker_pair_relationships$speaker.x))
vis_edges <- data.frame(from=speaker_pair_relationships$speaker.x, 
                        to=speaker_pair_relationships$speaker.y, 
                        value=speaker_pair_relationships$weight)
# removing self loops
vis_edges <- vis_edges[(vis_edges$from != vis_edges$to),]

vis_graph<- visNetwork(nodes=vis_nodes, edges = vis_edges)
vis_graph

visSave(vis_graph, file="hamiltondynamic.html", selfcontained = TRUE, background = "white")


### Removing 'outlier' vertices for secondary analysis
g2 <- delete_vertices(graph_obj, "KING GEORGE")


# Graph for Betwenness
plot.igraph(g2,
            layout=layout.auto(g2),
            edge.width= E(g2)$weight/5, 
            # edge thickness is relative to weight
            vertex.label.cex = (betweenness(g2)/20)+0.5, 
            # vertex font is relative to degree
            edge.curved=0.3, vertex.color=NA, vertex.frame.color = NA
)

# Graph for Eigenvector
g2 <- delete_vertices(g2, "MARIA")
plot.igraph(g2,
            layout=layout.davidson.harel(g2),
            edge.width= E(g2)$weight/4, 
            # edge thickness is relative to weight
            vertex.label.cex = (eigen_centrality(g2)$vector*2)+0.6, 
            # vertex font is relative to degree
            edge.curved=0.3, vertex.color=NA, vertex.frame.color = NA
)





