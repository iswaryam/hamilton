

######## Author: Iswarya Murali ################################################################################
#######  Purpose: Transforming ham_lyrics.csv into an 'Edge List' dataframe with source, destination, and weight
####### Source ham_lyrics.csv file comes from  https://www.kaggle.com/lbalter/hamilton-lyrics# #####################


par(mar=c(0,0,0,0))
dev.off()
library(dplyr)
library(igraph)
library(ggplot2)



dataset <- read.csv('ham_lyrics.csv')

####### Part 1: Prepping the data ############
# EDA
summary(dataset)
head(dataset)
table(dataset$speaker) 

# Cleaning and pruning the dataset to remove outliers
dataset <- unique(dataset[1:2],) # get DISTINCT combinations of song and speaker by removing lines column and doing a unique()

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
                                          'MARIA', 'LEE'),]
dataset <- unique(dataset)

# lets use dplyr to build the speaker relationships
speaker_pair_relationships <- 
  full_join(dataset, dataset, c('title' = 'title'))%>% ##### full outer join dataset
  filter('speaker.x' != 'speaker.y')%>%                ##### join only cases where leftside and rightside aren't the same name
  group_by(speaker.x, speaker.y)                 %>%   ##### group each pair of speakers
  summarise(weight = n())                              ##### by count() of how often they appear together

adj_mat <- as_adjacency_matrix(
  graph_from_data_frame(speaker_pair_relationships, 
                        directed = FALSE), 
  attr = "weight")
#as.matrix(adj_mat)

# Creating an igraph object
graph_obj <- graph.adjacency(adj_mat,mode="undirected",weighted=TRUE,diag=FALSE)

# Lets explore the graph object
vcount(graph_obj) # number of vertices
ecount(graph_obj) #number of edges
V(graph_obj) 

# Centrality Measures
degree(graph_obj)
betweenness(graph_obj)  
eigen_centrality(graph_obj)$vector


# Plot the igraph using star layout
plot.igraph(graph_obj,
            layout=layout.star(graph_obj, center='HAMILTON'),
            vertex.color=NA, vertex.frame.color = NA
)


# 6) Not bad! But the old graph doesn't show anSything about relative importance of speakers or relationships
#    This is where we can make good use of degree and edge_weight

plot.igraph(graph_obj,
            vertex.label=V(graph_obj)$name,
            layout=layout.star(graph_obj, center='HAMILTON'),
            edge.width= floor(E(graph_obj)$weight/3)+0.1, # edge thickness is relative to weight
            edge.curved=0.3 ,
            vertex.label.cex = floor(degree(graph_obj)/10)+0.75, # vertex font is relative to degree
            vertex.color=NA, vertex.frame.color = NA
    )
par(bg = 'white')

## Visigraph
vis_nodes <- data.frame(id=unique(speaker_pair_relationships$speaker.x), 
                label=unique(speaker_pair_relationships$speaker.x))
vis_edges <- data.frame(from=speaker_pair_relationships$speaker.x, 
                       to=speaker_pair_relationships$speaker.y, 
                       value=speaker_pair_relationships$weight)
# removing self loops
vis_edges <- vis_edges[(vis_edges$from != vis_edges$to),]

visNetwork(nodes=vis_nodes, edges = vis_edges)




