######## Author: Iswarya Murali ################################################################################
#######  Purpose: Generate a babble (random text generator) using ngrams from ham_lyrics.csv #####################


############ !!!!!PLEASE READ!!!!!!!!!!################################################################################
##  If you want to run the code directly in R studio (ie without PowerBI)  PLEASE UNCOMMENT THIS LINE FIRST!!!

dataset <- read.csv('hamilton_speaker_relationships.csv')

#### Make sure to also download the file to the same folder and set working directory to source file location!

#######################################################################################################################


dev.off()
par(mar=c(0,0,0,0))
library(dplyr)
library(igraph)
library(ggplot2)

summary(dataset)
head(dataset)
#Note: weight represents the number of songs featuring both speaker.x and speaker.y

#1) Explore the distribution of weights
hist(dataset$weight)
table(dataset$weight)

#2) We only need these 3 columns, drop the rest
dataset2 <- dataset[,(names(dataset) %in% c("speaker.x", "speaker.y", "weight"))]

#3a) create an adjacency matrix 
adj_mat <- as_adjacency_matrix(graph_from_data_frame(dataset2, directed = FALSE), attr = "weight")
x<-as.matrix(adj_mat)

# 3b) creating a graph object
graph_obj <- graph.adjacency(adj_mat,mode="undirected",weighted=TRUE,diag=FALSE)


# 4) Lets explore the graph object
vcount(graph_obj) # number of vertices
ecount(graph_obj) #number of edges
V(graph_obj) 

get.data.frame(graph_obj)

# Centrality Measures
degree(graph_obj)
betweenness(graph_obj)  
eigen_centrality(graph_obj)$vector

#Hamiltonian Path


# 3) Plot the igraph as is
plot.igraph(graph_obj)


# 4) Lets make the graph a little more readable using star layout
plot.igraph(graph_obj,
            layout=layout.star(graph_obj, center='HAMILTON'),
            vertex.color=NA, vertex.frame.color = NA
)


# 6) Not bad! But the old graph doesn't show anything about relative importance of speakers or relationships
#    This is where we can make good use of degree and edge_weight

par(bg = 'darkslategray')
plot.igraph(graph_obj,
            vertex.label=V(graph_obj)$name,
            layout=layout.star(graph_obj, center='HAMILTON'),
            edge.width= floor(E(graph_obj)$weight/3)+0.1, # edge thickness is relative to weight
            edge.curved=0.3 ,
            vertex.label.color= "azure1",
            edge.color = "azure3",
            vertex.label.cex = floor(degree(graph_obj)/10)+0.75, # vertex font is relative to degree
            vertex.color=NA, vertex.frame.color = NA
            )

graph_obj_2 <- delete.vertices(graph_obj, 'KING GEORGE')



V(graph_obj)
E(graph_obj)$weight
visIgraph(graph_obj) %%
  visEdges(graph_obj,width=E(graph_obj)$weight)

d <- data.frame(id=unique(dataset1$from), label=unique(dataset1$from))
visNetwork(nodes=d, edges = dataset1)


dataset1 <- data.frame(from=dataset$speaker.x, to=dataset$speaker.y, value=dataset$weight)


plot.igraph(graph_obj_2,
            vertex.label=V(graph_obj_2)$name,
            layout=layout.grid(graph_obj_2),
            #edge.width= floor(E(graph_obj)$weight/2.5)+0.1,
            edge.curved=0.3 ,
            vertex.label.color= "navy",
            edge.color = "gray75",
            #vertex.label.cex = floor(degree(graph_obj)/4)+0.5,
            vertex.color=NA, vertex.frame.color = NA
)


