#This file contains a script to the Introduction to Social Network Analysis Workshop
#The Graduate Institute, Geneva, 11th November 2022
#Module 5: Social Network Analysis
#By Chanaya Punyakumpol

#download necessary R packages:
library(tidyverse)
library(igraph)
library(RColorBrewer)
library(ggplot2)

#--------------------I. download data--------------------
#go to https://github.com/chanyaaaa/environmental_dispute for all data

#--------------------II. create the network--------------------
#two types of network: undirected and directed network
#II.A Undirected network example:
url <- "https://www.designoftradeagreements.org/media/filer_public/87/e2/87e266f0-5874-4596-913a-251e99ab6632/desta_list_of_treaties_02_01_dyads.csv"
##import the data from DESTA and take a first look at the data structure
pta_data <- read_csv(url)

##dealing with descriptive statistics:
#this line counts, particularly the function count(), the frequency of ties between any two countries formed by year and region.
pta_data_count <- pta_data %>% group_by(year, regioncon) %>% count(name) #Let's see what kind of data we have now

#notice that that pta_data has a variable called regioncon, to which states a region a particular agreement belongs:
#so, we will explore the differences between bilateral and multilateral by region by looking at count variable we just created:
#creating a new variable to see if a tie belongs to a bilateral or multilateral treaty
pta_data_count <- pta_data_count %>% mutate(bilat = ifelse(n == 1, "bilateral", "multilateral"))
pta_data_count$bilat <- as.factor(pta_data_count$bilat) #make this as a factor for the plot
region_count <- pta_data_count %>% group_by(regioncon, bilat) %>% count(regioncon) #create a new data counting regioncon by bilat factor
#now we are able to plot the frequency of bilateral/multilateral by region: what do you notice that's interesting?
ggplot(region_count, aes(x= regioncon, y = n, color = bilat, fill = bilat)) +
  geom_col()

#create an undirected graph:
head(pta_data) #look at the first few rows of the data
pta_edge_net <- graph_from_edgelist(cbind(head(pta_data$country1), head(pta_data$country2)), directed = F) #create a graph from edgelist
set.seed(123) #set.seed() fixes the configuration of the plot for the sake of reproducibility and comparison in this case
plot(pta_edge_net)

#alternatively, you can also create the same network from a matrix:
get.adjacency(pta_edge_net) #function to create adjacency matrix from the graph

pta_adjacency_net <- pta_edge_net %>% get.adjacency() %>%  graph_from_adjacency_matrix(mode = "undirected")
set.seed(123)
plot.igraph(pta_adjacency_net)


#II.B Dirtected Network example:
#First, we will try to create a network from a small number of citation: 
#relevant functions: head(); as.matrix(), graph_from_edgelist(); plot.igraph()
env_sample_data <- as.matrix(head(env_network))
env_sample_net <- graph_from_edgelist(sample_data)
set.seed(123)
plot.igraph(sample_net)

#create adjacency matrix:
get.adjacency(env_sample_net)
env_sample_net <- env_sample_net %>% get.adjacency() %>% graph_from_adjacency_matrix()
set.seed(123)
plot.igraph(sample_net)


#--------------------III. network visualization and statistics--------------------
#III.A: creating undirected graph visualization
#Let's create a network from the DESTA data set:
#select only intercontinential ties and creating a network
pta_intercon <- pta_data %>% filter(regioncon == "Intercontinental") #filter for only inter-regional PTAs
pta_intercon_net <- graph_from_edgelist(cbind(pta_intercon$country1,pta_intercon$country2), directed = F) #create the full network from the edgelist

#network visualization:
set.seed(123)
V(pta_intercon_net)$shape = "none" #this deletes the network vertex and replaces the vertex with the names only
plot.igraph(pta_intercon_net,
            vertex.size = 5,
            vertex.color = "red",
            vertex.label = V(pta_intercon_net)$name,
            vertex.label.cex = .5,
            vertex.frame.color = NA,
            layout = layout.fruchterman.reingold,
            main = "The network of inter-regional PTAs"
)

#Difficult to get any insight, I will create two separate networks: bilateral and multilateral
E(pta_intercon_net)$bilateral <- ifelse(pta_intercon$typememb == 1, 1,0) #add the edge attribute indicating whether the agreement is bilateralater
pta_bilat_net <- subgraph.edges(pta_intercon_net, which(E(pta_intercon_net)$bilateral == 1)) #subgraph.edges() create a subgraph based on edges' attribute
set.seed(123)
plot.igraph(pta_bilat_net,
            vertex.size = 3,
            vertex.label = V(pta_bilat_net)$name,
            vertex.label.cex = .5,
            vertex.frame.color = NA,
            layout = layout.fruchterman.reingold,
            main = "Inter-Regional, Bilateral PTAs")



#III.B: creating directed graph visualization 
env_net <- graph_from_edgelist(as.matrix(env_network), directed = T)
V(env_net)$shape = "none"
plot.igraph(env_net,
            vertex.size = 1,
            vertex.label = V(env_net)$name,
            vertex.label.cex = .5,
            layout = layout.fruchterman.reingold,
            edge.arrow.size = 0.15,
            edge.arrow.width = 2,
            main = "environmental disputes: citation netweork")

#what do you notice immediately here?

#III.C: network statistics: density, transitivity, and centrality
#Density: a network density denotes the proportion of ties over all possible ties in the network
edge_density(pta_intercon_net)

#let's try to plot density over the years:
#first, set an edge value:
E(pta_intercon_net)$year <- pta_intercon$year

#calculate of the intercontinential network's density by year:
pta_intercon_density <- c()

for(i in 1953:2021){ #all the years available
  pta_intercon_density[i-1952] <- edge_density(subgraph.edges(pta_intercon_net, which(E(pta_intercon_net)$year < i+1)))
}

pta_intercon_density <- as.data.frame(cbind(1953:2021, pta_intercon_density))
colnames(pta_intercon_density) <- c("year", "density")

#plot density over time
ggplot(pta_intercon_density, aes(year, density)) +
  geom_line() + theme_minimal()

#Transitivity: a network transitivity denotes the proportion of connected ties over all possible connected ties in the network
transitivity(pta_intercon_net)

pta_intercon_transitivity <- c()

for(i in 1953:2021){ #all the years available
  pta_intercon_transitivity[i-1952] <- transitivity(subgraph.edges(pta_intercon_net, which(E(pta_intercon_net)$year < i+1)))
}

pta_intercon_net_attr <- as.data.frame(cbind(1953:2021, pta_intercon_density$density, pta_intercon_transitivity))
colnames(pta_intercon_net_attr) <- c("year","density", "transitivity")

#plot transitivity over time
ggplot(pta_intercon_net_attr, aes(year, transitivity)) +
  geom_line() + scale_x_continuous(breaks = c(1953, 1963, 1971, 1975, 1988, 2021),labels = c(1953, 1963, 1972, 1975,1988, 2021))
  theme_minimal()


#Centrality: help examine which countries are important nodes in the network
#degree centrality
sort(degree(pta_intercon_net), decreasing = T)

#eigenvalue centrality
sort(eigen_centrality(pta_intercon_net)$vector, decreasing = T)

#this is useful for network visualization:




#--------------------III. modeling network: ergm --------------------

