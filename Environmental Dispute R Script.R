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
#--------------------II. create the network--------------------
env_net <- graph_from_edgelist(as.matrix(env_network))

set.seed(123) #set.seed() fixes the configuration of the plot for the sake of reproducibility and comparison in this case
plot.igraph(env_net)

