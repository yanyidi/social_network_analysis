######################################################################
#######################################################################

## Course Analysing Social Networks with Statistics 2019-2020
## Lab 6 - 28 February 2020
## Author: Gil Viry
## Last revision: 27 February 2020
## Version for class

#######################################################################
#######################################################################

###------------ 6.0 Getting Started
# Ctrl-Return to run current line/selection in RStudio
## Save the current R script/code under a new name for editing it: File > Edit > Save as
## Then regularly save your document: Ctrl+S


# If you are working on a lab computer, be sure to set your working directory on your 
# home drive (University server)

getwd() # Check what directory you're in

# Should be "M:R" in the lab
# if this is not the case:
# setwd("M:/R") #set your working directory (on your home drive)

list.files() # List the names of files in the directory
ls() # Check what objects are currently in the working directory


# If you haven't installed the following packages, do so with the following code.
# These packages will be necessary for network analysis and visualization in R

install.packages("statnet") # statnet is a suite of packages for network analysis, including network, ergm and sna. This is the package we will mainly use.
install.packages("igraph")  # igraph is another major package for network analysis. With both statnet and igraph, we'll be well equipped to start our SNA journey in R!
install.packages("intergraph") # package very useful to  transform network data objects between statnet and igraph
install.packages("RColorBrewer") # useful package for setting up colour palettes

# package documentation
# help(package='statnet')
# help(package='igraph')
# help(package='intergraph')

# load packages
# We start first with statnet.
# IMPORTANT: Some functions have the same names in statnet and igraph, so we don't load igraph
library(statnet)

#######################################################################
#######################################################################

###------------ 6.1 importing network data
## 6.1.1 Built-in network datasets

data(flo) # Load a built-in data set of Florentine Wedding Data (Padgett)
flo # Examine the flo adjacency matrix
class(flo) # flo is a R object of class matrix
gplot(flo, gmode = "graph", displaylabels = TRUE) #gplot works with a matrix object
plot(flo, displaylabels = TRUE) #plot does not!


# Download the RData file "data_lab6_ASNwS2020" from Learn and save it in your working directory
# It includes the data of the Bali, Moreno and Lazega networks
# Data can be loaded into R 
setwd("/Users/kate/Desktop/SNA/lab6")
load("data_lab6.RData")

Bali #basic description of the Bali network
class(Bali) # Bali is a R object of class network (statnet)
gplot(Bali, gmode = "graph", displaylabels = TRUE) #gplot works with a network object
plot(Bali, displaylabels = TRUE) #plot as well!


#For more information...
?flo
?plot
?gplot

#######################################################################
## 6.1.2 Importing network data

# Download the following files from LEARN on your home drive (working directory):
# SNclass2020_relational_data.csv
# SNclass2020_node_attributes.csv
# Open the files in MS Excel and see how the data are formatted
# Valued network data adding up knowledge, chat and coffee/lunch scores as a measure of tie strength

#Read an adjacency matrix (R stores it as a data frame by default)
SNAclass.df <- read.csv("SNclass2020_relational_data.csv",header=FALSE,stringsAsFactors=FALSE)
head(SNAclass.df) #visualise the first part
class(SNAclass.df) #class data.frame

#Here's a case where matrix format is preferred
SNAclass.mat <- data.matrix(SNAclass.df[-1,-1]) # convert to a numeric matrix
is.matrix(SNAclass.mat)
head(SNAclass.mat)
dim(SNAclass.mat) #46 class participants, that's correct!

#Read in some vertex attribute data (okay to leave it as a data frame)
SNAclass_nodeA <- read.csv("SNclass2020_node_attributes.csv",header=TRUE,stringsAsFactors=FALSE)
head(SNAclass_nodeA)
SNAclass_nodeA[1,1]

#Since our relational data have no row/column names, let's set them now
rownames(SNAclass.mat) <- SNAclass_nodeA$name
colnames(SNAclass.mat) <- SNAclass_nodeA$name
head(SNAclass.mat)

#Why did we set stringsAsFactors=FALSE?
SNAclass_nodeA_wFactors<-read.csv("SNclass2020_node_attributes.csv",header=TRUE,stringsAsFactors=TRUE)
head(SNAclass_nodeA_wFactors)
SNAclass_nodeA_wFactors[1,1]
# Let's now imagine we made a mistake: the first network member's name is Derek
SNAclass_nodeA_wFactors[1,1]<-"Derek"

numFactor<-as.factor(c(1,3,5))
numFactor+1
num<-as.numeric(numFactor)
num+1 

rm(SNAclass_nodeA_wFactors, num, numFactor) # we delete these objects from our WD

#For more information...
?read.csv
?as.matrix
?data.matrix
?rownames
?as.numeric

#######################################################################
#######################################################################

###------------ 6.2 Creating a network object
## 6.2.1 Creating a network object in statnet

#create a network object for the florentine wedding (undirected) data
flo.net<-network(flo, matrix.type="adjacency", directed=FALSE)
class(flo.net)
flo.net # Get a quick description of the data
summary(flo.net) # get an overall summary
summary(flo.net,print.adj = FALSE) # without the adjacency structure
plot(flo.net,displaylabels = TRUE) # plot the network with names

# Additional R code is often necessary to produce an effective and meaningful network visualisation
# Here reducing margins on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.
op <- par(mar = rep(0,4))
plot(flo.net,displaylabels = TRUE) 
par(op)
# That's better!

# Same thing but with our directed and valued class network
# In statnet the values are stored in an edge attribute. We call it 'strength'
SNAclass.net<-network(SNAclass.mat,matrix.type="adjacency", ignore.eval=FALSE, names.eval="strength")
SNAclass.net

as.sociomatrix(SNAclass.net) #show it as a binary adjacency matrix
SNAclass.net[,] # Another way to do it
as.sociomatrix(SNAclass.net, "strength") #show it with values
as.sociomatrix(SNAclass.net, "strength")[1:10,1:10] #the first 10 students

par(mar = rep(0.15,4))
plot(SNAclass.net,displaylabels = TRUE)


## Setting node and tie attributes
# One major advantage of using network objects in R rather than simpler matrix objects is 
# the ability to store attribute information within the same network object

# Adding some node (vertex) attributes
set.vertex.attribute(SNAclass.net, "sex", SNAclass_nodeA$sex)
SNAclass.net %v% "sex" <- SNAclass_nodeA$sex # shorthand, same outcome
SNAclass.net %v% "subject" <- SNAclass_nodeA$subject
SNAclass.net %v% "position" <- SNAclass_nodeA$position
SNAclass.net %v% "program" <- SNAclass_nodeA$program
SNAclass.net %v% "indegree" <- degree(SNAclass.net, cmode="indegree") # assign indegree scores as a new node attribute

# Listing node attributes
list.vertex.attributes(SNAclass.net) # list all node attributes

# Retrieving node attributes
get.vertex.attribute(SNAclass.net, "vertex.names") # retrieve node names 
SNAclass.net %v% "vertex.names" # Another way to do it

# Tie (edge) attributes
list.edge.attributes(SNAclass.net) # list all edge attributes
get.edge.attribute(SNAclass.net, "strength") # shows the values stored in an edge attribute
SNAclass.net %e% "strength" # Another way to do it
summary(SNAclass.net %e% "strength") # summary of the tie strength distribution
table(SNAclass.net %e% "strength") # table of the tie strength distribution
prop.table(table(SNAclass.net %e% "strength")) # same with proportions
# Among existing ties, about 16% was at the knowledge level, 41% chat and 43% lunch/coffee

summary(SNAclass.net, print.adj = FALSE) #node and tie attributes stored within a single network object

# Network attributes
list.network.attributes(SNAclass.net) # list all network attributes



# We can also import network data by directly typing the data as an adjacency matrix
net_ex1.mat <- rbind(c(0,1,1,0,0),
                     c(0,0,1,1,0),
                     c(0,1,0,0,0),
                     c(0,0,0,0,0),
                     c(0,0,1,0,0))
net_ex1.mat
net_ex1 <- network(net_ex1.mat, matrix.type="adjacency")
network.vertex.names(net_ex1) <- c("A", "B", "C", "D", "E")
summary(net_ex1)
plot(net_ex1,displaylabels = TRUE)

# ...or as an edge list
net_ex2.mat <- rbind(c(1,2),
                     c(1,3),
                     c(2,3),
                     c(2,4),
                     c(3,2),
                     c(5,3))
net_ex2.mat
net_ex2 <- network(net_ex2.mat, matrix.type="edgelist")
network.vertex.names(net_ex2) <- c("A", "B", "C", "D", "E")
summary(net_ex2)
plot(net_ex2,displaylabels = TRUE)


# coercing network data into a matrix format: fullmatrix or edgelist
as.matrix(net_ex1)
as.matrix(net_ex2, matrix.type = "edgelist")


#For more information...
?network
?as.network
?as.matrix
?as.network.matrix
?as.sociomatrix
?as.matrix.network
?as.edgelist
?attribute.methods
?summary.network
?is.directed

#######################################################################
# 6.2.2 Exporting network data as csv file, for example to import it into UCINet

Bali.mat <- as.sociomatrix(Bali, "IC")
Bali_node_attr <- cbind(Bali %v% "vertex.names", Bali %v% "role")
colnames(Bali_node_attr) <- c("names", "role")
write.csv(Bali.mat, "Bali.csv")
write.csv(Bali_node_attr, "Bali_node_attr.csv")


#######################################################################
## 6.2.3 Creating a network object in igraph

# IMPORTANT: some functions have the same names in statnet and igraph, so we detach statnet to use igraph functions
detach(package:statnet)
library(igraph)

# create a network object from an adjacency matrix
inet_ex1 <- graph.adjacency(net_ex1.mat)
class(inet_ex1) # igraph network objects have a class 'igraph'
V(inet_ex1)$name <- c("A", "B", "C", "D", "E") # create node attribute
summary(inet_ex1) # get an overall summary D = directed graph N = vertices are named
inet_ex1 # with the list of arcs

# create a network object from an edge list
inet_ex2 <- graph.edgelist(net_ex2.mat)
V(inet_ex2)$name <- c("A", "B", "C", "D", "E") # create a new node attribute
E(inet_ex2)$val <- c(1:6) # create a new edge attribute
summary(inet_ex2)


# asIgraph and asNetwork functions from the intergraph package are very useful
# for going back and forth between statnet and igraph
library(intergraph)
class(SNAclass.net)
iSNAclass.net <- asIgraph(SNAclass.net)
class(iSNAclass.net)
iSNAclass.net

class(inet_ex2)
net_ex2_from_igraph <- asNetwork(inet_ex2)
class(net_ex2_from_igraph)
net_ex2_from_igraph


# A nice option for quickly visualising networks in a web browser is to use the netCoin package
# See the Twitter lab
library(netCoin)

V(iSNAclass.net)$name <- V(iSNAclass.net)$vertex.names

visualize.browser <- function(graph){
  nodes <- as.data.frame(vertex_attr(graph, "name"))
  colnames(nodes) <- "name"
  nodes$name <-as.character(nodes$name)
  nodes$indegree <- degree(graph, mode = "in")
  nodes$outdegree <- degree(graph, mode = "out")
  net <- allNet(incidence=as_adjacency_matrix(graph),nodes=nodes)
  plot(net)
}

visualize.browser(iSNAclass.net)

# the read_graph function from the igraph package reads network objects in various foreign file formats,
# such as pajek and UCINet DL files (local file or URL)
# the write_graph function exports network objects to foreign file formats, such as pajek (not UCINet)
# for more information...
?read_graph
?write_graph


#######################################################################
#######################################################################

###------------ 6.3 Visualising networks
## 6.3.1 Basic network plotting and layout

# some functions have the same names in statnet and igraph, so we detach igraph to use statnet functions
detach(package:igraph)
library(statnet)

op <- par(mar = rep(0.15,4))
plot(SNAclass.net,displaylabels = TRUE)
par(op)

# Additional R code will often be necessary to produce an effective and meaningful network visualisation
# Here, labels plotted within node circles
op <- par(mar = rep(0.15,4))
plot(SNAclass.net,displaylabels = TRUE, vertex.cex = 3, label.cex = 0.8, label.pos = 5, pad = 0.1)
par(op)


op <- par(mar = rep(0.15,4))
gplot(flo, gmode="graph", displaylabels = TRUE) # gplot works with a matrix object too
plot(flo) # not plot!
plot(flo.net, displaylabels = TRUE)
par(op)

# saving the network plot as an image
op <- par(mar = rep(0.15,4))
png(filename = "SNAclass.png", width = 10000, height = 5000, pointsize = 100)
plot(SNAclass.net,displaylabels = TRUE)
par(op)
dev.off()

# or as a pdf file
pdf(file = "SNAclass.pdf")
op <- par(mar = rep(0.15,4))
plot(SNAclass.net,displaylabels = TRUE)
par(op)
dev.off()

# different layout algorithms
op <- par(mar = rep(0.15,4), mfrow = c(1,3)) # Set up a 3x1 display
gplot(SNAclass.net,displaylabels = TRUE, mode = "random", arrowhead.cex = 0.5, edge.col = "grey")
title("Random", line = -2)
gplot(SNAclass.net,displaylabels = TRUE, mode = "kamadakawai", arrowhead.cex = 0.5, edge.col = "grey")
title("Kamada-Kawai", line = -2)
gplot(SNAclass.net,displaylabels = TRUE, arrowhead.cex = 0.5, edge.col = "grey") # default mode = "fruchtermanreingold", 
title("Fruchterman-Reingold", line = -2)
par(mfrow=c(1,1)) # Restore display

#When a layout is generated, the results can be saved for later reuse:
coords1 <- gplot(SNAclass.net,displaylabels = TRUE, arrowhead.cex = 0.5, edge.col = "grey") # Capture the magic of the moment
coords1 # Show the vertex coordinates

#Saved (or a priori) layouts can be used via the coord argument
plot(SNAclass.net, displaylabels = TRUE, arrowhead.cex = 0.5, edge.col = "grey", coord=coords1)

#When the default settings are insuficient, interactive mode allows for tweaking
coords2 <- gplot(SNAclass.net, displaylabels = TRUE, arrowhead.cex = 0.5, edge.col = "grey", interactive=TRUE) # Modify and save
plot(SNAclass.net, displaylabels = TRUE, arrowhead.cex = 0.5, edge.col = "grey", coord=coords2) # Should reproduce the modified layout

# for more information...
?plot
?gplot
?gplot.layout
?par

#######################################################################
## 6.3.2 Changing node labels, colours, sizes and shapes

# node labels
# by default, labels are vertex.names
gplot(Bali, usearrows = FALSE, displaylabels = TRUE)

# Other ways to specify the labeling
gplot(Bali, gmode="graph",label=network.vertex.names(Bali))
gplot(Bali, gmode="graph",label=colnames(Bali[,]))

# easy way to use any node attribute as node label
list.vertex.attributes(Bali)
summary(Bali %v% "role")
gplot(Bali, gmode="graph",label=Bali %v% "role")

# easy way to change label font size, colour and distance from node
gplot(Bali, usearrows = FALSE, displaylabels = TRUE, label.cex = 1.2, pad = 0.1, label.col = "darkblue")

# Node colours
#Let's colour the nodes in sex-stereotypic colours
nodeColours <- ifelse(SNAclass_nodeA$sex=="F","hotpink","dodgerblue")
op <- par(mar = rep(0.2,4))
gplot(SNAclass.net, displaylabels=TRUE, usearrows = FALSE, vertex.col = nodeColours)
par(op)

# the plot function can use a vertex attribute to automatically pick node colours 
# from the default colour palette palette() in R (does not work for gplot)
palette()
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = "role")

# But the default colour palette  is limited to 8 colours, so it is better to set up 
# our own colour palette and index into it for colour selection
library(RColorBrewer)
display.brewer.pal(5, "Set1") # We choose the palette called 'Set1'

my_pal <- brewer.pal(5, "Set1")
# Indexing works with factor or numeric vectors, but not character vectors,
# so we convert the role vertex attribute character vector to a factor for indexing
Bali_role.factor <- as.factor(Bali %v% "role")
Bali_role.factor
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor])

# With a legend...
op <- par(mar = rep(0.15,4))
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor])
legend("bottomleft", legend = c("BM - Bomb Maker","CT - Command Team","OA - Operational Assistant","SB - Suicide Bomber","TL - Team Lima"), 
       col = my_pal, pch = 19, pt.cex = 1.5, title = "Terrorist Role")
par(op)

# Node shapes 
# Vertex sides = number of polygon sides
# Example with squares
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.sides = 4)

# Setting node shape according to terrorist roles
side_nb <- 3:7
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.sides = side_nb[Bali_role.factor])

# Node size
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.cex = 1.8, vertex.sides = side_nb[Bali_role.factor])

# Setting the node size according to centrality scores
deg <- degree(Bali, gmode = "graph") # compute the node degree
deg
betw <- betweenness(Bali, gmode = "graph") #compute the node betweenness centrality
betw

plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.cex = deg)
# not ideal...

plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.cex = 1.5*log(deg))
# adjusted

plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.cex = betw)
# whoops...

plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.cex = sqrt(betw+1))
# adjusted

# The adjustments for relative node sizes can be tedious.
# Using the following rescale function can be helpful to figure out the best node sizes
# the node characteristic nchar can be any numeric vector
rescale <- function(nchar,low,high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

op <- par(mar=c(0,1,2,0), mfrow=c(1,2), xpd = NA)
mycoords1 <- plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.cex = rescale(deg,1,6), main = "node size ~ degree")
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", vertex.col = my_pal[Bali_role.factor], vertex.cex = rescale(betw,1,6), coord = mycoords1, main = "node size ~ betweenness")
par(op)

# for more information...
?plot
?gplot
?Bali
?colors
?palette
?col2rgb
?rgb
?RColorBrewer
?display.brewer.pal
?legend

#######################################################################
## 6.3.3 Changing the tie width and tie colour 

list.edge.attributes(Bali)
# IC = interactional criteria, measures the frequency and duration of the contact
table(Bali %e% "IC")

# tie width
# setting thicker ties to denote greater strength
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", label.col = "darkblue", edge.lwd= Bali %e% "IC")

# slightly better
op <- par(mar = c(0,0,2,0))
plot(Bali,displaylabels = TRUE, label = Bali %v% "role", label.col = "darkblue", edge.lwd= rescale(Bali %e% "IC",0.2,8), 
     main = "Bali network with edge widths indicating amount of interaction") 
par(op)

# tie colour
# Using different tie colours to denote greater strength within the class network:
# blue=knowledge, red=chat, green=coffee or lunch
my_pal2 <- c("blue", "red", "green")
op <- par(mar = rep(0.15,4))
plot(SNAclass.net,displaylabels = TRUE, label.col = "darkblue", edge.lwd= 2, edge.col = my_pal2[SNAclass.net %e% "strength"], arrowhead.cex = 1.5) 
legend("bottomleft", legend = c("know","chat","coffee/lunch"), col = my_pal2, pch = 45, pt.cex = 1.5)
par(op)

# with edge curve to better visualise different colours within the same dyad
op <- par(mar = rep(0.15,4))
plot(SNAclass.net,displaylabels = TRUE, label.col = "darkblue", edge.lwd= 2, edge.col = my_pal2[SNAclass.net %e% "strength"], usecurve = TRUE, edge.curve = 0.01, arrowhead.cex = 1.5) 
legend("bottomleft", legend = c("know","chat","coffee/lunch"), col = my_pal2, pch = 45, pt.cex = 1.5)
par(op)

#######################################################################
#######################################################################

###------------ 6.4 Manipulating networks
## 6.4.1 Manipulating edges

#Adding Edges
g <- network.initialize(5) # Create an empty network with n nodes
g[1,2] <- 1 # Add an edge from 1 to 2
g[3,4:5] <- 1 # add an edge from 3 to 4 and 5
g[2,] <- 1 # Add edges from 2 to everyone else
g # Examine the result
plot(g, displaylabels = TRUE) # Plot the network

#Delete edges
g[3,5] <- 0 # Remove the edge from 3 to 5
plot(g, displaylabels = TRUE) # Its gone!
g[,] <- 0 # Remove all edges
g # Now, an empty graph

# For more information...
?network.extraction
?add.edge
?delete.edges
?delete.vertices
?get.edges
?network.edgecount

#######################################################################
## 6.4.2 Extracting a subnetwork based on node characteristics

# select the women only
SNAclass.net_F <- get.inducedSubgraph(SNAclass.net, which(SNAclass.net %v% "sex" == "F"))
SNAclass.net_F <- SNAclass.net %s% which(SNAclass.net %v% "sex" == "F") # shortcut
network.size(SNAclass.net_F)

op <- par(mar = rep(0.15,4))
plot(SNAclass.net_F,displaylabels = TRUE)
par(op)

# select the largest component
SNAclass.net_lgc <- SNAclass.net %s% which(component.largest(SNAclass.net, connected="weak", result = "membership"))

# select nodes with indegree greater or equal to 5 
SNAclass.net.indegree <- get.vertex.attribute(SNAclass.net, "indegree")
SNAclass.net_degGT5 <- SNAclass.net %s% which(SNAclass.net.indegree > 4)
network.size(SNAclass.net_degGT5)

op <- par(mar = rep(0.15,4))
plot(SNAclass.net_degGT5,displaylabels = TRUE)
par(op)

# delete.vertices is a dangerous function. It does not return a R object, but directly operates on the network.
# So it is safer to work on a copy of the network object
net_temp <- SNAclass.net # create a copy of the network object
delete.vertices(net_temp,isolates(net_temp)) # remove the isolates
SNAclass.net_wo_isolates <- net_temp
network.size(SNAclass.net_wo_isolates)

op <- par(mar = rep(0.15,4))
plot(SNAclass.net_wo_isolates,displaylabels = TRUE)
par(op)

# an alternative
op <- par(mar = rep(0.15,4))
plot(SNAclass.net,displaylabels = TRUE, displayisolates=FALSE)
par(op)


#######################################################################
## 6.4.3 Filtering ties based on tie values

# For example, We want to keep ties of value = 3 in the class network
table(SNAclass.net %e% "strength")
SNAclass.mat.value <- as.sociomatrix(SNAclass.net, "strength")
SNAclass.mat.value[SNAclass.mat.value < 3] <- 0
SNAclass.net_value3 <- as.network(SNAclass.mat.value, directed=TRUE, matrix.type="adjacency", ignore.eval=FALSE, names.eval="strength") # network function would work too
summary(SNAclass.net_value3, print.adj=FALSE)

op <- par(mar = rep(0.15,4))
plot(SNAclass.net_value3,displaylabels = TRUE, displayisolates=FALSE)
par(op)

# Alternatively, we can use the thresh option of the gplot() fonction with a matrix object
op <- par(mar = rep(0.15,4))
SNAclass.mat.value <- as.sociomatrix(SNAclass.net, "strength")
gplot(SNAclass.mat.value, thresh=2, displaylabels = TRUE, displayisolates=FALSE, arrowhead.cex = 0.7)
par(op)

# Symmetrizing the network
# Using the "and" rule, i.e. by the minimum: only reciprocal ties are kept
SNAclass.mat.sym <- symmetrize(SNAclass.mat, rule = "strong")
head(SNAclass.mat.sym) # returns a symmetrized binary matrix
SNAclass.net.sym <- network(SNAclass.mat.sym, matrix.type="adjacency")
network.vertex.names(SNAclass.net.sym) <- SNAclass.net %v% "vertex.names"

op <- par(mar=c(0,1,0,0), mfrow=c(1,2), xpd = NA)
mycoords1 <- gplot(SNAclass.net,displaylabels = TRUE)
title("Directed network", line = -1)
gplot(SNAclass.net.sym,gmode = "graph", displaylabels = TRUE, coord = mycoords1)
title("Symmetrized network", line = -1)
par(op)

# the symmetrize function returns a binary matrix and ignores values
# Simple function to symmetrize a valued matrix by the minimum or the maximum
symmetrize_min <- function(x){
  k <- matrix(nrow=nrow(x), ncol=ncol(x))
  rownames(k) <- rownames(x)
  colnames(k) <- colnames(x)
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      k[i,j] <- min(x[i,j], x[j,i])
    }
  }
  return(k)
}

symmetrize_max <- function(x){
  k <- matrix(nrow=nrow(x), ncol=ncol(x))
  rownames(k) <- rownames(x)
  colnames(k) <- colnames(x)
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      k[i,j] <- max(x[i,j], x[j,i])
    }
  }
  return(k)
}

SNAclass.mat.sym.min <- symmetrize_min(SNAclass.mat)
head(SNAclass.mat.sym.min) # tie values are kept
SNAclass.net.sym.min <- network(SNAclass.mat.sym.min, matrix.type="adjacency", directed=FALSE, ignore.eval=FALSE, names.eval="strength")
head(as.sociomatrix(SNAclass.net.sym.min, "strength"))
head(as.sociomatrix(SNAclass.net, "strength"))


#######################################################################
#######################################################################

###------------ 6.5 Descriptive network analysis
## 6.5.1 Exploring network cohesion

# basic exploration of networks
network.dyadcount(SNAclass.net) # How many dyads? (n*n-1)
network.edgecount(SNAclass.net) # How many edges?
network.size(SNAclass.net) # How large is the network?

# The statnet package can handle network data in many forms
# For instance, the function gden() calculates network density; we can use it on a network object,
# an adjacency matrix, a list of such matrices, etc.

gden(SNAclass.net) # Density
grecip(SNAclass.net) # Dyadic reciprocity
grecip(SNAclass.net, measure="edgewise") # Edgewise reciprocity
gtrans(SNAclass.net) # Transitivity

#Likewise, many routines exist for handling isolates
is.isolate(SNAclass.net, 3) # Is the third node an isolate?
isol <- isolates(SNAclass.net) # Get the entire list of isolates
isol
network.vertex.names(SNAclass.net)[isol] # Which people are isolates?

#Another way to remove isolates from sociograms
gplot(SNAclass.net[-isol,-isol],label=network.vertex.names(SNAclass.net)[-isol])

dyad.census(SNAclass.net) # M,A,N counts
triad.census(SNAclass.net) # Directed triad census
triad.census(SNAclass.net, mode="graph") # Undirected triad census
kpath.census(SNAclass.net, maxlen=6, tabulate.by.vertex=FALSE) # Count paths of length <=6
kcycle.census(SNAclass.net, maxlen=6, tabulate.by.vertex=FALSE) # Count cycles of length <=6


# For more information...
?network.dyadcount
?network.edgecount
?gden
?grecip
?gtrans
?is.isolate
?isolates
?clique.census
?dyad.census
?kcycle.census
?kpath.census
?triad.census

#######################################################################
## 6.5.2 Exploring node centrality and network centralization

# Degree centrality
# Centrality functions produce a vector of node centrality scores
degree(SNAclass.net) # Default: total degree (in- + out-degree)
degree(SNAclass.net, rescale = TRUE) # normalised centrality scores between 0 and 1
summary(degree(SNAclass.net))
indeg <- degree(SNAclass.net, cmode="indegree") # Indegree
outdeg <- degree(SNAclass.net, cmode="outdegree") # Outdegree
all(degree(SNAclass.net) == indeg+outdeg) # In + out = total?

# Once centrality scores are computed, we can handle them using standard R methods:
plot(indeg, outdeg, type="n", xlab="Indegree", ylab="Outdegree") # Plot indeg by outdeg
abline(0, 1, lty=3)
text(jitter(indeg), jitter(outdeg), network.vertex.names(SNAclass.net), cex=0.75, col=2)

# Plot simple histograms of the degree distribution:
par(mar = c(1,0,1,0), mfrow=c(2,1))
hist(indeg, xlab="Indegree", main="Indegree Distribution", prob=TRUE)
hist(outdeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
par(mfrow=c(1,1))

# Betweenness and closeness centrality
bet <- betweenness(SNAclass.net) #  betweenness centrality
bet
clo <- closeness(SNAclass.net) # closeness centrality
clo # A large world after all?

# an alternative to the closeness function
SNAclass.net_gdist<-geodist(SNAclass.net)$gdist # matrix of geodesic distances
head(SNAclass.net_gdist)
1/sum(SNAclass.net_gdist[1,2:network.size(SNAclass.net)]) # calculate closeness for node 1

which(SNAclass.net_gdist[,1]=="Inf") # disconnected matrix

sum(1/SNAclass.net_gdist[1,2:network.size(SNAclass.net)]) # alternate closeness for node 1

closeness2 <- function(x){ # Create an alternate closeness function
  geo <- 1/geodist(x)$gdist # Get the matrix of 1/geodesic distance
  diag(geo) <- 0 # Define self-ties as 0
  apply(geo, 1, sum) # Return sum(1/geodist) for each vertex
}

clo2 <- closeness2(SNAclass.net) # Use our new function on contiguity data
clo2

hist(clo2, xlab="Alternate Closeness", prob=TRUE) # Much better behaved!
cor(clo2, bet) # Correlate with betweenness
plot(clo2, bet) # Plot the bivariate relationship
# Actually, this is supported in statnet!
all(clo2/(network.size(SNAclass.net)-1) == closeness(SNAclass.net,cmode="suminvdir"))

# From centrality to centralization
centralization(SNAclass.net, degree, cmode="indegree")
centralization(SNAclass.net, evcent) # Eigenvector centralization

# Presenting centrality and centralization scores in a table
SNAclass_centrality.df <- data.frame(
  degree = degree(SNAclass.net),
  closeness = closeness(SNAclass.net, cmode = "suminvdir"),
  betweenness = betweenness(SNAclass.net)
)
row.names(SNAclass_centrality.df) <- SNAclass.net %v% "vertex.names"
SNAclass_centrality.df.sorted <- SNAclass_centrality.df[order(-SNAclass_centrality.df$degree),] # sorted by highest degree scores
cd <- centralization(SNAclass.net, degree)
cc <- centralization(SNAclass.net, closeness, cmode = "suminvdir")
cb <- centralization(SNAclass.net, betweenness)
SNAclass_centrality.df.sorted <- rbind(SNAclass_centrality.df.sorted, c(cd,cc,cb))
row.names(SNAclass_centrality.df.sorted)[network.size(SNAclass.net)+1] <- "Centralization"
SNAclass_centrality.df.sorted <- round(SNAclass_centrality.df.sorted, digits = 2)
SNAclass_centrality.df.sorted

write.csv(SNAclass_centrality.df.sorted, "SNAclass_centrality.df.sorted.csv")
# save the table as csv file for possible formatting in MS Excel

#For more information...
?degree
?bonpow
?closeness
?betweenness
?evcent
?graphcent
?infocent
?prestige
?stresscent
?centralization
?geodist

#######################################################################
## 6.5.3 Community (cohesive subgroups) detection in statnet

is.connected(SNAclass.net) # Is the network strongly connected?
is.connected(SNAclass.net, connected="weak") # How about weakly connected?
geodist(SNAclass.net) # Get information on geodesics
head(reachability(SNAclass.net)) # Return the reachability matrix

# Several ways to get relatively cohesive groups
clique.census(SNAclass.net) # Maximal clique census
clique.census(SNAclass.net, tabulate.by.vertex=FALSE, enumerate=FALSE) # Find maximal cliques
kcores(SNAclass.net) # k-cores (by degree) 

# Show the nesting of cores
kc<-kcores(SNAclass.net, cmode="indegree")
gplot(SNAclass.net, displaylabels = TRUE, vertex.col=rainbow(max(kc)+1)[kc+1])

#Showing members of the 2-core only
gplot(SNAclass.net[kc>1,kc>1], displaylabels = TRUE, vertex.col=rainbow(max(kc)+1)[kc[kc>1]+1])

# Component information can be obtained in various ways
components(SNAclass.net) # Strong component count
components(SNAclass.net, connected="weak") # Weak component count
cd <- component.dist(SNAclass.net, connected="weak") # Get weak components
cd

#Component sizes
plot(1:length(cd$cdist),cd$cdist,xlab="Size",ylab="Frequency")

#Who's in the largest component?
cl <- component.largest(SNAclass.net, connected="weak")
cl

# Plot the largest weak component
gplot(SNAclass.net[cl,cl], boxed.lab=FALSE, label=network.vertex.names(SNAclass.net)[cl])

# Cutpoints = nodes that, if dropped, would increase the number of components
cutpoints(SNAclass.net) # find cutpoints

# Showing cohesion information can aid visualization. Here, show critical positions
SNAclass_cp <- cutpoints(SNAclass.net, mode="graph", return.indicator=TRUE)
gplot(SNAclass.net, displaylabels = TRUE, gmode="graph", vertex.col=SNAclass_cp+2)

#For more information...
?bicomponent.dist
?cutpoints
?kcores
?is.connected
?reachability
?symmetrize
?components
?component.dist

#######################################################################
## 6.5.4 Community (cohesive subgroups) detection in igraph

# the igraph package includes most community detection approaches, including the very useful modularity function
# some functions have the same names in statnet and igraph, so we detach statnet to use igraph functions
detach(package:statnet)
library(igraph)
library(intergraph)

iSNAclass.net # graph previously created, see 6.2.3

# Identify and map the k-core structure
SNAclass_coreness <- graph.coreness(iSNAclass.net)
table(SNAclass_coreness)

# Copy over vertex names stored in the statnet node attribute vertex.names
V(iSNAclass.net)$name <- get.vertex.attribute(iSNAclass.net, name = "vertex.names", index = V(iSNAclass.net))
V(iSNAclass.net)$color <- SNAclass_coreness + 1 # because 1 is black

lay <- layout.fruchterman.reingold(iSNAclass.net)
op <- par(mfrow=c(1,2), mar = rep(0,4))
plot(iSNAclass.net, vertex.size = 15, vertex.label.cex = 1.2, edge.arrow.size = 0.5, layout=lay) # In igraph, name attribute is used by default to label the nodes in a plot
plot(iSNAclass.net, vertex.label = SNAclass_coreness, vertex.label.cex = 2, edge.arrow.size = 0.5, layout=lay) # Label the nodes with their k-core membership value
par(op)

# Mapping the nested structure of k-cores
iSNAclass.net_core5 <- induced.subgraph(iSNAclass.net, vids = which(SNAclass_coreness > 4))
iSNAclass.net_core10 <- induced.subgraph(iSNAclass.net, vids = which(SNAclass_coreness > 9))
iSNAclass.net_core15 <- induced.subgraph(iSNAclass.net, vids = which(SNAclass_coreness > 14))

lay <- layout.fruchterman.reingold(iSNAclass.net)
op <- par(mfrow=c(2,2), mar = c(2,0,2,0))
plot(iSNAclass.net, vertex.label = SNAclass_coreness, layout = lay, main = "All k-cores", edge.arrow.size = 0.1)
plot(iSNAclass.net_core5, vertex.label = SNAclass_coreness[which(SNAclass_coreness > 4)], layout = lay[which(SNAclass_coreness > 4),], main = "k-cores 5+", edge.arrow.size = 0.1)
plot(iSNAclass.net_core10, vertex.label = SNAclass_coreness[which(SNAclass_coreness > 9)], layout = lay[which(SNAclass_coreness > 9),], main = "k-cores 10+", edge.arrow.size = 0.1)
plot(iSNAclass.net_core15, vertex.label = SNAclass_coreness[which(SNAclass_coreness > 14)], layout = lay[which(SNAclass_coreness > 14),], main = "k-cores 15+", edge.arrow.size = 0.1)
par(op)

# Community detection algorithm and the modularity measure
# The modularity statistic reflects the extent to which a node grouping variable explains the observed clustering
# The modularity statistic can range from -1/2 to +1. The closer to 1, the more the network clustering follows the given node grouping

# The modularity function expects that the node grouping variable is numbered starting at 1.
# !!! Otherwise risk of crash!!!
class(V(iSNAclass.net)$sex) # string of characters
sex_grp_num <- as.numeric(factor(V(iSNAclass.net)$sex))
subject_grp_num <- as.numeric(factor(V(iSNAclass.net)$subject))
position_grp_num <- as.numeric(factor(V(iSNAclass.net)$position))
program_grp_num <- as.numeric(factor(V(iSNAclass.net)$program))

modularity(iSNAclass.net, sex_grp_num, weights = E(iSNAclass.net)$strength) # Sex explains very little of the clustering
modularity(iSNAclass.net, subject_grp_num, weights = E(iSNAclass.net)$strength) # Subject area is a good predictor of the clustering
modularity(iSNAclass.net, position_grp_num, weights = E(iSNAclass.net)$strength) # Status is a poor predictor
modularity(iSNAclass.net, program_grp_num, weights = E(iSNAclass.net)$strength) # The ongoing programme explains some of the clustering
# the subject area is the best predictor of clustering

subj <- factor(V(iSNAclass.net)$subject)
levels(subj)[levels(subj)=="Sociology"] <- "Soc"
levels(subj)[levels(subj)=="Social Research"] <- "SR"
levels(subj)[levels(subj)=="Politics and IR"] <- "PIR"
levels(subj)[levels(subj)=="Social and Public policy"] <- "SP"
subj

V(iSNAclass.net)$color <- as.numeric(subj) + 2
op <- par(mar = rep(0.15,4))
plot(iSNAclass.net, vertex.label = as.character(subj), edge.arrow.size = 0.5)
par(op)


# Many community detection algorithms are included in the igraph package
iBali <- asIgraph(Bali)
iBali

plot(iBali, vertex.label = V(iBali)$role)

Bali_cw <- cluster_walktrap(iBali)
# igraph community detection functions return results as an object from the communities class
class(Bali_cw)
membership(Bali_cw)
modularity(Bali_cw)

plot(Bali_cw, iBali, vertex.label = V(iBali)$role, main = "Walktrap")

# Or more traditionally
plot(iBali, vertex.label = V(iBali)$role, vertex.color = membership(Bali_cw), main = "Walktrap")

# We can compare the community detection results to the roles of terrorists in the group
# We can use the adjusted Rand statistic as a classification comparison metric
table(V(iBali)$role, membership(Bali_cw))
compare(as.numeric(factor(V(iBali)$role)),Bali_cw, method = "adjusted.rand")

# A common practice is to use more than one community detection algorithm and compare the modularity results

Bali_ceb <- cluster_edge_betweenness(iBali)
membership(Bali_ceb)
modularity(Bali_ceb)

Bali_cs <- cluster_spinglass(iBali)
membership(Bali_cs)
modularity(Bali_cs)

Bali_cfg <- cluster_fast_greedy(iBali)
membership(Bali_cfg)
modularity(Bali_cfg)

# Results show that all the detection algorithms identify either 2 or 3 subgroups. Modularity ranges from about 0.24 to 0.30
# We can also plot mulltiple solutions to better understand similarities and differences between community detesction algorithms

lay2 <- layout.fruchterman.reingold(iBali)
op <- par(mfrow = c(2,2), mar = c(0,0,1,0))
plot(Bali_cw, iBali, vertex.label = V(iBali)$role, layout = lay2, main = "Walktrap")
plot(Bali_ceb, iBali, vertex.label = V(iBali)$role, layout = lay2, main = "Edge betweenness")
plot(Bali_cs, iBali, vertex.label = V(iBali)$role, layout = lay2, main = "Spinglass")
plot(Bali_cfg, iBali, vertex.label = V(iBali)$role, layout = lay2, main = "Fastgreedy")
par(op)

#For more information...
?graph.coreness
?induced.subgraph
?modularity
?cluster_edge_betweenness
?cluster_fast_greedy
?cluster_walktrap
?membership

#######################################################################
#######################################################################

###------------ 6.6 Saving and reloading your current workspace

# Saving your current workspace under the name "lab6_ASNwS.RData"
save.image("M:/R/lab6_ASNwS.RData")

# Reload the workspace previously saved
load("M:/R/lab6_ASNwS.RData")

#######################################################################
#######################################################################

###------------ 6.7 Practical Exercise 6

# Data

# If not done before, download the RData file "data_lab6_ASNwS2017" from Learn and save it on your home drive
load("data_lab6_ASNwS2020.RData")
v.attr.lazega
elist.lazega.df

# some functions have the same names in statnet and igraph, so detach igraph if this package has been loaded previously
# detach(package:igraph)

# Load statnet
library(statnet)
