library(statnet)
load("data_lab6.RData")

###############################################################
### Question (a)
class(elist.lazega.df) #edge list
class(v.attr.lazega) #node attributes

#transform edge list to matrix
mat.lazega.df <- data.matrix(elist.lazega.df)
class(mat.lazega.df)

lazega.net <- network(mat.lazega.df, matrix.type = "edgelist", directed = FALSE, names.eval = "strength")
class(lazega.net)
summary(lazega.net, print.adj = FALSE)
plot(lazega.net)

as.sociomatrix(lazega.net,"strength") #binary adjacency matrix 
plot(lazega.net, displaylabels = T)

#adding attributes 
set.vertex.attribute(lazega.net, "name", c(1:36))
lazega.net %v% "sex" <- v.attr.lazega$Gender
lazega.net %v% "senior" <- v.attr.lazega$Seniority
lazega.net %v% "status" <- v.attr.lazega$Status
lazega.net %v% "office" <- v.attr.lazega$Office
lazega.net %v% "years" <- v.attr.lazega$Years
lazega.net %v% "age" <- v.attr.lazega$Age
lazega.net %v% "practice" <- v.attr.lazega$Practice
lazega.net %v% "school" <- v.attr.lazega$School
lazega.net %v% "indegree" <- degree(lazega.net, cmode = "indegree")

list.vertex.attributes(lazega.net)
get.vertex.attribute(lazega.net, "vertex.names")

library(igraph)
inet <- graph.edgelist(mat.lazega.df)
class(inet)
V(inet)$name <- paste("V",c(1:36))
E(inet)$val <- c(1:115)
summary(inet)

library(intergraph)
ilazega.net <- asIgraph(lazega.net)
class(ilazega.net)

library(netCoin)
V(ilazega.net)$name <- V(ilazega.net)$vertex.names

visualize.broswer <- function(graph){
  nodes <- as.data.frame(vertex_attr(graph, "name"))
  colnames(nodes) <- "name"
  nodes$name <- as.character(nodes$name)
  nodes$indegree <- degree(graph, mode = "in")
  nodes$outdegree <- degree(graph, mode = "out")
  net <- allNet(incidence=as_adjacency_matrix(graph),nodes=nodes)
  plot(net)
}
  
visualize.broswer(ilazega.net)

###############################################################
### Question (b)
detach(package:igraph)
library(statnet)
library(ggplot2)
par(mar = rep(0.15, 4))
plot(lazega.net, displaylabels = T, vertex.cex=3, label.cex=0.5, label.pos=5, pad=0.1)
png(filename = "lazega.png", width = 10000, height = 5000, pointsize = 100)
plot(lazega.net, displaylabels = T, vertex.cex=3, label.cex=0.5, label.pos=5, pad=0.1)
dev.off()

# diagram 1 
nodeColours <- ifelse(v.attr.lazega$Gender=="1", "dodgerblue", "hotpink")
lazega.school.factor <- as.factor(v.attr.lazega$School)
cex_age <- v.attr.lazega$Age
side_nb <- 3:7

par(mar = rep(0.2,4))
gplot(lazega.net, mode = "kamadakawai", 
      label = v.attr.lazega$Office, label.cex=0.6, pad=0.1, #label
      arrowhead = F, vertex.col = nodeColours, #color
      vertex.sides = side_nb[lazega.school.factor], #node shape
      vertex.cex = log(cex_age-30)-1.2) #node size 
legend("bottom", legend = c("Male", "Female"), col = c("dodgerblue","hotpink"), pch=19, pt.cex=1.5, title = "Gender")
legend("bottomleft", legend = c("Office 1", "Office 2", "Office 3"), title = "Node label")
title("Kamada-Kawai layout algorithm", line = -2)

# diagram 2
lazega.practice.factor <- as.factor(v.attr.lazega$Practice)
cex_yrs <- v.attr.lazega$Years 
side_nb <- 3:7

par(mar = rep(0.2,4))
gplot(lazega.net, mode = "kamadakawai", 
      label = v.attr.lazega$School, label.cex=0.6, pad=0.1, #label
      arrowhead = F, vertex.col = nodeColours, #color
      vertex.sides = side_nb[lazega.practice.factor], #node shape
      vertex.cex = log(cex_yrs)-1) #node size 
legend("bottom", legend = c("Male", "Female"), col = c("dodgerblue","hotpink"), pch=19, pt.cex=1.5)
legend("bottomleft", legend = c("School 1", "School 2", "School 3"), title = "Node label")
legend("bottomright", legend = c("Practice 1", "Practice 2"), pch = c(0,2))
title("Kamada-Kawai layout algorithm", line = -2)

###############################################################
## Question (c)
gden(lazega.net)
is.connected(lazega.net)
is.connected(lazega.net, connected = "weak")

###############################################################
## Question (d)

#node centrality 
detach(package:igraph)
library(statnet)
deg <- degree(lazega.net,rescale = TRUE)
summary(deg)

#closeness centrality
lazega.gdist <- geodist(lazega.net)$gdist
1/sum(lazega.gdist)[1,2:network.size(lazega.net)]
which(lazega.gdist[,1]=="Inf")
sum(1/lazega.gdist[1,2:network.size(lazega.net)])
closeness2 <- function(x){
  geo <- 1/geodist(x)$gdist
  diag(geo) <- 0
  apply(geo,1,sum)
}
close2 <- closeness2(lazega.net)

#betweenness centrality 
between <- betweenness(lazega.net)

#centralization
centralization(lazega.net, degree, cmode = "indegree")

cd <- centralization(lazega.net, degree)
cc <- centralization(lazega.net, closeness, cmode = "suminvdir")
cb <- centralization(lazega.net, betweenness)
#Bonacich power centrality 
power_centrality(ilazega.net)


##############################################################
## Question (e)
detach(package:statnet)
library(igraph)
library(intergraph) 
#k-core structure analysis
lazega.coreness <- graph.coreness(ilazega.net)
table(lazega.coreness)

kc<-kcores(lazega.net, cmode="indegree")
#plot the nest of cores
gplot(lazega.net, displaylabels = TRUE, usearrows = FALSE,vertex.col=rainbow(max(kc)+1)[kc+1])
#plot only 2-core 
gplot(lazega.net[kc>1,kc>1], displaylabels = TRUE, vertex.col=rainbow(max(kc)+1)[kc[kc>1]+1])

V(ilazega.net)$name <- get.vertex.attribute(ilazega.net, name = "vertex.names", index = V(ilazega.net))
V(ilazega.net)$color <- lazega.coreness + 1

lay <- layout.fruchterman.reingold(ilazega.net)
op <- par(mfrow=c(1,2), mar = rep(0,4))
# In igraph, name attribute is used by default to label the nodes in a plot
plot(ilazega.net, vertex.size = 15, vertex.label.cex = 0.7, edge.arrow.size = 0.5, layout=lay) 
# Label the nodes with their k-core membership value
plot(ilazega.net, vertex.label = lazega.coreness, vertex.label.cex = 0.7, edge.arrow.size = 0.5, layout=lay) 
par(op)

ilazega.net_core1 <- induced.subgraph(ilazega.net, vids = which(lazega.coreness >= 1))
ilazega.net_core3 <- induced.subgraph(ilazega.net, vids = which(lazega.coreness >= 3))
ilazega.net_core5 <- induced.subgraph(ilazega.net, vids = which(lazega.coreness >= 5))

lay <- layout.fruchterman.reingold(ilazega.net)
op <- par(mfrow=c(2,2), mar = c(2,0,2,0))
plot(ilazega.net, vertex.label = lazega.coreness, layout = lay, main = "All k-cores", edge.arrow.size = 0.1)
plot(ilazega.net_core1, vertex.label = lazega.coreness[which(lazega.coreness >=1)], layout = lay[which(lazega.coreness >=1),], main = "k-cores 1+", edge.arrow.size = 0.1)
plot(ilazega.net_core3, vertex.label = lazega.coreness[which(lazega.coreness >=3)], layout = lay[which(lazega.coreness >=3),], main = "k-cores 3+", edge.arrow.size = 0.1)
plot(ilazega.net_core5, vertex.label = lazega.coreness[which(lazega.coreness >=5)], layout = lay[which(lazega.coreness >=5),], main = "k-cores 5+", edge.arrow.size = 0.1)


###############################################################
## Question (f)

mod_sex <- modularity(ilazega.net, V(ilazega.net)$sex, weights = E(ilazega.net)$strength) 
mod_office <- modularity(ilazega.net, V(ilazega.net)$office, weights = E(ilazega.net)$strength) 
mod_years <- modularity(ilazega.net, V(ilazega.net)$years, weights = E(ilazega.net)$strength) 
mod_age <- modularity(ilazega.net, V(ilazega.net)$age, weights = E(ilazega.net)$strength) 
mod_practice <- modularity(ilazega.net, V(ilazega.net)$practice, weights = E(ilazega.net)$strength) 
mod_school <- modularity(ilazega.net, V(ilazega.net)$school, weights = E(ilazega.net)$strength) 

mod_table <- matrix(c(mod_sex, mod_office, mod_years, mod_age, mod_practice,mod_school), 
                    ncol = 6, byrow = T)
colnames(mod_table) <- c("Sex", "Office", "Years", "Age", "Practice", "School")
rownames(mod_table) <- "Modularity"
as.table(mod_table)

###############################################################
## Question (g)

# remove the isolates 
lazega.iso.net <- lazega.net
delete.vertices(lazega.iso.net, isolates(lazega.iso.net))
lazega.iso <- lazega.iso.net
plot(lazega.iso)
ilazega.iso <- asIgraph(lazega.iso)

lazega.cw <- cluster_walktrap(ilazega.iso)
class(lazega.cw)
membership(lazega.cw)
mod_cw <- modularity(lazega.cw)



#using network with isolates 
lazega.edge <- cluster_edge_betweenness(ilazega.net)
mod_edge <- modularity(lazega.edge)

lazega.greedy <- cluster_fast_greedy(ilazega.net)
mod_greedy <- modularity(lazega.greedy)

#plots
par(mfrow=c(2,2), mar = c(2,0,2,0))
lay2 <- layout.fruchterman.reingold(ilazega.iso)
lay3 <- layout.fruchterman.reingold(ilazega.net)
plot(lazega.cw, ilazega.iso, vertex.label = V(ilazega.iso)$office, layout = lay2, main = "Walktrap")
plot(lazega.edge, ilazega.net, vertex.label = V(ilazega.net)$office, layout = lay3, main = "Edge betweenness")
plot(lazega.greedy, ilazega.net, vertex.label = V(ilazega.net)$office, layout = lay3, main = "Fastgreedy")

mod_table_argo <- matrix(c(mod_cw, mod_edge, mod_greedy), ncol = 3, byrow = T)
colnames(mod_table_argo) <- c("Walktrap", "Edge Betweenness", "Fastgreedy")
rownames(mod_table_argo) <- "Modularity"
as.table(mod_table_argo)






###############################################################