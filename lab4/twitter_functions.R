
#####################################################################################################################
# Helper functions for twitter network analysis
# Author: Tod Van Gunten, University of Edinburgh (tvangun@ed.ac.uk)
# This version: 4/2/2020
# 
#
#####################################################################################################################


get.retweet.network <- function(tweet.data){
  d <- tweet.data[tweet.data$is_retweet==T,]                        # select only rows that contain retweets
  el <- data.frame(from = d$retweet_screen_name, to =d$screen_name, n = 1) # create a data frame that contains the source (from) and retweeter (to). n is just a placeholder to sum up on the next line
  el <- aggregate(n ~ from + to, data = el, FUN = function(x){NROW(x)}) # count over dyads to produce a weighted edgelist
  colnames(el)[3] <- "weight"    
  return(el)
}

# get.mentions.network retrieves an edgelist with all mentions
# from is the person mentioning, and to is the person mentioned

get.mentions.network <- function(tweet.data){
  d <- tweet.data[!is.na(tweet.data$mentions_screen_name),] # select only tweets that mention another account
  to <- d$mentions_screen_name                              # to = as returned by the search_tweets function, a list of the mentions
  from <- rep(d$screen_name, lapply(to, length))            # from = the tweeter repeated n times, where n = the number of mentions
  el <- data.frame(from=unlist(from),to = unlist(to), n=1)       # put this in a data frame
  el <- aggregate(n ~ from + to, data = el, FUN = function(x){NROW(x)})                          # count over dyads
  colnames(el)[3] <- "weight"                                  # rename column 3 'weight' in accordance with igraph requirements
  return(el)
}

# this is a function to extract the 'main'/'giant' component

giant.component <- function(graph, ...) {
  cl <- clusters(graph, ...)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

# this is a function to reduce the size of the network by filtering by degree, i.e. including only nodes above

degree.filter <- function(graph, mode = "total", threshold = 0){
  n <- names(which(degree(graph, mode = mode)>=threshold))
  d <- length(unique(vertex_attr(graph, "name"))) - length(unique(n))
  mode[mode=="all"] <- "total" 
  print(paste0("Removing ", d, " nodes with ", mode, " degree less than ", threshold))
  induced_subgraph(graph,n)
  #return(length(unique(n)))
}


visualize.browser <- function(graph){
    nodes <- as.data.frame(vertex_attr(graph, "name"))
    colnames(nodes) <- "name"
    nodes$name <-as.character(nodes$name)
    nodes$indegree <- degree(graph, mode = "in")
    nodes$outdegree <- degree(graph, mode = "out")
    m <- as_adjacency_matrix(as.undirected(graph, mode = "collapse"))
    net <- allNet(incidence=m,nodes=nodes)
    plot(net)
    #return(net)
}


combine.search.results <- function(l){
  r <- do.call(rbind, l)
  d <- duplicated(r)
  print(paste0("dropping ", length(which(d==T)), " duplicate tweets"))
  #return(list(r,d))
  return(r[which(d==T),])
}


