# Twitter network Lab exercise 
# Social Network Research/Analyzing Social networks with statistics
# University of Edinburgh
# Author: Tod Van Gunten (tvangun@ed.ac.uk)
# Revised Gil Viry (gil.viry@ed.ac.uk)
# Date: 5 February 2020

# in order to run a block of script, select the block and press CTRL-RETURN

# a note on comments for R novices:
# any line that begins with a '#' is a COMMENT (like this line)
# this means that this is not treated as code when processing the script, and can contain instructions or information for 
# humans (as opposed to the computer)
# you can also put a '#' in the middle of a line, and everything after it (on the line) will be ignored
# so you can use comments to create notes
# you can also 'comment out' lines of code by adding '#' at the beginning
# for example, you only need to run the 'install.packages' lines the first time you need this package on a particular computer
# so you can 'comment out' these lines after the first time

### SET WORKING DIRECTORY
# If you are working on a lab computer, check that your working directory is on your home drive (University server)
getwd() # Check what directory you're in
# Should be "M:R"
# if this is not the case:
# setwd("M:/R") #set your working directory (on your home drive)

# If you are working on your own computer, you can change your working directory to an appropriate location on your computer
# the first time you run this, it can be helpful to change the directory manually:
# go to session --> set working directory -- > choose directory
# you can then copy and paste from the console (output) below for next time you run
setwd("twitter_lab")


### SAVING YOUR FILE
## Save the current document under a new name for editing it: File --> Edit --> Save as
## Then regularly save your document: Ctrl+S


# install packages
# on your own computer: 
install.packages(c("rtweet", "igraph", "ggplot2", "netCoin"))


# from the lab in 50 George square, you only need to install retweet and netCoin
install.packages(c("rtweet", "netCoin"))

library(rtweet)
library(igraph)
library(ggplot2)
library(netCoin)

### LOAD HELPER FUNCTIONS
# this loads helper functions written by Tod Van Gunten
# for extracting, manipulating, and visualizing retweet and mentions networks
# you need to have previously saved the file twitter_functions.R in your working directory

source("twitter_functions.R")


############################################################################################################
### GETTING ACCESS TO THE TWITTER API
# replace the words FILL IN HERE with the characters you got when you created
# a twitter developer account

create_token(
    app = "yandi_twitter",
    consumer_key = "e0h6G4RHxzSXoXRLZutCVEnbp",
    consumer_secret = "xQ2Qa0daNtLTDBP6shg5P7zG9Su1PVHon1HEn64ejvXnv6hBeq",
    access_token = "852005599508406272-kavOCnn3u0Oo44pJB38dN8H2wGe9COu",
    access_secret = "eYluPGyr8R3yTpALbE9yhBOs0zBOJuFbPNAj4aXif3WWj")

# The create_token() function automatically save your token as an environment variable, so you should be set for future sessions as well.
# To make sure it worked, compare the created token object to the object returned by
get_token()

############################################################################################################
# SEARCH TWITTER API
# 

search.query <- "#brexit"    # this is what you are searching for.  replace with whatever you are interested in

# note: if you run multiple searches, you may need to rename the object 'search.results'
# note: this can be slow and you may need to switch to your browser to give permission
search.results <- search_tweets(search.query, 
                                n = 1000, # number of results to retrieve,maximum is 18000 but this can be slow
                                include_rts = T)

# IT IS ALWAYS A GOOD IDEA TO SAVE YOUR SEARCH
# next time, you will get different results
# note that the function 'save_as_csv' is specific to the twitter search results returned by the rtweet package
save_as_csv(search.results, "search-results-brexit.csv") # be sure to give this an appropriate name

#################################################################################
# LOADING SAVED DATA
# sometimes you will want to save the data and look at it later
# if so you can load the data like this
# here it is unnecessary, since we are just reloading the data we just saved

search.results <- read_twitter_csv("search-results-brexit.csv")

#######################################################################################
# inspecting data
# it can be useful to know how many tweets you have collected, how many users, what the time coverage is, etc

colnames(search.results) # this tells you the names of the different kinds of information returned
View(search.results) # this opens the data in a new tab

nrow(search.results) # this is the number of tweets
length(unique(search.results$screen_name)) # this is the number of users
min(search.results$created_at) # the date and time of the earliest tweet
max(search.results$created_at) # the date and time of the last tweet

# you can also plot the frequency of posts over time
ts_plot(search.results, "5 minutes") # note that you can adjust this interval to 'smooth' the results


############################################################################################################
# GENERATE NETWORKS 

# extract the retweet network from search results, using the function loaded above
# note that these functions return EDGELISTS
# which is why the objects are called .el
brexit.rt.el <- get.retweet.network(search.results) # generate network

# extract the mention network from search results
brexit.mentions.el <- get.mentions.network(search.results) # generate network

# inspect this object, what does each row refer to?  what do the numbers refer to?
# how many retweets/mentions are in our data?
View(brexit.rt.el)


# save the networks to look at it in UCINet, etc.
write.csv(brexit.rt.el, "brexit-rt-el.csv") #be sure to give this an appropriate name
write.csv(brexit.mentions.el, "brexit-mentions-el.csv") #be sure to give this an appropriate name

####################################################################################
# examine the networks in igraph
# It can be quite helpful to do this in R rather than UCINet, as the networks are large

# we need to convert the objects to igraph format
brexit.rt.igraph <- graph.data.frame(brexit.rt.el, directed=T)
brexit.mentions.igraph <- graph.data.frame(brexit.mentions.el, directed=T)

plot(brexit.rt.igraph) # note that the default plots settings are not helpful

plot(brexit.rt.igraph,
     vertex.size=3,       #make the nodes smaller
     vertex.label=NA,     #drop the labels
     edge.arrow.size=.5)  #make the arrows smaller

plot(simplify(brexit.rt.igraph), #adding simplify() removes the loops
     vertex.size=3,       
     vertex.label=NA,     
     edge.arrow.size=0) #although arrows are relevant information here, they clutter the diagram too much

#we are often most interested in the main component (largest connected subgraph)

plot(giant.component(simplify(brexit.rt.igraph)), 
     vertex.size=3,       
     vertex.label=NA,     
     edge.arrow.size=0)




#############################################################################################
### reducing the size of the network
# the full network may be very large and hard to interpret
# sometimes looking at only the main component is not enough
# another way to explore the network is by 'filtering' to include only some nodes
# for example, include only nodes with a total degree (in- + out-degree) greater or equal to 2 (at least two ties)

brexit.rt.igraph.filtered <- degree.filter(giant.component(brexit.rt.igraph), threshold = 2, mode = "total")

# try changing the threshold and changing "total" to "in" or "out" and then visualise the network again


###############################################################################################
### visualizing in your browser
# this is one useful way to visualise the network in your web browser
# this will allow you to manipulate the network, scale the nodes by degree, etc
# this can be very slow with a large network - one reason to reduce the size first

visualize.browser(giant.component(simplify(brexit.rt.igraph.filtered)))



###############################################################################################
### identifying the most influential nodes

sort(degree(brexit.rt.igraph, mode = "out"), decreasing = T)[1:10]

# alternatively:

centrality <- data.frame(screen_name = names(degree(brexit.rt.igraph, mode = "out")),
                         degree.out = degree(brexit.rt.igraph, mode = "out"))

View(centrality) # click on the small arrow in the corner of the 'degree.out' header to sort


###############################################################################################
### aggregating searches
# not covered in lab
# if you want to analyse twitter networks for your final essay, you might need to run multiple searches and combine the results
# for example, you might want to look at all tweets during one week
# also the twitter API will only return ~18000 tweets at a time
# this might be just a few minutes worth of tweets!

# load two previously saved (compararable) searches
search.results1 <- read_twitter_csv("brexit_data_lab4_twitter/brexit_1_2_1100AM.csv")
search.results2 <- read_twitter_csv("brexit_data_lab4_twitter/brexit_1_2_1145AM.csv")

# note that the search results overlap
max(search.results1$created_at) # the time of the last tweet in the first data set
min(search.results2$created_at) # the time of the first tweet in the second data set
# this means that some tweets are included in both datasets and you will have duplicate records

pooled.results <- combine.search.results(list(search.results1, search.results2))


# you can now extract networks as above on the pooled results


