######################################################################
#######################################################################

## Course Analysing Social Networks with Statistics 2019-20
## Lab 7 (online)
## Testing hypothesis
## Gil Viry
## Version 19 March 2020
## Version for class

#######################################################################
#######################################################################

#Be sure to set your working directory on your home drive (University server)
#Check that your working directory is on your home drive (University server) if you are working on a computer lab
getwd() # Check what directory you're in
# Should be "M:R"
# if this is not the case:
# setwd("M:/R") #set your working directory (on your home drive)


# If not done before, download the RData file "data_lab7_ASNwS2020" from Learn and save it on your home drive
load("data_lab7.RData")

list.files() # Check what's in the working directory
ls() # same

# load statnet
library(statnet)


#######################################################################
#######################################################################
## 7.1 QAP permutation test
## Padgett's Florentine families data

data(package="ergm") # List available datasets in the ergm package
data(florentine) # Load two built-in datasets of Padgett: Florentine family marriage (flomarriage) and Florentine family business (flobusiness)
flomarriage # Examine the network object
flobusiness # Same for business ties
as.sociomatrix(flomarriage) # Examine the marriage alliances as an adjacency matrix
as.sociomatrix(flobusiness) # Examine the marriage alliances as an adjacency matrix

op <- par(mfrow=c(1,2), xpd = NA, oma = c(0, 1, 0, 1))
my_coord <- plot(flomarriage, displaylabels = TRUE, vertex.cex = 1.2, main = "marriage alliance")
plot(flobusiness, displaylabels = TRUE, vertex.cex = 1.2, main = "business ties", coord = my_coord)
par(op)

#For more information...
?flomarriage
?flobusiness

## QAP test of network correlation between marriage and business ties

g <- array(dim = c(2,16,16))
flomarriage.mat <- as.sociomatrix(flomarriage)
flobusiness.mat <- as.sociomatrix(flobusiness)
g[1,,] <- flomarriage.mat
g[2,,] <- flobusiness.mat
qap_result <- qaptest(g, gcor, g1=1,g2=2, mode = "graph", reps = 5000)
summary(qap_result)
plot(qap_result)


## Campnet data

Camp_attr.df # Campnet attribute file
Camp[1,,] # Week 2 (1=weakest tie, 17=strongest ties)
Camp[2,,] # Week 3


# Function to build a matrix based on a **categorical** attribute (vector)
v2m<-function(y){
  yi <- as.matrix(y)
  n <- dim(yi)[1]
  ym <- matrix(nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      if (yi[i]==yi[j]){ym[i,j]<-1}
      else{ym[i,j]<-0}
    }
  }
  return(ym)
}


# Function to build a matrix based on a **continuous** attribute (vector)
# The function builds a node-by-node matrix of differences (e.g. years of seniority in a company)
cv2m<-function(y){
  yi <- as.matrix(y)
  n <- dim(yi)[1]
  ym <- matrix(nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      ym[i,j]<-yi[i]-yi[j]
    }
  }
  return(ym)
}



Camp_gender.mat <- v2m(Camp_attr.df$Gender)
row.names(Camp_gender.mat) <- row.names(Camp[1,,])
colnames(Camp_gender.mat) <- colnames(Camp[1,,])
Camp_gender.mat

## QAP test of network correlation between 'is the same gender' matrix and Week 3 Camp data 

g <- array(dim = c(2,18,18))
g[1,,] <- Camp_gender.mat
g[2,,] <- Camp[2,,]
qap_result <- qaptest(g, gcor, g1=1,g2=2, mode = "digraph", reps = 5000)
summary(qap_result)
plot(qap_result)

#For more information
?qaptest


#######################################################################
#######################################################################
## 7.2 QAP regression


# linear regression with Padgett network
mrqap_result<-netlm(flobusiness, flomarriage, mode="graph", nullhyp=c("qapspp"), test.statistic = c("t-value"), tol=1e-7, reps=2000)
mrqap_result

# Same but with the logistic regression
lrqap_result<-netlogit(flobusiness, flomarriage, mode="graph", nullhyp=c("qap"), test.statistic = c("beta"), tol=1e-7, reps=2000)
lrqap_result

# Logistic regression with Camp data
# goal: Are new friendship ties formed in Week 3 explained by (1) reciprocity and (2) transitivity of ties formed in Week 2, 
# regardless of gender and roles

# We first dichotomise the data, by selecting the three top friends

Camp_w2_di.mat <- Camp[1,,]
Camp_w2_di.mat[Camp[1,,]>=15 & Camp[1,,]<=17] <- 1
Camp_w2_di.mat[Camp[1,,]>=1 & Camp[1,,]<=14] <- 0
Camp_w2_di.mat

Camp_w3_di.mat <- Camp[2,,]
Camp_w3_di.mat[Camp[2,,]>=15 & Camp[2,,]<=17] <- 1
Camp_w3_di.mat[Camp[2,,]>=1 & Camp[2,,]<=14] <- 0
Camp_w3_di.mat


# Transpose of Week 2 data (reciprocity)

Camp_w2_di.mat.tr <- t(Camp_w2_di.mat)
Camp_w2_di.mat
Camp_w2_di.mat.tr

# The Week 2 data squared (equates to the number of paths of length 2)
# and dichotomise (transitivity: indicates if there is or not a path of length 2 between any two actors)

Camp_w2_di.mat.sq <- Camp_w2_di.mat %*% Camp_w2_di.mat
Camp_w2_di.mat.sq

Camp_w2_di.mat.sq_di <- Camp_w2_di.mat.sq
Camp_w2_di.mat.sq_di[Camp_w2_di.mat.sq>1] <- 1
Camp_w2_di.mat.sq_di

# Construct a 'has the same role' matrix

Camp_role.mat <- v2m(Camp_attr.df$Role)
row.names(Camp_role.mat) <- row.names(Camp[1,,])
colnames(Camp_role.mat) <- colnames(Camp[1,,])
Camp_role.mat

# creates an array including the five independent variables:
# (1) Week 2 dichotomised data, (2) reciprocity, (3) transitivity, (4) gender and (5) role

g <- array(dim = c(5,18,18))
g[1,,] <- Camp_w2_di.mat
g[2,,] <- Camp_w2_di.mat.tr
g[3,,] <- Camp_w2_di.mat.sq_di
g[4,,] <- Camp_gender.mat
g[5,,] <- Camp_role.mat


lrqap_result<-netlogit(Camp_w3_di.mat, g, mode="digraph", nullhyp=c("qap"), test.statistic = c("z-value","beta"), tol=1e-7, reps=2000)
lrqap_result
# Contrary to the LR-QAP run in UCINet, reciprocity has a significant effect (in addition to transitivity and gender)


# Example of node-level hypothesis
# Hypothesis: women have higher indegree score than men.
# We first perform a standard multiple regression using the dependent and independent vectors
# We then permute the elements of the dependent vector and recompute the regression a thousand of times
# For each coefficient, we count the proportion of random permutations that yielded a coefficient
# as large/small/extreme as the one computed with the original vector


Camp_indeg <- sna::degree(Camp[2,,], cmode="indegree") 
Camp_indeg

x <- as.matrix(Camp_attr.df$Gender)
y <- as.matrix(Camp_indeg)

lm_result<-lm(y ~ x)
summary(lm_result)
coefficients(lm_result)[2] #extract the regression coefficient
anova(lm_result)[[5]][1] #extract the p-value


QAPv<-function(y){  # y is a vector
  n<-dim(y)[1]
  nid<-seq(1,n,by=1)[order(runif(n))] # n nodes into a vector of 1 to n, runif = generates randomly uniformed number
  yp<-y[nid]
  return(yp) # yp permuted vector
}

x <- as.matrix(Camp_attr.df$Gender)
y <- as.matrix(Camp_indeg)
count_large <- 0
count_small <- 0
count_extreme <- 0

for (i in 1:1000){
  yp <- QAPv(y)
  yp_mat <- as.matrix(yp)
  lm_result_p<-lm(yp_mat ~ x)
  if(coefficients(lm_result_p)[2]>= coefficients(lm_result)[2]){count_large <- count_large + 1}
  if(coefficients(lm_result_p)[2]<= coefficients(lm_result)[2]){count_small <- count_small + 1}
  if(abs(coefficients(lm_result_p)[2])>= abs(coefficients(lm_result)[2])){count_extreme <- count_extreme + 1}
}
p_value_large <- count_large/1000
p_value_small <- count_small/1000
p_value_extreme <- count_extreme/1000

p_value <- c(p_value_large, p_value_small,p_value_extreme)
p_value

#For more information
?netlm
?netlogit

#######################################################################
#######################################################################
## 7.3 Saving and reloading your current workspace


save.image("M:/R/lab7_ASNwS.RData")

# Reload the workspace previously saved
load("M:/R/lab7_ASNwS.RData")


#######################################################################
#######################################################################
## 7.4 Practical Exercise 7

# If not done before, download the RData file "data_lab7_ASNwS2020.RData" from Learn and save it on your home drive
load("data_lab7_ASNwS2020.RData")
Krack[1,,]
Krack_attr.df
class(Krack)
class(Krack_attr.df)

# Load statnet
library(statnet)









