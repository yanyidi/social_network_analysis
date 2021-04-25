library(kableExtra)
library(dplyr)
######################################################################
## descriptive network analysis

#density
adv.den <- gden(adv.net) #0.452381
adv.rec <- grecip(adv.net) #0.5238095 
adv.tran <- gtrans(adv.net) #0.6639785

fri.den <- gden(fri.net) #0.2428571
fri.rec <- grecip(fri.net) #0.7333333
fri.tran <- gtrans(fri.net) #0.4610526

adv.density <- rbind(adv.den, adv.rec, adv.tran)
fri.density <- rbind(fri.den,fri.rec, fri.tran)
table.density <- round(cbind(adv.density,fri.density),3)
rownames(table.density) <- c("Density", "Reciprocity", "Transivitiy")
colnames(table.density) <- c("Advice", "Friendship")

kable(table.density, "latex", booktabs = T, caption = "The summary of network cohesion measures") %>%
  save_kable("/Users/kate/desktop/SNA/SNA_final/table1.png")

#centrality
adv.in <- sna::degree(adv.net,cmode = "indegree")
adv.out <- sna::degree(adv.net,cmode = "outdegree")
adv.bet <- round(betweenness(iadv.net),2)

fri.in <- sna::degree(fri.net,cmode = "indegree")
fri.out <- sna::degree(fri.net,cmode = "outdegree")
fri.bet <- round(betweenness(ifri.net),2)

table.degree <- t(rbind(adv.in,adv.out,adv.bet,fri.in,fri.out,fri.bet))
colnames(table.degree) <- c("In", "Out","Betw", "In", "Out","Betw")
rownames(table.degree) <- paste0('ID', 1:21)

kable(table.degree,"latex", longtable = T, booktabs = T, caption = "The summary of node centrality measures", label = NULL) %>%
  add_header_above(c("", "Advice"=3,"Friendship"=3))%>%
  save_kable("/Users/kate/desktop/SNA/SNA_final/table2.png")


##########################################################################################
##########################################################################################
## k-cores cohesive groups
kcores(adv.net)
kcores(fri.net)

kc<-kcores(fri.net, cmode="indegree")
gplot(fri.net, displaylabels = TRUE, vertex.cex = 1.6,
      vertex.col=rev(topo.colors(max(kc)+1))[kc])

# Mapping the nested structure of k-cores
fri_coreness <- graph.coreness(ifri.net)
table(fri_coreness)

ifri.net_core2 <- induced.subgraph(ifri.net, vids = which(fri_coreness > 2))
ifri.net_core4 <- induced.subgraph(ifri.net, vids = which(fri_coreness > 4))
ifri.net_core6 <- induced.subgraph(ifri.net, vids = which(fri_coreness > 6))

op <- par(mfrow=c(2,2), mar = c(2,0,2,0))
lay <- layout.fruchterman.reingold(ifri.net)

plot(ifri.net, layout = lay, main = "All k-cores", edge.arrow.size = 0.1)
plot(ifri.net_core2, layout = lay[which(fri_coreness > 2),], main = "k-cores 2+", edge.arrow.size = 0.1)
plot(ifri.net_core4, layout = lay[which(fri_coreness > 4),], main = "k-cores 4+", edge.arrow.size = 0.1)
plot(ifri.net_core6, layout = lay[which(fri_coreness > 6),], main = "k-cores 6+", edge.arrow.size = 0.1)
par(op)


##########################################################################################
##########################################################################################
#H1:seek advice from friends rather than boss
regqap <- netlogit(k.adv, list(k.fri,k.rep), mode = "diagraph", nullhyp = c("qap"), tol = 1e-7, reps = 2000)
regqap

fri_adv.cor <- qaptest(list(k.adv,k.fri), gcor, g1=1,g2=2, mode = "digraph", reps = 5000)
plot(fri_adv.cor)

adv_rep.cor <- qaptest(list(k.adv,k.rep), gcor, g1=1,g2=2, mode = "digraph", reps = 5000)
plot(adv_rep.cor)

#H2:develop friendship within department and level
fri_dept.cor <- qaptest(list(dept.mat,k.fri), gcor, g1=1,g2=2, mode = "digraph", reps = 5000)
fri_dept.cor
plot(fri_dept.cor)

lev.mat <- v2m(Krack_attr.df$LEVEL)
row.names(lev.mat) <- row.names(k.adv)
colnames(lev.mat) <- colnames(k.adv)
fri_lev.cor <- qaptest(list(lev.mat,k.fri), gcor, g1=1,g2=2, mode = "digraph", reps = 5000)
plot(fri_lev.cor)

fri_dept.logit <- netlogit(k.fri, list(dept.mat,lev.mat), mode = "diagraph", nullhyp = c("qap"), tol = 1e-7, reps = 2000)

#H3:seek advice within department and from people of longer tenure. 
ten.diff.mat <- cv2m(Krack_attr.df$TENURE)
rownames(ten.diff.mat) <- rownames(k.adv)
colnames(ten.diff.mat) <- colnames(k.adv)
adv_ten.cor <- qaptest(list(k.adv, ten.diff.mat), gcor, g1=1,g2=2,
                       mode = "diagraph", reps = 5000)
plot(adv_ten.cor) #big p-value
adv_ten.logit <- netlogit(k.adv, ten.diff.mat, mode = "diagraph", nullhyp = c("qap"), tol = 1e-7, reps = 2000)

lev.diff.mat <- cv2m(Krack_attr.df$LEVEL)
rownames(lev.diff.mat) <- rownames(k.adv)
colnames(lev.diff.mat) <- colnames(k.adv)
adv_lev.cor <- qaptest(list(k.adv, lev.diff.mat), gcor, g1=1,g2=2,
                       mode = "diagraph", reps = 5000)
plot(adv_lev.cor)

adv_lev.logit <- netlogit(k.adv, lev.diff.mat, mode = "diagraph", nullhyp = c("qap"), tol = 1e-7, reps = 2000)

#H4:longer tenure have more friends(indegree+outdegree) than those of shorter tenure
fri.degree <- fri.in + fri.out
lm_result <- lm(fri.degree ~ tenure)
count_large <- 0
count_small <- 0
count_extreme <- 0
x <- as.matrix(Krack_attr.df$TENURE)
y <- as.matrix(fri.degree)

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




