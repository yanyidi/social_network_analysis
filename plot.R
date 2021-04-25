library(fields)

pal1 <- rev(topo.colors(36, alpha=1))
pal11 <- rev(topo.colors(31, alpha=1))
pal2 <- rev(topo.colors(20:70, alpha=0.8))
pal3 <- c("gray50", "tomato", "gold")
pal4 <- brewer.pal(4,"Accent")
pal5 <- brewer.pal(5,"Set3")
shape <- c("sphere", "circle","triangle","square", "star")
shape2 <- c("square","circle","triangle")

#advice network 
plot(iadv.net, edge.arrow.size = 0.14,
     vertex.label.dist = 1.3,vertex.label.degree=0,
     vertex.color = pal1[(V(iadv.net)$age-26)],
     vertex.size = (20-5*V(iadv.net)$level),
     vertex.shape = shape[V(iadv.net)$dept+1],
     main = "Figure 2: Advice Network")

image.plot(legend.only = T, zlim = range(27,62), col = pal1,
           legend.lab = "Age by color", smallplot= c(.83,.86,0.7,0.95))
legend(x = -2.1,y = 1.5, c("President","Vice President", "Manager"), pch = 21, pt.cex = c(3,2,1),
       cex = 1, bty = "n", title = "Level by size")
legend(x = -2.4, y = -0.65, c("President", "Dept.1","Dept.2","Dept.3","Dept.4"), 
       pch = c(19,1,2,0,8), cex=1,bty = "n", title = "Department by shape")

# vertex.size = (25-4.5*V(ifri.net)$level),
#friend network
plot(ifri.net, edge.arrow.size = 0.2,
     vertex.color = pal5[V(ifri.net)$dept+1],
     vertex.size = 4*log(V(ifri.net)$tenure+2),
     vertex.shape = shape2[(V(ifri.net)$level)],
     main = "Figure 3: Friendship Network")

legend(x = 0.6, y = 1.5, fill = pal5, c("President", "Dept.1","Dept.2","Dept.3","Dept.4"), 
       cex = 0.7,bty = "n", title = "Department by color")
legend(x = -1.6,y = -0.2, c("President","Vice President", "Manager"), pch = c(0,1,2),
       cex = 1, bty = "n", title = "Level by shape   ")
legend(x = -1.8,y = 1.2, c(30,15,0), pch = 21, pt.cex = c(5,3,1), 
       cex = 0.8, bty = "n", ncol = 1, title = "Tenure by size")


#report network
plot(irep.net, edge.arrow.size = 0.2, vertex.cex=8,
     vertex.color = pal4[V(irep.net)$level],
     vertex.shape = shape[V(irep.net)$dept+1],
     vertex.size = 5*log((V(irep.net)$tenure+1)),
     main = "Figure 4: Report Network")

legend(x=-1.6,y=1.5, fill = pal4, c("President","Vice President", "Manager"), 
       cex = 0.85,bty = "n", title = "Level by color")
legend(x = -1.85, y = 0.6, c("President", "Dept.1","Dept.2","Dept.3","Dept.4"), 
       pch = c(19,1,2,0,8), cex=0.8,bty = "n", title = "Department by shape")
legend(x = -1.8,y = -0.5, c(30,15,0), pch = 21, pt.cex = c(5,3,0.5), 
       cex = 0.8, bty = "n", ncol = 1, title = "Tenure by size")

###################################################################################
###################################################################################
##table

#H1
table.regqap <- array(dim = c(3,5))
table.regqap[1,] <- c(-0.43,0.65,0.2345,0.7655,0.2345)
table.regqap[2,] <- c(0.58,1.79,0.9385,0.0615,0.1170)
table.regqap[3,] <- c(3.06,21.31,0.9990,0.0010,0.0015)
rownames(table.regqap) <- c("(intercept)","Friend","Report")
colnames(table.regqap) <- c("Estimate","Exp(b)","Pr(<=b)","Pr(>=b)","Pr(>=|b|)")

kable(table.regqap, "latex", booktabs = T, caption = "Hypothesis 1: Result") %>%
  save_kable("/Users/kate/desktop/SNA/SNA_final/table3.png")

#H2
table.h2 <- array(dim = c(3,5))
table.h2[,1] <- c(-1.94,0.99,0.83)
table.h2[,2] <- c(0.143,2.693,2.284)
table.h2[,3] <- c(0.0000,0.000,0.0675)
table.h2[,4] <- c(1.000,0.000,0.024)
table.h2[,5] <- c(0.0000,0.0000,0.0675)
colnames(table.h2) <- c("Estimate","Exp(b)","Pr(<=b)","Pr(>=b)","Pr(>=|b|)")
rownames(table.h2) <- c("(intercept)","Department", "Level")
kable(table.h2, "latex", booktabs = T, caption = "Hypothesis 2: Result") %>%
  save_kable("/Users/kate/desktop/SNA/SNA_final/table4.png")

#H3
table.h3 <- array(dim = c(2,5))
table.h3[1,] <- c(-0.058,0.944, 0.0025,0.9975,0.0075)
table.h3[2,] <- c(0.532,1.703,0.962,0.0385,0.0835)
colnames(table.h3) <- c("Estimated","Exp(b)","Pr(<=b)","Pr(>=b)","Pr(>=|b|)")
rownames(table.h3) <- c("Tenure", "Level")

kable(table.h3, "latex", booktabs = T, caption = "Hypothesis 3: Result") %>%
  save_kable("/Users/kate/desktop/SNA/SNA_final/table5.png")

#H4
table.h4 <- array(dim = c(1,4))
table.h4[1,] <- c(0.068,0.314, 0.686, 0.663)
rownames(table.h4) <- c("Tenure")
colnames(table.h4) <- c("Estimate","Large p-value", "Small p-value", "Extreme p-value")
kable(table.h4, "latex", booktabs = T, caption = "Hypothesis 4: Result") %>%
  save_kable("/Users/kate/desktop/SNA/SNA_final/table6.png")



