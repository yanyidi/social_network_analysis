# try plotting report network with igraph
library(igraph)
library(intergraph)
library(RColorBrewer)

iadv.net <- asIgraph(adv.net)
ifri.net <- asIgraph(fri.net)
irep.net <- asIgraph(rep.net)


V(iadv.net)$name <- V(iadv.net)$vertex.names
V(ifri.net)$name <- V(ifri.net)$vertex.names
V(irep.net)$name <- V(irep.net)$vertex.names

# triangle vertex shape
# ref: https://rdrr.io/cran/igraph/man/shapes.html 
mytriangle <- function(coords, v=NULL, params) {
        vertex.color <- params("vertex", "color")
        if (length(vertex.color) != 1 && !is.null(v)) {
                vertex.color <- vertex.color[v]
        }
        vertex.size <- 1/150 * params("vertex", "size")
        if (length(vertex.size) != 1 && !is.null(v)) {
                vertex.size <- vertex.size[v]
        }
        
        symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
                stars=cbind(vertex.size, vertex.size, vertex.size),
                add=TRUE, inches=FALSE)
}
add_shape("triangle", clip=shapes("circle")$clip,
          plot=mytriangle)

#star shape 
mystar <- function(coords, v=NULL, params) {
        vertex.color <- params("vertex", "color")
        if (length(vertex.color) != 1 && !is.null(v)) {
                vertex.color <- vertex.color[v]
        }
        vertex.size  <- 1/150 * params("vertex", "size")
        if (length(vertex.size) != 1 && !is.null(v)) {
                vertex.size <- vertex.size[v]
        }
        norays <- params("vertex", "norays")
        if (length(norays) != 1 && !is.null(v)) {
                norays <- norays[v]
        }
        
        mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
               FUN=function(x, y, bg, size, nor) {
                       symbols(x=x, y=y, bg=bg,
                               stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
                               add=TRUE, inches=FALSE)
               })
}

add_shape("star", clip=shape_noclip,
          plot=mystar, parameters=list(vertex.norays=5))



#####################################################################

