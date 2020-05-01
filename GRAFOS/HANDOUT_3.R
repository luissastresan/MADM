rm(ls())
library(igraph)
library(network)
library(printr)

#(1) Its adjacency matrix
#Primero trazamos el grafo sugerido:

G0 <- graph(edges = c('4','2','5','9','4','5','2','9','1','5','1','4','1',
                      '8','4','3','4','6','3','6','6','7','6','8','3','2','5','8'), directed = FALSE)
plot(G0, vertex.color = "gold", vertex.size = 15)
# Ahora buscamos la matriz adyacente:

as_adjacency_matrix(G0)

#(2) Its density

edge_density(G0, loops = FALSE)

#(3) The degree of each node

degree(G0)["1"]
degree(G0)["2"]
degree(G0)["3"]
degree(G0)["4"]
degree(G0)["5"]
degree(G0)["6"]
degree(G0)["7"]
degree(G0)["8"]
degree(G0)["9"]

#(4) Its average degree and its degrees standard deviation
#Primero calculamos los grados totales del grafo y le asignamos
#una variable. Despues calculamos su media:

degree(G0)
G1 <- degree(G0)
mean(G1)

#Ahora queremos la desviacion tipica de los grados del grafo, entonces
#calculamos su varianza y aplicamos la raiz cuadrada:

var(G1)
x <- var(G1)
sqrt(x)

#(5) Its degrees distribution and cumulative distribution, and plot them

degree.distribution(G0, cumulative = FALSE) # Grados de distribucion
degree_distribution(G0, cumulative = TRUE) # Grados de distribucion acumulativo

y <- degree.distribution(G0, cumulative = FALSE)
z <- degree_distribution(G0, cumulative = TRUE)
hist.default(y, col = 'green')
hist.default(z, col = 'green')

#(6) The distances from 1 to the other nodes

distances(G0, "1","2")
distances(G0, "1","3")
distances(G0, "1","4")
distances(G0, "1","5")
distances(G0, "1","6")
distances(G0, "1","7")
distances(G0, "1","8")
distances(G0, "1","9")

#(7) Its diameter, and a pair of nodes at maximum distance

diameter(G0)
eccentricity(G0)
radius(G0)

#(8) The clustering coefficient of each node, and a node with maximum clustering coefficient

transitivity(G0, type = "local")
max.node <- transitivity(G0, type = "local")
max(max.node)

#(9) Its average clustering coefficient

transitivity(G0, type = "average")

#(10) Its transitivity coefficient

transitivity(G0, type = "global")

#(11) Its average distance

mean_distance(G0)

#(12) Its relative hop plot (and plot it)

rel.hop.plot=function(G){
  F=function(x){length(unlist(ego(G,order=x,nodes=V(G))))/length(V(G))^2}
  sapply(1:diameter(G),FUN=F)}
plot(rel.hop.plot(G0),pch=20,main="Relative hop plot",xlab="Hops",ylab="Probability",type="o")

#(13) Its 90% effective diameter (the smallest distance d such that at least 
#a 90% of pairs of different nodes are at distance at most d).






