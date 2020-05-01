rm(list = ls())
library(igraph)

# PREGUNTA 1

g7 <- graph_from_literal(LUIS-ELI:JOSE-ANGIE:ELI-JOSE,TITO-MARIA-BIEL-JOSE-LUIS-SIMON-ELI)
plot(g7)

#PREGUNTA 2

g9 <- graph_from_literal(LUIS-ELI:JOSE-ANGIE:ELI-JOSE,TITO-MARIA-BIEL-JOSE-LUIS-SIMON-ELI, JOSE-TITO-MARIA-BIEL)
plot(g9)

# PREGUNTA 3

E(g7) # Estos son las aristas del grafo del primer ejercicio.
E(g9) # Estos son las aristas del grafo del segundo ejercicio.
V(g7) # Estos son el numero de vertices del grafo del primer ejercicio.
V(g9) # Estos son el numero de vertices del grafo del segundo ejercicio.

# PREGUNTA 4

as_adj(g7) # Matriz adyacente para el primer grafo.
as_adj(g9) # Matriz adyacente para el segundo grafo.

# PREGUNTA 5
# Usare el grafo creado en la segunda pregunta:

# PREGUNTA 6

# Usare el grafo creado en la segunda pregunta:

n <- c(vcount(g9)) # Order: Nos indica en numero de vertices del grafo.
m <- c(ecount(g9)) # Size: Nos indica en nuermo de aristas que tiene el grafo.
dens <- c(graph.density(g9)) # Density: Nos indica la densidad del grafo.

v1 <- c("The order of G is", n, "its size is", m, "and its density is", dens)
v1

# PREGUNTA 7

degree(g9) # Estos son todos los grados de cada nodo.

# PREGUNTA 8

g10 <- graph_from_literal(c-+d:f-+g:f-d,a-+e-+h+-f-c+-b-+d)
plot(g10)

# PREGUNTA 9

vcount(g10)
ecount(g10)

# PREGUNTA 10

as_adj(g10)

# PREGUNTA 11

g12 <- graph_from_literal(c-+d:h+-g:g+-c, a-+e-+h+-d+-b-+c)
plot(g12)

# PREGUNTA 12

g13 <- simplify(g12, remove.multiple = TRUE, remove.loops = TRUE)
plot(g13)

# PREGUNTA 13

degree(g13)
degree(g13, mode = c("out", "in"))
