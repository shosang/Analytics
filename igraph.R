library(igraph)
# Template to plot some simple networks

# Template 1
g1 <- graph( edges=c("O","Z", "N","S", "O","K", "O","M", "M","N", "Z","M", "N","O"), directed=F) 
plot(g1)
E(g1) # display edges

E(g1)$weight <- c(1,2,3,1,2,2,3)
plot(g1, edge.label = E(g1)$weight)
degree(g1)

V(g1)$shape <- "circle"
plot(g1, edge.label = E(g1)$weight, edge.label.font=c(2), edge.label.size=60, 
     edge.width = E(g1)$weight, edge.curved = c(0.5), 
     vertex.label.font=c(2), vertex.label.color="black")


# Template 2
g2 <- graph.empty(n=0,directed=F)
g2 <- g2 + vertex("A", color="blue")
g2 <- g2 + vertex("B", color="yellow")
g2 <- g2 + vertex("C", color="red")
g2 <- g2 + vertex("D", color="green")
g2 <- g2 + edge("A", "B")
g2 <- g2 + edge("B", "C")
g2 <- g2 + edge("C", "D")
g2 <- g2 + edge("B", "D")
E(g2)$weight <- c(1,2,3,4)
plot(g2, edge.label=E(g2)$weight)
