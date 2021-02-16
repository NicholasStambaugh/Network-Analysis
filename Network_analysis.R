#Different Networking codes compiled in one!
#Run in bunches
#sample analysis, Basic network structure.
library(igraph)
g <- graph(c(1,2,2,3,3,4,4,1),
           directed = F,
           n=7)
plot(g,
     vertex.color = "purple",
     vertex.size = 40,
     edge.color = "red"
    )
g[]

g1 <- graph(c("Rob", "Peter","Peter","Cole",
             "Cole","Nick","Nick","Rob",
             "Rob","Cole","Steve","Cole","Rob","Nick"),
            directed = T)
plot(g1,
     vertex.color = "purple",
     vertex.size = 40,
     edge.color = "red"
)

#Measures of the Network, Connection could mean
#a follow, like, email, etc.
#Use each separate
degree(g1, mode='all')#How many connections
degree(g1, mode='in')#Connections coming in
degree(g1, mode='out')#Connections going out
######

#These are used to compare networks, use each separate
diameter(g1, directed=F, weights = NA)
edge_density(g1, loops=F)#How many connections there are vs. How many are possible
ecount(g1)/(vcount(g1)*(vcount(g1)-1))#Calculation for line above
reciprocity(g1)
closeness(g1, mode='all', weights = NA)#Most likely to get a connection
betweenness(g1, directed=T, weights = NA)
edge_betweenness(g1, directed=T, weights = NA)

#Reading data files to create Network
data <- read.csv(file.choose(), header=T)
View(data)#displays network
y <-data.frame(data$first, data$second)
######

#Creating the network from the data we just imported
net <- graph.data.frame(y, directed=T)
V(net)
E(net)
V(net)$label <- V(net)$name#Assigns data to names
V(net)$degree <- degree(net)#Assigns number of connections
######

#Histogram of node degree, Number of connections displayed!
hist(V(net)$degree,
       col = "purple",
       main = 'Histogram of Node Degree',
       ylab = 'Frequency',
       xlab = 'Degree of Vertices')
######

#Full diagram of the Network
set.seed(222)
plot(net,
     vertex.color = 'green',
     vertext.size = 2,
     vertex.label.distance = 1.5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)
#######

#Showing degrees & Modifying layout, people with more connections will appear larger
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold)
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.graphopt)
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)#Best layout
#######

#Hub and authorities, incoming and outgoing information
hs <- hub_score(net)$vector
as <-authority_score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)#Most number of outgoing is largest
set.seed(123)
plot(net,
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)#Most number of incoming is largest
par(mfrow=c(1,1))
#######

#Detect densely connected users, easily identify communities!
net1 <- graph.data.frame(y, directed=F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,
     net1,
     vertex.size = 10,
     vertex.label.cex = 0.8)