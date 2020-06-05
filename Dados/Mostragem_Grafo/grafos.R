


library('igraph')


#################


link <- read.csv2("viagensGrafo.csv") 
head(link)

link2 <- link[,-1]

net.pedro <- graph_from_data_frame(d=link2, directed=F) 
net.pedro

windows()
plot(net.pedro)

############################

#restingido a saida de 2 pontos: 183 e 791

link3 <- link2[link2$origem==183 | link2$origem==791,]

net.pedro2 <- graph_from_data_frame(d=link3, directed=T) 
net.pedro2

windows()
plot(net.pedro2)

############################

#restingido aos nodos da carreira 

link4 <- link[link$carreira==1,]
link4 <- link4[,-1]

net.pedro3 <- graph_from_data_frame(d=link4, directed=T) 
net.pedro3

windows()
plot(net.pedro3)


############################

#restingido aos nodos da carreira 1 e 2

link5 <- link[link$carreira==1 | link$carreira==2,]
link5 <- link5[,-1]

net.pedro4 <- graph_from_data_frame(d=link5, directed=T) 
net.pedro4

windows()
plot(net.pedro4)

