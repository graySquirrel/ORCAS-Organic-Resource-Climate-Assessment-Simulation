# sankey diagram sample

library(riverplot)

# nodes <- c( LETTERS[1:3] )
# edges <- list( A= list( C= 10 ), B= list( C= 10 ) )
# r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2 ),
#                 node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
#                 node_styles= list( A= list( col= "yellow" )) )
# plot( r )
# # equivalent form:
# nodes <- data.frame( ID= LETTERS[1:3],
#                      x= c( 1, 1, 2 ),
#                      col= c( "yellow", NA, NA ),
#                      labels= c( "Node A", "Node B", "Node C" ),
#                      stringsAsFactors= FALSE )
# r <- makeRiver( nodes, edges )
# plot( r )
# # all nodes but "A" will be red:
# r <- makeRiver( nodes, edges, default_style= list( col="red" ) )
# plot( r )
# # overwrite the node information from "nodes":
# r <- makeRiver( nodes, edges, node_styles= list( A=list( col="red" ) ) )
# plot( r )

# x <- riverplot.example()
# plot(x)
nodes <- data.frame(ID=LETTERS[1:6],
                    x=c(1,1,1,2,2,2),
                    col=c("green","yellow","red","green","yellow","red"),
                    labels= c("low","med","high","low","med","high"),
                    stringsAsFactors = FALSE)

edges <- data.frame(N1=c("A","B","C","A","B","C","A","B","C"),
                    N2=c("D","E","E","E","D","D","F","F","F"),
                    Value=c(228,198,203,7,35,2,4,6,34))

riv <- makeRiver(nodes,edges)

plot(riv)