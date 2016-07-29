# sankey diagram sample

library(riverplot)
library(tidyr)
library(dplyr)
f <- read.csv("sankeyinput6.csv",stringsAsFactors = FALSE)
# convert f data frame 2d matrix to key value pair plus value
sin <- gather(f,To,Tons,-From)
sin$To <- as.character(sin$To)
sin <- sin[sin$Tons > 0,]
sin <- sin[order(sin[3],decreasing=TRUE),]
a <- data.frame(unique(sin[[1]]),1)
colnames(a) <- c("name","val")
b <- data.frame(unique(sin[[2]]),2)
colnames(b) <- c("name","val")
c <- rbind(a,b)
d <- unlist(unique(c[1]))
nodeNameVal <- aggregate(c[2], c[1], mean)
nodeIDs <- d
x <- (nodeNameVal[d,])$val
cols <- rainbow(length(x))
# col=c("green","yellow","palevioletred","lightblue",
#       "green","yellow","palevioletred","gray","lightblue"),
nodes <- data.frame(ID=nodeIDs,
                    x=x, col=cols,
                    labels= nodeIDs,
                    stringsAsFactors = FALSE)
nodes[nodes$ID == "Donation","x"] <- 1.25
edges <- data.frame(N1=sin$From,
                    N2=sin$To,
                    Value=sin$Tons)
ds <- default.style()
ds[["srt"]]<- "0"
riv <- makeRiver(nodes,edges)
#ds[["srt"]] <- 45
#ds[["edgestyle"]] <- "straight"
#ds[["nodestyle"]] <- "invisible"
#par(mar=c(10,10,10,10))
plot(riv,default_style = ds)