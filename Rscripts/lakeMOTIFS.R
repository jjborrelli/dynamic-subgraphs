library(cheddar)
library(magrittr)
library(taxize)
library(igraph)
library(NetIndices)

## FUNCTIONS

### Counting triads in a list of networks (igraph graph objects)

motif_counter <- function(graph.lists){
  require(igraph)
  
  if(!is.list(graph.lists)){
    stop("The input should be a list of graph objects")
  }
  
  triad.count <- lapply(graph.lists, triad.census)
  triad.matrix <- matrix(unlist(triad.count), nrow = length(graph.lists), ncol = 16, byrow = T)
  colnames(triad.matrix) <- c("empty", "single", "mutual", "s5", "s4", "s1", "d4",
                              "d3", "s2", "s3","d8", "d2", "d1", "d5", "d7", "d6")
  
  triad.df <- as.data.frame(triad.matrix)
  
  motif.data.frame <- data.frame(s1 = triad.df$s1, s2 = triad.df$s2, s3 = triad.df$s3, s4 = triad.df$s4, 
                                 s5 = triad.df$s5, d1 = triad.df$d1, d2 = triad.df$d2, d3 = triad.df$d3, d4 = triad.df$d4,
                                 d5 = triad.df$d5, d6 = triad.df$d6, d7 = triad.df$d7, d8 = triad.df$d8)
  
  return(motif.data.frame)
}

### Jacknife species to get list of subgraphs

listROLES <- function(x){
  lam <- list()
  for(i in 1:nrow(x)){
    lam[[i]] <- x[-i,-i]
  }
  
  qgs <- lapply(lam, graph.adjacency)
  
  m.each <- motif_counter(qgs)
  m.all <- motif_counter(list(graph.adjacency(x)))
  
  m2 <- matrix(nrow = nrow(m.each), ncol = 13)
  for(i in 1:nrow(m.each)){
    m2[i,] <- as.numeric(m.all - m.each[i,])
  }
  
  return(as.data.frame(m2))
}

## Data

data(TL84)
data(TL86)

### Make graphs 
g84 <- PredationMatrix(TL84) %>% graph.adjacency()
g86 <- PredationMatrix(TL86) %>% graph.adjacency()

## Species names 

# from union of 84 and 86 networks 
sp.names <- graph.union(g84, g86) %>% get.adjacency(sparse = F) %>% colnames

#write.csv(sp.names, file = "data/TL_species.csv")
#sp.cl <- classification(sp.names, db = "itis")
#sp.cl[is.na(sp.cl)] <- classification(names(sp.cl[is.na(sp.cl)]), db = "gbif")
#save(sp.cl, file = "data/TL_species_class.Rdata")

##
#  Link structure and dynamics
## 
spp <- read.csv("./data/TL_empirical_species.csv", row.names = 1)

TLg <- graph.union(g84, g86)
aTL <- get.adjacency(TLg, sparse = F)

TL.zoop.stat <- read.csv("./time_series_stats/ts_stats_TL_zoop_all_yr.csv", row.names = 1)
TL.phyto.stat <- read.csv("./time_series_stats/ts_stats_TL_phyto_all_yr.csv")

# common names between two datasets
inDAT <- intersect(rownames(TL.zoop.stat), rownames(aTL))
inDATphyto <- intersect(TL.phyto.stat[,1], rownames(aTL))

#trophic position and omnivory index
TL.troph <- TrophInd(aTL)[inDAT,]
TL.troph.p <- TrophInd(aTL)[inDATphyto,]

#degree
indeg <- degree(graph = TLg, mode = "in")[inDAT]
outdeg <- degree(graph = TLg, mode = "out")[inDAT]

indegP <- degree(graph = TLg, mode = "in")[inDATphyto]
outdegP <- degree(graph = TLg, mode = "out")[inDATphyto]


#centrality
## vertex betweenness
vbet <- betweenness(TLg)[inDAT]
vbetP <- betweenness(TLg)[inDATphyto]
## eigenvector centrality
evc <- evcent(graph = TLg)$vector[inDAT]
evcP <- evcent(graph = TLg)$vector[inDATphyto]
## google pagerank
pr <- page.rank(graph = TLg)$vector[inDAT]
prP <- page.rank(graph = TLg)$vector[inDATphyto]
## subgraph centrality
subcent <- subgraph.centrality(graph = TLg)[inDAT]
subcentP <- subgraph.centrality(graph = TLg)[inDATphyto]

# zooDAT is a dataframe of zooplankton data
zooDAT <- cbind(TL.zoop.stat[inDAT,], TL.troph, indeg, outdeg, vbet, evc, pr, subcent)
phytoDAT <- cbind(TL.phyto.stat[TL.phyto.stat[,1] == inDATphyto, -1], TL.troph.p, indegP, outdegP, vbetP, evcP, prP, subcentP)
rownames(phytoDAT) <- inDATphyto
colnames(phytoDAT) <- colnames(zooDAT)

TL.data <- rbind(phytoDAT, zooDAT)
TL.data <- cbind(web = factor("TuesdayLake"), TL.data)

summary(prcomp(TL.data[-10, -c(1:4)]))


## Little Rock Lake
lr <- read.csv("./data/littlerock-edges.csv")[,c(2,1)]
lr.spp <- read.csv("./data/LittleRockSpeciesList.csv")

# fix "nf" to "sp." 
lr.spp$Species[lr.spp$Species=="nf" & lr.spp$Genus!="nf"] <- "sp."

# Add a genus,species column 
lr.spp$taxa <- paste(lr.spp$Genus, lr.spp$Species, sep = " ")

lr.zoop <- read.csv("./data/LR_zoop_species.csv", row.names = 1)


lrg <- graph.edgelist(matrix(c(lr[,2], lr[,1]), ncol = 2))
alr <- get.adjacency(lrg, sparse = F)
alr.fix <- alr[-c(which(colSums(alr) == 0 & rowSums(alr) == 0)), -c(which(colSums(alr) == 0 & rowSums(alr) == 0))]

lrg <- graph.adjacency(alr.fix)

spp.names.lr <- paste(lr.spp$Genus, lr.spp$Species, sep = " ")

#trophic position and omnivory index
TL.troph.lr <- TrophInd(alr.fix)

#degree
indeg.lr <- degree(graph = lrg, mode = "in")
outdeg.lr <- degree(graph = lrg, mode = "out")

#centrality
## vertex betweenness
vbet.lr <- betweenness(lrg)
## eigenvector centrality
evc.lr <- evcent(graph = lrg)$vector
## google pagerank
pr.lr <- page.rank(graph = lrg)$vector
## subgraph centrality
subcent.lr <- subgraph.centrality(graph = lrg)

zooDAT.lr <- cbind(TL.troph.lr, indeg.lr, outdeg.lr, vbet.lr, evc.lr, pr.lr, subcent.lr)
zooDAT.lr <- cbind(spp.names.lr, zooDAT.lr)

lr.zoop.stat <- read.csv("./time_series_stats/ts_stats_LR_zoop_all_yr.csv")
LR.names <- intersect(lr.zoop.stat$new_name, zooDAT.lr$spp.names.lr)

zooDAT.LR <- zooDAT.lr[zooDAT.lr$spp.names.lr %in% LR.names,]
zooDAT.LR <- zooDAT.lr[zooDAT.lr$spp.names.lr %in% LR.names,][with(zooDAT.LR, order(spp.names.lr)),]

LR.data <- cbind(web = factor("LittleRock"), lr.zoop.stat[lr.zoop.stat$new_name %in% LR.names,2:5], zooDAT.LR[,2:9])
rownames(LR.data) <- zooDAT.LR[,1]

## Shelf

sh <- read.csv("./data/shelf-edges.csv")[,c(2,1)]
sh.names <- read.csv("./data/atlantic_shelf_taxa.csv")

sh1 <- as.matrix(data.frame(sh.names[sh[,1],], sh.names[sh[,2],]))
shg <- graph.edgelist(sh1)
shm <- get.adjacency(shg, sparse = F)

TL.troph.sh <- TrophInd(shm)

#degree
indeg.sh <- degree(graph = shg, mode = "in")
outdeg.sh <- degree(graph = shg, mode = "out")

#centrality
## vertex betweenness
vbet.sh <- betweenness(shg)
## eigenvector centrality
evc.sh <- evcent(graph = shg)$vector
## google pagerank
pr.sh <- page.rank(graph = shg)$vector
## subgraph centrality
subcent.sh <- subgraph.centrality(graph = shg)

allDAT.sh <- cbind(TL.troph.sh, indeg.sh, outdeg.sh, vbet.sh, evc.sh, pr.sh, subcent.sh)
