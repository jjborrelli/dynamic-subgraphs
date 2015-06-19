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

zoop.stat <- read.csv("./time_series_stats/ts_stats_zoop_all_yr.csv", row.names = 1)

# common names between two datasets
inDAT <- intersect(rownames(zoop.stat), rownames(aTL))


#trophic position and omnivory index
TL.troph <- TrophInd(aTL)[inDAT,]

#degree
indeg <- degree(graph = TLg, mode = "in")[inDAT]
outdeg <- degree(graph = TLg, mode = "out")[inDAT]

#centrality
## vertex betweenness
vbet <- betweenness(TLg)[inDAT]
## eigenvector centrality
evc <- evcent(graph = TLg)$vector[inDAT]
## google pagerank
pr <- page.rank(graph = TLg)$vector[inDAT]
## subgraph centrality
subcent <- subgraph.centrality(graph = TLg)[inDAT]

# zooDAT is a dataframe of zooplankton data
zooDAT <- cbind(zoop.stat[inDAT,], TL.troph, indeg, outdeg, vbet, evc, pr, subcent)

summary(prcomp(zooDAT[-10, -c(1:4)]))



## Little Rock Lake
lr <- read.csv("./data/littlerock-edges.csv")
lr.spp <- read.csv("./data/LittleRockSpeciesList.csv")

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

