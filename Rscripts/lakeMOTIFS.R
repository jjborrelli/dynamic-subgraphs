library(cheddar)
library(magrittr)
library(taxize)
library(igraph)

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

### Properties of the dataset 

NPS(TL84)
NPS(TL86)
PlotWebByLevel(TL84)
pmTL84 <- PredationMatrix(TL84)
pmTL86 <- PredationMatrix(TL86)

### Count subgraphs 
g84 <- PredationMatrix(TL84) %>% graph.adjacency()
g86 <- PredationMatrix(TL86) %>% graph.adjacency()


mots84 <- PredationMatrix(TL84) %>% graph.adjacency() %>% list() %>% motif_counter()
mots86 <- PredationMatrix(TL86) %>% graph.adjacency() %>% list() %>% motif_counter()

### list subgraph participation  
s.part84 <- listROLES(pmTL84)
rownames(s.part84) <- rownames(NPS(TL84))
colnames(s.part84) <- colnames(mots84)

s.part86 <- listROLES(pmTL86)
rownames(s.part86) <- rownames(NPS(TL86))
colnames(s.part86) <- colnames(mots86)


degs84 <- data.frame(inD = InDegree(TL84), outD = OutDegree(TL84))
degs86 <- data.frame(inD = InDegree(TL86), outD = OutDegree(TL86))

same <- rownames(s.part84) %in% rownames(s.part86)
rownames(s.part86) == rownames(s.part84)[same]

s.part86[rownames(s.part84[same, ]), ] - s.part84[same, ] 

same2<- intersect(rownames(s.part86), rownames(s.part84))


## Species names 

# from union of 84 and 86 networks 
sp.names <- graph.union(g84, g86) %>% get.adjacency(sparse = F) %>% colnames
write.csv(sp.names, file = "data/TL_species.csv")

sp.names[1:3]
sp.cl <- classification(sp.names, db = "itis")

sp.cl[is.na(sp.cl)] <- classification(names(sp.cl[is.na(sp.cl)]), db = "gbif")

save(sp.cl, file = "data/TL_species_class.Rdata")
