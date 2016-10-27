library(igraph)
library(NetIndices)
library(reshape2)


get_fw_indices <- function(adj.list, graphs, web){
  require(igraph)
  require(NetIndices)
  require(reshape2)
  
  if(!is.list(adj.list)){
    stop("Input 'adj.list' must be a list of adjacency matrices")
  }
  if(!is.list(graphs)){
    stop("Input 'graphs' should be a list of graph objects")
  }
  if(length(adj.list) != length(graphs)){
    warning("Length of adjacency list and graph list differ")
  }
  # For all food webs calculate general indices
  g.ind <- lapply(adj.list, GenInd)
  # NOTE: Connectance is Links / (N * (N - 1))
  
  # Create a data frame of general indices for all webs
  data <- lapply(g.ind, unlist)
  indices <- data.frame(data[[1]])
  for (i in 2:length(g.ind)){
    indices <- cbind(indices, data[[i]])
  }
  
  # Make indices the columns, webs the rows
  colnames(indices) <- web
  indices <- as.data.frame(t(indices))
  
  
  # Subset the data frame into the desired indices of N, num Links, link density, connectance
  indices <- as.data.frame(indices[,c(1,5,6,7)])
  # Create new column in data frame for web diameter 
  indices <- cbind(indices, D = melt(lapply(graphs,diameter))$value)
  # Add column for average path length
  indices <- cbind(indices, AvPath = melt(lapply(graphs, average.path.length))$value)
  # Add column for "transitivity" or clustering coefficient
  indices <- cbind(indices, Clust = melt(lapply(graphs, transitivity))$value)
  # Loops 
  diagonals <- lapply(adj.list, diag)
  looper2 <- lapply(diagonals, sum)
  # Add column for number of loops in web
  indices$Loop <- unlist(looper2)
  # Calculate proportion Top, Intermediate, Basal
  degrees <- lapply(graphs, degree, mode = "all")
  indegrees <- lapply(graphs, degree, mode = "in")
  outdegrees <- lapply(graphs, degree, mode = "out")
  
  top <- list()
  basal <- list()
  int <- list()
  
  for (i in 1:length(graphs)){
    numBas <- length(indegrees[[i]][which(indegrees[[i]] == 0)])
    numTop <- length(outdegrees[[i]][which(outdegrees[[i]] == 0)])
    basal[[i]] <- (numBas/indices$N[i]) * 100
    top[[i]] <- (numTop/indices$N[i]) * 100
    int[[i]] <- ((indices$N[i] - (numBas + numTop))/indices$N[i]) * 100
  }
  
  indices <- cbind(indices, Top = unlist(top), Int = unlist(int), Bas = unlist(basal))
  
  return(indices)
}


get_node_properties <- function(adj.list, web){
  require(igraph)
  require(NetIndices)
  require(reshape2)
  
  if(!is.list(adj.list)){
    stop("Input 'adj.list' must be a list of adjacency matrices")
  }
  
  # For all food webs calculate trophic position and omnivory index
  t.ind <- lapply(adj.list, TrophInd)
  # Convert list of trophic levels to single dataframe
  trophics <- melt(t.ind, id.vars = c("TL", "OI"))
  
  return(trophics)
}

###
### Read and name all food web datasets
###
library(reshape2)

folder1 <- "C:/Users/jjborrelli/OneDrive/FoodWebs/"
folder1 <- "C:/Users/borre_000/OneDrive/FoodWebs/"
allfi <- list.files(folder1)
foi <- grep(pattern = ".csv", x = allfi)

web <- list()
for(i in 1:length(foi)){
  web[[i]] <- read.csv(paste0(folder1, allfi[foi[i]]), header = F, skip = 1, na.strings = c("", "NA"))
  print(i)
}

names(web) <- sapply(sapply(allfi[foi], strsplit, split = ".", fixed = T), "[[", 1)

#lapply(web, dim)


###
### fix rows and columns to make square matrices
### 

alledges <- list()
for(i in 1:length(web)){
  newweb <- as.matrix(web[[i]][-1,-1])
  if(sum(is.na(newweb))>0){newweb <- newweb[,!is.na(newweb[1,])];newweb <- newweb[!is.na(newweb[,1]),]}
  rownames(newweb) <- 1:nrow(newweb)
  colnames(newweb) <- 1:ncol(newweb)
  #newweb <- apply(newweb, c(1,2), as.numeric)
  
  
  el1 <- melt(newweb)
  el1[,1] <- web[[i]][-1,1][el1[,1]]
  #el1[,2] <- as.character(unlist(as.matrix(web[[i]])[1,-1]))[!is.na(as.character(unlist(web[[i]][1,-1])))][el1[,2]]
  el1[,2] <- as.matrix(web[[i]])[1,-1][!is.na(as.matrix(web[[i]])[1,-1])][el1[,2]]
  
  alledges[[i]] <- as.matrix(el1[el1[,3] != "0",1:2])
  
  print(i)
}

allg <- lapply(alledges, graph.edgelist)
allmat <- lapply(allg, get.adjacency, sparse = F)
sort(table(tolower(unlist(lapply(allmat, colnames)))), decreasing= T)[1:5]


conn <- sapply(allmat, function(x) sum(x)/(nrow(x)*(nrow(x) -1)))
S <- sapply(allmat, nrow)

plot((conn)~(S))

library(rnetcarto)
mod1 <- lapply(allmat, netcarto)
plot(conn, sapply(mod1, "[[", 2))
plot(log(S), sapply(mod1, "[[", 2))



fw.ind <- get_fw_indices(allmat, allg, names(web))
fw.ind$mod <- sapply(mod1, "[[", 2)

node.p <- get_node_properties(allmat, allg)
node.mod <- do.call(rbind, lapply(mod1, "[[", 1))

