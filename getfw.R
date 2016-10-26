###
### Read and name all food web datasets
###


folder1 <- "C:/Users/jjborrelli/OneDrive/FoodsWeb_35160027/"
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
