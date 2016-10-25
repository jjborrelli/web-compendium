###
### Read and name all food web datasets
###


folder1 <- "C:/Users/jjborrelli/OneDrive/FoodsWeb_35160027/"
allfi <- list.files(folder1)
foi <- grep(pattern = ".csv", x = allfi)

web <- list()
for(i in 1:length(foi)){
  web[[i]] <- read.csv(paste0(folder1, allfi[foi[i]]), header = T, skip = 1)
  print(i)
}

names(web) <- sapply(sapply(allfi[foi], strsplit, split = ".", fixed = T), "[[", 1)

lapply(web, dim)


###
### fix rows and columns to make square matrices
### 

for(i in 1:length(web)){
  newweb <- as.matrix(web[[i]][,-1])
  rownames(newweb) <- web[[i]][,1]
  
  colnames(newweb) <- lapply(sapply(colnames(newweb), strsplit, split = ".", fixed = T),
                             paste, collapse = " ")
  addcols <- rownames(newweb)[!rownames(newweb) %in% colnames(newweb)]
  newcols <- matrix(0, nrow = nrow(newweb), ncol = length(addcols))
  colnames(newcols) <- addcols
}