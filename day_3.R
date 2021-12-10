setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('global_functions.R')

data.in <- fread('data/day_3.txt', colClasses='character')
data.in[,index := 1:.N]
colnames(data.in) <- 'full'
tab <- data.in[, .(
  as.integer(substr(full, 1, 1)),
  as.integer(substr(full, 2, 2)),
  as.integer(substr(full, 3, 3)),
  as.integer(substr(full, 4, 4)),
  as.integer(substr(full, 5, 5)),
  as.integer(substr(full, 6, 6)),
  as.integer(substr(full, 7, 7)),
  as.integer(substr(full, 8, 8)),
  as.integer(substr(full, 9, 9)),
  as.integer(substr(full, 10, 10)),
  as.integer(substr(full, 11, 11)),
  as.integer(substr(full, 12, 12))
), by=index]

val[ ,index:=NULL] 
val <- sapply(X = val, mean)
gam <- as.integer(val > 0.5)
eps <- as.integer(val < 0.5)

gamma <- 189 # '000010111101'
eps <- 3906
gamma*eps

# -----------------------

tab.gam <- tab
tab.gam[,index:=NULL]
res <- ''
while (tab.gam[,.N > 1]) {
  
  col <- as.list(tab.gam)[[nchar(res) + 1]]
  if (sum(col) >= sum(!col)) {
    res.bit <- 1
  } else {
    res.bit <- 0
  }
  res <- paste0(res, res.bit)
  tab.gam <- tab.gam[ col == res.bit]
}
paste(tab.gam, collapse='')

tab.gam <- tab
tab.gam[,index:=NULL]
res <- ''
while (tab.gam[,.N > 1]) {
  
  col <- as.list(tab.gam)[[nchar(res) + 1]]
  if (sum(col) >= sum(!col)) {
    res.bit <- 0
  } else {
    res.bit <- 1
  }
  res <- paste0(res, res.bit)
  tab.gam <- tab.gam[ col == res.bit]
}
paste(tab.gam, collapse='')



1071*3706

010000101100
