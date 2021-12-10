setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('global_functions.R')

is.test <- FALSE
day <- 8

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')


lines <- readLines(data.in)
list.elems <- strsplit(x = lines, split = ' ')

get_per_elem <- function(row.data, ...) {
  inds <- row.data[1:10]
  sols <- row.data[12:15]
  inds.let <- strsplit(x = inds, split = '')
  inds.sol <- strsplit(x = sols, split = '')
  inds.lets <- lapply(X=inds.let, FUN = sort)
  inds.letv <- sapply(inds.lets, paste0, collapse='')
  
  inds.sols <- lapply(X=inds.sol, FUN = sort)
  inds.letl <- sapply(X=inds.let, FUN = length)
  
  all.let <- data.table(let=unlist(inds.lets))
  let.cnt <- all.let[,.N, by=let]
  
  a <- setdiff(inds.let[inds.letl == 3][[1]], inds.let[inds.letl == 2][[1]])
  b <- let.cnt[N == 6, let]
  e <- let.cnt[N == 4, let]
  f <- let.cnt[N == 9, let]
  
  c <- setdiff(inds.let[inds.letl == 2][[1]], f)
  d <- setdiff(inds.let[inds.letl == 4][[1]], c(b,c,f))
  g <- setdiff(inds.let[inds.letl == 7][[1]], c(a,b,c,d,e,f))
  
  v0 <- which(inds.letv == paste(sort(c(a, b, c, e, f, g)), collapse=''))
  v1 <- which(inds.letl == 2)
  v2 <- which(inds.letv == paste(sort(c(a,c,d,e,g)), collapse=''))
  v3 <- which(inds.letv == paste(sort(c(a,c,d,f,g)), collapse=''))
  v4 <- which(inds.letl == 4)
  v5 <- which(inds.letv == paste(sort(c(a,b,d,f,g)), collapse=''))
  v6 <- which(inds.letv == paste(sort(c(a,b,d,f,e,g)), collapse=''))
  v7 <- which(inds.letl == 3)
  v8 <- which(inds.letl == 7)
  v9 <- which(inds.letv == paste(sort(c(a,b,c,d,f,g)), collapse=''))
  
  idd <- data.table(
    new = 1:10,
    old = c(v0,v1,v2,v3,v4,v5,v6,v7,v8,v9)
  )
  
  fnd <- ''
  for (j in 1:length(inds.sol)) {
    v.j <- inds.sol[j][[1]]
    v.j <- paste0(sort(v.j), collapse = '')
    for (i in 1:length(inds.let)) {
      v.i <- inds.letv[i]
      if (v.i == v.j) {
        new.v <- idd[old == i, new - 1]
        fnd <- paste0(fnd, new.v)
      }
    }
  }
  
  list(
    fnd
  )
}
res.fnd <- lapply(X=list.elems, FUN=get_per_elem)
res.tab <- rbindlist(res.fnd)
res.tab[,intv := as.integer(V1)]
res.tab[,message(sum(intv))]
print(res.tab)
