setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('global_functions.R')

is.test <- FALSE
day <- 9

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')


lines <- readLines(data.in)
list.elems <- strsplit(x = lines, split = '')

list.elems <- lapply(list.elems, as.integer)
mat.elem <- as.matrix(as.data.table(list.elems))
mat.elem[1,5]

low.found <- list()
for (i in 1:nrow(mat.elem)) {
  for (j in 1:ncol(mat.elem)) {
    v1 <- (i == 1) || mat.elem[i-1, j] > mat.elem[i, j]
    v2 <- (i == nrow(mat.elem)) || mat.elem[i+1, j] > mat.elem[i, j]
    v3 <- (j == 1) || mat.elem[i, j-1] > mat.elem[i, j]
    v4 <- (j == ncol(mat.elem)) || mat.elem[i, j+1] > mat.elem[i, j]
    
    low <- v1 && v2 && v3 && v4
    if (low) {
      res <- list(data.table(
        val=mat.elem[i, j],
        p.i = i,
        p.j = j
      ))
      low.found <- c(
        low.found, 
        res
      )
    }
  }
}

message(length(low.found))
low.fnd <- rbindlist(low.found)
message(low.fnd[,sum(val + 1)])


# Part 2

get.bath <- function(i, j, mat.elem) {
  inbath <- data.table(i=i, j=j)
  todo <- data.table(i=i, j=j, done=FALSE, in_bath =TRUE)
  
  nr <- nrow(mat.elem)
  nc <- ncol(mat.elem)
  while (todo[done == FALSE, .N]) {
    current.ind <- todo[,which(done == FALSE)][1]
    
    is.bath <- mat.elem[todo[current.ind, i], todo[current.ind, j]] < 9
    todo[current.ind, in_bath := is.bath]
    
    todo[current.ind, done := TRUE]
    # message(is.bath)
    if (is.bath) {
      nxt <- todo[current.ind,.(
        i = c(i-1, i+1, i, i),
        j = c(j, j, j+1, j-1),
        done = FALSE,
        in_bath = NA
      )]
      nxt <- nxt[i > 0 & j > 0 & i <= nr & j <= nc]
      
      nxt.2 <- merge(nxt, todo[,.(i,j,exist=TRUE)], all.x=TRUE, all.y=FALSE)
      nxt.2 <- nxt.2[is.na(exist), .(i,j,done,in_bath)]
      
      todo <- rbind(
        todo,
        nxt.2
      )
    }
    # message(todo[done == FALSE, .N])
  }
  
  todo[,sum(in_bath)]
}

low.fnd[,index:=1:.N]
bt.s <- low.fnd[,get.bath(i=p.i, j=p.j, mat.elem=mat.elem), by=index]
setorder(bt.s, -V1)
top.3 <- bt.s[1:3, V1]
message(top.3[1]*top.3[2]*top.3[3])
