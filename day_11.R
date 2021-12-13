setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 11

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')
lines <- readLines(data.in)
lines <- strsplit(x = lines, split = '')
lines <- lapply(X=lines, FUN=as.numeric)

dt.l <- as.data.table(lines)
dt.m <- as.matrix(dt.l)

expl.cnt <- 0
for (i in 1:2000) {
  dt.m <- dt.m + 1
  sim.flash <- 0
  while (any(dt.m > 9)) {
    ind.light <- which(dt.m > 9, arr.ind = TRUE)
    for (light.i in 1:nrow(ind.light)) {
      expl.cnt <- expl.cnt + 1
      sim.flash <- sim.flash + 1
      
      r.i <- ind.light[light.i, 'row']
      c.i <- ind.light[light.i, 'col']
      dt.m[r.i, c.i] <- 0
      
      adj <- data.table(
        rows = c(r.i-1, r.i-1, r.i-1, r.i, r.i, r.i+1, r.i+1, r.i+1),
        cols = c(c.i-1, c.i, c.i+1, c.i-1, c.i+1, c.i-1, c.i, c.i+1)
      )
      adj <- adj[cols > 0 & cols <= 10 & rows > 0 & rows <= 10]
      
      for (adj.j in 1:adj[,.N]) {
        r.j <- adj[adj.j, rows]
        c.j <- adj[adj.j, cols]
        
        dt.m[r.j, c.j] <- ifelse(dt.m[r.j, c.j] == 0, 0, dt.m[r.j, c.j] + 1)
      }
    }
  }
  
  if (sim.flash == 100) {
    message('Part 2: ', i)
    stop("part 2 done")
  }
  
  if (i == 100) {
    message('Part 1: ', expl.cnt)
  }
}


