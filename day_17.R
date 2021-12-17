setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 17

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')
lines <- readLines(data.in)
pat <- '^target area: x=([0-9-]*)\\.\\.([0-9-]*), y=([0-9-]*)\\.\\.([0-9-]*)$'

lims <- list(
  x0 = as.numeric(regex(x=lines, pat=pat, repl='\\1')),
  x1 = as.numeric(regex(x=lines, pat=pat, repl='\\2')),
  y0 = as.numeric(regex(x=lines, pat=pat, repl='\\3')),
  y1 = as.numeric(regex(x=lines, pat=pat, repl='\\4'))
)


solv_try <- function(init.spd) {
  curr.pt <- c(0, 0)
  curr.spd <- init.spd
  
  
  highest.y <- 0
  while (TRUE) {
    new.pt <- curr.pt + curr.spd
    highest.y <- max(highest.y, new.pt[2])
    
    curr.spd[1] <- curr.spd[1] - sign(curr.spd[1])
    curr.spd[2] <- curr.spd[2] - 1
    
    if (new.pt[2] < lims[['y0']]) {
      return(NA_integer_)
    }
    
    if (
      (new.pt[1] >= lims[['x0']])
      && (new.pt[1] <= lims[['x1']])
      && (new.pt[2] >= lims[['y0']])
      && (new.pt[2] <= lims[['y1']])
    ) {
      return(as.integer(highest.y))
    }
    
    curr.pt <- new.pt
  }
}


grid.dt <- expand.grid(x = 0:lims[['x1']], y = lims[['y0']]:(-lims[['y0']]))
grid.dt <- as.data.table(grid.dt)
grid.dt[,highest := solv_try(init.spd = c(x, y)), by = c('x', 'y')]
  
best.jump <- grid.dt[,which.max(highest)]
grid.dt[best.jump,message('First part: ', highest, ' on (', x, ',', y,')')]
grid.dt[!is.na(highest), message('Second part: ', .N)]

