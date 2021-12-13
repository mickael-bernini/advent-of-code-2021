setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 12

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')
lines <- readLines(data.in)
pat <- '^([a-zA-Z]*)-([a-zA-Z]*)$'
links <- data.table (
  x = regex(x = lines, pat=pat, repl = '\\1'),
  y = regex(x = lines, pat=pat, repl = '\\2')
)

compl.path <- list()
incl.path <- list(c('start'))
while (length(incl.path)) {
  
  incl.step <- list()
  
  for (path.i in 1:length(incl.path)) {
    path <- incl.path[[path.i]]
    last.pt <- tail(path, 1)
    
    if (last.pt == 'end') {
      compl.path <- c(compl.path, list(path))
      next()
    } else {
      new.pts <- c(links[x == last.pt, y], links[y == last.pt, x])
      for (new.i in new.pts) {
        if (new.i == 'start') {
          next()
        }
        
        #  For part 1:
        # if (tolower(new.i) == new.i && (new.i %in% path)) {
        #   # cave already in the path
        #   next()
        # }
        
        # For part 2:
        if (tolower(new.i) == new.i && (new.i %in% path)) {
          small.caves <- path[tolower(path) == path]
          if (length(small.caves) != length(unique(small.caves))) {
            # There is already one cave visited twice
            next()
          }
        }
        new.path.full <- c(path, new.i)
        
        incl.step <- c(incl.step, list(new.path.full))
      }
    }
  }
  
  incl.path <- incl.step
}
message(length(compl.path))

