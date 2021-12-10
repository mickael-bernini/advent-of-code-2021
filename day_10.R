setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('global_functions.R')

is.test <- FALSE
day <- 10

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')


lines <- readLines(data.in)


pat.corr <- '(\\(\\))|(<>)|(\\{\\})|(\\[\\])'
while (any(regex(x = lines, pat = pat.corr))) {
  lines <- regex(x = lines, pat = pat.corr, repl='')
}

lines.splt <- strsplit(x = lines, split = '')
line.i <- lines.splt[[1]]
get_first_wrong <- function(line.i) {
  pat.ic <- c(')', '>', '}', ']')
  is.wrong <- line.i %in% pat.ic
  if (any(is.wrong)) {
    res <- line.i[which.max(is.wrong)]
    switch(res, ')'=3, '>'=25137, '}'=1197, ']'=57)
  } else {
    return(0)
  }
}

wrong.score <- sapply(X = lines.splt, FUN = get_first_wrong)
message(sum(wrong.score))

corr.lines <- lines[wrong.score == 0]


fix_one_line <- function(line.i) {
  pat.corr <- '(\\(\\))|(<>)|(\\{\\})|(\\[\\])'
  pat.ic <- c(')', '>', '}', ']')
  
  score <- 0
  compl.str <- ''
  
  line.splt <- strsplit(x = line.i, split = '')[[1]]
  while (length(line.splt)) {
    lst <- line.splt[length(line.splt)]
    
    res.score <- switch(lst, '('=1, '<'=4, '{'=3, '['=2)
      
    compl.str <- paste0(compl.str, lst)
    score <- score * 5 + res.score
      
    line.splt <- line.splt[-length(line.splt)]
    
  }
  
  data.table(
    str = compl.str,
    score = score
  )
}
res.all <- lapply(X = corr.lines, FUN = fix_one_line)
res.all <- rbindlist(res.all)
setorder(res.all, score)
message(res.all[(.N+1)/2, score])
