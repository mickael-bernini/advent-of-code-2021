setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('global_functions.R')

is.test <- FALSE
day <- 6

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')


lines <- readLines(data.in)
state.f <- as.integer(strsplit(x = lines, split = ',')[[1]])
init.state <- state.f

states.cnt <- rep(0, 9)
for (i in 0:8) {
  num.i <- sum(init.state + 1 == i)
  states.cnt[i + 1] <- num.i
}

list.res <- c()
for (day.i in 1:257) {
  dead.cnt <- states.cnt[1]
  states.cnt <- c(states.cnt[-1], 0)
  states.cnt[7] <- states.cnt[7] + dead.cnt
  states.cnt[9] <- states.cnt[9] + dead.cnt
  
  if (day.i %in% c(18, 19, 80, 81, 256, 257)) {
    message(day.i - 1, ': ', sum(states.cnt))
  }
  
  list.res <- c(
    list.res,
    sum(states.cnt)
  )
}

