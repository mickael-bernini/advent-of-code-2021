setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('D:/Relive drive/Relive/3. Analytics/7. Analysis/SQL queries/global_functions.R')

is.test <- FALSE
day <- 7

data.in <- paste0('day_', day, ifelse(is.test,'_test',''), '.txt')


lines <- readLines(data.in)
state.f <- as.integer(strsplit(x = lines, split = ',')[[1]])
init.state <- state.f

mean(init.state)
pos.min <- min(init.state)
pos.max <- max(init.state)


states.cnt <- rep(0, pos.max + 1)
for (i in 0:pos.max) {
  num.i <- sum(init.state == i)
  states.cnt[i + 1] <- num.i
}

calc_one <- function(i, ...) {
  diff.p <- abs(0:pos.max - i)
  # sum(states.cnt * diff.p)
  val <- diff.p*(diff.p + 1)/2
  sum(states.cnt * val)
}
dt.all <- data.table(index = 0:pos.max, cnt = states.cnt)
dt.all[, diff := calc_one(i = index), by=index]

print(dt.all[which.min(diff)])


# 343: too low