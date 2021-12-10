setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('D:/Relive drive/Relive/3. Analytics/7. Analysis/SQL queries/global_functions.R')

is.test <- FALSE
data.in <- ifelse(is.test,'day_5_test.txt','day_5.txt')
max.s <- ifelse(is.test,10,1000)


lines <- readLines(data.in)

lines_to_dt <- function(line, ...) {
  pat <- '^([0-9]*),([0-9]*) *-> *([0-9]*),([0-9]*)$'
  regex(x = line, pat=pat, repl='\\1')
  list(
    from.x = 1 + as.integer(regex(x = line, pat=pat, repl='\\1')),
    from.y = 1 + as.integer(regex(x = line, pat=pat, repl='\\2')),
    to.x = 1 + as.integer(regex(x = line, pat=pat, repl='\\3')),
    to.y = 1 + as.integer(regex(x = line, pat=pat, repl='\\4'))
  )
}
lines.dt <- lapply(X=lines, FUN=lines_to_dt)
lines.dt <- rbindlist(lines.dt)
summary(lines.dt)

mat.found <- matrix(0, max.s, max.s)
for (line.i in 1:lines.dt[,.N]) {
  line.v <- c(as.list(lines.dt[line.i]))
  if (line.v[[1]] == line.v[[3]]) {
    mat.found[line.v[[1]], c(line.v[[2]]:line.v[[4]])] <-
      mat.found[line.v[[1]], c(line.v[[2]]:line.v[[4]])] + 1
  } else if (line.v[[2]] == line.v[[4]]) {
    mat.found[c(line.v[[1]]:line.v[[3]]), line.v[[2]]] <- 
      mat.found[c(line.v[[1]]:line.v[[3]]), line.v[[2]]] + 1
  } else if (
    abs(line.v[[1]] - line.v[[3]])
    == abs(line.v[[2]] - line.v[[4]])
  ) {
    ev.x <- c(line.v[[1]]:line.v[[3]])
    ev.y <- c(line.v[[2]]:line.v[[4]])
    for (i in 1:length(ev.x)) {
      mat.found[ev.x[i], ev.y[i]] <-
        mat.found[ev.x[i], ev.y[i]] + 1
    }
    
  }
}
message(sum(mat.found >= 2))

# Too high 7531
