setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('global_functions.R')

is.test <- FALSE
data.in <- ifelse(is.test,'data/day_4_test.txt','data/day_4.txt')
num.cards <- ifelse(is.test, 3, 100)

list.cards <- list()


lines <- readLines(data.in)


list.found <- lines[1]
list.found <- as.integer(strsplit(x = list.found, split = ',')[[1]])


string_to_nums <- function (line.i, ...) {
  line.ints <- strsplit(x = line.i, split = ' ')[[1]]
  line.ints <- line.ints[line.ints != '']
  line.ints <- as.integer(line.ints)
  as.list(line.ints)
}
  
apply_found <- function(list.ex, new.val, ...) {
  fnd <- which(list.ex == new.val)
  list.ex[fnd] <- NA
  list.ex
}

line_done <- function(list.ex, ...) {
  all(is.na(list.ex))
}

get_card <- function(card.index, list.found, lines.cards, ...) {
  lines.card <- lines.cards[(1:5) + card.index*6]
  lines.nums <- lapply(X = lines.card, FUN = string_to_nums)
  lines.mat <- as.matrix(rbindlist(lines.nums))
  
  new.val <- 54
  for (new.val in list.found) {
    lines.mat[lines.mat == new.val] <- NA
    
    is.line.done <- apply(X = lines.mat, MARGIN = 1, FUN = line_done)
    if (any(is.line.done)) {
      break
    }
    is.line.done <- apply(X = lines.mat, MARGIN = 2, FUN = line_done)
    if (any(is.line.done)) {
      break
    }
  }
  
  return(list(
    val = new.val,
    index.val = which(list.found == new.val),
    rest.sum = sum(lines.mat, na.rm=TRUE)
  ))
}
lines.cards <- lines[-c(1,2)]

list.cards <- (1:num.cards) - 1
res.card <- lapply(X=list.cards, FUN = get_card, list.found=list.found, lines.cards=lines.cards)

res.tab <- rbindlist(res.card)
message(res.tab[which.min(index.val), val*rest.sum])
message(res.tab[which.max(index.val), val*rest.sum])

# 45122 too low

