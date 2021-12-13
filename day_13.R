setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 13

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')
lines <- readLines(data.in)
pat <- '^([0-9]*),([0-9]*)$'

markers <- regex(x=lines, pat = pat, value = TRUE)
markers <- data.table(
  x = as.numeric(regex(x = markers, pat=pat, repl = '\\1')) + 1,
  y = as.numeric(regex(x = markers, pat=pat, repl = '\\2')) + 1
)
markers[,index:=1:.N]

pat.2 <- '^fold along (x|y)=([0-9]*)$'
folds <- regex(x=lines, pat = pat.2, value = TRUE)
folds <- data.table(
  side = regex(x = folds, pat=pat.2, repl = '\\1'),
  val = as.numeric(regex(x = folds, pat=pat.2, repl = '\\2')) + 1
)

max.x <- max(markers[,max(x)], folds[side == 'x', 1 + 2*max(val - 1)])
max.y <- max(markers[,max(y)], folds[side == 'y', 1 + 2*max(val - 1)])
mat.elem <- matrix(0, nrow = max.x, ncol = max.y)


add_marker <- function(i, j, ...) {
  mat.elem[i, j] <<- 1
  NULL
}
markers[,add_marker(i=x, j=y), by=index]


for (fold.i in 1:folds[,.N]) {
  fold.axis <- folds[fold.i, side]
  fold.val <- folds[fold.i, val]
  
  n.r <- nrow(mat.elem)
  n.c <- ncol(mat.elem)
  # message('Step ', fold.i, ' - ', n.r, 'x', n.c)
  if (fold.axis == 'y') {
    mat.spl <- mat.elem[1:n.r, 1:(fold.val - 1)]
    mat.rev <- mat.elem[1:n.r, n.c:(fold.val + 1)]
    
    mat.new <- mat.spl + mat.rev
  } else {
    mat.spl <- mat.elem[1:(fold.val - 1), 1:n.c]
    mat.rev <- mat.elem[n.r:(fold.val + 1), 1:n.c]
    
    mat.new <- mat.spl + mat.rev
  }
  
  mat.elem <- mat.new
  if (fold.i == 1) {
    message("Solution step ", fold.i, ": ", sum(mat.elem > 0))
  }
}

mat.res <- as.data.table(t(mat.elem > 0))
fwrite(x = mat.res, file = 'res_day_13.csv')
