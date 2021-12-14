setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 14

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')
lines <- readLines(data.in)
pat <- '^([A-Z])([A-Z]) -> ([A-Z])$'

init.pat <- lines[[1]]

transf <- lines[-(1:2)]
transf <- data.table(
  bef = regex(x=transf, pat= pat, repl='\\1'),
  aft = regex(x=transf, pat= pat, repl='\\2'),
  new = regex(x=transf, pat= pat, repl='\\3')
)

new.string <- init.pat
init.let <- strsplit(x = new.string, split = '')[[1]]
let.tab <- data.table(
  bef = head(init.let, -1),
  aft = tail(init.let, -1)
)
let.tab <- let.tab[,.(cnt=.N), by=c('bef', 'aft')]

for (i in 1:40) {
  
  let.new <- merge(x= let.tab, y = transf, all.x=TRUE, all.y=FALSE)
  let.new[is.na(new), new := '']
  
  
  new.tab <- rbind(
    let.new[,.(bef, aft = new, cnt)],
    let.new[,.(bef = new, aft, cnt)]
  )
  new.tab <- new.tab[,.(
    cnt = sum(cnt)
  ), by=c('bef', 'aft')]
  
  let.tab <- new.tab
}



tab.tot <- let.tab[,.(cnt = sum(cnt)), by=bef]
last.let <- tail(init.let, 1)
tab.tot[bef == last.let, cnt := cnt + 1]
setorder(tab.tot, cnt)

message(tab.tot[.N, cnt] - tab.tot[1, cnt])
