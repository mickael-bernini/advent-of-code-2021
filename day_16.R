setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 16

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')
lines <- readLines(data.in)

varchar_to_bits <- function(lines) {
  lines.elm <- strsplit(lines, '')[[1]]
  elem.bits <- lines.elm
  
  elem.bits[elem.bits == '0'] <- '0000'
  elem.bits[elem.bits == '1'] <- '0001'
  elem.bits[elem.bits == '2'] <- '0010'
  elem.bits[elem.bits == '3'] <- '0011'
  elem.bits[elem.bits == '4'] <- '0100'
  elem.bits[elem.bits == '5'] <- '0101'
  elem.bits[elem.bits == '6'] <- '0110'
  elem.bits[elem.bits == '7'] <- '0111'
  elem.bits[elem.bits == '8'] <- '1000'
  elem.bits[elem.bits == '9'] <- '1001'
  elem.bits[elem.bits == 'A'] <- '1010'
  elem.bits[elem.bits == 'B'] <- '1011'
  elem.bits[elem.bits == 'C'] <- '1100'
  elem.bits[elem.bits == 'D'] <- '1101'
  elem.bits[elem.bits == 'E'] <- '1110'
  elem.bits[elem.bits == 'F'] <- '1111'
  
  
  bits.all <- paste0(elem.bits, collapse='')
  bits.all <- strsplit(bits.all, '')[[1]]
  bits.all <- as.integer(bits.all)
  
  bits.all
}

bin_to_int <- function(vec) {
  n <- length(vec)
  powers.of.two <- 2^((n - 1):0)
  vec %*% powers.of.two
}



process_one_packet <- function(bits.all) {
  if (length(bits.all) < 6) {
    return(NULL)
  }
  bit.i <- 1
  vers <- bin_to_int(bits.all[1:3])
  tot.version <<- tot.version + vers
  
  p.type <- bin_to_int(bits.all[4:6])
  
  if (p.type == 4) {
    bit.i <- 7
    bin.num <- c()
    while (bits.all[bit.i]) {
      bin.num <- c(bin.num, bits.all[bit.i + (1:4)])
      bit.i <- bit.i + 5
    }
    bin.num <- c(bin.num, bits.all[bit.i + (1:4)])
    bit.i <- bit.i + 5
    
    final.bit <- bit.i - ((bit.i - 1) %% 4) + 3
    
    if (is.na(bin_to_int(bin.num))) {
      message('oups')
    }
    list.new <- list(
      version = vers,
      type = p.type,
      number = bin_to_int(bin.num),
      len_packet = bit.i - 1
    )
    
    return(list.new)
    
  } else {
    list.pack <- list()
    tot.pack.len <- 0
    
    bits.all[7]
    if (bits.all[7]) {
      pack.cnt <- bin_to_int(bits.all[7 + (1:11)])
      len.op <- 7 + 11
      subpack <- bits.all[-(1:len.op)]
      
      for (i in 1:pack.cnt) {
        rm.bits <- len.op + tot.pack.len
        new.packet <- process_one_packet(bits.all[-(1:rm.bits)])
        tot.pack.len <- tot.pack.len + new.packet[['len_packet']]
        
        list.pack <- c(
          list.pack,
          list(new.packet)
        )
      }
    } else {
      tot.len <- bin_to_int(bits.all[7 + (1:15)])
      len.op <- 7 + 15
      subpack <- bits.all[-(1:len.op)]
      
      while (TRUE) {
        rm.bits <- len.op + tot.pack.len
        new.packet <- process_one_packet(bits.all[-(1:rm.bits)])
        tot.pack.len <- tot.pack.len + new.packet[['len_packet']]
        
        list.pack <- c(
          list.pack,
          list(new.packet)
        )
        if (tot.pack.len >= tot.len) {
          break
        }
      }
    }
    
    
    sub.dt <- rbindlist(list.pack)
    sub.nums <- sub.dt[,number] 
    new_num <- switch(
      as.character(p.type),
      "0" = sum(sub.nums),
      "1" = prod(sub.nums),
      "2" = min(sub.nums),
      "3" = max(sub.nums),
      "5" = as.integer(sub.nums[1] > sub.nums[2]),
      "6" = as.integer(sub.nums[1] < sub.nums[2]),
      "7" = as.integer(sub.nums[1] == sub.nums[2]),
      0
    )
    
    list.new <- list(
      version = vers,
      type = p.type,
      number = new_num,
      len_packet = len.op + tot.pack.len
    )
    return(list.new)
  }
}

tot.version <- 0
if (is.test) {
  str.test <- '9C0141080250320F1802104A08'
  
  bits.all <- varchar_to_bits(str.test)
  res <- process_one_packet(bits.all)
} else {
  bits.all <- varchar_to_bits(lines)
  res <- process_one_packet(bits.all)
}
message('Part 1:', tot.version)
message('Part 2:', res['number'])
