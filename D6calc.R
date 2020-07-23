#D6calc.R

makeD6data <- function(output) {
  N <- 12
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btn123456","(123456)","gray90")
  DF[2,] <- c("btn246135","(135)(246)","gray90")
  DF[3,] <- c("btn362514","(14)(25)(36)","gray90")
  DF[4,] <- c("btn264153","(153)(264)","gray90")
  DF[5,] <- c("btn165432","(165432)","gray90")
  DF[6,] <- c("btn4613","(13)(46)","gray90")
  DF[7,] <- c("btn3526","(26)(35)","gray90")
  DF[8,] <- c("btn2415","(15)(24)","gray90")
  DF[9,] <- c("btn453612","(12)(36)(45)","gray90")
  DF[10,] <- c("btn562314","(14)(23)(56)","gray90")
  DF[11,] <- c("btn342516","(16)(25)(34)","gray90")
  DF[12,] <- c("btnI","I","gray90")
  return(DF)
}

#DF <- makeD6data()

