# last modified 29 March 2010 by J. Fox

# the following function was posted to r-help list by Don MacQueen

showColours <- function (indx = 0:6) 
{
     for (ii in unique(indx)) {
         is <- 100 * ii + 1:100
         if (min(is) > length(colors())) {
             cat("Maximum value of arg is", floor(length(colors())/100),
                 "\n")
             return(NULL)
         }
         foo <- matrix(colors()[is], nrow = 10)
         par(mar = c(3, 3, 0.25, 0.25))
         plot(1:10, 1:10, type = "n", yaxt = "n", xlab = "", ylab = "")
         axis(2, at = 1:10, lab = 10:1)
         for (j in 1:10) {
             for (i in 1:10) {
                 points(j, 11 - i, col = foo[i, j], pch = 16,
                   cex = 4)
                 text(j, 11 - i - 0.3, foo[i, j], cex = 0.8)
             }
         }
         if (length(indx) > 1 & ii < max(indx))
             readline(paste("Currently showing group", ii, "  CR to continue "))
     }
     invisible(foo)
}

# the following function was posted to r-help list by Henrik Bengtsson, slightly modified by J. Fox

plotSymbols <- function(interactive=FALSE) {  
  ASCII <- c("", sapply(1:255, function(i) parse(text=paste("\"\\",
                    structure(i,class="octmode"), "\"", sep=""))[[1]]))
  intToChar <- function(i) {
    ASCII[i %% 256 + 1]
  }

  as.character.hexmode <- function(x) {
    hexDigit <- c(0:9, "A", "B", "C", "D", "E", "F")
    isna <- is.na(x)
    y <- x[!isna]
    ans0 <- character(length(y))
    z <- NULL
    while (any(y > 0) | is.null(z)) {
      z <- y%%16
      y <- floor(y/16)
      ans0 <- paste(hexDigit[z + 1], ans0, sep = "")
    }
    ans <- rep(NA, length(x))
    ans[!isna] <- ans0
    ans
  }
  
  intToHex <- function(x) {
    y <- as.integer(x);
    class(y) <- "hexmode";
    y <- as.character(y);
    dim(y) <- dim(x);
    y;
  }

  as.character.octmode <- function(x, ...) {
    isna <- is.na(x)
    y <- x[!isna]
    ans0 <- character(length(y))
    z <- NULL
    while (any(y > 0) | is.null(z)) {
      z <- y%%8
      y <- floor(y/8)
      ans0 <- paste(z, ans0, sep="")
    }
    ans <- rep(as.character(NA), length(x))
    ans[!isna] <- ans0
    ans
  }
  
  intToOct <- function(x) {
    y <- as.integer(x);
    class(y) <- "octmode";
    y <- as.character(y);
    dim(y) <- dim(x);
    y;
  } 

  interactive <- interactive && interactive()

  i <- 0:255
  i[27:32] <- 0
  ncol <-16
  
  top <- 3 + 2*interactive
  opar <- par(cex.axis=0.7, mar=c(3,3,top,3)+0.1)
  on.exit(par(opar))

  plot(i%%ncol,1+i%/%ncol, pch=i, xlim=c(0,ncol-1), xlab="", ylab="", 
 
axes=FALSE)
  axis(1, at=0:15)
  axis(2, at=1:16, labels=0:15*16, las=2)
  axis(3, at=0:15)
  axis(4, at=1:16, labels=0:15*16+15, las=2)
  if (interactive) {
    title(main="Click on a symbol to add it to the data frame. Click in margin to quit!", cex.main=0.8, line=3.5)
  }

  if (interactive) {
    df <- list()
    usr <- par("usr")
    ready <- FALSE
    while (!ready) {
      click <- locator(n=1)
      x <- click$x
      y <- click$y - 1
      ready <- !(x > 0.5 && x < 15.5 && y > 0.5 && y < 15.5)
      if (!ready) {
        x <- round(x)
        y <- round(y)
        z <- 16*y + x
        ch  <- intToChar(z)
        dec <- as.character(z) 
        hex <- intToHex(z)
        oct <- intToOct(z)
        spc <- paste(rep("0", 2-nchar(hex)), collapse="")
        hex <- paste(spc, hex, sep="")
        spc <- paste(rep("0", 3-nchar(oct)), collapse="")
        oct <- paste(spc, oct, sep="")
        df$ch  <- c(df$ch , ch )
        df$dec <- c(df$dec, dec)
        df$hex <- c(df$hex, hex)
        df$oct <- c(df$oct, oct)

        if (nchar(ch) == 0) ch <- " "
        spc <- paste(rep(" ", 3-nchar(dec)), collapse="")
        dec <- paste(spc, dec, sep="")
        cat("Selected ASCII character '", ch, "' ", dec, " 0x", hex, 
                                                 " \\", oct, "\n", sep="")
      }
    }
    return(df)
  }

  invisible()
} 
