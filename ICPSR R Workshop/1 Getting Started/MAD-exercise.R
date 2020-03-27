# median absolute deviation from the median

MAD <- function(x) median(abs(x - median(x)))

MAD(1:100)
mad(1:100, constant=1)

?mad
mad
