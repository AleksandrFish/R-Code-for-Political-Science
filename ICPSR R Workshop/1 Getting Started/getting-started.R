##--------------------------------------------------------------##
##                  Script for Lecture 1:                       ##
##                 Getting Started With R                       ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##


# Using the R Commander

install.packages("Rcmdr")
library(Rcmdr)

#Working Directory

getwd() #get working directory
setwd("..") # set working directory

# Basics

    # arithmetic, interacting with the interpreter

        # basic arithmetic operations

2 + 3 # addition
2 - 3 # subtraction
2*3   # multiplication
2/3   # division
2^3   # exponentiation

        # precedence of operators
        
4^2 - 3*2
1 - 6 + 4
2^-3

(4^2) - (3*2) # use parentheses to group, clarify
4 + 3^2
(4 + 3)^2

-2--3
-2 - -3 # use spaces to clarify


    # functions, arguments to functions, obtaining help and information

log(100) # natural log
log(100, base=10) # log base-10
log10(100) # equivalent
log(100, b=10)  # argument abbreviation

args(log) # arguments of the log() function

help("log")    # documentation
?log           # equivalent 
example("log") # execute examples in help page


log(100, 10)  #  specifyingarguments by position

apropos("log")
help.search("log")

RSiteSearch("loglinear", "functions")

`+`(2, 3) # even operators are functions

    # creating vectors

c(1, 2, 3, 4)  # combine

1:4     # integer-sequence operator
4:1
-1:2    # note precedence
-(1:2)
seq(1, 4)
seq(2, 8, by=2) # specify interval
seq(0, 1, by=0.1) # non-integer sequence
seq(0, 1, length=11) # specify number of elements

    # vectorized arithmetic
    
c(1, 2, 3, 4)/2
c(1, 2, 3, 4)/c(4, 3, 2, 1)
log(c(0.1, 1, 10, 100), 10)



    # creating variables (named objects) by assignment

x <- c(1, 2, 3, 4) # assignment
x # print

x = c(1, 2, 3, 4) # can use = for assignment (best avoided)
x

x/2            # equivalent to c(1, 2, 3, 4)/2
(y <- sqrt(x)) # parentheses to assign and print trick

(x <- rnorm(100))
head(x) # first few
summary(x)  # a "generic" function

    # character and logical data
    
(words <- c("To", "be", "or", "not", "to", "be"))
paste(words, collapse=" ")

(logical.values <- c(TRUE, TRUE, FALSE, TRUE))
!logical.values # negation (not operator)



    # coercion

sum(logical.values)      # number of TRUEs (coercion to numeric)
sum(!logical.values)     # number of FALSEs (TRUE-> 1, FALSE -> 0)
c("A", FALSE, 3.0)       # coerced to character
c(10, FALSE, -6.5, TRUE) # coerced to numeric

X<- as.factor(x)

    # basic indexing
    
x[12]             # 12th element
words[2]          # second element
logical.values[3] # third element
x[6:15]           # elements 6 through 15
x[c(1, 3, 5)]     # 1st, 3rd, 5th elements 

x[-(11:100)] # omit elements 11 through 100
x[1:10]      # same!

v <- 1:4
v[c(TRUE, FALSE, FALSE, TRUE)] # logical indexing

    #comparison and logical operators

1 == 2       # equal to
1 != 2       # not equal to
1 <= 2       # less than or equal to
1 < 1:3      # less than (vectorized) 
3:1 > 1:3    # greater than
3:1 >= 1:3   # greater than or equal to

TRUE & c(TRUE, FALSE)                        # logical and
c(TRUE, FALSE, FALSE) | c(TRUE, TRUE, FALSE) # logical or
TRUE && FALSE  # unvectorized and (for programming)
TRUE || FALSE  # unvectorized or

! c(T, F)   # abbreviations of TRUE and FALSE, best avoided!
T <- FALSE  # perverse! (but in most cases innocuous)
T
remove(T)
TRUE <- FALSE  # fails

(z <- x[1:10])      # first 10 elements of x
z < -0.5            # is each element less than -0.5?
z > 0.5             # is each element greater than 0.5
z < -0.5 | z > 0.5  #  < and > are of higher precedence than |
abs(z) > 0.5        # absolute value, equivalent to last expression
z[abs(z) > 0.5]     # values of z for which |z| > 0.5


    # user-defined functions

mean(x)  # of 100 random-normal numbers

sum(x)/length(x)  # equivalent


myMean <- function(x) {
    sum(x)/length(x)
}
myMean # can be printed like any object

myMean(x)
y # from sqrt(c(1, 2, 3, 4))
myMean(y)
myMean(1:100)
head(x) # global x undisturbed

mySD <- function(x) {
  sqrt(sum((x - myMean(x))^2)/(length(x) - 1))
}
mySD(1:100)
sd(1:100) # check

    # cleaning up

objects()
remove(v, x, y, z, logical.values, words)
objects()

    # using traceback()

letters
mySD(letters)

traceback()
