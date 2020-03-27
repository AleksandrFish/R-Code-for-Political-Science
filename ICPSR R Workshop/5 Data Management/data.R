##--------------------------------------------------------------##
##                  Script for Lecture 5:                       ##
##                Data and Data Management                      ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

# Data in R packages

library("carData")
head(Duncan)  # from carData, via "lazy data"
objects()     # Duncan not in workspace

data("Animals", package="MASS")  
search()   # MASS package not loaded
objects()  # Animals in workspace
head(Animals)
help("Animals", package="MASS")

# reading data

    # entering data at the keyboard

(x <- c(1, 2, 3, 4)) # numeric data
(names <- c("John", "Sandy", 'Mary')) # character data
(v <- c(TRUE, FALSE)) # logical data


cooperation <- scan()
49 64 37 52 68 54
61 79 64 29
27 58 52 41 30 40 39
44 34 44

  # must enter empty line to end scan()


cooperation

# patterned data

rep(5, 3)
rep(c(1, 2, 3), 2)
rep(1:3, 3:1)
rep(c(1, 2, 3), each=2)

(condition <- rep(c("public", "anonymous"), c(10, 10)))
(condition <- rep(c("public", "anonymous"), each=10))  # equivalent
(sex <- rep(rep(c("male", "female"), each=5), 2))

# defining a data frame (data set)

(Guyer <- data.frame(cooperation, condition, sex))

Guyer <- data.frame(  # equivalent
    cooperation = c(49, 64, 37, 52, 68, 54, 61, 79, 64, 29, 
                    27, 58, 52, 41, 30, 40, 39, 44, 34, 44),
    condition = rep(c("public", "anonymous"), c(10, 10)),
    sex = rep(rep(c("male", "female"), each=5), 2)
)

    # reading data from a file into a data frame

Duncan <- read.table("Duncan.txt", header=TRUE)  # Duncan in current directory
getwd()  # current directory, setwd() to change
View(Duncan)

file.choose()  # returns character string with path to file
Duncan <- read.table(file.choose(), header=TRUE) # alternative
head(Duncan)

    # reading data via the clipboard (e.g., from Excel)

Duncan <- read.table("clipboard", header=TRUE)

# in macOS: Duncan <- read.table(pipe("pbpaste"), header=TRUE)
View(Duncan)

    # reading data from a URL

Duncan <- read.table(
    "http://socserv.mcmaster.ca/jfox/Courses/R/ICPSR//Duncan.txt",
    header=TRUE)
head(Duncan)

    # comma-separated values: Duncan.csv

Duncan <- read.csv("Duncan.csv")
str(Duncan) 
rownames(Duncan) <- Duncan$occupation
Duncan$occupation <- NULL  # remove variable
head(Duncan)

    # Reading data from a spreadsheet

library("car")

Duncan <- Import("Duncan.xlsx")
str(Duncan)

  # Input() also supports importing data from SPSS, SAS, Stata, etc.
  # see help("Import)

    # writing data to a file

write.table(Duncan, "c:/temp/Duncan.txt")  # adjust directory for your system
write.table(Duncan, "c:/temp/Duncan.txt", quote=FALSE, sep="\t")

    # saving an object in "external" format

save(Duncan, file="c:/temp/Duncan.RData")   # adjust directory for your system
objects() # no copy of Duncan in workspace
load("c:/temp/Duncan.RData") # restores Duncan as side effect 
objects()
head(Duncan)
remove(Duncan)

saveRDS(Duncan, file="c:/temp/Duncan.RData")
Otis <- readRDS("c:/temp/Duncan.RData")  # returns data frame
objects()
head(Otis)
remove(Otis)

# working with data frames

    # The search path (time permitting)

search()
prestige  # not accessible on path

Duncan[, "prestige"]  # but exists in Duncan

attach(Duncan)
prestige  # now accessible
search()

attach(Prestige)
search()  # Prestige before Duncan
prestige  # prestige in Prestige shadows prestige in Duncan


Duncan[, "prestige"]  # still there!

detach(Prestige)
search()
prestige  # now from Duncan -- confusing?

Duncan$log.prestige <- log(prestige)
head(Duncan)
log.prestige  # not in attached version of data frame 
prestige # there -- confusing?
Duncan$log.prestige

mean(prestige)
mean(prestige, trim=0.1)

mean <- function(x){
    warning("the mean function in the base package is shadowed")
    sum(x)/length(x)
}

mean(prestige)  # our mean shadows mean in base package
mean(prestige, trim=0.1)  # fails!

remove(mean)
mean(prestige, trim=.1)  # finds mean in base

mean <- mean(prestige)  # variable named "mean" --  no problem!
mean
mean(prestige)

remove(mean)
detach(Duncan)

    # avoiding attach()

mean(Duncan$prestige) # explicit indexing
mean(Duncan[ , "prestige"]) # equivalent, column by name
mean(Duncan[ , 4]) # equivalent, column by number

(lm(prestige ~ income + education, data=Duncan)) # using the data argument

with(Duncan, mean(prestige))  # using with()
with(Duncan, lm(prestige ~ income + education))  # better to use data argument

Duncan2 <- within(Duncan, {  # using within(); note compound statement with {}
    log.income <- log(income)
    log.education <- log(education)
})
head(Duncan2)
head(Duncan)

    # missing data

head(Freedman, 10)  # first 10 rows
tail(Freedman)  # last 6 rows
some(Freedman)  # 10 randomly sampled rows

Freedman$density
median(Freedman$density)
median(Freedman$density, na.rm=TRUE)
# other similar functions: mean, var, sd, etc.

with(Freedman, {
  plot(density, crime)
  showLabels(density, crime, labels=row.names(Freedman), 
             n=5, method="x") # from car package
})

log(c(1, 10, NA, 100), base=10) # NAs propogated

with(Freedman, plot(log(density, base=10), crime))

lm(crime ~ log(density, base=10), data=Freedman) # NAs handled by na.action
getOption("na.action")  # default NA action

abline(lm(crime ~ log(density, base=10), data=Freedman), lty="dashed")

        # dealing directly with NAs (time permitting)

good <- with(Freedman, complete.cases(crime, density))
head(good, 20)  # first 20 values
with(Freedman,  # NAs handled by indexing:
    lines(lowess(log(density[good], base=10), crime[good], f=1.0)))

        # filtering NAs

Freedman.good <- na.omit(Freedman) 
head(Freedman.good)  # first 6 rows
dim(Freedman.good)   # number of rows and columns
dim(Freedman)

        # testing for NAs

NA == c(1, 2, NA, 4) # wrong!
is.na(c(1, 2, NA, 4))
sum(is.na(Freedman)) # count of NAs
sum(!is.na(Freedman)) # count of valid values


objects()
remove(good, Freedman.good, Duncan, Duncan2)

# numeric variables, character variables, and factors

condition
is.character(condition)

condition <- as.factor(condition)
condition

remove(cooperation, condition, sex)

Guyer$condition
is.character(Guyer$condition)
is.factor(Guyer$condition)

summary(Guyer)

# modifying data (as time permits)

Guyer$perc.coop <- 100*Guyer$cooperation/120 # assign to data frame
Guyer <- within(Guyer, perc.coop <- 100*cooperation/120) # equivalent
head(Guyer)  # first 6 rows

Guyer$cooperation <- with(Guyer, log(perc.coop/(100 - perc.coop))) # replace
Guyer <- within(Guyer, 
    cooperation <- log(perc.coop/(100 - perc.coop))) # equivalent
head(Guyer)


# matrices, arrays, and lists (time permitting)

(A <- matrix(1:12, 3, 4))  # filled column-wise
(B <- matrix(c("a","b","c"), 4, 3, byrow=TRUE))
dim(A)
dim(B)

attributes(A)

str(A) # structure of an object (very useful!)
str(B)

(v <- sample(10, 10)) # permutation of 1:10
dim(v)
str(v)

vv <- v  # make a copy
dim(vv) <- c(5, 2) # reshape into a matrix
vv
is.matrix(v)
is.matrix(vv)

(array.3 <- array(1:24, c(4,3,2)))  # 3D array
dim(array.3)

(list.1 <- list(mat.1=A, mat.2=B, vec=v))  # a list

# indexing

    # vectors

v
v[2]        # one element
v[c(4, 2, 6)] # several elements
v[c(4, 2, 4)] # elements may be repeated

v[-c(2, 4, 6, 8, 10)]   # omitting elements

names(v) <- letters[1:10]
v
names(v)
v[c("f", "i", "g")]   # indexing by names

v < 6
v[v < 6]    # logical indexing

(vv <- v)  # make a copy

vv[c(1, 3, 5)] <- c(1, 2, 3)    # replacing elements
vv

vv[c("b", "d", "f", "h", "j")] <- 0
vv

remove(vv)

    # matrices

A
A[2, 3]
A[c(1, 2), 2]
A[c(1, 2), c(2, 3)]
A[c(1, 2), ]

A[c(1, 2), 2, drop=FALSE]    # retain column dimension

A[ , -c(1, 3)]  # delete columns 1 and 3
A[-1, -2]       # delete row 1 and column 2

rownames(A) <- c("one", "two", "three")
colnames(A) <- c("w", "x", "y", "z")
A

A[c("one", "two"), c("x", "y")]
A[c(TRUE, FALSE, TRUE), ]

(AA <- A)

AA[1, ] <- 0
AA

remove(AA)

    # lists

list.1

list.1[c(2, 3)]

list.1[2]   # a one-element list
str(list.1[2])

list.1[[2]] # a list element
str(list.1[[2]])

list.1["mat.1"]  # one-element list
list.1[["mat.1"]]  # list element

list.1$mat.1  # list element

list.1$mat.1 <- matrix(1, 2, 2)     # replacing a list element
list.1$title <- "an arbitrary list" # adding an element (causes list to be copied)
list.1$mat.2 <- NULL                  # removing an element
list.1

    # data frames

head(Guyer)

Guyer[, 1] # first column
Guyer[, "cooperation"] # by name
Guyer[c(1, 2), ] # first two rows
Guyer[c("1", "2"), "cooperation"]
Guyer[-(6:20), ] # remove rows

Guyer$sex=="female" & Guyer$condition=="public" # note: & is vectorized
Guyer[Guyer$sex=="female" & Guyer$condition=="public", ] # selected rows

Guyer$cooperation  # a vector (column from data frame)
Guyer[["cooperation"]]  # equivalent
Guyer["cooperation"]  # a one-column data frame

# large data sets

    # LS regression

set.seed(123456789) # for reproducibility
X <- rnorm(100000*100)  # 10,000,000 values
X <- matrix(X, 100000, 100)  # 100,000 rows, 100 columns

y <- 10 + as.vector(X %*% rep(1, 100) + rnorm(100000, sd=10))

memory.size()  # memory used in Mb, Windows only
memory.limit() # Windows only

system.time(m <- lm(y ~ X)) # linear least-squares regression
head(coef(m))  # first 6 coefficients

    # logistic regression

p <- as.vector(1/(1 + exp(-X %*% rep(0.25, 100))))
summary(p)
yy <- rbinom(100000, 1, prob=p)
table(yy)

system.time(m <- glm(yy ~ X, family=binomial)) # logistic regression
head(coef(m))

objects()
remove(list=objects())
objects()

    # try this on your own time (1 million cases):

set.seed(123456789) # for reproducibility
X <- rnorm(1e6*100)  # 100,000,000 values
X <- matrix(X, 1e6, 100)  # 1,000,000 rows, 100 columns

y <- 10 + as.vector(X %*% rep(1, 100) + rnorm(1e6, sd=10))

memory.size() # memory used in Mb, Windows only
memory.limit() # Windows only

system.time(m <- lm(y ~ X)) # linear least-squares regression
head(coef(m))  # first 6 coefficients

p <- as.vector(1/(1 + exp(-X %*% rep(0.25, 100))))
summary(p)
yy <- rbinom(1e6, 1, prob=p)
table(yy)

system.time(m <- glm(yy ~ X, family=binomial)) # logistic regression
head(coef(m))

objects()
remove(list=objects())
objects()

# Using "Pipes" (time permitting)

library("magrittr")  # provides pipe operator 
                     # (Rene Magritte: "Ceci n'est pas une pipe")

x <- rnorm(10)

    # direct function call

sort(x)  
sort(x, decreasing=TRUE)

    # piping into a function

x %>% sort()  
x %>% sort(decreasing=TRUE)

    # piping into an argument other than the first

TRUE %>% sort(x, decreasing=.)
TRUE %>% sort(x, .)  # equivalent


    # A problem from r-help

(concept_df <- data.frame(concept=c("butan acid ", 
    "nano diamond particl", "slurri composit", 
    "composit ph polis", " inorgan particl ",  
    "grind liquid", "liquid formul", "nanoparticl", 
    "size abras particl", "agent malic acid")))

(chemical_df <- data.frame(chemical=
    c("basic", "alkalin", "alkali", "acid",  " ph ", "hss")))

        # Problem: Find names in concept in the first 
        #          data frame that match names in
        #          chemical in the second data frame; 
        #          label the corresponding rows
        #          in the first data frame as "chemical".

        # solution using direct function calls

            # a "regular expression": 
            #  \\b = a word boundary  | = or

(match_string <- paste0("\\b", 
    paste(chemical_df$chemical, collapse="\\b|\\b"),
    "\\b"))  

            # test:

grepl(match_string, 
    c("butan acid ", "nano diamond particl", "butanacid "))

concept_df$category <- ifelse(
    grepl(match_string, concept_df$concept), 
    "chemical", "")
concept_df

        # solution using pipes (due to Georges Monette)

(chemical_df$chemical %>%
        paste(collapse = '\\b|\\b') %>%
        paste0('\\b', . ,'\\b') ->  # note forward assign
        match_string)

concept_df$concept %>%
    grepl(match_string, .) %>%
    ifelse('chemical', '') ->
    concept_df$category
concept_df
