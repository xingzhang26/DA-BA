## Variable Assignment

# Basic datatype for R inclues int/float，logical（TRUE, FALSE), character/str。 
# R supports NA as missing value
x5 <- 6.25
x6 = sqrt(x5)
## assigning values:
x <- 1:5
y <- x
y[3] <- 0
x
y

##  numerical vectors
marks <- c(9, 7, 4, 7, 8)
x <- c(1:3, 110:130)
x1 <- c(1: 5)
x2 <- c(7, 8)
x <- c(x1, x2)
x

## factor: used to represent category data 
x <- c("F", "M", "F", "F",  "M")
gender <- factor(x)
gender
attributes(gender)
class(gender)
levels(gender)
as.numeric(gender)
as.character(gender)

## typeof
typeof(1:3)
typeof(c(1,2,3))

typeof(c(TRUE, NA, FALSE))
typeof('Abc')
#type of factor is integer
typeof(factor(c('A', 'B', 'C', 'd')))

## is.xxx to judge if a variable belongs to certain type
is.integer(c(1, -3))
is.na(NA)


## as.xxx() to change data type
as.numeric(c(FALSE, TRUE))
as.logical(0,1)

## indexing and subset: from 1 not 0

x[2]
x[c(1,3,1)]


x <- c(1,2,3)
x[6]
x[6] <- 6
x
x <- c(1:9)
x[x > 5]

## Series/Dictionary for indexing
mapping <- c("A"=1, "B"=2, "C"=3)
# or:
mapping <- c(1, 2, 3)
names(mapping) <- c("A", "B", "C")
# or:
mapping <- setNames(c(1, 2, 3), c("A", "B", "C"))
mapping
mapping["B"]
mapping[c("A", "C")]

## Vectorize and broadcasting (similar as Numpy)
v1 <- c(1: 10)
v2 <- c(11: 20)
v1 + v2

mapping + c(10:12)

#broadcasting
v1 <- c(10: 19)
v2 <- c(1, 3)
v1 + v2

c(1,2) + c(1,2,3)

## Vectorized function
sqrt(c(1:10))
log10(c(1:10))
exp(c(1:10))


## Sorting in R (Python sort&rank)
x <- c(3, 5, 11, 7, 9, 2, 8)
sort(x)
rev(sort(x))
order(x)
x[order(x)]


## seq, rep, cumsum, cumprod
seq(5)
cumsum(seq(5))
cumprod(seq(5))

rep(0,10)
rep(c(1,3),2)
rep(seq(5),3)
rep(c(1,3), c(2,4))
rep(c(1,3), each=2)



## logical type
x <- (log10(15) < 2); 
x
c(1:5) > 2
1:10 >= 10:1

c(1,NA,3) > 2
NA == NA

is.na(c(1,NA,3,5) > 2)
# %in%
c(1,3) %in% c(2,4)
c(1,3) %in% c(2,3,4)
# match(x, y): find the position of each x in y 
match(c(1, 3), c(2,3,4,3))

#all/any/which/duplicated
all(c(1:9) > 2)
any(c(1:3) > 2)
which(c(FALSE, TRUE, TRUE, FALSE, NA))
which((1:15) > 12)

identical(c(1,2,3), c(1,2,NA))
identical(c(1L,2L,3L), c(1,2,3))
identical(c(1,2,3), c(1,2,3))
identical(c(1:3),c(1,2,3))

all.equal(c(1L,2L,3L), c(1,2,3))

duplicated(c(1,2,1,3,NA,4,NA))





