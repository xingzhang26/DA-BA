## Matrix:
matA <- matrix(1:6, nrow=3, ncol=2); 
matA
matB <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)
matB

nrow(matA)
ncol(matB)

dim(matA)

attributes(matA)

## Slicing and indexing:
#by row
matA[1,]
#by col
matA[,1]

matA[c(1,2),]
matA[c(1,3),c(1)]

colnames(matA) <- c('A', 'B')
matA
rownames(matA) <- c('a', 'b', 'c')
matA

matA['a',]
matA[, 'A']
matA[c('a', 'c'), 'B', drop=FALSE]

## bind by col&row
cbind(c(1,2), c(3,4), c(5,6))
cbind(matA, c(0,1,2))
cbind(matA,1)

rbind(c(1,2,3),c(4,5,6))

## Calculation
matA+2
matB/2

matA + matA/2
matA * matA/2


matA %*% matB

c(1,2) %*% matB

matB%*%c(1,2)


## product
#inner
sum(c(1,2,3) * c(1,2,3))
#outer
c(1,2,3) %o% c(1,2,3)

## invert
solve(matB)
solve(matB, c(1,2))


apply(matA, 2, sum)
apply(matA, 1, sum)

## n-dim array
ndarray <- array(1:24, dim=c(2,3,4))
ndarray
ndarray[,,c(1,3)]

ndarray[,2,1:3]


## DataFrame
d <- data.frame(
  name=c("Kevin", "Peter", "Jerry"), 
  age=c(26, 23, 25), 
  height=c(170, 178, 175),
  stringsAsFactors=FALSE)
## data.frame will turn characters into factors, stringAsFactors=FALSE to stop that
d
names(d)
colnames(d)

d[2,3]
d[2,'height']
d[[2]]
d[,2]
d['name']
d[['name']]
d[,'name']
d$name
d[1:2, 'name']
d[d[,'age']>=25,,drop=FALSE]
rownames(d)<-d$name
d$name <- NULL
d

dm <- data.frame(
  'Grade'=1:6,
  'Course'=c(0, 2, 2, 2, 2, 1),
  'Pass'=c(T, F, F, F, T, F)
)
dm
# DataFrame data can be represented by rows and row names
rownames(dm) <- dm[['Grade']]
dm[['Grade']] <- NULL

idx <- c(2,1,1,3)
v<-dm[as.character(idx),]


rownames(v) <- NULL
v


d2 <- as.matrix(d[,c("age", "height")])
d3 <- crossprod(d2); 




## List: for saving different type of data
r <- list(name='Abel', age=30, year=c(1994, 1995,1998), book = c('Analysis', 'Algebra'))
r

typeof(r)
is.list(r)
r[4]
r[[4]]
r[4]$book

r['age']
r[['age']]
r['age']$age


names(r)
names(r) <- c('name','age', 'grade', 'author')
r
names(r)

as.list(1:5)



