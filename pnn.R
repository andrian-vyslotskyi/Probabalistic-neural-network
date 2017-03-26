##pnn
calcR <- function(trainMatr, x) {
  apply(trainMatr, 1, function(row) sqrt(sum(( row - x )^2)))
}
calcD <- function(r, sigma = 3) {
  exp(-(r/sigma)^2)
}
calcP <- function(cl, all) {
  sum(cl)/sum(all)
}
pnn <- function(train, x , sigma) {
  #each column will be R list for some class
  R <- sapply(levels(train$V5), function(l)  calcR(train[train$V5 == l, 1:4], x))
  #process lists of each classes
  D <- t(lapply(R, function(r)  calcD(r, sigma) ))
  #create arrays from lists to calculate sums
  P <- sapply(colnames(D), function(coln) calcP(unlist(D[,coln]), unlist(D)), 
              USE.NAMES = T, simplify = T)
  return(P)
}
####
sigma = 3
input <- read.csv("iris.csv",stringsAsFactors = T, header = F)

smp_size <- floor(0.75 * nrow(input))
set.seed(123)
train_ind <- sample(seq_len(nrow(input)), size = smp_size)

train <- input[train_ind, ]
test <- input[-train_ind, ]

x <- c(5.0,3.6,1.4,0.2)

p <- pnn(train, x, sigma)

print(p)
names(which.max(p))
max(p)
