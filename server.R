library(shiny)

calcR <- function(trainMatr, x) {
  apply(trainMatr, 1, function(row) sqrt(sum(( row - x )^2)))
}
calcD <- function(r, sigma = 1) {
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

shinyServer(function(input, output) {
   
  output$table <- renderTable({
    set.seed(input$seed)
    
    input_data <- read.csv("iris.csv",stringsAsFactors = T, header = F)
    
    smp_size <- floor(input$train_coef * nrow(input_data))
    train_ind <- sample(seq_len(nrow(input_data)), size = smp_size)
    
    train <- input_data[train_ind, ]
    test <- input_data[-train_ind, ]
    
    result <- apply(test, 1, function(x) pnn(train, as.numeric(x[1:4]), input$sigma) )
    classes <- apply(result, 2, function(probs) names(which.max(probs)) )
    probs <- apply(result, 2, function(probs) max(probs) )
    
    colnames(test) <- c("v1","v2","v3","v4","Class")
    test["NN class"] <- classes
    test["NN probabilities"] <- probs
    test
  })
  
})
