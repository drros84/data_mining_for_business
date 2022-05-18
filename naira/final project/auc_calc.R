## plot the ROC curve for classification of y with p
auc_calc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  aucx <- sum(specificity*(diff(c(0,1-sensitivity))))
  
  my_list <- list("AUC" = aucx, "filler"= "filler")
  return(my_list) 
  
}
