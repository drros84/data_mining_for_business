install.packages("randomForest")
library(Matrix)
library(gamlr)
library(rpart)
library(rpart.plot)
library(ranger)
library(pROC)
library(caret)
library(randomForest)
# import dataset
bank <- read.csv("bank-additional-full.csv", sep=";") 
# remove default variable bc of logit error
bank <- subset(bank, select = -c(default)) 
bank_clean <- bank[!(bank$job=="unknown" | bank$marital=="unknown" |
                       bank$education=="unknown" | bank$housing=="unknown" |
                       bank$loan=="unknown"),] 
str(bank_clean)
factor <- c(2:9,14)
bank <- bank_clean
MSE <- list(CV = NULL, LASSO = NULL, CART = NULL, RF = NULL)
ROCAUC <- list(CV = NULL, LASSO = NULL, CART = NULL, RF = NULL)
##turn y into binary
bank$y <-as.integer(bank$y == "yes")
# factor variables
bank[,factor] <- lapply(bank[,factor], factor) 
bank$month <- factor(bank$month, levels = c("mar", "apr", "may", "jun", "jul",
                                            "aug", "sep", "oct", "nov", "dec"))
bank$day_of_week <- factor(bank$day_of_week, levels = c("mon", "tue", "wed", "thu", "fri"))
bank <- naref(bank)

##check for n/a values & missing
sum(is.na(bank))

data.frame(num_missing=colSums(is.na(bank)))

## deeper dive into specific variables

## plot class variable of y to see distr
hist(bank$y)
ggplot(bank, aes(x=y)) +
  geom_bar()

for (col in colnames(bank)){
  print(col)
  #res <- cor.test(bank$y, bank[,col], method = "pearson")
  print(cor(bank[,col], bank$y))
}

##density plot
ggplot(bank, aes(x=age))+ geom_density(color="darkblue", fill="lightblue")
ggplot(bank, aes(x=duration))+ geom_density(color="darkblue", fill="lightgreen")

##data interaction exploring
# split input and output


## mosaic plot
par(mai=c(.8,.8,.1,.1))
plot(factor(y) ~ job, data=bank, col=c(8,2), ylab="Y", xlab="Job") 



# summary statistics post-cleaning
table(bank$y)
summary(bank)
aggregate(.~y, bank, sd)


# lasso reg of outcome on all other covariates
set.seed(0)
# training & test set
choose = sample(seq_len(nrow(bank)), size = floor(0.9*nrow(bank))) 
training_set = bank[choose,]
test_set = bank[-choose,]

scribex <- sparse.model.matrix( y ~ .^2, data=training_set)[,-1]
scribex_test <- sparse.model.matrix( y ~ .^2, data=test_set)[,-1]

logit_lasso <- cv.gamlr(scribex, training_set$y , family="binomial", verb=TRUE)

##plot regularization path and
par(mfrow=c(1,2))
plot(logit_lasso$gamlr)  
plot(logit_lasso) ####Plot CV model/penalty chosen by the 1 standard error rule
plot(logit_lasso$gamlr$lambda, AICc(logit_lasso$gamlr))## plot of AICc against lambda
##plot(log(logit_lasso$gamlr$lambda), AICc(logit_lasso$gamlr))## plot of AICc against lambda


##coefficents for AIC,CV,BIC, AICc
sum(coef(logit_lasso, s="min")!=0) # Num of CV model min error coefficients
sum(coef(logit_lasso$gamlr)!=0) # AICc model coeffcients
sum(coef(logit_lasso)!=0) # 1se
sum(coef(logit_lasso$gamlr, s=which.min(AIC(logit_lasso$gamlr)))!=0) # AIC
sum(coef(logit_lasso$gamlr, s=which.min(BIC(logit_lasso$gamlr)))!=0) # BIC

## Printing names of coefficients for AIC
logit_AIC_coefs <- coef(logit_lasso$gamlr)
drop(logit_AIC_coefs)
logit_AIC_coefs <- logit_AIC_coefs[-1,]
names(logit_AIC_coefs[which(logit_AIC_coefs!=0)])

sort(abs(logit_AIC_coefs),decreasing = TRUE)[1:5]


##oos R2 on CV IS R@ a valid thing to look at for classification models?
logit_dev <- function(y, pred) {
  return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
}


##IS Pred on AIC
pred <- predict(logit_lasso$gamlr, scribex, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ training_set$y, xlab="subribed", ylab="prob of subscibed", col=c("pink","dodgerblue"))
##OOS pred/fit on AIC
predoos <- predict(logit_lasso$gamlr,scribex_test, type="response")
predoos <- drop(predoos)

## calc OOS R2
mean_pred_aic <- mean(training_set$y)
1 - logit_dev(new_y, logit_pred) / logit_dev(new_y, mean_pred)
MSE$LASSO <- c(MSE$LASSO,1-(logit_dev(test_set$y,predoos))/logit_dev(test_set$y,mean_pred_aic))
## True positive and False positive at p=.2. maximize True Positives
##

## load taddy's code
source("roc.R")
source("auc_calc.R")

aic_auc <- auc_calc(p=pred, y=training_set$y, bty="n")
ROCAUC$LASSO <- c(ROCAUC$LASSO,aic_auc)

par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred, y=training_set$y, bty="n", main="in-sample AIC")
# our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[training_set$y==0]), 
       y=mean((pred>.2)[training_set$y==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[training_set$y==0]), 
       y=mean((pred>.5)[training_set$y==1]), 
       cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
cat("AUC for AIC model is",aic_auc$AUC)



##Edited ROC to print OOS AUC
par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=predoos, y=test_set$y, bty="n", main="Out of sample AIC")
aic_auc <- auc_calc(p=predoos, y=test_set$y, bty="n")
points(x= 1-mean((predoos<.2)[test_set$y==0]), 
       y=mean((predoos>.2)[test_set$y==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos<.5)[test_set$y==0]), 
       y=mean((predoos>.5)[test_set$y==1]), 
       cex=1.5, pch=20, col='blue')

p <- 0.2
fp <- sum((predoos>p)[test_set$y==0] )/sum(predoos>p) ## 1-spec
fn<- sum((predoos<p)[test_set$y==1] )/sum(predoos<p) ## 1- sens
tp<- sum((predoos>p)[test_set$y==1] )/sum(test_set$y==1) ## Sensitivity
tn <- sum((predoos<p)[test_set$y==0] )/sum(test_set$y==0) ## Specificity
cat("AUC for AIC OOS model is",aic_auc$AUC)
cat("AIC OOS Model sensitivity at cutoff of",p,":", mean((predoos>p)[test_set$y==1] ), "\n")
cat("AIC OOS  Accuracy at cutoff of",p,":", (tp+tn)/(tp+tn+fp+fn), "\n")
cat("AIC OOS  Precision at cutoff of",p,":", tp/(tp+fp), "\n")
cat("AIC OOS  Recall at cutoff of",p,":", tp/tp+fn, "\n")

## Same process of calculating metrcs for CV

##CV Coefs
logit_CV_coefs <- coef(logit_lasso)
drop(logit_CV_coefs)
logit_CV_coefs <- logit_CV_coefs[-1,]
names(logit_CV_coefs[which(logit_CV_coefs!=0)])

sort(abs(logit_CV_coefs),decreasing = TRUE)[1:5]

##IS Pred on IS
pred_cv <- predict(logit_lasso, scribex, type="response")
pred_cv <- drop(pred_cv) # remove the sparse Matrix formatting
boxplot(pred_cv ~ training_set$y, xlab="subscribed", ylab="prob of subscibed", col=c("pink","dodgerblue"))
##OOS pred/fit on AIC
scribex_test_cv <- sparse.model.matrix( y ~ .^2, data=test_set)[,-1]
predoos_cv <- predict(logit_lasso,scribex_test_cv, type="response")
predoos_cv <-drop(predoos_cv)


##Calc IS CV
cv_auc <- auc_calc(p=pred_cv, y=training_set$y, bty="n")
par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred_cv, y=training_set$y, bty="n", main="in-sample CV")
# our 1/5 rule cutoff
points(x= 1-mean((pred_cv<.2)[training_set$y==0]), 
       y=mean((pred_cv>.2)[training_set$y==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_cv<.5)[training_set$y==0]), 
       y=mean((pred_cv>.5)[training_set$y==1]), 
       cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
cat("AUC for In Sample CV model is",cv_auc$AUC)


##Calc for OOS CV
cv_auc_os <- auc_calc(p=predoos_cv, y=test_set$y, bty="n")
par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=predoos_cv, y=test_set$y, bty="n", main="Out of Sample CV")
points(x= 1-mean((predoos_cv<.2)[test_set$y==0]), 
       y=mean((predoos_cv>.2)[test_set$y==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos_cv<.5)[test_set$y==0]), 
       y=mean((predoos_cv>.5)[test_set$y==1]), 
       cex=1.5, pch=20, col='blue')

p <- 0.2
fp_cv <- sum((predoos_cv>p)[test_set$y==0] )/sum(predoos_cv>p) ## 1-spec
fn_cv<- sum((predoos_cv<p)[test_set$y==1] )/sum(predoos_cv<p) ## 1- sens
tp_cv<- sum((predoos_cv>p)[test_set$y==1] )/sum(test_set$y==1) ## Sensitivity
tn_cv <- sum((predoos_cv<p)[test_set$y==0] )/sum(test_set$y==0) ## Specificity
cat("AUC for Cv OOS model is",cv_auc_os$AUC)
cat("CV OOS Model sensitivity at cutoff of",p,":", mean((predoos_cv>p)[test_set$y==1] ), "\n")
cat("CV OOS  Accuracy at cutoff of",p,":", (tp_cv+tn_cv)/(tp_cv+tn_cv+fp_cv+fn_cv), "\n")
cat("CV OOS  Precision at cutoff of",p,":", tp_cv/(tp_cv+fp_cv), "\n")
cat("CV OOS  Recall at cutoff of",p,":", tp_cv/tp_cv+fn_cv, "\n")
ROCAUC$CV <- c(ROCAUC$CV,cv_auc)
## calc OOS R2
mean_pred_cv <- mean(training_set$y)
MSE$CV <- c(MSE$CV,1-(logit_dev(test_set$y,predoos_cv))/logit_dev(test_set$y,mean_pred_aic))

# Classification tree- cost complexity pruning(using CV for optimal model selection)

bank_tree <- bank
bank_tree$y <- factor(bank_tree$y)
choose_t = sample(seq_len(nrow(bank)), size = floor(0.9*nrow(bank_tree))) 
training_set_tree = bank_tree[choose_t,]
test_set_tree = bank_tree[-choose_t,]

## make big tree
tree_train <-rpart(y ~ ., data = training_set_tree, minsplit = 2,
                   minbucket = 1, cp = 0, xval = 2)
##prune with cp
opt_CP <- tree_train$cptable[which.min(tree_train$cptable[, "xerror"]),
                             "CP"]
optimaltree <- prune(tree_train, cp = opt_CP + 1e-05)
rpart.plot(optimaltree)
## Top Coefs
CART_coefs <- coef(optimaltree)
drop(CART_coefs)
CART_coefs <- CART_coefs[-1,]
names(CART_coefs[which(CART_coefs!=0)])

sort(abs(CART_coefs),decreasing = TRUE)[1:5]

## Look at fit/pred
library(ROCR)
bank_train.pred.tree<- predict(optimaltree, test_set_tree, type="prob")
pred = prediction(bank_train.pred.tree[,2], test_set_tree$y)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
bank_train.pred.tree_resp<- predict(optimaltree, test_set_tree, type="class")

auc_tree <- slot(performance(pred, "auc"), "y.values")[[1]]
perf_acc<- performance(pred, measure = "acc")
p_tree <- .2
cat("AUC for Classification Tree OOS model is",auc_tree)
cat("Classification Tree OOS  Accuracy at cutoff of",p_tree,":", slot(perf_acc,"y.values")[[1]][p_tree], "\n")
confusionMatrix(bank_train.pred.tree_resp, test_set_tree$y)
ROCAUC$CART <- c(ROCAUC$CART,auc_tree)
MSE$CART <- c(MSE$CART, sqrt(sum(( as.integer(test_set_tree$y==1) - as.integer(bank_train.pred.tree_resp==1))^2)))
## calc r2 of tree
bank_tree_pred<- predict(optimaltree, test_set_tree, type="class")

# random forest
rf <- ranger(y ~ ., data = training_set_tree, num.tree = 200, min.node.size = 5, write.forest = TRUE)
yhat.rf <- predict(rf, data = test_set_tree)$predictions

##eval performance metrics
rf_prob <- ranger(y ~ ., data = training_set_tree, num.tree = 200, min.node.size = 5, write.forest = TRUE, probability = TRUE)
rf_y_prob = predict(rf_prob, data = test_set_tree)
pred_rfprob = prediction(rf_y_prob$predictions[,2],test_set_tree$y)
perf = performance(pred_rfprob, "tpr", "fpr")
plot(perf, colorize=TRUE)
cm_rf <- confusionMatrix(yhat.rf, test_set_tree$y)
auc_rf <- unlist(slot(performance(pred_rfprob, "auc"), "y.values"))
cat("AUC for Classification Tree OOS model is",auc_rf)
cat("Classification Tree OOS  Accuracy is:",cm_rf, "\n")
MSE$RF <- c(MSE$RF, sqrt(sum(( as.integer(test_set_tree$y==1) - as.integer(yhat.rf==1))^2)))


