# Ikea Report

## For the dataset that I was given I decided to run 2 model Decision tree and Random Forest
## So below you can see rusults for both of the model including final coclusion

## Decision Tree Model

![](Rplotrocdt.jpeg)

### Final results of Decision Tree Model

1. **Accuracy**          0.905
2. **Precision**         0.903
3. **Recall**            0.743
4. **ROC AUC**           0.903

### From the final results of Decision Tree we run we can conclude that:

#### 90.5% of our predictions were correct 
#### 90.3% of the chocolates we predicted had high rating were correctly predicted
#### Our model detected correctly 74.3% of the chocolates which had high rating in reality 


## Random Forest Model

![](Rplotrf.jpeg)


### Final results of Random Forest Model

1. **Accuracy**          0.925
2. **Precision**         0.895
3. **Recall**            0.832
4. **ROC AUC**           0.981

### From the final results of Random Forest model we run we can conclude that:

#### 92.5% of our predictions were correct 
#### 89.5% of the chocolates we predicted had high rating were correctly predicted
#### Our model detected correctly 83.2% of the chocolates which had high rating in reality 


 

# Conclusion

### From the Results we got by running both models we can defenetely say that for this particular dataset Random Forest Model  better. Except for the Recall results for all three parameters were higher in the Random Tree model than the results we got through Decision Tree. 
### Especially that is apperent in the ROC parameter. In case of Random Forest AUC is obviously bigger than the Decision Tree ROC curve.

