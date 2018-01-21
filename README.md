# Boston-Housing-Data

In this problem, we work with the Boston Housing Data obtained as part of the MASS package in R. The Boston dataset consists of housing values in the suburbs of Boston. The goal is to predict the median value of owner-occupied homes (medv) using all the other predictor variables available using various methods such as, (generalized) linear regression, tree, generalized additive models, and neural network. 
A seed is set with the M no: 10714314 and randomly sampled into 2 datasets of 75% (training) and 25% (testing) each. As the data set, has been explored as part of previous assignments in the course, here we dive into model fitting directly. The different model performances based on fits of the training data (in-sample) and out-of-sample are then compared for each method, along with 3-fold cross validation for GLM.The major performance indicators are tabulated in the attached files.

The GAM method seems to have the lowest model MSE amngst GLM, CART and Neural Networks (considering both in sample and out sample data). The Neural network MSE is actually the lowest for in sample but the out of sample MSE is very high comparatively and may be the result of the random sample split. For the GLM, it can be seen both the MSE values are comparable and the CV value falls between the in and out sample MSE. This is as expected because 3-fold CV is equivalent to taking the average after fitting three different testing models. Thus, out of these models considering both in and out sample datasets GAM seems to be the best model for this dataset.



