#Boston Housing Data

library(MASS)
library(rpart)
library(boot)
head(Boston)
set.seed(10714314)
subset = sample(nrow(Boston), nrow(Boston) * 0.75) #sampling
Boston.train = Boston[subset, ]
Boston.test = Boston[-subset, ]
head(Boston.test)
Bostonnew<-Boston


#glm
Boston.glm = glm(medv ~., data = Boston.train)
summary.glm(Boston.glm)
plot(fitted(Boston.glm), residuals(Boston.glm), xlab = 'fitted', ylab = 'residuals', main
     = 'Residuals by Fitted from GLM', col = 'blue') 
NullBoston=glm(medv~1,data=Boston.train)
StepwiseModel=step(NullBoston,scope = list(lower=NullBoston,upper=Boston.glm), direction = "both")
summary(StepwiseModel)
Boston.glm.pred.train=predict(StepwiseModel,Boston.train)
Boston.glm.train.mean=mean((Boston.glm.pred.train - Boston.train$medv)^2)
Boston.glm.train.mean
Boston.glm.pred.test=predict(StepwiseModel,Boston.test)
Boston.glm.test.mean=mean((Boston.glm.pred.test - Boston.test$medv)^2)
Boston.glm.test.mean

#cv
FullModel=glm(medv~.,data=Boston)
NullModel=glm(medv~1,data=Boston)
StepwiseModel=step(NullModel,scope = list(lower=NullModel,upper=FullModel), direction = "both")
cvResult=cv.glm(data=Boston,glmfit=StepwiseModel,K=3)
cvResult$delta[2]


#cart
set.seed(1071434)

Boston.rpart <- rpart(formula = medv ~ ., data = Boston.train)
Boston.rpart
plot(predict(prune.Boston), residuals(prune.Boston), xlab = "fitted",ylab = "residuals", main =
       'Residuals by Fitted from CART', col = 'red') 
plot(Boston.rpart)
text(Boston.rpart)
plotcp(Boston.rpart) 
#printcp(Boston.rpart)
prune.Boston<-prune(Boston.rpart,cp=0.035)#
printcp(prune.Boston)
plot(prune.Boston)
text(prune.Boston)
Boston.train.pred.tree = predict(prune.Boston, Boston.train)
mean((Boston.train.pred.tree - Boston.train$medv)^2)
Boston.test.pred.tree = predict(prune.Boston,Boston.test)
mean((Boston.test.pred.tree - Boston.test$medv)^2)

##cvtree
#Boston.tree.cv=cv.glm(data = Boston, glmfit = prune.Boston, K = 3)
##Boston.tree.cv$delta[2]

#gam
library(mgcv)
Boston.gaminitial <- gam(medv ~ s(crim)+s(zn)+s(indus)+chas+s(nox)+s(rm)+s(age)+s(dis)+rad+s(tax)+s(ptratio)+s(black)+s(lstat),
                  data=Boston.train)
summary(Boston.gaminitial)
plot(Boston.gaminitial, shade = TRUE, seWithMean = TRUE, scale = 0)
Boston.gam <- gam(medv ~ s(crim)+s(indus)+s(nox)+s(rm)+s(dis)+rad+s(tax)+ptratio+s(black)+s(lstat),
                  data=Boston.train)
plot(fitted(Boston.gam), residuals(Boston.gam), xlab = 'fitted', ylab = 'residuals', main= 'Residuals by Fitted from GAM') 
summary(Boston.gam)
AIC(Boston.gam)
BIC(Boston.gam)
Boston.gam$deviance
Boston.gam.mse.train1 <-Boston.gam$dev/Boston.gam$df.res###
Boston.gam.predict.train <- predict(Boston.gam,Boston.train) 
Boston.gam.mse.train <- mean((Boston.gam.predict.train-Boston.train$medv)^2)
Boston.gam.mse.train 
Boston.gam.predict.test <- predict(Boston.gam,Boston.test) 
Boston.gam.mse.test <- mean((Boston.gam.predict.test -Boston.test$medv)^2)
Boston.gam.mse.test

#nnet

library(nnet)
#Scaling
for (i in 1:(ncol(Bostonnew) - 1)) {
 Bostonnew[, i] = scale(Bostonnew[, i]) 
}
Bostonnew<-as.data.frame(Bostonnew)
set.seed(10714314)
subset1 = sample(nrow(Boston), nrow(Boston) * 0.75) #sampling
Boston.scaletrain = Boston[subset1, ]
Boston.scaletest = Boston[-subset1, ]

Boston.nnet <- nnet(medv ~ ., size = 12, data = Boston.scaletrain, linout = TRUE,maxit=1000,decay=0.001)
Boston.nnet.pred.train = predict(Boston.nnet, Boston.scaletrain)

pr.nn <-(Boston.nnet.mse.train*(max(Boston.train$response)-min(Boston.train$response)))+min(Boston.train$response)
Boston.nnet.mse.train = mean((Boston.nnet.pred.train-Boston.scaletrain$medv)^2)
Boston.nnet.mse.train 
Boston.nnet.pred.test = predict(Boston.nnet, Boston.scaletest)
Boston.nnet.mse.test= mean((Boston.nnet.pred.test-Boston.scaletest$medv)^2)
Boston.nnet.mse.test 

##########
train_sse<-0;
test_sse<-0; 

# train networks with sizes of hidden units ranging from 0 to 20
for (n in 0:20)
{
  train_predict<-0;
  test_predict<-0;
  # for each size, train 10 networks with different random starting points
  for(i in 1:10)
  {
    set.seed(i);
    net<-nnet(medv~.,size = n, data=Boston.scaletrain, rang = 0.00001, linout = TRUE, maxit = 5000, decay = 0, skip = TRUE);
    train_predict<-train_predict+predict(net, Boston.scaletrain);
    test_predict<-test_predict+predict(net, Boston.scaletest);
  }
  # average outcomes of 10 networks
  Boston.scaletrain$pred_medv<-train_predict/i;
  Boston.scaletest$pred_medv<-test_predict/i;
  # calculate the sum of squared residuals for training and validation sets, SSEs
  test_sse<-cbind(test_sse,mean((Boston.scaletest$medv-Boston.scaletest$pred_medv)^2));
  train_sse<-cbind(train_sse,mean((Boston.scaletrain$medv-Boston.scaletrain$pred_medv)^2));
  train_predict<-0;
  test_predict<-0;
}
neustat<-data.frame(test_sse,train_sse,layer_size)
neural_res = neustat[neustat$test_sse > 0,]

View(neural_res)
library(ggplot2)
library(stats)
p <- melt(neural_res, id.vars="layer_size")

ggplot(p, aes(layer_size,value, col=variable)) + geom_line()



