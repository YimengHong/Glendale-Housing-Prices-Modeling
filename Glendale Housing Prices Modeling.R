Glendale=read.table("C:/Users/yimen/OneDrive/Documents/GlendaleHousing.txt",header=TRUE);
Glendale=Glendale[,-c(1,12)];
dim(Glendale) # [1] 173  12

### Use the best subset selection and the lasso to see, whether all the variables are necessary
### to explain the house's \Transfervalue". 
install.packages("leaps");
library(leaps);
regfit.full=regsubsets(Transfervalue ~.,Glendale);
summary(regfit.full);

install.packages("glmnet");
library(glmnet);
x=model.matrix(Transfervalue ~., Glendale)[,-1];
y=Glendale$Transfervalue;
set.seed(1);
lasso.mod=cv.glmnet(x,y,alpha=1)
lasso.mod$lambda.min  # [1] 567.78
lasso.mod=glmnet(x,y,alpha=1,lambda=567.78)
coef(lasso.mod)

### Estimate the decision boundary of the training set, respectively using linear, polynomial and radial kernels. 
x=Glendale[,c(1,4)]
y=Glendale[,7]
x_train=x[1:150,]
y_train=y[1:150]
plot(x_train,col=3-y_train)

dat=data.frame(x=x_train, y=as.factor(y_train));
set.seed(1);
tune.out=tune(svm,y ~.,data=dat,kernel ="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)));
summary(tune.out);

bestmod =tune.out$best.model;
summary(bestmod);

best.tune(method = svm, train.x = y ~ ., data = dat, ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), kernel = "linear")

svmfit=svm(y_train ~.,data=dat,kernel="linear",cost=1,scale=FALSE);
plot(svmfit,dat);# We see linear kernel fails.

x=Glendale[c(1,4)]
x_test=x[151:173,]
y=Glendale[,7]
y_test=y[151:173]
testdat=data.frame(x=x_test,y=as.factor(y_test));
ypred = predict(bestmod,testdat);
table(predict =ypred , truth= testdat$y);# We see 8/23 of the predictions are wrong.

# Now we try polynomial kernel.
x=Glendale[c(1,4)];
y=Glendale[,7];
x_train=x[1:150,];
y_train=y[1:150]
plot(x_train,col=3-y_train);
dat=data.frame(x=x_train, y=as.factor(y_train));
set.seed(1);
tune.out=tune(svm,y ~.,data=dat,kernel ="polynomial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)));
summary(tune.out);

bestmod =tune.out$best.model;
summary(bestmod);
svmfit=svm(y_train ~.,data=dat,kernel="polynomial",cost=0.1,scale=FALSE);
plot(svmfit , dat);
ypred=predict(bestmod,testdat);
table(predict =ypred , truth= testdat$y);# The prediction error rate is 8/23.

# Now we try radial kernel.
set.seed(1);
tune.out=tune(svm,y ~.,data=dat,kernel ="radial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)));
summary(tune.out);

bestmod =tune.out$best.model;
summary(bestmod);
svmfit=svm(y_train ~.,data=dat,kernel="radial",cost=0.1,scale=FALSE);
plot(svmfit , dat);
ypred=predict(bestmod,testdat);
table(predict =ypred , truth= testdat$y);# The prediction error rate is 6/23, better








