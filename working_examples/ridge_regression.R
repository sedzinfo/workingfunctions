##########################################################################################
# 
##########################################################################################
set.seed(123)
library(glmnet)
library(dplyr)
library(psych)
##########################################################################################
# RIDGE
##########################################################################################
# Center y,X will be standardized in the modelling function
y<-as.matrix(scale(mtcars$mpg,center=TRUE,scale=FALSE))
X<-as.matrix(mtcars[,!names(mtcars)%in%"mpg"])

# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas_to_try<-10^seq(-3,5,length.out=100)
# Setting alpha=0 implements ridge regression
ridge_cv<-cv.glmnet(X,y,alpha=0,lambda=lambdas_to_try,standardize=TRUE,nfolds=10)
plot(ridge_cv)
lambda_cv<-ridge_cv$lambda.min
# Fit final model,get its sum of squared residuals and multiple R-squared
model_cv<-glmnet(X,y,alpha=0,lambda=lambda_cv,standardize=TRUE)
y_hat_cv<-predict(model_cv,X)
ssr_cv<-t(y-y_hat_cv) %*% (y-y_hat_cv)
rsq_ridge_cv<-cor(y,y_hat_cv)^2

# Use information criteria to select lambda -----------------------------------
X_scaled<-scale(X)
aic<-bic<-c()
for (lambda in seq(lambdas_to_try)) {
  model<-glmnet(X,y,alpha=0,lambda=lambdas_to_try[lambda],standardize=TRUE)
  betas<-as.vector((as.matrix(coef(model))[-1,]))
  resid<-y-(X_scaled %*% betas)
  # Compute hat-matrix and degrees of freedom
  ld<-lambdas_to_try[lambda]*diag(ncol(X_scaled))
  H<-X_scaled %*% solve(t(X_scaled) %*% X_scaled+ld) %*% t(X_scaled)
  df<-tr(H)
  aic[lambda]<-nrow(X_scaled)*log(t(resid) %*% resid)+2*df
  bic[lambda]<-nrow(X_scaled)*log(t(resid) %*% resid)+2*df*log(nrow(X_scaled))
}

# Plot information criteria against tried values of lambdas
plot(log(lambdas_to_try),aic,col="orange",type="l",ylim=c(190,260),ylab="Information Criterion")
lines(log(lambdas_to_try),bic,col="skyblue3")
legend("bottomright",lwd=1,col=c("orange","skyblue3"),legend=c("AIC","BIC"))
# Optimal lambdas according to both criteria
lambda_aic<-lambdas_to_try[which.min(aic)]
lambda_bic<-lambdas_to_try[which.min(bic)]
# Fit final models,get their sum of squared residuals and multiple R-squared
model_aic<-glmnet(X,y,alpha=0,lambda=lambda_aic,standardize=TRUE)
y_hat_aic<-predict(model_aic,X)
ssr_aic<-t(y-y_hat_aic) %*% (y-y_hat_aic)
rsq_ridge_aic<-cor(y,y_hat_aic)^2

model_bic<-glmnet(X,y,alpha=0,lambda=lambda_bic,standardize=TRUE)
y_hat_bic<-predict(model_bic,X)
ssr_bic<-t(y-y_hat_bic) %*% (y-y_hat_bic)
rsq_ridge_bic<-cor(y,y_hat_bic)^2

# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables,for different lambdas. The higher the lambda,the more the coefficients are shrinked towards zero.
plot(glmnet(X,y,alpha=0,lambda=lambdas_to_try,standardize=FALSE),xvar="lambda")
legend("bottomright",lwd=1,col=1:6,legend=colnames(X),cex=.7)

# Optimal lambdas according to both criteria
lambda_aic<-lambdas_to_try[which.min(aic)]
lambda_bic<-lambdas_to_try[which.min(bic)]

# Fit final models,get their sum of squared residuals and multiple R-squared
model_aic<-glmnet(X,y,alpha=0,lambda=lambda_aic,standardize=TRUE)
y_hat_aic<-predict(model_aic,X)
ssr_aic<-t(y-y_hat_aic) %*% (y-y_hat_aic)
rsq_ridge_aic<-cor(y,y_hat_aic)^2

model_bic<-glmnet(X,y,alpha=0,lambda=lambda_bic,standardize=TRUE)
y_hat_bic<-predict(model_bic,X)
ssr_bic<-t(y-y_hat_bic) %*% (y-y_hat_bic)
rsq_ridge_bic<-cor(y,y_hat_bic)^2

# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables,for different lambdas. The higher the lambda,the more the coefficients are shrinked towards zero.
plot(glmnet(X,y,alpha=0,lambda=lambdas_to_try,standardize=FALSE),xvar="lambda")
legend("bottomright",lwd=1,col=1:6,legend=colnames(X),cex=.7)
##########################################################################################
# HETEROSCEDASTIC RIDGE
##########################################################################################
# Calculate the weights from univariate regressions
weights<-sapply(seq(ncol(X)),function(predictor) {
  uni_model<-lm(y~X[,predictor])
  coeff_variance<-summary(uni_model)$coefficients[2,2]^2
})

# Heteroskedastic Ridge Regression loss function-to be minimized
hridge_loss<-function(betas) {
  sum((y-X %*% betas)^2)+lambda*sum(weights*betas^2)
}

# Heteroskedastic Ridge Regression function
hridge<-function(y,X,lambda,weights) {
  # Use regular ridge regression coefficient as initial values for optimization
  model_init<-glmnet(X,y,alpha=0,lambda=lambda,standardize=FALSE)
  betas_init<-as.vector(model_init$beta)
  # Solve optimization problem to get coefficients
  coef<-optim(betas_init,hridge_loss)$par
  # Compute fitted values and multiple R-squared
  fitted<-X %*% coef
  rsq<-cor(y,fitted)^2
  names(coef)<-colnames(X)
  output<-list("coef"=coef,"fitted"=fitted,"rsq"=rsq)
  return(output)
}

# Fit model to the data for lambda=0.001
hridge_model<-hridge(y,X,lambda=0.001,weights=weights)
rsq_hridge_0001<-hridge_model$rsq

# Cross-validation or AIC/BIC can be employed to select some better lambda! You can find some useful functions for this at https://github.com/MichalOleszak/momisc/blob/master/R/hridge.R
##########################################################################################
# LASSO
##########################################################################################
# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas_to_try<-10^seq(-3,5,length.out=100)
# Setting alpha=1 implements lasso regression
lasso_cv<-cv.glmnet(X,y,alpha=1,lambda=lambdas_to_try,standardize=TRUE,nfolds=10)
# Plot cross-validation results
plot(lasso_cv)
# Best cross-validated lambda
lambda_cv<-lasso_cv$lambda.min
# Fit final model,get its sum of squared residuals and multiple R-squared
model_cv<-glmnet(X,y,alpha=1,lambda=lambda_cv,standardize=TRUE)
y_hat_cv<-predict(model_cv,X)
ssr_cv<-t(y-y_hat_cv) %*% (y-y_hat_cv)
rsq_lasso_cv<-cor(y,y_hat_cv)^2
# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables,for different lambdas. The higher the lambda,the more the coefficients are shrinked towards zero.
res<-glmnet(X,y,alpha=1,lambda=lambdas_to_try,standardize=FALSE)
plot(res,xvar="lambda")
legend("bottomright",lwd=1,col=1:6,legend=colnames(X),cex=.7)
rsq<-cbind("R-squared"=c(rsq_ridge_cv,rsq_ridge_aic,rsq_ridge_bic,rsq_hridge_0001,rsq_lasso_cv))
rownames(rsq)<-c("ridge cross-validated","ridge AIC","ridge BIC","hridge 0.001","lasso cross_validated")
print(rsq)
##########################################################################################
# ELASTIC NET
##########################################################################################
library(caret)
# Set training control
train_control<-trainControl(method="repeatedcv",number=5,repeats=5,search="random",verboseIter=TRUE)

# Train the model
elastic_net_model<-train(mpg~.,data=cbind(y,X),method="glmnet",preProcess=c("center","scale"),tuneLength=25,trControl=train_control)

# Check multiple R-squared
y_hat_enet<-predict(elastic_net_model,X)
rsq_enet<-cor(y,y_hat_enet)^2





