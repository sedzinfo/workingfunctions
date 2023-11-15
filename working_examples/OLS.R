##########################################################################################
# LOAD
##########################################################################################
library(psycholatefunctions)
options(scippen=999)
options(digits=4)
##########################################################################################
# GENERATE CORRELATION DATA
##########################################################################################
df1<-data.frame(matrix(.9,ncol=5,nrow=5))
correlation_martix<-as.matrix(df1)
diag(correlation_martix)<-1
res<-round(generate_correlation_matrix(correlation_martix,nrows=1000)+10,2)
plot(res)
##########################################################################################
# SIMPLE OLS
##########################################################################################
regression_result<-lm(X5~X1,data=res) # this is the model done by the lm package
coef(regression_result) # these are the coefficients of the model from the lm package
cov1<-sum(res$X5-mean(res$X5))*sum(res$X1-mean(res$X1))
var1<-sum(res$X1-mean(res$X1))
b1<-cov(res$X1,res$X5)/var(res$X1)
a1<-mean(res$X5)-b1*mean(res$X1)
rbind(data.frame("(Intercept)"=a1,X1=b1,check.names=FALSE),coef(regression_result))
##########################################################################################
# MULTIPLE OLS
##########################################################################################
regression_result<-lm(X5~X1+X2+X3+X4,data=res) # This is the model done by the lm package
data.frame(beta=coef(regression_result)) # these are the coefficients of the model from the lm package
# Bellow is the matrix algebra for the whole thing
y<-res$X5
y_matrix<-matrix(res$X5)
x_matrix<-as.matrix(res[,1:4])
m1<-solve(t(x_matrix)%*%x_matrix)%*%t(x_matrix)%*%y_matrix
# Compare coefficients of model and matrix algebra
data.frame(beta=coef(regression_result))
m1
# Estimate alpha coefficient
mean(y_matrix)-m1%*%colMeans(x_matrix)

