#############################################################################################################################################################################################################
# 
#############################################################################################################################################################################################################
library(semPlot)
source("http://faculty.missouri.edu/huangf/data/mcfa/mcfa.R")
raw<-read.csv("http://faculty.missouri.edu/huangf/data/mcfa/raw.csv")
x<-mcfa.input("sid",raw)
#############################################################################################################################################################################################################
# INITIAL MODEL
#############################################################################################################################################################################################################
onefactor<-'f1=~x1+x2+x3+x4+x5+x6'
twofactor<-'f1=~x1+x2+x4
            f2=~x3+x5+x6'

results1<-lavaan::cfa(onefactor,sample.cov=x$pw.cov,sample.nobs=x$n-x$G)
semPlot::semPaths(results1)
lavaan::summary(results1,fit.measures=T,standardized=T)

results2<-lavaan::cfa(twofactor,sample.cov=x$pw.cov,sample.nobs=x$n-x$G)
semPlot::semPaths(results2,curvePivot=FALSE,layout="tree",structural=FALSE,style="mx",title=FALSE)
lavaan::summary(results2,fit.measures=T,standardized=T)
#############################################################################################################################################################################################################
# NULL MODEL
#############################################################################################################################################################################################################
combined.cov<-list(within=x$pw.cov,between=x$b.cov)
combined.n<-list(within=x$n-x$G,between=x$G)

nullmodel<-"f1=~x1+c(a,a)*x2+c(b,b)*x4
f2=~x3+c(c,c)*x5+c(d,d)*x6
x1~~c(e,e)*x1
x2~~c(f,f)*x2
x3~~c(g,g)*x3
x4~~c(h,h)*x4
x5~~c(i,i)*x5
x6~~c(j,j)*x6
f1~~c(k,k)*f1
f2~~c(l,l)*f2
f1~~c(m,m)*f2"

results3<-lavaan::cfa(nullmodel,sample.cov=combined.cov,sample.nobs=combined.n)
lavaan::summary(results3,fit.measures=T,standardized=T)
#############################################################################################################################################################################################################
# INDEPENDENCE MODEL
#############################################################################################################################################################################################################
independence<-"f1=~x1+c(a,a)*x2+c(b,b)*x4
f2=~x3+c(c,c)*x5+c(d,d)*x6
x1~~c(e,e)*x1
x2~~c(f,f)*x2
x3~~c(g,g)*x3
x4~~c(h,h)*x4
x5~~c(i,i)*x5
x6~~c(j,j)*x6
f1~~c(k,k)*f1
f2~~c(l,l)*f2
f1~~c(m,m)*f2
x1b=~c(0,3.91)*x1
x1b~~c(0,NA)*x1b
x2b=~c(0,3.91)*x2
x2b~~c(0,NA)*x2b
x3b=~c(0,3.91)*x3
x3b~~c(0,NA)*x3b
x4b=~c(0,3.91)*x4
x4b~~c(0,NA)*x4b
x5b=~c(0,3.91)*x5
x5b~~c(0,NA)*x5b
x6b=~c(0,3.91)*x6
x6b~~c(0,NA)*x6b"

results4<-lavaan::cfa(independence,sample.cov=combined.cov,sample.nobs=combined.n,orthogonal=T)
lavaan::summary(results4,fit.measures=T)
#############################################################################################################################################################################################################
# SATURATED MODEL
#############################################################################################################################################################################################################
saturated<-'f1=~x1+c(a,a)*x2+c(b,b)*x4
f2=~x3+c(c,c)*x5+c(d,d)*x6
x1~~c(e,e)*x1
x2~~c(f,f)*x2
x3~~c(g,g)*x3
x4~~c(h,h)*x4
x5~~c(i,i)*x5
x6~~c(j,j)*x6
f1~~c(k,k)*f1
f2~~c(l,l)*f2
f1~~c(m,m)*f2
x1b=~c(0,3.91)*x1
x1b~~c(0,NA)*x1b
x2b=~c(0,3.91)*x2
x2b~~c(0,NA)*x2b
x3b=~c(0,3.91)*x3
x3b~~c(0,NA)*x3b
x4b=~c(0,3.91)*x4
x4b~~c(0,NA)*x4b
x5b=~c(0,3.91)*x5
x5b~~c(0,NA)*x5b
x6b=~c(0,3.91)*x6
x6b~~c(0,NA)*x6b
x1b~~c(0,NA)*x2b+c(0,NA)*x3b+c(0,NA)*x4b+c(0,NA)*x5b+c(0,NA)*x6b
x2b~~c(0,NA)*x3b+c(0,NA)*x4b+c(0,NA)*x5b+c(0,NA)*x6b
x3b~~c(0,NA)*x4b+c(0,NA)*x5b+c(0,NA)*x6b
x4b~~c(0,NA)*x5b+c(0,NA)*x6b
x5b~~c(0,NA)*x6b #fully saturated'

results5<-lavaan::cfa(saturated,sample.cov=combined.cov,sample.nobs=combined.n,orthogonal=T)
lavaan::summary(results5,fit.measures=T,standardized=T)
#############################################################################################################################################################################################################
# HYPOTHESIZED MODEL
#############################################################################################################################################################################################################
level2.1factor<-'f1=~x1+c(a,a)*x2+c(b,b)*x4
f2=~x3+c(c,c)*x5+c(d,d)*x6
x1~~c(e,e)*x1
x2~~c(f,f)*x2
x3~~c(g,g)*x3
x4~~c(h,h)*x4
x5~~c(i,i)*x5
x6~~c(j,j)*x6
f1~~c(k,k)*f1
f2~~c(l,l)*f2
f1~~c(m,m)*f2
x1b=~c(0,3.91)*x1
x1b~~c(0,NA)*x1b
x2b=~c(0,3.91)*x2
x2b~~c(0,NA)*x2b
x3b=~c(0,3.91)*x3
x3b~~c(0,NA)*x3b
x4b=~c(0,3.91)*x4
x4b~~c(0,NA)*x4b
x5b=~c(0,3.91)*x5
x5b~~c(0,NA)*x5b
x6b=~c(0,3.91)*x6
x6b~~c(0,NA)*x6b
bf1=~c(0,1)*x1b+c(0,NA)*x2b+c(0,NA)*x3b+c(0,NA)*x4b +c(0,NA)*x5b+c(0,NA)*x6b
bf1~~c(0,NA)*bf1+c(0,0)*f1+c(0,0)*f2'

results6<-lavaan::cfa(level2.1factor,sample.cov=combined.cov,sample.nobs=combined.n,orthogonal=T)
lavaan::summary(results6,fit.measures=T,standardized=T)
#############################################################################################################################################################################################################
# HYPOTHESIZED MODEL
#############################################################################################################################################################################################################
level2.2factors<-'f1=~x1+c(a,a)*x2+c(b,b)*x4
f2=~x3+c(c,c)*x5+c(d,d)*x6
x1~~c(e,e)*x1
x2~~c(f,f)*x2
x3~~c(g,g)*x3
x4~~c(h,h)*x4
x5~~c(i,i)*x5
x6~~c(j,j)*x6
f1~~c(k,k)*f1
f2~~c(l,l)*f2
f1~~c(m,m)*f2
x1b=~c(0,3.91)*x1
x1b~~c(0,NA)*x1b
x2b=~c(0,3.91)*x2
x2b~~c(0,NA)*x2b
x3b=~c(0,3.91)*x3
x3b~~c(0,NA)*x3b
x4b=~c(0,3.91)*x4
x4b~~c(0,NA)*x4b
x5b=~c(0,3.91)*x5
x5b~~c(0,NA)*x5b
x6b=~c(0,3.91)*x6
x6b~~c(0,NA)*x6b
bf1=~c(0,1)*x1b+c(0,NA)*x2b+c(0,NA)*x4b
bf2=~c(0,1)*x3b+c(0,NA)*x5b+c(0,NA)*x6b
bf1~~c(0,NA)*bf1+c(0,0)*f1+c(0,0)*f2+c(0,NA)*bf2
bf2~~c(0,NA)*bf2+c(0,0)*f1+c(0,0)*f2'

results7<-lavaan::cfa(level2.2factors,sample.cov=combined.cov,sample.nobs=combined.n,orthogonal=T)
lavaan::summary(results7,fit.measures=T,standardized=T)