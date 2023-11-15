library(ggplot2)
mydata<-read.csv("R/Data/Delimited/TZOKER.csv")

numbers<-c(mydata$NUMBER1,mydata$NUMBER2,mydata$NUMBER3,mydata$NUMBER4,mydata$NUMBER5)
joker<-mydata$JOKER

par(mfrow=c(2,2),ann=TRUE,bg='white',mai=c(.9,.9,.9,.9),adj=0)
hist(numbers,main="",xlab="")
hist(numbers,prob=TRUE,main="",xlab="")
lines(density(numbers,kernel="gaussian"))
boxplot(numbers,main="")
qqnorm(numbers,main="")
title(main=paste("ΑΡΙΘΜΟΙ"),sub=paste("Observations",length(numbers)),outer=TRUE,line=-1)

par(mfrow=c(2,2),ann=TRUE,bg='white',mai=c(.9,.9,.9,.9),adj=0)
hist(joker,main="",xlab="")
hist(joker,prob=TRUE,main="",xlab="")
lines(density(joker,kernel="gaussian"))
boxplot(joker,main="")
qqnorm(joker,main="")
title(main=paste("ΤΖΟΚΕΡ"),sub=paste("Observations",length(joker)),outer=TRUE,line=-1)

par(mfrow=c(1,1),ann=TRUE,bg='white',mai=c(2,2,2,2),adj=0)
hist(numbers,main="",breaks=c(0:45))
##########################################################################################################################################################################################
freq<-data.frame(table(numbers))
y1997<-chisq.test(freq$Freq)

freq<-data.frame(table(mydata$JOKER))
names(freq)<-c("numbers", "Freq")

p1<-ggplot(freq, aes(x=reorder(numbers,-Freq),Freq))
p2<-p1+geom_bar(stat="identity")+labs(x="ΝΟΥΜΕΡΟ", y="ΣΥΧΝΟΤΗΤΑ ΕΜΦΑΝΙΣΗΣ", title="ΠΕΝΤΑΔΑ 16-11-1997 ΕΩΣ 27-12-2015")
finalplotp<-p2+geom_hline(yintercept=0)+themeplot
finalplotp

p1<-ggplot(freq, aes(x=reorder(numbers, -Freq), Freq))
p2<-p1+geom_bar(stat="identity", width=1, fill=colourcode[[1]])+labs(x="ΝΟΥΜΕΡΟ", y="ΣΥΧΝΟΤΗΤΑ ΕΜΦΑΝΙΣΗΣ", title="ΤΖΟΚΕΡ 16-11-1997 ΕΩΣ 27-12-2015")
finalplotj<-p2+geom_hline(yintercept=0)+themeplot
finalplotj

multiplot(finalplotp, finalplotj, cols=1)
##########################################################################################################################################################################################
data<-mydata[mydata$YEAR %in% "2015",]
numbers<-c(data$NUMBER1,data$NUMBER2,data$NUMBER3,data$NUMBER4,data$NUMBER5)

freq<-data.frame(table(numbers))
y2015<-chisq.test(freq$Freq)

freq<-data.frame(table(data$JOKER))
names(freq)<-c("numbers", "Freq")

p1<-ggplot(freq, aes(x=reorder(numbers, -Freq), Freq))
p2<-p1+geom_bar(stat="identity", width=1, fill=1)+labs(x="ΝΟΥΜΕΡΟ", y="ΣΥΧΝΟΤΗΤΑ ΕΜΦΑΝΙΣΗΣ", title="ΠΕΝΤΑΔΑ 1-1-2015 ΕΩΣ 27-12-2015")
finalplotp<-p2+geom_hline(yintercept=0, size=1)+themeplot
finalplotp

p1<-ggplot(freq, aes(x=reorder(numbers, -Freq), Freq))
p2<-p1+geom_bar(stat="identity", width=size$barwidth, fill=colourcode[[1]])+labs(x="ΝΟΥΜΕΡΟ", y="ΣΥΧΝΟΤΗΤΑ ΕΜΦΑΝΙΣΗΣ", title="ΤΖΟΚΕΡ 1-1-2015 ΕΩΣ 27-12-2015")
finalplotj<-p2+geom_hline(yintercept=0, size=size$linesize)+themeplot
finalplotj

multiplot(finalplotp, finalplotj, cols=1)
##########################################################################################################################################################################################
library(gtools)
numbers<-seq(1:45)
joker<-seq(1:20)

permutations(n=45, r=5, v=numbers, repeats.allowed=F)
nrow(permutations(n=45, r=5, v=numbers, repeats.allowed=F))
##########################################################################################################################################################################################
# RANDOM NUMBERS
##########################################################################################################################################################################################
for (i in 1:5) {
  cat(sort(sample(1:45,5,replace=F)),"JOKER:",sort(sample(1:20,1,replace=F)),"\n")
}

cat(sort(sample(1:45,5,replace=F)),"JOKER:",sort(sample(1:20,1,replace=F)),"\n")