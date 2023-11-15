# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")
dat<-read.csv("/home/dimitrios/Dropbox (Psycholate)/dimitrios/Working/R/data/kaggle/VietnamConflict.csv",stringsAsFactors=FALSE)
check(dat)
dat$BIRTH_YEAR<-NULL

for (i in names(dat)){
  result<-data.frame(table(dat[,i]))
  names(result)<-c(i,"Frequency")
  if(nrow(result)<50){
    print(ggplot(result,aes(x=result[,i],y=Frequency))+geom_bar(stat="identity")+theme_bw()+labs(title="Fatalities",x=i)+coord_flip())
  }
}
for (i in names(dat)){
  result<-data.frame(table(dat[,i]))
  names(result)<-c(i,"Frequency")
  if(nrow(result)>50){
    z<-1
    sequence<-seq(from=1,to=nrow(result),by=100)
    for (z in 1:length(sequence)){
      if(z+1<=length(sequence)){
        temp<-result[sequence[z]:sequence[z+1],]
        print(ggplot(temp,aes(x=temp[,i],y=Frequency))+geom_bar(stat="identity")+theme_bw(base_size=5)+labs(title="Fatalities",x=i)+coord_flip())
      }
    }
  }
}
