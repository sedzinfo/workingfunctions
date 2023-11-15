# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")
dat<-read.csv("/home/dimitrios/Dropbox (Psycholate)/dimitrios/Working/R/data/kaggle/GlobalTemperatures.csv")
check(dat)

Land.Average.Temperature<-ts(dat$LandAverageTemperature,frequency=12,start=c(1900,1))
plot(Land.Average.Temperature)
stl.Land.Average.Temperature<-stl(Land.Average.Temperature,s.window=12)
autoplot(stl.Land.Average.Temperature)+theme_gray(base_size=15)+labs(title='Seasonal decomposition of average land temperature')
time_series_plot(Land.Average.Temperature,title='Average land temperature')


ggplot(dat,aes(x=dt,y=LandAverageTemperature,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
ggplot(dat,aes(x=dt,y=LandAverageTemperatureUncertainty,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
ggplot(dat,aes(x=dt,y=LandMaxTemperature,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
ggplot(dat,aes(x=dt,y=LandMaxTemperatureUncertainty,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
ggplot(dat,aes(x=dt,y=LandMinTemperature,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
ggplot(dat,aes(x=dt,y=LandMinTemperatureUncertainty,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
ggplot(dat,aes(x=dt,y=LandAndOceanAverageTemperature,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
ggplot(dat,aes(x=dt,y=LandAndOceanAverageTemperatureUncertainty,group=1))+geom_point(size=1)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+geom_line()
