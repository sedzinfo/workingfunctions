library(RHRV)
rm(hrv.data)
hrv.data=CreateHRVData()
hrv.data=SetVerbose(hrv.data,TRUE)
hrv.data=LoadBeatAscii(hrv.data,"/home/dimitrios/Dropbox (Psycholate)/dimitrios/Working/PROJECTS/juul/data/beats2",RecordPath=".")
hrv.data=BuildNIHR(hrv.data)
hrv.data=FilterNIHR(hrv.data)
#PlotNIHR(hrv.data)
#hrv.data=EditNIHR(hrv.data)
hrv.data=InterpolateNIHR(hrv.data,freqhr=1,method="linear",verbose=TRUE)
hrv.data=CreateTimeAnalysis(hrv.data,size=100,interval=1)
hrv.data=CreateFreqAnalysis(hrv.data,verbose=TRUE)
hrv.data=CalculatePowerBand(hrv.data,indexFreqAnalysis=1,type="wavelet",wavelet="la8")
CalculateEmbeddingDim(hrv.data,numberPoints=5000,timeLag=1000,maxEmbeddingDim=15,threshold=0.95,maxRelativeChange=0.01,doPlot=TRUE)
#Plotting Fourier analysis
PlotPowerBand(hrv.data,indexFreqAnalysis=1,ymax=200,ymaxratio=1.7)
spectrogram=data.frame(PlotSpectrogram(HRVData=hrv.data,size=100,shift=20))

i<-1
HRV<-data.frame("TYPE"=rep("HRV",length(hrv.data$FreqAnalysis[[i]]$HRV)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$HRV)),"DATA"=hrv.data$FreqAnalysis[[i]]$HRV)
ULF<-data.frame("TYPE"=rep("ULF",length(hrv.data$FreqAnalysis[[i]]$ULF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$ULF)),"DATA"=hrv.data$FreqAnalysis[[i]]$ULF)
VLF<-data.frame("TYPE"=rep("VLF",length(hrv.data$FreqAnalysis[[i]]$VLF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$VLF)),"DATA"=hrv.data$FreqAnalysis[[i]]$VLF)
LF<-data.frame("TYPE"=rep("LF",length(hrv.data$FreqAnalysis[[i]]$LF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$LF)),"DATA"=hrv.data$FreqAnalysis[[i]]$LF)
HF<-data.frame("TYPE"=rep("HF",length(hrv.data$FreqAnalysis[[i]]$HF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$HF)),"DATA"=hrv.data$FreqAnalysis[[i]]$HF)
LFHF<-data.frame("TYPE"=rep("LFHF",length(hrv.data$FreqAnalysis[[i]]$LFHF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$LFHF)),"DATA"=hrv.data$FreqAnalysis[[i]]$LFHF)
HRD<-rbind(HRV,ULF,VLF,LF,HF,LFHF)

size<-hrv.data$FreqAnalysis[[i]]$size
shift<-hrv.data$FreqAnalysis[[i]]$shift
sizesp<-hrv.data$FreqAnalysis[[i]]$sizesp
type<-hrv.data$FreqAnalysis[[i]]$type
ULFmin<-hrv.data$FreqAnalysis[[i]]$ULFmin
ULFmax<-hrv.data$FreqAnalysis[[i]]$ULFmax
VLFmin<-hrv.data$FreqAnalysis[[i]]$VLFmin
VLFmax<-hrv.data$FreqAnalysis[[i]]$VLFmax
LFmin<-hrv.data$FreqAnalysis[[i]]$LFmin
LFmax<-hrv.data$FreqAnalysis[[i]]$LFmax
HFmin<-hrv.data$FreqAnalysis[[i]]$HFmin
HFmax<-hrv.data$FreqAnalysis[[i]]$HFmax

ggplot(HRD,aes(y=HRD$DATA,x=HRD$TIME,colour=TYPE))+geom_line()+labs(x="",y="",title="Power")+facet_wrap(~TYPE)
ggplot(HRD,aes(y=HRD$DATA,x=HRD$TIME,colour=HRD$TYPE))+geom_line()+labs(x="",y="",title="Power")
#############################################################################################################################################################################################################
hrv.data=CalculateApEn(hrv.data)
hrv.data=CalculateCorrDim(hrv.data)
hrv.data=CalculateDFA(hrv.data)
hrv.data=CalculateEmbeddingDim(hrv.data)
hrv.data=CalculateFracDim(hrv.data)
hrv.data=CalculateInfDim(hrv.data)
hrv.data=CalculateMaxLyapunov(hrv.data)
hrv.data=CalculatePowerBand(hrv.data)
hrv.data=CalculateRfromCorrelation(hrv.data)
hrv.data=CalculateSampleEntropy(hrv.data)
hrv.data=CalculateSpectrogram(hrv.data)
hrv.data=CalculateTimeLag(hrv.data)
#############################################################################################################################################################################################################
rm(hrv.data)
hrv.data=CreateHRVData()
hrv.data=SetVerbose(hrv.data,TRUE)
hrv.data=LoadBeatAscii(hrv.data,"/home/dimitrios/Dropbox (Psycholate)/dimitrios/Working/PROJECTS/juul/data/beats",RecordPath=".")
hrv.data=BuildNIHR(hrv.data)
PlotNIHR(hrv.data)
hrv.data=FilterNIHR(hrv.data)
PlotNIHR(hrv.data)
#hrv.data=EditNIHR(hrv.data)
hrv.data=AddEpisodes(hrv.data,InitTimes=c(200,2000),Tags=c("Not drug","Drug"),Durations=c(1400,2500),Values=c(0,0))
PlotNIHR(hrv.data,Tag="all")
hrv.data=InterpolateNIHR (hrv.data,freqhr=4)
hrv.data=CreateFreqAnalysis(hrv.data)
hrv.data=CalculatePowerBand( hrv.data ,indexFreqAnalysis= 1,type="wavelet",wavelet="la8",bandtolerance=0.01,relative=FALSE,ULFmin=0,ULFmax=0.03,VLFmin=0.03,VLFmax=0.05,LFmin=0.05,LFmax=0.15,HFmin=0.15,HFmax=0.4 )
PlotPowerBand(hrv.data,indexFreqAnalysis=1,ymax=1000,ymaxratio=100)
PlotPowerBand(hrv.data,indexFreqAnalysis=1,ymax=300,ymaxratio=50,Tag="all")
splitting.data=SplitPowerBandByEpisodes(hrv.data,indexFreqAnalysis=1,Tag=c("Drug"))

cat("ULF power during drug administration: ",mean(splitting.data$InEpisodes$ULF),"\n")
cat("ULF power before drug administration: ",mean(splitting.data$OutEpisodes$ULF),"\n")
cat("VLF power during drug administration: ",mean(splitting.data$InEpisodes$VLF),"\n")
cat("VLF power before drug administration: ",mean(splitting.data$OutEpisodes$VLF),"\n")
cat("LF power during drug administration: ",mean(splitting.data$InEpisodes$LF),"\n")
cat("LF power before drug administration: ",mean(splitting.data$OutEpisodes$LF),"\n")
cat("HF power during drug administration: ",mean(splitting.data$InEpisodes$HF),"\n")
cat("HF power before drug administration: ",mean(splitting.data$OutEpisodes$HF),"\n")
#############################################################################################################################################################################################################
rm(hrv.data)
hrv.data=CreateHRVData() 
hrv.data=SetVerbose(hrv.data,TRUE)
hrv.data=LoadBeatAscii(hrv.data,RecordName="/home/dimitrios/Dropbox (Psycholate)/dimitrios/Working/PROJECTS/juul/data/beats",RecordPath=".")
#we add the info about the episodes
hrv.data=AddEpisodes(hrv.data,InitTimes=c(700,2000,5000),Tags=c("Before","During","After"),Durations=c(900,2000,600),Values=c(0,0,0))
hrv.data=BuildNIHR(hrv.data)
hrv.data=FilterNIHR(hrv.data)
# plot all tags
PlotNIHR(hrv.data,Tag="all")
hrv.data=InterpolateNIHR(hrv.data,freqhr=4)
# Plot only the "After" episodic information
PlotHR(hrv.data ,Tag=c("After"))

#Perform frequency analysys
hrv.data=CreateFreqAnalysis(hrv.data)
hrv.data=CalculatePowerBand( hrv.data ,indexFreqAnalysis= 1,type="wavelet",wavelet="la8",bandtolerance=0.01,relative=FALSE)
# plot episodic information
PlotPowerBand(hrv.data,indexFreqAnalysis=1,ymax=5000,ymaxratio=50,Tag="all")
PlotPowerBand(hrv.data,indexFreqAnalysis=1,ymax=5000,ymaxratio=50,Tag="After")

#We divide the heart rate by tag "before"
splitting.data=SplitHRbyEpisodes(hrv.data,Tag=c("Before"))
cat("Apnea mean: ",mean(splitting.data$InEpisodes),"\n")
cat("Apnea mean: ",mean(splitting.data$OutEpisodes),"\n")

#We divide the heart rate by tag "During"
splitting.data=SplitHRbyEpisodes(hrv.data,Tag=c("During"))
cat("Apnea mean: ",mean(splitting.data$InEpisodes),"\n")

#We divide the spectral analysis for tag "Before"
splitting.data=SplitPowerBandByEpisodes(hrv.data,indexFreqAnalysis=1,Tag=c("Before"))

cat("Apnea mean: ",mean(splitting.data$InEpisodes$ULF),"\n")

#We divide the spectral analysis for tag "After"
splitting.data=SplitPowerBandByEpisodes(hrv.data,indexFreqAnalysis=1,Tag=c("After"))
cat("Normal mean: ",mean(splitting.data$OutEpisodes$ULF),"\n")
#############################################################################################################################################################################################################
hrv.data=CreateHRVData()
hrv.data=SetVerbose(hrv.data,TRUE)
hrv.data=LoadBeatAscii(hrv.data,RecordName="nonlinearHB.beats",RecordPath=".")
hrv.data=BuildNIHR(hrv.data)

#We create the data structure to store the nonlinear analysis
hrv.data=CreateNonLinearAnalysis(hrv.data)
#We check that the RR series is nonlinear
hrv.data=NonlinearityTests(hrv.data)
hrv.data=SurrogateTest(hrv.data,significance=0.05,useFunction=timeAsymmetry2,tau=4,doPlot=TRUE)
#We estimate the time lag to be used in phase space reconstruction
kTimeLag=CalculateTimeLag(hrv.data,method="first.e.decay",lagMax=100,doPlot=TRUE)
#We estimate the embedding dimension of the phase space reconstruction
kEmbeddingDim=CalculateEmbeddingDim(hrv.data,numberPoints=10000,timeLag=kTimeLag,maxEmbeddingDim=15)
kEmbeddingDim
#We calculate the correlation dimension
hrv.data=CalculateCorrDim(hrv.data,indexNonLinearAnalysis=1,minEmbeddingDim=kEmbeddingDim-1,maxEmbeddingDim=kEmbeddingDim+2,timeLag=kTimeLag,minRadius=1,maxRadius=100,pointsRadius=100,theilerWindow=20,doPlot=FALSE)
PlotCorrDim(hrv.data,indexNonLinearAnalysis=1)
hrv.data=EstimateCorrDim(hrv.data,indexNonLinearAnalysis=1,regressionRange=c(1.5,10),useEmbeddings=(kEmbeddingDim-1):(kEmbeddingDim+2),doPlot=FALSE)
#We calculate the Sample entropy
hrv.data=CalculateCorrDim(hrv.data,indexNonLinearAnalysis=1,minEmbeddingDim=4*kEmbeddingDim,maxEmbeddingDim=4*kEmbeddingDim+5,timeLag=kTimeLag,minRadius=1,maxRadius=100,pointsRadius=100,theilerWindow=20,doPlot=FALSE)
hrv.data=CalculateSampleEntropy(hrv.data,indexNonLinearAnalysis= 1,doPlot=FALSE)
PlotSampleEntropy(hrv.data,indexNonLinearAnalysis=1)
hrv.data=EstimateSampleEntropy(hrv.data,indexNonLinearAnalysis=1,regressionRange=c(10,20),useEmbeddings=20:24,doPlot=TRUE)
par(mfrow=c(1,1))
#We calculate the Maximum Lyapunov exponent
hrv.data=CalculateMaxLyapunov(hrv.data,indexNonLinearAnalysis=1,minEmbeddingDim= kEmbeddingDim,maxEmbeddingDim= kEmbeddingDim+2,timeLag=kTimeLag,radius=3,theilerWindow=20,doPlot=TRUE)
hrv.data=EstimateMaxLyapunov(hrv.data,indexNonLinearAnalysis=1,regressionRange=c(1,6),useEmbeddings=(kEmbeddingDim):(kEmbeddingDim+2),doPlot=TRUE)
#We calculate the Detrended Fluctuation Analysis alpha1 and alpha 2 parameters
hrv.data=CalculateDFA(hrv.data,indexNonLinearAnalysis=1,windowSizeRange=c(6,300),npoints=25,doPlot=TRUE)
hrv.data=EstimateDFA(hrv.data,indexNonLinearAnalysis=1,regressionRange=c(20,100),doPlot=TRUE)
#We perform Recurrence Quantifcation Analysis
hrv.data=RQA(hrv.data,indexNonLinearAnalysis=1,embeddingDim=kEmbeddingDim,timeLag=kTimeLag,radius=2,doPlot=TRUE)
names(hrv.data$NonLinearAnalysis[[1]]$rqa)
cat("Entropy of the diagonal lines: ",hrv.data$NonLinearAnalysis[[1]]$rqa$ENTR,"\n")
#Rear RR series example
hrv.data=CreateHRVData( )
hrv.data=LoadBeatAscii(hrv.data, RecordName="example2.beats",RecordPath=".")
hrv.data=BuildNIHR(hrv.data)
hrv.data=FilterNIHR(hrv.data)
hrv.data=InterpolateNIHR (hrv.data,freqhr=4)
hrv.data=CreateNonLinearAnalysis(hrv.data)
hrv.data=SetVerbose(hrv.data,TRUE)
#We check that the RR series is nonlinear
hrv.data=NonlinearityTests(hrv.data)
hrv.data=SurrogateTest(hrv.data,significance=0.05,useFunction=timeAsymmetry2,tau=4,doPlot=TRUE)
kTimeLag=CalculateTimeLag(hrv.data,method="first.minimum",lagMax=300)
kEmbeddingDim=CalculateEmbeddingDim(hrv.data,numberPoints=10000,timeLag=kTimeLag,maxEmbeddingDim=18)
kEmbeddingDim
#We apply nonlinear noise reduction
hrv.data=NonLinearNoiseReduction(hrv.data,embeddingDim=kEmbeddingDim)
#We check that the RR series is nonlinear
hrv.data=NonlinearityTests(hrv.data)
hrv.data=SurrogateTest(hrv.data,significance=0.05,useFunction=timeAsymmetry2,tau=4,doPlot=TRUE)
hrv.data=PoincarePlot(hrv.data,indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation=TRUE,confidence=0.9,doPlot=TRUE)
#We calculate the correlation dimension
hrv.data=CalculateCorrDim(hrv.data,indexNonLinearAnalysis=1,minEmbeddingDim=kEmbeddingDim-1,maxEmbeddingDim=kEmbeddingDim+2,timeLag=kTimeLag,minRadius=1,maxRadius=100,pointsRadius=100,theilerWindow=20,doPlot=FALSE)
PlotCorrDim(hrv.data,indexNonLinearAnalysis=1)
hrv.data=EstimateCorrDim(hrv.data,indexNonLinearAnalysis=1,regressionRange=c(60,90),useEmbeddings=(kEmbeddingDim-1):(kEmbeddingDim+2),doPlot=TRUE)
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
#############################################################################################################################################################################################################
ecg<-read.table("/home/dimitrios/Dropbox (Psycholate)/dimitrios/Working/PROJECTS/juul/data/100.csv",sep=",",header=TRUE)
ecg.original<-read.table("/home/dimitrios/Dropbox (Psycholate)/dimitrios/Working/PROJECTS/juul/data/100.csv",sep=",",header=TRUE)
names(ecg)<-c("si0.0027777.sec","MLII","V5")
names(ecg.original)<-c("si0.0027777.sec","MLII","V5")

plot(ecg$MLII,type="l",main="ECG WITH 0.002 INTERVAL",ylab="mV",xlab="Time Index")
#for (i in 1:length(ecg$MLII)){
#  if(ecg$MLII[i]<0) {ecg$MLII[i]=0}
#  cat(".")
#}
ecg$MLII<-sapply(ecg$MLII,function(x) if(x<0) {x=0} else {x})
#sapply(ecg$MLII,function(x){y=x+a;a<<-x;x=y;})
#sapply(xx,function(x){y=x+a;a<<-x;y;})
#sapply(xx,function(x){a<<-a+1;a;})
#sapply(xx,function(x){y=x+a;a<<-x;x=y;});
plot(ecg$MLII,type="l",main="ECG 0.002 INTERVAL",ylab="mV",xlab="Time Index")
for (i in 1:length(ecg$MLII)){
  if(ecg$MLII[i]<ecg$MLII[i+1]){
    ecg$MLII[i]=0
  }
  #cat(".")
}
plot(ecg$MLII,type="o",main="ECG",ylab="mV",xlab="Time Index")
ecg$MLII<-rev(ecg$MLII)
for (i in 1:length(ecg$MLII)){
  if(ecg$MLII[i]<ecg$MLII[i+1]){
    ecg$MLII[i]=0
  }
  #cat(".")
}
ecg$MLII<-rev(ecg$MLII)

seqp<-seq(0,length(ecg.original$MLII)-5001,by=5000)
seqp
points<-ecg$MLII
points<-replace(points,points==0,NA)
cairo_pdf("OUTPUT/PLOTS/PEAK_RECOGNITION.pdf",onefile=TRUE,width=20,height=10)
for (i in seqp){
  plot(ecg.original$MLII,type="l",main="ECG",ylab="mV",xlab="Time Index",xlim=c(i,i+5000))
  points(points,col="red")
  grid()
}
dev.off()

peaks<-data.frame(ecg.original,points)
peaks$points<-replace(peaks$points,!is.na(peaks$points),"PEAK")
check(peaks)
peaks<-peaks[peaks$points %in% "PEAK",]
check(peaks)
write.csv(peaks,"peaks.csv")

library(RHRV)
rm(hrv.data)
hrv.data=CreateHRVData()
hrv.data=SetVerbose(hrv.data,TRUE)
hrv.data=LoadBeatAscii(hrv.data,"peaks",RecordPath=".")
hrv.data=BuildNIHR(hrv.data)
hrv.data=FilterNIHR(hrv.data,long=1,last=10,minbpm=60,maxbpm=200)
PlotNIHR(hrv.data)
#hrv.data=EditNIHR(hrv.data)
hrv.data=InterpolateNIHR(hrv.data,freqhr=1,method="linear",verbose=TRUE)
hrv.data=CreateTimeAnalysis(hrv.data,size=100,interval=1)
hrv.data=CreateFreqAnalysis(hrv.data,verbose=TRUE)
hrv.data=CalculatePowerBand(hrv.data,indexFreqAnalysis=1,type="wavelet",wavelet="la8")
CalculateEmbeddingDim(hrv.data,numberPoints=5000,timeLag=100,maxEmbeddingDim=15,threshold=0.95,maxRelativeChange=0.01,doPlot=TRUE)
#Plotting Fourier analysis
PlotPowerBand(hrv.data,indexFreqAnalysis=1,ymax=200,ymaxratio=1.7)
spectrogram=PlotSpectrogram(HRVData=hrv.data,size=100,shift=20)

i<-1
HRV<-data.frame("TYPE"=rep("HRV",length(hrv.data$FreqAnalysis[[i]]$HRV)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$HRV)),"DATA"=hrv.data$FreqAnalysis[[i]]$HRV)
ULF<-data.frame("TYPE"=rep("ULF",length(hrv.data$FreqAnalysis[[i]]$ULF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$ULF)),"DATA"=hrv.data$FreqAnalysis[[i]]$ULF)
VLF<-data.frame("TYPE"=rep("VLF",length(hrv.data$FreqAnalysis[[i]]$VLF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$VLF)),"DATA"=hrv.data$FreqAnalysis[[i]]$VLF)
LF<-data.frame("TYPE"=rep("LF",length(hrv.data$FreqAnalysis[[i]]$LF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$LF)),"DATA"=hrv.data$FreqAnalysis[[i]]$LF)
HF<-data.frame("TYPE"=rep("HF",length(hrv.data$FreqAnalysis[[i]]$HF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$HF)),"DATA"=hrv.data$FreqAnalysis[[i]]$HF)
LFHF<-data.frame("TYPE"=rep("LFHF",length(hrv.data$FreqAnalysis[[i]]$LFHF)),"TIME"=(1:length(hrv.data$FreqAnalysis[[i]]$LFHF)),"DATA"=hrv.data$FreqAnalysis[[i]]$LFHF)
HRD<-rbind(HRV,ULF,VLF,LF,HF,LFHF)

size<-hrv.data$FreqAnalysis[[i]]$size
shift<-hrv.data$FreqAnalysis[[i]]$shift
sizesp<-hrv.data$FreqAnalysis[[i]]$sizesp
type<-hrv.data$FreqAnalysis[[i]]$type
ULFmin<-hrv.data$FreqAnalysis[[i]]$ULFmin
ULFmax<-hrv.data$FreqAnalysis[[i]]$ULFmax
VLFmin<-hrv.data$FreqAnalysis[[i]]$VLFmin
VLFmax<-hrv.data$FreqAnalysis[[i]]$VLFmax
LFmin<-hrv.data$FreqAnalysis[[i]]$LFmin
LFmax<-hrv.data$FreqAnalysis[[i]]$LFmax
HFmin<-hrv.data$FreqAnalysis[[i]]$HFmin
HFmax<-hrv.data$FreqAnalysis[[i]]$HFmax

ggplot(HRD,aes(y=HRD$DATA,x=HRD$TIME,colour=TYPE))+geom_line()+labs(x="",y="",title="Power")+facet_wrap(~TYPE)
ggplot(HRD,aes(y=HRD$DATA,x=HRD$TIME,colour=HRD$TYPE))+geom_line()+labs(x="",y="",title="Power")+xlim(0,100)

cairo_pdf("OUTPUT/PLOTS/POWER.pdf",onefile=TRUE,width=20,height=20)
s<-seq(0,1500,by=100)
for (i in s) {
  p<-ggplot(HRD,aes(y=HRD$DATA,x=HRD$TIME,colour=HRD$TYPE))+geom_line()+labs(x="",y="",title="Power")+xlim(i,i+100)
  print(p)
}  
dev.off()

plotly::plot_ly(HRD,y=~DATA,x=~TIME,color=~TYPE,type="scatter",mode="lines")
#############################################################################################################################################################################################################
numofbins=NULL
mydata<-hrv.data$Beat$Time
length(mydata)
sd(RR(hrv.data$Beat$Time))
sd(hrv.data$Beat$RR)

hrv.data$Beat$Time