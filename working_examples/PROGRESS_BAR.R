##########################################################################################
# PROGRESS CONSOLE
##########################################################################################
# create progress bar
pb<-txtProgressBar(min=0,max=total,style=3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb,i)
}
close(pb)
##########################################################################################
# PROGRESS GUI
##########################################################################################
pb<-tcltk::tkProgressBar(title="progress bar",min=0,max=total,width=getOption("width"))
for(i in 1:total){
  Sys.sleep(0.1)
  tcltk::setTkProgressBar(pb,i,label=paste(round(i/total*100,0),"% done"))
}
close(pb)
##########################################################################################
# PROGRESS GUI WINDOWS
##########################################################################################
pb<-winProgressBar(title="progress bar",min=0,max=total,width=getOption("width"))
for(i in 1:total){
  Sys.sleep(0.1)
  setWinProgressBar(pb,i,title=paste(round(i/total*100,0),"% done"))
}
close(pb)
