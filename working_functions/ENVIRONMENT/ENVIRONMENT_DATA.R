##########################################################################################
# PREPARE DATA
##########################################################################################
infert_formula<-formula(case~parity+induced+spontaneous)
##########################################################################################
# THEME
##########################################################################################
# line=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# rect=element_rect(fill=NULL,colour=NULL,size=NULL,linetype=NULL)
# text=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# title=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
##########################################################################################
# axis.title=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# axis.title.x=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# axis.title.y=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# axis.text=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# axis.text.x=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# axis.text.y=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# axis.ticks=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# axis.ticks.x=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# axis.ticks.y=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# axis.ticks.length=(unit)
# axis.ticks.margin=(unit)
# axis.line=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# axis.line.x=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# axis.line.y=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
##########################################################################################
# legend.background=element_rect(fill=NULL,colour=NULL,size=NULL,linetype=NULL)
# legend.margin=(unit)
# legend.key=element_rect(fill=NULL,colour=NULL,size=NULL,linetype=NULL)
# legend.key.size=(unit; inherits from legend.key.size)
# legend.key.height=(unit; inherits from legend.key.size)
# legend.key.width=(unit; inherits from legend.key.size)
# legend.text=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# legend.text.align=(number from 0 (left) to 1 (right))
# legend.title=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# legend.position=("left","right","bottom","top",or two-element numeric vector)
# legend.direction=("horizontal" or "vertical")
# legend.justification=("center" or two-element numeric vector)
# legend.box=("horizontal" or "vertical")
# legend.box.just=("top","bottom","left",or "right")
##########################################################################################
# panel.background=element_rect(fill=NULL,colour=NULL,size=NULL,linetype=NULL)
# panel.border=element_rect(fill=NULL,colour=NULL,size=NULL,linetype=NULL)
# panel.spacing=(unit)
# panel.grid=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# panel.grid.major=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# panel.grid.minor=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# panel.grid.major.x=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# panel.grid.major.y=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# panel.grid.minor.x=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
# panel.grid.minor.y=element_line(colour=NULL,size=NULL,linetype=NULL,lineend=NULL)
##########################################################################################
# plot.background=element_rect(fill=NULL,colour=NULL,size=NULL,linetype=NULL)
# plot.title=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# plot.margin=(unit with the sizes of the top,right,bottom,and left margins)
##########################################################################################
# strip.background=element_rect(fill=NULL,colour=NULL,size=NULL,linetype=NULL)
# strip.text=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# strip.text.x=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
# strip.text.y=element_text(family=font,face=face,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,lineheight=NULL)
