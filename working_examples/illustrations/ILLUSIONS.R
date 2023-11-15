library(grid)
library(plyr)
rs<-expand.grid(x=seq(0,1,1/10),y=seq(0,1,1/10))
grid.rect(rs$x,rs$y,1/10/2,1/10/2,gp=gpar(fill="black",col=NA))
grid.rect(rs$x+1/10/4,rs$y+1/10/2,1/10/2,1/10/2,gp=gpar(fill="black",col=NA))
ls<-expand.grid(x=0:1,y=seq(0,1,1/20)-1/20/2)
grid.polyline(ls$x,ls$y,id=gl(nrow(ls)/2,2),gp=gpar(col="grey50",lwd=1))

nx<-6; ny<-6; lwd<-10
grid.newpage()
grid.rect(0.5,0.5,1,1,gp=gpar(fill="black"))
ls<-expand.grid(x=0:1,y=seq(0,1,1/ny/2)-1/ny/2/2)
grid.polyline(ls$x,ls$y,id=gl(nrow(ls)/2,2),gp=gpar(col="white",lwd=lwd))
ls<-expand.grid(y=0:1,x=seq(0,1,1/ny/2)-1/ny/2/2)
grid.polyline(ls$x,ls$y,id=gl(nrow(ls)/2,2),gp=gpar(col="white",lwd=lwd))

nx<-6; ny<-6; lwd<-10; cr<-1/100
grid.newpage()
grid.rect(0.5,0.5,1,1,gp=gpar(fill="black"))
ls<-expand.grid(x=0:1,y=seq(0,1,1/nx/2)-1/nx/2/2)
grid.polyline(ls$x,ls$y,id=gl(nrow(ls)/2,2),gp=gpar(col="grey",lwd=lwd))
ls<-expand.grid(y=0:1,x=seq(0,1,1/ny/2)-1/ny/2/2)
grid.polyline(ls$x,ls$y,id=gl(nrow(ls)/2,2),gp=gpar(col="grey",lwd=lwd))
ls<-expand.grid(x=seq(0,1,1/nx/2)-1/nx/2/2,y=seq(0,1,1/ny/2)-1/ny/2/2)
grid.circle(ls$x,ls$y,r= cr,gp=gpar(col=NA,fill="white"))

grid.newpage()
nx<-10; ny<-30
rs<-expand.grid(x=seq(0,1,1/nx/2),y=seq(0,1,1/ny/2))
grid.rect(rs$x,rs$y,1/nx/2,1/ny/2,gp=gpar(col=NA,fill=c("black","white")))
rs<-expand.grid(x=seq(0.25,0.75,1/nx/2),y=seq(0.25,0.75,1/ny/2))
grid.rect(rs$y,rs$x,1/ny/2,1/nx/2,gp=gpar(col=NA,fill=c("black","white")))

grid.newpage()
grid.rect(c(1,3,1,3)/4,c(3,3,1,1)/4,1/2,1/2,gp=gpar(col=NA,fill=gray(1:4/5)))
grid.rect(c(1,3,1,3)/4,c(3,3,1,1)/4,1/6,1/6,gp=gpar(col=NA,fill=gray(0.5)))

grid.newpage()
rs<-expand.grid(x=0:100,y=0:100)
rs$c<-ifelse(rs$x%%2 == rs$y%%2,"blue","yellow")
grid.rect(rs$x/100,rs$y/100,1/100,1/100,gp=gpar(col=NA,fill=rs$c))
r<-subset(rs,10 <= x & x <= 40 & 10 <= y & y <= 40 & c == "blue")
grid.rect(r$x/100,r$y/100,1/100,1/100,gp=gpar(col=NA,fill="green"))
r<-subset(rs,60 <= x & x <= 90 & 10 <= y & y <= 40 & c == "yellow")
grid.rect(r$x/100,r$y/100,1/100,1/100,gp=gpar(col=NA,fill="green"))
r<-subset(rs,10 <= x & x <= 40 & 60 <= y & y <= 90 & c == "blue")
grid.rect(r$x/100,r$y/100,1/100,1/100,gp=gpar(col=NA,fill="red"))
r<-subset(rs,60 <= x & x <= 90 & 60 <= y & y <= 90 & c == "yellow")
grid.rect(r$x/100,r$y/100,1/100,1/100,gp=gpar(col=NA,fill="red"))

grid.newpage()
n<-36
grid.rect(c(1,3)/4,1/2,1/2,1,gp=gpar(col=NA,fill=c("yellow","blue")))
grid.rect(1/2,c(2,4,6)/8,1,1/8,gp=gpar(col=NA,fill=c("green","white","red")))
grid.rect(c(1:(n/2-1))/n,1/2,1/n/2,1,gp=gpar(col=NA,fill=c("blue")))
grid.rect(c(n/2+1:(n/2-1))/n,1/2,1/n/2,1,gp=gpar(col=NA,fill=c("yellow")))

grid.newpage()
nx<-6; ny<-6
an<-c(1,-1,1,1,-1,1,-1,-1,1,-1,1,1)
rs<-expand.grid(x=seq(0,1,1/nx/2),y=seq(0,1,1/ny/2))
grid.rect(rs$x,rs$y,1/nx/2,1/ny/2,gp=gpar(col=NA,fill=c("black","white")))
rs<-expand.grid(x=seq(1/nx/2,1,1/nx/2)-1/nx/4,y=seq(1/ny/2,1,1/ny/2)-1/ny/4)
rs$an<-c(an,-an)
l_ply(1:nrow(rs),function(i) {
  pushViewport(viewport(rs$x[i],rs$y[i],1/30,1/30,angle=rs$an[i]*45))
  grid.rect(c(1,3,1,3)/4,c(3,3,1,1)/4,1/2,1/2,gp=gpar(col=NA,fill=gray(c(0,1,1,0))))
  popViewport()
})

nt<-41; nr<-15; br<-0.8
col1<-c("black","white")
col2<-c("aquamarine4","gold2")

f<-function(x0,y0) {
  r<-embed(br^(0:nr),2)
  t<-embed(seq(0,2*pi,length=nt),2)
  i<-as.matrix(expand.grid(1:nrow(r),1:nrow(t)))
  ci<-1+(i[,2]%%2+i[,1]%%2) %% 2
  
  p<-t(apply(i,1,function(x) c(r[x[1],],t[x[2],])))
  x<-c(p[,1]*cos(p[,3]),p[,1]*cos(p[,4]),p[,2]*cos(p[,4]),p[,2]*cos(p[,3]))
  y<-c(p[,1]*sin(p[,3]),p[,1]*sin(p[,4]),p[,2]*sin(p[,4]),p[,2]*sin(p[,3]))
  grid.polygon(x0+x/2,y0+y/2,id=rep.int(1:nrow(p),4),gp=gpar(fill=col1[ci],col=NA),default.units="native")
  
  p<-expand.grid(1:nrow(r),sign((abs(x0-y0)==1)-0.5)*seq(0,2*pi,length=41)[-1])
  p<-cbind(p[,2],rowMeans(r)[p[,1]],(r[,2]-r[,1])[p[,1]]/2)
  t<-seq(0,2*pi,length=20)[-1]
  x<-c(apply(p,1,function(a) a[2]*cos(a[1])+a[3]*(cos(a[1])*cos(t)-0.5*sin(a[1])*sin(t))))
  y<-c(apply(p,1,function(a) a[2]*sin(a[1])+a[3]*(sin(a[1])*cos(t)+0.5*cos(a[1])*sin(t))))
  col<-if(abs(x0-y0)==1) {col2} else {rev(col2)}
  grid.polygon(x0+x/2,y0+y/2,id=rep(1:nrow(p),each=length(t)),gp=gpar(fill=col[ci],col=NA),default.units="native")
  
}

grid.newpage()
pushViewport(viewport(xscale=c(0,3),yscale=c(0,3)))
for (x0 in 0.5+0:2) for (y0 in 0.5+0:2) f(x0,y0)
for (x0 in 1:2) for (y0 in 1:2) f(x0,y0)

grid.newpage()
pushViewport(viewport(0.5,0.75,1,0.5,clip=TRUE))
n<-25; t<-seq(0,pi,length=n)[2:(n-1)]
grid.polyline(2*c(cos(t),cos(t+pi))/2+0.5,2*c(sin(t),sin(t+pi))/2+0.5,id=rep(1:(n-2),2))
grid.polyline(c(0,1,0,1),c(1,1,3,3)/4,id=c(1,1,2,2))
popViewport()

pushViewport(viewport(0.5,0.25,1,0.5,clip=TRUE))
n<-10; a<-135/180*pi; s<-0.1
p<-seq(0,1,len=n)
x<-c(p+cos(a)*s,p-cos(a)*s)
y<-c(rep(sin(a),n)*s,rep(-sin(a),n)*s)
grid.polyline(x,y+0.3,id=rep(1:n,2))
grid.polyline(x,rev(y)+0.7,id=rep(1:n,2))
grid.polyline(c(0,1,0,1),c(3,3,7,7)/10,id=c(1,1,2,2))

grid.newpage()
n<-10; ny<-8; L<-0.01; c<-seq(0,1,length=n); d<-1.2*diff(c)[1]/2
col<-c("black","white")
x<-c(c-d,c,c+d,c)
y<-rep(c(0,-d,0,d),each=n)
w<-c(c-d,c-d+L,c+d,c+d-L)
z<-c(0,L,0,-L)
ys<-seq(0,1,length=ny)
grid.rect(gp=gpar(fill=gray(0.5),col=NA))
l_ply(1:ny,function(i) {
  if (i%%2==0) {
    co<-rev(col)
    z<--z
  } else {
    co<-col
  }  
  grid.polygon(x,y+ys[i],id=rep(1:n,4),gp=gpar(fill=co,col=NA))
  grid.polygon(w,rep(z,each=n)+ys[i],id=rep(1:n,4),gp=gpar(fill=rev(co),col=NA))
})

nt<-41; br<-0.2
col1<-c("black","white")

f<-function(x0,y0) {
  r<-embed(c(1,0.2),2)
  t<-embed(seq(0,2*pi,length=nt),2)
  i<-as.matrix(expand.grid(1:nrow(r),1:nrow(t)))
  
  p<-t(apply(i,1,function(x) c(r[x[1],],t[x[2],])))
  x<-c(p[,1]*cos(p[,3]),p[,1]*cos(p[,4]),p[,2]*cos(p[,4]),p[,2]*cos(p[,3]))
  y<-c(p[,1]*sin(p[,3]),p[,1]*sin(p[,4]),p[,2]*sin(p[,4]),p[,2]*sin(p[,3]))
  grid.polygon(x0+x/2,y0+y/2,id=rep.int(1:nrow(p),4),gp=gpar(fill=col1,col=NA),default.units="native")
}

grid.newpage()
pushViewport(viewport(xscale=c(0,3),yscale=c(0,3)))
for (x0 in 0.5+0:2) for (y0 in 0.5+0:2) f(x0,y0)

grid.newpage()
grid.rect(c(1,3,1,3)/4,c(3,3,1,1)/4,1/2,1/2,gp=gpar(col=NA,fill=c("green","red")))
grid.rect(c(1,3,1,3)/4,c(3,3,1,1)/4,1/6,1/6,gp=gpar(col=NA,fill=c("blue","blue","yellow","yellow")))

grid.newpage()
pushViewport(viewport(layout=grid.layout(3,3)))
for (vx in 1:3) {
  for (vy in 1:3) {
    pushViewport(viewport(layout.pos.col=vx,layout.pos.row=vy,clip=TRUE))
    co<-colorRampPalette(c(rep("red3",1),"purple2",rep("purple",2),rep("magenta2",1),rep("red2",2)),interpolate="spline")
    col<-co(100)
    N<-1000
    for (ri in 1:10) {
      r<-(0.7^(-1:20))[ri]
      ofs<-if (ri%%2) 
        0
      else pi/2
      a<-embed(seq(0+ofs,2*pi+ofs,length=N+1),2)
      x<-r*c(rep(0,N),cos(a[,1]),cos(a[,2]))/2+0.5
      y<-r*c(rep(0,N),sin(a[,1]),sin(a[,2]))/2+0.5
      id=rep(1:N,3)
      grid.polygon(x,y,id,gp=gpar(col=col,fill=col))
    }    
    popViewport()
  }
}

grid.newpage()
No<-3
wo<-1/3/2
po<-seq(0,1,by=wo)[(1:No)*2]
Nc<-8
tc<-seq(pi*11/12,pi*1/12,len=Nc)
px<-c(outer(wo*cos(tc),po,`+`))
wc<-rep(sin(tc),No)
ag<-rep(1:No,each=Nc)
dc<-21
th<-seq(0,2*pi,len=dc)
grid.rect(gp=gpar(col=NA,fill="#D2D200"))
for (y0 in seq(0,1,len=10)) {
  for (i in seq_along(px)) {
    th<-seq(pi/2,pi/2+2*pi,len=21)
    if (ag[i]%%2==0) th<-rev(th)
    x<-px[i]+0.5*0.04*cos(th)*wc[i]
    y<-y0+0.04*sin(th)
    grid.polygon(x,y,gp=gpar(fill="#3278FE"))
    grid.polyline(x[1:((dc+1)/2)],y[1:((dc+1)/2)],gp=gpar(lineend="butt",lwd=3,col=gray(0)))
    grid.polyline(x[-(1:((dc-1)/2))],y[-(1:((dc-1)/2))],gp=gpar(lineend="butt",lwd=3,col=gray(1)))
  }
}

library(animation)
vi.lilac.chaser(np=40)
oopt=ani.options(interval=0.05,nmax=20)
par(pty="s")
vi.lilac.chaser()

## HTML animation page; nmax=1 is enough!
saveHTML({
  ani.options(interval=0.05,nmax=1)
  par(pty="s",mar=rep(1,4))
  vi.lilac.chaser() 
  },img.name="vi.lilac.chaser",htmlfile="vi.lilac.chaser.html",
ani.height=480,ani.width=480,title="Visual Illusions: Lilac Chaser",
description=c("Stare at the center cross for a few (say 30) seconds to experience the phenomena of the illusion."))
ani.options(oopt)
