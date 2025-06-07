
## ++++++++++++++++++++++++++++++
##       Plot in Figure 1
## ++++++++++++++++++++++++++++++

cex.tickLabel <- 0
cex.axesLabel <- 0.8
cex.main <- 0.9
bw <- TRUE
pal <- colorRampPalette(c("#333333", "#888888", "#EEEEEE"))(8)

# Congruence pattern

p1 <- plotRSA(b0=3,x=0, y=0, x2=-.25, xy=.50,y2=-.25, xlab="X", ylab="Y", zlab=" Z", project=c("LOC","LOIC","PA1"), 
              axes=c("LOC","LOIC","PA1"),param=FALSE, legend=FALSE, gridsize=12, 
              cex.tickLabel=cex.tickLabel, cex.axesLabel=cex.axesLabel, cex.main=cex.main, border=TRUE,distance = c(1.2, 1.2,1.2), 
              bw=bw, pal=pal,main=as.expression(bquote(atop("(A) Congruence pattern:", a[1]==0~","~a[2]==0~","~a[3]==0~","~a[4]<0~","~a[5]==0))))

# Rising ridge (or weak congruence pattern)

p2 <- plotRSA(b0=3,x=.2, y=.2, x2=-.25, xy=.50,y2=-.25, xlab="X", ylab="Y", zlab="Z", project=c("LOC","LOIC","PA1"), 
              axes=c("LOC","LOIC","PA1"),param=FALSE, legend=FALSE, gridsize=12, 
              cex.tickLabel=cex.tickLabel, cex.axesLabel=cex.axesLabel, cex.main=cex.main, border=TRUE, distance = c(1.2, 1.2,1.2), 
              bw=bw, pal=pal,main=as.expression(bquote(atop("(B) Rising ridge:", a[1]>0~","~a[2]==0~","~a[3]==0~","~a[4]<0~","~a[5]==0))))

# Optimal margin effect

p3 <- plotRSA(b0=3,x=.3, y=-.3, x2=-.20, xy=.40,y2=-.201, xlab="X", ylab="Y", zlab="Z", project=c("LOC","LOIC","PA1"), 
              axes=c("LOC","LOIC","PA1"),param=FALSE, legend=FALSE, gridsize=12, 
              cex.tickLabel=cex.tickLabel, cex.axesLabel=cex.axesLabel, cex.main=cex.main, border=TRUE, distance = c(1.2, 1.2,1.2), 
              bw=bw, pal=pal,main=as.expression(bquote(atop("(C) Optimal margin effect:", a[1]==0~","~a[2]==0~","~a[3]>0~","~a[4]<0~","~a[5]==0))))

# Plane
p4 <- plotRSA(b0=3,x=-.25, y=.25, x2=0, xy=0, y2=0, xlab="X", ylab="Y", zlab="Z", project=c("LOC", "LOIC"), 
              axes=c("LOC", "LOIC"), param=FALSE, legend=FALSE, gridsize=12, 
              cex.tickLabel=cex.tickLabel, cex.axesLabel=cex.axesLabel, cex.main=cex.main, border=TRUE, distance = c(1.2, 1.2,1.4), 
              bw=bw, pal=pal, main=as.expression(bquote(atop("(D) Plane:", a[1]==0~","~a[2]==0~","~a[3]<0~","~a[4]==0~","~a[5]==0))))

# Dome
p5 <- plotRSA(b0=3,x=0, y=0, x2=-.25, xy=0, y2=-.25, xlab="X", ylab="Y", zlab="Z", project=c("LOC","LOIC"), 
              axes=c(), param=FALSE, legend=FALSE, gridsize=12, 
              cex.tickLabel=cex.tickLabel, cex.axesLabel=cex.axesLabel, cex.main=cex.main, border=TRUE, distance = c(1.2, 1.2,1.4), 
              bw=bw, pal=pal, main=as.expression(bquote(atop("(E) Dome:", a[1]==0~","~a[2]<0~","~a[3]==0~","~a[4]<0~","~a[5]==0))))

# Bowl
p6 <- plotRSA(x=0, y=0, x2=.25, xy=0, y2=.25, xlab="X", ylab="Y", zlab="  Z", project=c("LOC","LOIC"), 
              axes=c(), param=FALSE, legend=FALSE, gridsize=12, 
              cex.tickLabel=cex.tickLabel, cex.axesLabel=cex.axesLabel, cex.main=cex.main, border=TRUE, distance = c(1.2, 1.2,1.4),
              bw=bw, pal=pal, main=as.expression(bquote(atop("(F) Bowl:", a[1]==0~","~a[2]>0~","~a[3]==0~","~a[4]>0~","~a[5]==0))))


pdf(file="Figure1.pdf", width=24/2.54, height=20/2.54)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, widths=unit(c(8,8,8), c("cm")), heights=unit(c(10,10), "cm"), padding = unit(0.9, "line"))
dev.off()

## ++++++++++++++++++++++++
##       Figure 2
## ++++++++++++++++++++++++

## Use this code directly after using the MLRSA_PickaPoint-function in
## the file AnalyzeData.r

p1 <- MLRSA_PickaPointPlot(m2,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),
                           outcome="z",data=dat,
                           name_cov="g.s",value_cov=0,
                           xlab="Real age",ylab="Felt age",zlab="Satisfaction",
                           param=FALSE,
                           main=as.expression(bquote(atop("(A) Average Response Surface"))))
                          
p2 <- MLRSA_PickaPointPlot(m2,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),
                           outcome="z",data=dat,
                           name_cov="g.s",value_cov=-1,
                           xlab="Real age",ylab="Felt age",zlab="Satisfaction",
                           param=FALSE,
                           main=as.expression(bquote(atop("(B) Low Covariate Value")))) 
                     
p3 <- MLRSA_PickaPointPlot(m2,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),
                           outcome="z",data=dat,
                           name_cov="g.s",value_cov=1,
                           xlab="Real age",ylab="Felt age",zlab="Satisfaction",
                           param=FALSE,
                           main=as.expression(bquote(atop("(C) High Covariate Value"))))                        
                        
lay <- rbind(c(1,1),c(2,3))
pdf(file="Figure2.pdf", width=24/2.54, height=20/2.54)
grid.arrange(p1, p2, p3, layout_matrix = lay, ncol=2, widths=unit(c(8,8), c("cm")), heights=unit(c(9,9), "cm"), padding = unit(0.9, "line"))
dev.off() 