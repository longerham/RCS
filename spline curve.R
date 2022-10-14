dt <- read.csv('data1.csv')
library(rms) #RCS
library(survival) ##cox
col=c('#5BA5DA','#F06955','#FDC059','skyblue','blue') ## Color

##Environment
dd<-datadist(dt) 
options(datadist='dd')
S <- Surv(dt$time,dt$status==1)

fit <- cph(S ~ rcs(BMI,4)+Gender+pT, x=TRUE, y=TRUE,data=dt)# Cox regression
Pre0<-rms::Predict(fit,BMI,fun=exp,type="predictions",ref.zero=T,conf.int = 0.95,digits=2);
par(mar=c(5,4,4,5)) #
plot(density(dt$BMI),  
     axes=F,   ##Don't show coordinate
     xlab="",ylab="",  ## X&Y lable
     xlim =c(0,100),   
     ylim=c(0,0.04),   
     type='l',     
     lty=1, 
     lwd=2,
     col=col[1], 
     main='curve')   
##fill color
polygon(density(dt$BMI), 
        col = col[1], 
        border = col[1]) 
##Add Coordinate
axis(4)  
par(new=T)##
###Add smooth curve
plot(Pre0[,1],Pre0$yhat,axes=T,type='l',lty=1,lwd=2,col=col[2],
     ylim=c(0,9),    ##Y axis
     xlim =c(30,100), 
     xlab='BMI',ylab='HR(95%CI)')
##95%CI
lines(Pre0[,1],Pre0$lower,type='l',lty=2,lwd=2,col=col[2])
lines(Pre0[,1],Pre0$upper,type='l',lty=2,lwd=2,col=col[2])
##text
mtext('density',side=4)##1，2，3，4represents up,down ,left, right of the graph，
##Scatters
points(x=dd$limits$BMI[2], ##X axis coordinate
       y=1, ####y axis coordinate
       pch=19,##shape of scatter。
       col='red')  
Text <- paste('Ref=',round(dd$limits$BMI[2],3),sep='')
text(Text,x=dd$limits$BMI[2]+5,y=0.5,cex = 1)