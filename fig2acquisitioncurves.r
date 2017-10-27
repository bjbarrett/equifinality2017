
#below is graphing code for figure 2a and 2b
#base graphics 4 lyf

###########payoff learning acquisition curve##############
ltypeh=c(1,2,3,4)		#line type
colind=c("blue4","blue1","cornflowerblue","cyan")
colind=rev(colind)

cairo_pdf("payoffsim_varyB.pdf" , width=7 , heigh=7 )
par(mar=c(5,5,4,1), xpd=TRUE)

N1<- seq(from=0 , to=100 , by=1)
N2<- 100-N1
FreqN1B4 <- N1/(N1+N2)
FreqN1After <- rep (0,100)

beta.plist <- c(0,0.25,0.75,1.5)
N1<- seq(from=0 , to=100 , by=1)
N2<- 100-N1
FreqN1B4 <- N1/(N1+N2)
FreqN1After <- rep (0,100)
plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of high-payoff trait after social learning" , xlab="frequency of high-payoff trait before social learning",type="n" , bty="n" , cex.lab=1.5)
for(i in 1:4){
   FreqN1After <- FreqN1B4*exp(beta.plist[i]*10)/(FreqN1B4*exp(beta.plist[i]*10) + (1-FreqN1B4)*exp(beta.plist[i]*9) )  
   lines( FreqN1B4,FreqN1After,  col=colind[i] , lty=ltypeh[i] , lwd=4) 
}

legend("top", title="Strength of \u03B2", c("\u03B2=0","\u03B2=0.25","\u03B2=0.75","\u03B2=1.5") , col=colind, horiz=TRUE , lty=ltypeh , lw =3 , cex=1.2 , bty="n" ,  inset=c(0,-.12) )
dev.off()



###########frequency dependent learning acquisition curve##############

cairo_pdf("freq_varyfc.pdf", width=7 , heigh=7 ) 
par(mar=c(5,5,4,1), xpd=TRUE)

#freq_dependent learning frequency beforelearning vs. frequency of trait after learning
ltypeh=c(2,1,3)
colind=c("lightsalmon","lightsalmon2","lightsalmon4")
freqseq <- c(0.5,1,2)
N1<- seq(from=0 , to=100 , by=1)
N2<- 100-N1
FreqN1B4 <- N1/(N1+N2)
FreqN1After <- rep (0,100)
plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1.5)
for(i in 1:3){
   FreqN1After <- N1^freqseq[i]/(N1^freqseq[i]+N2^freqseq[i])  
   lines( FreqN1B4,FreqN1After,  col=colind[i] , lty=ltypeh[i] , lwd=4) 
}
legend("top", title=expression("Strength of \u0192"), 
    c(expression('\u0192=0.5'),expression('\u0192=1'),expression('\u0192=2'))
     , col=colind, horiz=TRUE , lty=ltypeh , lw =3 , bty="n" ,  inset=c(0,-.12) , cex=1.2 )

dev.off()
