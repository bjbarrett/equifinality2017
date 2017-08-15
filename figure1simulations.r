###plot for 2 options probability frequencyxtime(evolutionary dynamics)

library(rethinking)
library(rstan)
library(truncnorm)

Softmax <- function(x){
exp(x)/sum(exp(x))
} #softmax function to simplify code



#try with multiple simulations

pdf("behproc_fig1_pop_curves_simavg.pdf" , width=10 , height=8)

par(mfrow=c(4, 5) , mai=c(0.5,0.5,0.5,0.5) , oma=c(6,8,4,0.5) , cex=0.5 , mar=c(0,0,0,0))

#betalist <- c(1, 0 , 0 , 1) #strength of payoff bias
betalist <- c(0, 0 , 0 , 1.5) #strength of payoff bias
#paylist <- c(0.32, 0.24, 0.16, 0.08)
#freqlist <- c(0,1,1.3,1) #strength of frequency dependence
freqlist <- c(0,1,1.5,0) #strength of frequency dependence
#gammalist <- c(0,1,1,1)
gammalist <- c(0,1,1,1)
nsims <- 500

prt1initlist <-c(0.99999999999999999999,0.7,0.50,0.3,0.00000000000000000001) #initial number o

prt2initlist <- 1 - prt1initlist 
Prtech1_100 <- Prtech2_100 <- array(0,c(nbouts,nsims))  ###DIFFERENET

xlabs=c("A=1 , B=0" , "A=0.7 , B=0.3" , "A=0.5 , B=0.5" , "A=0.3 , B=0.7" , "A=0 , B=1")
ylabs=c("individual learning" , "linear imitation" , "majority-bias" , " pay-off bias")

for (row in 1:4){
    for(column in 1:5){
        for(sim in 1:nsims){
        #data sims
        n <- 50 # number of individuals
        nbouts <- 30 #rounds foraged
        techmeans <- c(10,9) 
        techvar <- c(2,2)
        #parameter sims
        #fc.sim <- log(.5)               #frequency dependecy parameter
        phi.sim <- 0.25 ## stickiness parameter
        gamma.sim <- gammalist[row] ## weight of social info parameter
        k.lambda <- 1
        dsim_s <- data.frame( i=0 , bout=0 , tech=0 , y1=0 , y2=0, s1=0 , s2=0 , ps1=0 , ps2=0 , A1=0 , A2=0  )
        therow <- 1
        AC <- matrix(0,ncol=2,nrow=n) #attraction scores for each tech
        ##below makes choose each behavior according to with probability at each timestep
        AC[1,1] <- 1
        AC[1,2] <- (1/k.lambda)*log( ( exp(k.lambda*1) - prt1initlist[column]*exp(k.lambda*1)) / prt1initlist[column])
        S1 <- S2 <- rep(0, nbouts + 1) # num of individuals choosing each tech in previous bout
        PS1 <- PS2 <- rep(0,nbouts+1) # empty vector for mean observed in previous rounds
        Prtech1 <- Prtech2 <- rep(0,nbouts)  ###DIFFERENET
        lin_mod <- s_temp <-  rep(0,2)

        for ( r in 1:nbouts ) {
            for ( i in 1:n ) {  
                prtech_i <-  Softmax(k.lambda*AC[i,])
                #my.bp <- betalist[row]  + beta.p_i[i]  #payoff weight for individual i
                #my.gam <- logistic( gammalist[row] + gamma.sim_i[i] ) #social info weight for individual i
                #my.phi <- logistic( phi.sim + phi.sim_i[i] ) #social info weight for individual i
                #my.fconf <- exp( freqlist[row] + fc.sim_i[i]) 
                prtech_sp <- c(PS1[r],PS2[r]) #temp value for observed payoffs from t=r-1 to be used @ t=r
                prtech_su <- c(S1[r],S2[r]) #temp value for observed techs from t=r-1 to be used @ t=r

                #//conformity aspect below
                if ( r > 1 ) {
                    #if (sum( prtech_su ) > 0 ) { #if there was social info

                        #// compute non-frequency cues as log-linear model
                        for ( j in 1:2 ){lin_mod[j] <- exp(betalist[row]*prtech_sp[j])}
                        #// compute frequency cue
                        for ( j in 1:2 ){ s_temp[j] <- prtech_su[j]^freqlist[row]} 
                        for ( j in 1:2 ){lin_mod[j] <- lin_mod[j] * s_temp[j]}

                        prtech_s <- lin_mod/sum(lin_mod)
                        prtech <- (1-gammalist[row])*prtech_i + gammalist[row]*prtech_s


                } else {
                    #prtech_i <-  Softmax(k.lambda*AC[i,]) ##makes it attraction scores
                    #prtech_s <- c(prt1initlist[column],prt2initlist[column])#makes probs starting probs at 1st time step
                    #prtech <- (1-gammalist[row])*prtech_i + gammalist[row]*prtech_s
                    prtech <- c(prt1initlist[column],prt2initlist[column])#makes probs starting probs at 1st time step
                 }
                 Prtech1i <- prtech[1] # prop of inividuals in pop displaying a behavior 1 @ t=2
                 Prtech2i <- prtech[2] # prop of inividuals in pop displaying a behavior 2 @ t=r
        # choose tech
                tech <- sample( 1:2 , size=1 , prob=prtech)
                yield <- rtruncnorm( 1 , a=0 , b=Inf, mean=techmeans[tech] , sd=techvar[tech] )
        # update attractions
                yields <- rep(0,2)
                yields[tech] <- yield 
                for (k in 1:2){
                    AC[i,k] <- (1-phi.sim)*AC[i,k] + phi.sim*yields[k]
                }

                dsim_s[therow,] <- c( i , r , tech , yields[1] , yields[2] , S1[r] , S2[r] , PS1[r] , PS2[r] , AC[i,1] , AC[i,2] )
                therow <- therow + 1
            } #i
            PS1[r+1] <- ifelse(is.nan(mean( dsim_s$y1[dsim_s$bout==r & dsim_s$tech==1] )), 0, mean( dsim_s$y1[dsim_s$bout==r & dsim_s$tech==1] ) )
            PS2[r+1] <- ifelse(is.nan(mean( dsim_s$y2[dsim_s$bout==r & dsim_s$tech==2] )), 0, mean( dsim_s$y2[dsim_s$bout==r & dsim_s$tech==2] ) )
            S1[r+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$bout==r] )
            S2[r+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$bout==r] )

            #Prtech1[r] <- ifelse(is.nan(mean(dsim_s$Prtech1i[dsim_s$bout==r])) , 0 , is.nan(mean(dsim_s$Prtech1i[dsim_s$bout==r])))
            #Prtech2[r] <- ifelse(is.nan(mean(dsim_s$Prtech2i[dsim_s$bout==r])) , 0 , is.nan(mean(dsim_s$Prtech2i[dsim_s$bout==r])))
            Prtech1[r] <- S1[r+1]/n
            Prtech2[r] <- S2[r+1]/n
            Prtech1_100[r,sim] <- Prtech1[r]
            Prtech2_100[r,sim] <- Prtech2[r]

         }
}
        o <- order( dsim_s$i )
        dsim2 <- dsim_s[o,]

if(column!=1 & row!=4){
        plot((1:nbouts) , Prtech1[1:nbouts] , ylim=c(0,1) , xlim=c(0,nbouts) , xlab="" , ylab=""  ,type="n" , bty="n" , axes=FALSE )
        axis(side=1, labels=FALSE , tcl=-0.1 )
        axis(side=1, labels=FALSE , tcl=0.1 )
        axis(side=2, labels=FALSE , tcl=-0.1 )
        axis(side=2, labels=FALSE , tcl=0.1 )
        lines( (1:nbouts),apply(Prtech1_100,1,mean) ,  col="orange", lty=1 , lwd=2)  #orange is higher payoff
        lines( (1:nbouts),apply(Prtech2_100,1,mean)  , col="blue", lty=3, lwd=2)
        for (j in 1:10){
        lines( (1:nbouts),Prtech1_100[1:nbouts,j] ,  col=col.alpha("orange" , 0.2), lty=1 , lwd=1)  #orange is higher payoff
        lines( (1:nbouts),Prtech2_100[1:nbouts,j]  , col=col.alpha("blue" , 0.2), lty=3, lwd=1)
        }
    }
if(column==1  &  row!=4){
        plot((1:nbouts) , Prtech1[1:nbouts] , ylim=c(0,1) , xlim=c(0,nbouts) , xlab="" , ylab=""  ,type="n" , bty="n" , axes=FALSE )
        axis(side=2 , tcl=-0.1 ,labels=c(0,0.2,0.4,0.6,0.8,1), at = c(0,0.2,0.4,0.6,0.8,1))
        axis(side=2 , tcl=0.1 ,labels=FALSE)
        axis(side=2 , tcl=-0.1 ,labels=FALSE, at = seq(from=1 , to=1, by=0.1))
        axis(side=2 , tcl=0.1 ,labels=FALSE, at = seq(from=1 , to=1, by=0.1))
        axis(side=1 , tcl=-0.1 ,labels=FALSE)
        axis(side=1 , tcl=0.1 ,labels=FALSE)
        lines( (1:nbouts),apply(Prtech1_100,1,mean) ,  col="orange", lty=1 , lwd=2)  #orange is higher payoff
        lines( (1:nbouts),apply(Prtech2_100,1,mean)  , col="blue", lty=3, lwd=2)
        for (j in 1:10){
        lines( (1:nbouts),Prtech1_100[1:nbouts,j] ,  col=col.alpha("orange" , 0.2), lty=1 , lwd=1)  #orange is higher payoff
        lines( (1:nbouts),Prtech2_100[1:nbouts,j]  , col=col.alpha("blue" , 0.2), lty=3, lwd=1)
        }

        mtext(ylabs[row], side = 2, cex = 1, line = 2.5)

    }

if(row==4 & column !=1){
        plot((1:nbouts) , Prtech1[1:nbouts] , ylim=c(0,1) , xlim=c(0,nbouts) , xlab="" , ylab=""  ,type="n" , bty="n" , axes=FALSE )
        axis(side=1 , tcl=-0.2  )
        axis(side=1 , tcl=0.1 )
        axis(side=2 , tcl=-0.1 ,labels=FALSE)
        axis(side=2 , tcl=0.1 ,labels=FALSE)
        lines( (1:nbouts),apply(Prtech1_100,1,mean) ,  col="orange", lty=1 , lwd=2)  #orange is higher payoff
        lines( (1:nbouts),apply(Prtech2_100,1,mean)  , col="blue", lty=3, lwd=2)
        for (j in 1:10){
        lines( (1:nbouts),Prtech1_100[1:nbouts,j] ,  col=col.alpha("orange" , 0.2), lty=1 , lwd=1)  #orange is higher payoff
        lines( (1:nbouts),Prtech2_100[1:nbouts,j]  , col=col.alpha("blue" , 0.2), lty=3, lwd=1)
        }

    }

    if(row==4 & column ==1){
        plot((1:nbouts) , Prtech1[1:nbouts] , ylim=c(0,1) , xlim=c(0,nbouts) , xlab="" , ylab=""  ,type="n" , bty="n" , axes=FALSE )
        axis(side=1 , tcl=-0.2  )
        axis(side=1 , tcl=0.1 )
        axis(side=2 , tcl=-0.1 ,labels=c(0,0.2,0.4,0.6,0.8,1), at = c(0,0.2,0.4,0.6,0.8,1))
        axis(side=2 , tcl=0.1 ,labels=FALSE)
        axis(side=2 , tcl=-0.1 ,labels=FALSE, at = seq(from=1 , to=1, by=0.1))
        axis(side=2 , tcl=0.1 ,labels=FALSE, at = seq(from=1 , to=1, by=0.1) )
        lines( (1:nbouts),apply(Prtech1_100,1,mean) ,  col="orange", lty=1 , lwd=2)  #orange is higher payoff
        lines( (1:nbouts),apply(Prtech2_100,1,mean)  , col="blue", lty=3, lwd=2)
        for (j in 1:10){
        lines( (1:nbouts),Prtech1_100[1:nbouts,j] ,  col=col.alpha("orange" , 0.2), lty=1 , lwd=1)  #orange is higher payoff
        lines( (1:nbouts),Prtech2_100[1:nbouts,j]  , col=col.alpha("blue" , 0.2), lty=3, lwd=1)
        }
        mtext(ylabs[row], side = 2, cex = 1, line = 2.5)

    }

    if(row==1){ mtext(xlabs[column], side = 3, cex = 1, line = 1 )
    }


    }
}
    #title(main=paste("\u03B2p=",betalist[Y],"; \u03C01=",payprop[X],"*(\u03C02)") , cex.main=0.8)

mtext("timestep", side = 1, outer = TRUE, cex = 2, line = 4)
mtext("frequency of trait in population", side = 2, outer = TRUE, cex = 2, line = 5)
op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
legend(0.05,-.15, # Find suitable coordinates by trial and error
  c("behavior A; payoff=10", "behavior B; payoff=9"), lty=c(1,3), lwd=2, col=c("orange", "blue"), box.col=NA, cex=1.25)


xlabs=c("A=1 , B=0" , "A=0.7 , B=0.3" , "A=0.5 , B=0.5" , "A=0.3 , B=0.7" , "A=0 , B=1")
dev.off()
