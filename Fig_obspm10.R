#=========================================================================#
# Function Fig_obspm10 - Create and display figure                        #
#=========================================================================#
Fig_obspm10 = function(obs,prev,titlemain,FileName) {
    RMSE = round(sqrt(sum((obs - prev)^2)/length(obs)),2)
    EV   = round(1 - var(prev-obs)/var(obs), 2)
    plot(obs,prev, xlab="PM10 observés", ylab="PM10 estimés",
    main = titlemain, cex=1.5, cex.lab=1.6, cex.main = 1.7,
    cex.axis=1.5, xlim=c(0,90), ylim=c(0,90))
    abline(0,1,h=c(30,50),v=c(30,50),col=2,lwd=2) 
    legend('topleft',legend=c(paste("EV   ",EV),paste("RMSE ",RMSE)),cex=1.2)

    jpeg(FileName)
    plot(obs,prev, xlab="PM10 observés", ylab="PM10 estimés",
    main = titlemain, cex=1.5, cex.lab=1.6, cex.main = 1.7,
    cex.axis=1.5, xlim=c(0,90), ylim=c(0,90))
    abline(0,1,h=c(30,50),v=c(30,50),col=2,lwd=2) 
    legend('topleft',legend=c(paste("EV   ",EV),paste("RMSE ",RMSE)),cex=1.2)
     XXX= dev.off()
}

