#=========================================================================#
# Function TabDep - Create dependency tables                              #
#=========================================================================#
TabDep = function(obs,prev,ThrLow,ThrHigh,Thresh) {
    Dep_11 = sum((obs < ThrLow) & (prev < ThrLow))
    Dep_12 = sum((obs < ThrLow) & (prev >= ThrLow) & (prev < ThrHigh))
    Dep_13 = sum((obs < ThrLow) & (prev >= ThrHigh))
    Dep_21 = sum((obs >= ThrLow) & (obs < ThrHigh) & (prev < ThrLow))
    Dep_22 = sum((obs >= ThrLow) & (obs < ThrHigh) & (prev >= ThrLow) & 
                                                   (prev < ThrHigh))
    Dep_23 = sum((obs >= ThrLow) & (obs < ThrHigh) & (prev >= ThrHigh))
    Dep_31 = sum((obs >= ThrHigh) & (prev < ThrLow))
    Dep_32 = sum((obs >= ThrHigh) & (prev >= ThrLow) & (prev < ThrHigh))
    Dep_33 = sum((obs >= ThrHigh) & (prev >= ThrHigh))

    cat("**************************\n")
    cat(" Tableau des depassements \n")
    cat("**************************\n")

    cat(' \n')
    cat('         Niveau 0','Niveau 1','Niveau 2',' \n ')
    cat('Niveau 0 ', Dep_11,'    ',Dep_12,'    ',Dep_13,' \n ')
    cat('Niveau 1 ', Dep_21,'    ',Dep_22,'    ',Dep_23,' \n ')
    cat('Niveau 2 ', Dep_31,'    ',Dep_32,'    ',Dep_33,' \n ')
    
    D     = sum((obs < Thresh)  & (prev < Thresh))
    A     = sum((obs >= Thresh) & (prev >= Thresh))
    B     = sum((obs >= Thresh) & (prev < Thresh))
    C     = sum((obs < Thresh)  & (prev >= Thresh))

    cat(' \n')
    cat(' \n')
    cat("***************************\n")
    cat(" Performances en prevsion  \n")
    cat("***************************\n")

    cat(' \n ')
    cat('POD = ', round(A/(A+B),2), ' \n ')
    cat('FAR = ', round(C/(A+C),2), ' \n ')
    cat('TS  = ', round(A/(A+B+C),2), ' \n ')
    cat('SI  = ', round(A/(A+B) + D/(D+C) - 1,2), ' \n ')
    cat(' \n')

}
