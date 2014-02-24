#=========================================================================#
# Function Perfopm10 - Compute performances                                   #
#=========================================================================#
Perfopm10 = function(obs,prev) {
	Len_obs = length(obs)
	R    = cor(obs , prev)
	VarE = 1 - var(prev - obs) / var(obs)
	MSE  = sum((obs - prev)^2) / Len_obs
	MAE  = sum(abs(obs - prev)) / Len_obs
	RMSE = sqrt(sum((obs - prev)^2)/length(obs))
	moy  = mean(obs)
	xxxx = sum((abs(prev - moy) + abs(obs - moy))^2)

    cat(' \n')
    cat("********************** \n")
    cat(" Erreurs  en prevision  \n")
    cat("********************** \n")
    cat(' \n ')
    cat('R      = ', round(R,2),' \n ')
    cat('EV     = ', round(VarE,2),' \n ')
    cat('MAE    = ', round(MAE,2),' \n ')
    cat('RMSE   = ', round(RMSE,2),' \n ')
    cat(' \n')
}
