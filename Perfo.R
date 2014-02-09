#=========================================================================#
# Function Perfo - Compute performances                                   #
#=========================================================================#
Perfo = function(obs,obs_veille,prev) {
	Len_obs = length(obs)
	R    = cor(obs , prev)
	# VarE = var(prev) / var(obs)
	VarE = 1 - var(prev - obs) / var(obs)
	MSE  = sum((obs - prev)^2) / Len_obs
	MAE  = sum(abs(obs - prev)) / Len_obs
	RMSE = sqrt(sum((obs - prev)^2)/length(obs))
	moy  = mean(obs)
	xxxx = sum((abs(prev - moy) + abs(obs - moy))^2)
	IA   = 1 - sum((obs - prev)^2) / xxxx
	SS = 1 - sum((obs - prev)^2) / sum((obs - obs_veille)^2)
	VarExpl = var(prev) / var(obs)

    cat(' \n')
    cat("********************** \n")
    cat(" Erreurs  en prevsion  \n")
    cat("********************** \n")
    cat(' \n ')
    cat('R      = ', round(R,2),' \n ')
    cat('EV     = ', round(VarE,2),' \n ')
    cat('MAE    = ', round(MAE,2),' \n ')
    cat('RMSE   = ', round(RMSE,2),' \n ')
    cat('IA     = ', round(IA,2),' \n ')
    cat('SS     = ', round(SS,2),' \n ')
    cat(' \n')
}
