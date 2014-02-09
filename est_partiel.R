# TEst
# Estimation par backfitting dans les modèles additifs non linéaires
library(nlme)
library(mgcv)

### Etude par Simulation
# fonctions
f1 = function(x){
	f1 = x^2 - 1
}

f2 = function(x){
	f2 = x/2
}

# Simulation
n = 1000
sig2 = 0.25
mu = 0
x1 = rnorm(n, mean=0, sd=1)
x2 = rnorm(n, mean=0, sd=1)

y = mu + f1(x1) + f2(x2) + rnorm(n, mean=0, sd=sqrt(sig2))

# Estimation
modgam <- gam(y ~ s(x1) + s(x2))
summary(modgam)
par(mfrow=c(2, 1))
plot(modgam,scale=0,select=1,shade=TRUE, shade.col="cyan")
plot(modgam,scale=0,select=2,shade=TRUE, shade.col="cyan")

par(mfrow=c(3,2))
hist(x1)
plot(x1, x2)
abline(h=quantile(x2, 0.25))
abline(h=quantile(x2, 0.5))
abline(h=quantile(x2, 0.75))


x2_b = NULL
x1_b = NULL
y_b = NULL

x2_mb = NULL
x1_mb = NULL
y_mb = NULL

x2_mh = NULL
x1_mh = NULL
y_mh = NULL

x2_h = NULL
x1_h = NULL
y_h = NULL

b = 1
mb = 1
mh = 1
h = 1
for(i in 1:length(x2)){
	if(x2[i] < quantile(x2, 0.25)){
		x2_b[j] = x2[i]
		x1_b[j] = x1[i]
		y_b[j] = y[i]
		j = j + 1
	}
	if(x2[i] > quantile(x2, 0.25) & x2[i] < quantile(x2, 0.5)){
		x2_mb[mb] = x2[i]
		x1_mb[mb] = x1[i]
		y_mb[mb] = y[i]
		mb = mb + 1
	}
	if(x2[i] > quantile(x2, 0.5) & x2[i] < quantile(x2, 0.75)){
		x2_mh[mh] = x2[i]
		x1_mh[mh] = x1[i]
		y_mh[mh] = y[i]
		mh = mh + 1
	}
	if(x2[i] > quantile(x2, 0.75)){
		x2_h[h] = x2[i]
		x1_h[h] = x1[i]
		y_h[h] = y[i]
		h = h + 1
	}
}

modgam_b <- gam(y_b ~ s(x1_b) +s(x2_b))
modgam_mb <- gam(y_mb ~ s(x1_mb) +s(x2_mb))
modgam_mh <- gam(y_mh ~ s(x1_mh) +s(x2_mh))
modgam_h <- gam(y_h ~ s(x1_h) +s(x2_h))
plot(modgam_b,scale=0,select=1,shade=TRUE, shade.col="cyan")
plot(modgam_mb,scale=0,select=1,shade=TRUE, shade.col="cyan")
plot(modgam_mh,scale=0,select=1,shade=TRUE, shade.col="cyan")
plot(modgam_h,scale=0,select=1,shade=TRUE, shade.col="cyan")






#### Partial estimation
# 9 regions de proba 1/9
patial = function(x1, x2, x3, y){

n = length(x1)
x1r = matrix(data=NA, nrow=n, ncol=9)
x2r = matrix(data=NA, nrow=n, ncol=9)
x3r = matrix(data=NA, nrow=n, ncol=9)
yr = matrix(data=NA, nrow=n, ncol=9)

ir = c(1,1,1,1,1,1,1,1,1)

for(i in 1:n){
	# Which region
	if(x1[i] < quantile(x1, 1/3)){
		if(x2[i] < quantile(x2, 1/3)){
			region = 1
		}
		if(x2[i] > quantile(x2, 1/3) & x2[i] < quantile(x2, 2/3)){
			region = 2
		}
		if(x2[i] > quantile(x2, 2/3)){
			region = 3
		}
	}
	if(x1[i] > quantile(x1, 1/3) & x1[i] < quantile(x1, 2/3)){
		if(x2[i] < quantile(x2, 1/3)){
			region = 4
		}
		if(x2[i] > quantile(x2, 1/3) & x2[i] < quantile(x2, 2/3)){
			region = 5
		}
		if(x2[i] > quantile(x2, 2/3)){
			region = 6
		}
	}
	if(x1[i] > quantile(x1, 2/3)){
		if(x2[i] < quantile(x2, 1/3)){
			region = 7
		}
		if(x2[i] > quantile(x2, 1/3) & x2[i] < quantile(x2, 2/3)){
			region = 8
		}
		if(x2[i] > quantile(x2, 2/3)){
			region = 9
		}
	}
	
	# Affect
	x1r[ir[region], region] = x1[i]
	x2r[ir[region], region] = x2[i]
	x3r[ir[region], region] = x3[i]
	yr[ir[region], region] = y[i]
	ir[region] = ir[region] + 1
}

# Estimation partielle
modgam_1 <- gam(yr[1:ir[1]-1,1] ~ s(x1r[1:ir[1]-1,1]) + s(x2r[1:ir[1]-1,1]) +s(x3r[1:ir[1]-1,1]))
modgam_2 <- gam(yr[1:ir[2]-1,2] ~ s(x1r[1:ir[2]-1,2]) + s(x2r[1:ir[2]-1,2]) +s(x3r[1:ir[2]-1,2]))
modgam_3 <- gam(yr[1:ir[3]-1,3] ~ s(x1r[1:ir[3]-1,3]) + s(x2r[1:ir[3]-1,3]) +s(x3r[1:ir[3]-1,3]))
modgam_4 <- gam(yr[1:ir[4]-1,4] ~ s(x1r[1:ir[4]-1,4]) + s(x2r[1:ir[4]-1,4]) +s(x3r[1:ir[4]-1,4]))
modgam_5 <- gam(yr[1:ir[5]-1,5] ~ s(x1r[1:ir[5]-1,5]) + s(x2r[1:ir[5]-1,5]) +s(x3r[1:ir[5]-1,5]))
modgam_6 <- gam(yr[1:ir[6]-1,6] ~ s(x1r[1:ir[6]-1,6]) + s(x2r[1:ir[6]-1,6]) +s(x3r[1:ir[6]-1,6]))
modgam_7 <- gam(yr[1:ir[7]-1,7] ~ s(x1r[1:ir[7]-1,7]) + s(x2r[1:ir[7]-1,7]) +s(x3r[1:ir[7]-1,7]))
modgam_8 <- gam(yr[1:ir[8]-1,8] ~ s(x1r[1:ir[8]-1,8]) + s(x2r[1:ir[8]-1,8]) +s(x3r[1:ir[8]-1,8]))
modgam_9 <- gam(yr[1:ir[9]-1,9] ~ s(x1r[1:ir[9]-1,9]) + s(x2r[1:ir[9]-1,9]) +s(x3r[1:ir[9]-1,9]))

modgam <- gam(y ~ s(x1) + s(x2) + s(x3))

# Plots
par(mfrow=c(4,3))
hist(x3)

plot(x1, x2)
abline(v=quantile(x1, 1/3), col="red")
abline(v=quantile(x1, 2/3), col="red")
abline(h=quantile(x2, 1/3), col="red")
abline(h=quantile(x2, 2/3), col="red")

plot(modgam,scale=0,select=3,shade=TRUE, lwd=2, col="red" )

plot(modgam_1,scale=0,select=3,shade=TRUE, shade.col="cyan")
plot(modgam_2,scale=0,select=3,shade=TRUE, shade.col="cyan")
plot(modgam_3,scale=0,select=3,shade=TRUE, shade.col="cyan")

plot(modgam_4,scale=0,select=3,shade=TRUE, shade.col="cyan")
plot(modgam_5,scale=0,select=3,shade=TRUE, shade.col="cyan")
plot(modgam_6,scale=0,select=3,shade=TRUE, shade.col="cyan")

plot(modgam_7,scale=0,select=3,shade=TRUE, shade.col="cyan")
plot(modgam_8,scale=0,select=3,shade=TRUE, shade.col="cyan")
plot(modgam_9,scale=0,select=3,shade=TRUE, shade.col="cyan")
}














