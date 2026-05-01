
E <- matrix(0, nrow=1+nrow(elo), ncol=nrow(summ))
X <- matrix(0, nrow=1+nrow(elo), ncol=1)

summ.x <- summ[order(summ$index),]
summ.x$pre <- 0 * (summ.x$W - summ.x$L) + 0
E[1,] <- summ.x$pre

K <- 42

for (i in 1:nrow(elo)) {

j1 <- elo$index.x[i]
j2 <- elo$index.y[i]

X[i,1] <- ifelse(E[i,j1]==E[i,j2],0.5,ifelse( 
		(E[i,j1]>E[i,j2] & elo$win[i]==1) | (E[i,j1]<E[i,j2] & elo$win[i]==0),1,0))

We <- 1 / (10 ** (-1 * ( E[i,j1] - E[i,j2] ) / 400) + 1)
P  <- round ( K * (elo$win[i] - We), digits=1)

E[i,j1] <- E[i,j1] + P
E[i,j2] <- E[i,j2] - P

E[i+1,] <- E[i,]

print(paste(elo$team[i],"-",elo$opp[i],elo$rf[i],"-",elo$ra[i],E[i,j1],E[i,j2],P))

}

rating <- as.data.frame(E[nrow(elo),])
rating$index <- rownames(rating)

summ.x <- merge(summ.x[c("myname","W","L","index")], rating, by="index")
summ.x[order(-summ.x$"E[nrow(elo), ]"),]

mean(X[,1])*100