##ex 4_8_a
set.seed(1)
bach <- scan("C:/Users/STD/Downloads/codes/codes/menchild30bach.dat")
no_bach <- scan("C:/Users/STD/Downloads/codes/codes/menchild30nobach.dat")

a <- 2; b <-1

n1 <- length(bach); y1 <- sum(bach)
n2 <- length(no_bach); y2 <- sum(no_bach)

theta1.sim <- rgamma(5000, (y1+a), (n1+b))
theta2.sim <- rgamma(5000, (y2+a), (n2+b))

y1.sim <- rpois(5000, theta1.sim)
y2.sim <- rpois(5000, theta2.sim)

pdf("ex4_8_a.pdf",family="Times",height=7,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(2,1))

ds<- 0:11
plot(ds,(table(c(y1.sim,ds))-1)/length(y1.sim), type="h",lwd=3,
     xlab=expression(italic(D==tilde(Y)[1])),
     ylab=expression(paste(italic("p("),gamma,"|",bold(y[1]),",",bold(y[2]),")",sep="")) )
plot(ds,(table(c(y2.sim,ds))-1)/length(y2.sim), type="h",lwd=3,
     xlab=expression(italic(D==tilde(Y)[1])),
     ylab=expression(paste(italic("p("),gamma,"|",bold(y[1]),",",bold(y[2]),")",sep="")) )
dev.off()

theta.diff <- theta2.sim-theta1.sim
y.diff <- y2.sim-y1.sim

## ex 4_8_b
quantile(theta.diff, c(.025,.975))
quantile(y.diff, c(.025,.975))

## ex 4_8_c
ecdf <- dpois(ds,1.4)

pdf("ex4_8_c.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))

plot(ds,(table(c(no_bach,ds))-1)/n2, type="h", lwd=5,
     xlab=expression(italic(D==Y[2])),
     ylab=expression(paste(italic("p("),Y[2],")",sep="")))
points(ds, ecdf,lwd=3,col="grey",type="h")
dev.off()

## ex 4_8_d
n_0=c()
n_1=c()
for(i in 1:5000){
  y2i.sim <- rpois(218, theta2.sim[i])
  n_0 <- c(n_0, sum(y2i.sim==0))
  n_1 <- c(n_1, sum(y2i.sim==1))
}

n_0.exp = sum(no_bach==0)
n_1.exp = sum(no_bach==1)

pdf("ex4_8_d.pdf",family="Times",height=7,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))

plot(n_0, n_1, type="p", ylim=c(45,105),
     xlab=expression("number of people with zero child"),
     ylab=expression("number of people with one child"))
points(n_0.exp, n_1.exp, type="p", col="red")
dev.off()
