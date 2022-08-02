rm(list = ls())
setwd("/Users/dogdogt/Downloads")
sirdata=read.csv("be sir.csv",header = TRUE)
N=11560000


sirdata$susceptible=N-sirdata$infected-sirdata$recovered

sirdata$date<-ymd(sirdata$date)
sirdata$date <- as.Date(sirdata$date)


R0=vector(mode = "numeric")
beta=vector(mode = "numeric")
gamma=vector(mode = "numeric")
i=1
while (i<=112) {
  N=11560000
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta * I * S/N
      dI <- beta * I * S/N - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  
  sir_start_date <- "2020-04-16"
  
  Infected <- sirdata%>%pull(infected)
  Recovered=sirdata%>%pull(recovered)
  Day <- 1:(length(Infected))
  
  init <- c(S = N - Infected[i]-Recovered[i], I = Infected[i], R = Recovered[i])
  
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[, 3]
    sum((Infected - fit)^2)
  }
  
  Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))
  
  Opt_par <- setNames(Opt$par, c("beta", "gamma"))
  b=Opt_par[["beta"]]
  beta=append(beta,b)
  g=Opt_par[["gamma"]]
  gamma=append(gamma,g)
  R=Opt_par[["beta"]]/Opt_par[["gamma"]]
  R0 <-append(R0, R)
  i=i+1}
sirdata$beta<-beta
sirdata$gamma<-gamma
sirdata$R0<-R0


as.data.frame(sirdata)
write.table(sirdata,file="besirdata.csv",sep = ",")

sirdata2=read.csv("besirdata.csv",header = TRUE)

sirdata2$date<-ymd(sirdata2$date)
sirdata2$date <- as.Date(sirdata2$date)

sirdata2=sirdata2%>%filter(date < ymd("2020-08-01"))
ber0=ggplot(sirdata2,aes(date,R0))+
  geom_line()
ber0

png(filename = "ber0.png",
    width = 13,
    height = 7,
    units = "in",
    res = 300)
ber0
dev.off()
ggplot(sirdata2,aes(date,beta))+
  geom_line()
