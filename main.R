# Welcome to the r-file of Group 10
# The code has the following structure :
# lines 12-17 : importing different libraries, and the dataset
# lines 19-34 : defining useful functions for later
# lines 38-81 : data cleaning
# lines 85-150 : determining the model for claims frequency
# lines 154-194 : determining the model for claims severity
# lines 198-214 : Value at Risk, simulation
# lines 218-223 : Value at Risk, Collective Risk Model
# lines 227-241 : Proof of Concept, antithetic estimator

install.packages("readr")
install.packages("moments")
library(moments)
library(readr)
set.seed(1)
data <- read_csv2("./DATA_SET_10.csv",na=c("","NA","-"), locale = locale(grouping_mark = " "))

count_claims <- function(column){
  claims <- integer(max(column)+1)
  for(item in column){
    claims[item+1] <- claims[item+1] + 1
  }
  return(claims)
}

length_adjusted <- function(column){
  total <- 0
  for(item in column){
    if(is.na(item) == FALSE){
      total <- total + 1}
  }
  return(total)
}
##########################################################
# DATA CLEANING
##########################################################

#formatting into numerical
for (j in 1:nrow(data)){
  data[j,"PREMIUM"] <- gsub(" ","",data[j,"PREMIUM"])
  data[j,"CLM_AMT_1"] <- gsub(" ","",data[j,"CLM_AMT_1"])
}
data$CLM_AMT_1 <- as.numeric(data$CLM_AMT_1)
data$PREMIUM <- as.numeric(data$PREMIUM)

#first glance at raw data
boxplot(data$CLM_FREQ, horizontal = TRUE)
title(main = "Frequency of claims")
list_claims <- list(data$CLM_AMT_1, data$CLM_AMT_2, data$CLM_AMT_3,
                    data$CLM_AMT_4, data$CLM_AMT_5, data$CLM_AMT_6)
num <- 1
for (clm in list_claims){
  boxplot(clm)
  title(main = paste("Severity of claim", num), ylab = "Claim amount")
  num <- num + 1
}

#removing extremes (could be misinput), removing negatives
data[is.na(data)] <- 0.5
data <- data[data$CLM_AMT_1 > 0 & data$CLM_AMT_1 < 5000,]
data <- data[data$CLM_AMT_2 > 0 & data$CLM_AMT_1 < 5000,]
data <- data[data$CLM_AMT_3 > 0 & data$CLM_AMT_1 < 5000,]
data <- data[data$CLM_AMT_4 > 0 & data$CLM_AMT_1 < 5000,]
data <- data[data$CLM_AMT_5 > 0 & data$CLM_AMT_1 < 5000,]
data <- data[data$CLM_AMT_6 > 0 & data$CLM_AMT_1 < 5000,]
data[data == 0.5] <- NA

list_claims <- list(data$CLM_AMT_1, data$CLM_AMT_2, data$CLM_AMT_3,
                    data$CLM_AMT_4, data$CLM_AMT_5, data$CLM_AMT_6)
boxplot(list_claims)
title(main = "Severity of claims", xlab = "Claim number", ylab = "Claim amount")
#We will use the iid hypothesis for claim severity
#let's merge all claims in one vector
severity <- c(data$CLM_AMT_1, data$CLM_AMT_2, data$CLM_AMT_3,
            data$CLM_AMT_4, data$CLM_AMT_5, data$CLM_AMT_6)
severity <- na.omit(severity)
hist(severity, breaks = 20, ylim=c(0,275))
avg_data <- mean(severity)
var_data <- var(severity)
skew_data <- skewness(severity)
######################################################
#CLAIMS FREQUENCY
######################################################
frequency <- count_claims(data$CLM_FREQ)
#other candidates do not make sense in this scenario (binomial, hypergeometric, bernoulli)

#POISSON
frequency_poisson <- sum(frequency)*(dpois(0:6, lambda = mean(data$CLM_FREQ)))
plot(x = 0:6, y = frequency_poisson, type = "o",,xlab = "Number of claims",ylab = "Frequency")
title("Poisson Distribution")
chisq.test(frequency, frequency_poisson)

plot(x = 0:6, y = frequency, type="b", col="red",xlab = "number of claims",ylab="frequency", 
     pch=17, ylim=c(0,400), xlim=c(0,6))
lines(x = 0:6, frequency_poisson, type ="b", col="blue", pch=19)
title(main = "Empirical Frequency vs. Poisson Model")
legend("topright",legend=c("Empirical Data","Poisson Distribution"),col=c("red","blue"),
       pch = c(17,19),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F ,
       inset = c(0.1, 0.1))

#GEOMETRIC
frequency_geometric <- sum(frequency)*(dgeom(0:6, prob = 1/mean(data$CLM_FREQ)))
chisq.test(frequency, frequency_geometric)
plot(x=0:6, frequency,type="b", col="red",xlab = "number of claims",ylab="frequency", pch=17, ylim=c(0,400))
lines(x=0:6, frequency_geometric,, type="b",col="blue", pch=19)
title(main = "Empirical Frequency vs. Geometric Model")
legend("topright",legend=c("Empirical Data","Geometric Distribution"),col=c("red","blue"),pch = c(17,19),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F ,
       inset = c(0.1, 0.1))

#NEGATIVE BINOMIALE
p <- ( mean(data$CLM_FREQ) )/( var(data$CLM_FREQ) )
r <- (mean(data$CLM_FREQ))*(p/(1-p))
frequency_negbin <- sum(frequency)*(dnbinom(0:6, size = r, prob = p))
chisq.test(frequency, frequency_negbin)
plot(x = 0:6,y=frequency,type="b", col="red",xlab = "number of claims",ylab="frequency", pch=17, ylim=c(0,400))
lines(x=0:6, y=frequency_negbin,, type="b",col="blue", pch=19)
title(main = "Empirical Frequency vs. Neg Binomial Model")
legend("topright",legend=c("Empirical Data","Neg Binomial Distribution"),col=c("red","blue"),pch = c(17,19),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F ,
       inset = c(0.1, 0.1))

#comparison between negative binomial and poisson
plot(x = 0:6,y=frequency,type="b", col="red",xlab = "number of claims",ylab="frequency", pch=17, ylim=c(0,400),xlim = c(0,6))
lines(x=0:6, y=frequency_negbin,, type="b",col="green", pch=19)
lines(x=0:6,y=frequency_poisson, type="b",col="blue", pch=21)
title(main = "Empirical Frequency vs. Models")
legend("topright",legend=c("Empirical Data","Neg Binomial Distribution","Poisson Distribution"),col=c("red","green","blue"),pch = c(17,19,21),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F ,
       inset = c(0.1, 0.1))

######################################################
#CLAIMS SEVERITY
######################################################
#NORMAL QQ PLOT
plot(qnorm(ppoints(length(sort(severity))), mean = avg_data, sd = sqrt(var_data)), sort(severity),
      xlim=c(550,850), ylim=c(550,850), xlab = "", ylab = "")
abline(a = 0, b = 1, col = "red")
title(main = "Normal QQ-Plot of severity",xlab = "Theoretical Quantiles", ylab = "Empiricail Quantiles")

#GAMMA QQ PLOT
alpha_gamma <- avg_data^2 / var_data
beta_gamma <- avg_data/var_data

plot(qgamma(ppoints(length(sort(severity))), shape = alpha_gamma, rate = beta_gamma), sort(severity),
     xlim=c(550,850), ylim=c(550,850), xlab = "", ylab = "")
abline(a = 0, b = 1, col = "red")
title(main = "Gamma QQ-Plot of severity",xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles")

#histogram comparing severity and distributions
h <- hist(severity, xlab="", ylab="", main="", ylim=c(0,275), breaks = 20)
x_fit <- seq(min(severity),max(severity), length = 100)
y_fit <- dgamma(x_fit,shape = alpha_gamma, rate = beta_gamma)
y_fit_norm <- dnorm(x_fit, avg_data,sqrt(var_data))
y_fit_norm <- y_fit_norm * diff(h$mids[1:2])*length(severity)
y_fit <- y_fit*diff(h$mids[1:2])*length(severity)
lines(x_fit,y_fit, col = "blue",lwd=2,xlab ="",ylab="")
lines(x_fit,y_fit_norm, col="red",lwd=2, xlab ="", ylab = "")
legend("topleft", legend=c("Gamma Density","Normal Density"),col = c("blue","red"),pch = c(17,19),
       bty = "n",
       pt.cex = 2,
       cex = 0.8,
       text.col = "black",
       horiz = F ,
       inset = c(0.1, 0.1))
title(main ="Histogram of data vs. Density",xlab ="Claim severity",ylab="Frequency")

#Plot for Gamma distribution
plot(x_fit,y_fit, xlab = "Severity",ylab =" Frequency", type = "l", main = "Gamma Distribution")

ks.test(severity, "pgamma",alpha_gamma,beta_gamma)
ks.test(severity, "pnorm",mean(severity),sd(severity))


quantile(rgamma(1000000,shape = alpha_gamma,rate=beta_gamma),p=c(0.05,0.95))
#################################################
# Simulating the Value at Risk
#################################################
#Simulate 1000 synthethic portfolios that each give a sum of claim severities (through
# distribution properties), then take
#the appropriate quantile

#We know that a sum of Poissons is a Poisson with the sum of parameters, therefore to generate
#the total claims of a synthethic portfolio we take n * the parameter
total_claims <- rpois(1000000,length(data$CLM_FREQ)*mean(data$CLM_FREQ))

#To simulate the total claim severity, we can use the property of the gamma distribution
#Stating that a sum of Gammas is a Gamma with shape = sum of shapes, and scale = scale
total_severy <- integer(1000000)
for(i in c(1:1000000)){
  total_severy[i] <- rgamma(1,shape = total_claims[i]*alpha_gamma,rate = beta_gamma)
}
hist(total_severy, main ="Sum of severities distribution",xlab = "Sum of severity",ylab = "Frequency")
quantile(total_severy,probs = (1-0.5/100))

#################################################
#Compound Poisson approximation by Normal
#################################################
#For the entire portfolio
avg_claims <- mean(data$CLM_FREQ)*length(data$CLM_FREQ)
var_claims <- var(data$CLM_FREQ)*length(data$CLM_FREQ)
expected_loss <- avg_claims*alpha_gamma/beta_gamma
variance_loss <- (alpha_gamma / beta_gamma^2 )*avg_claims + (alpha_gamma/beta_gamma)^2*var_claims
VaR_theory <- qnorm(1-0.5/100, mean = expected_loss, sd = sqrt(variance_loss))
######################################################
#Variance Reduction through Antithetic Estimator
######################################################
# We believe that half the sample was generated "normally", the other half through the antithetic
# Giving us two vectors : l1 and l2
half <- round(length(data$CLM_FREQ)/2,digits = 0)
l1 <- data$CLM_FREQ[1:half]
l2 <- data$CLM_FREQ[(half+1):length(data$CLM_FREQ)]
mean_frequ_ant <- 1/2 * (mean(l1) + mean(l2))
var_frequ_ant <- 1/2 * var(l1)/(2*half) + 1/(4*half) * cov(l1,l2)
c(sqrt(var_frequ_ant),sqrt(var(data$CLM_FREQ)/(half*2)))

half_sev <- round(length(severity)/2,digits = 0)
s1 <- severity[1:(half_sev-1)]
s2 <- severity[(half_sev+1):(length(severity))]
mean_sev_ant <- 1/2 * (mean(s1) + mean(s2))
var_sev_ant <- 1/2 * var(s1)/(2*half_sev) + 1/(4*half_sev) * cov(s1,s2)
c(sqrt(var_sev_ant), sqrt((alpha_gamma/beta_gamma^2)/length(severity)))