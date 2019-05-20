
rm(list = setdiff(ls(), lsf.str())) # remove all variables except for functions
par(mfrow=c(1,3))

# 2
# Import the file of 2007 and 2017 with read.csv
d2007 <- read.csv(file= "/Users/Cecile/Documents/ISEP/A11/ProbStat/dJul07_62.csv", header = TRUE)
d2012 <- read.csv(file ="/Users/Cecile/Documents/ISEP/A11/ProbStat/dJul12_62_groupe3.csv", header = TRUE)
d2017 <- read.csv(file= "/Users/Cecile/Documents/ISEP/A11/ProbStat/dJul17_62.csv", header = TRUE)

# Delete error data by hand (or in excel directly)
# d2007 <- d2007[-c(63),]
# d2017 <- d2017[-c(53),]

# Delete error by detection 
d2007=d2007[which(d2007$Temp¨¦rature<=150),]
d2012=d2012[which(d2012$Temp¨¦rature<=150),] # normally there is no error, just in case
d2017=d2017[which(d2017$Temp¨¦rature<=150),]

# Obtain data by month

d2007Juillet <- d2007[c(1:31),]
d2007Aout <- d2007[-c(1:31),]

d2017Juillet <- d2017[c(1:31),]
d2017Aout <- d2017[c(32:62),]

d2012Juillet <- d2012[c(1:31),]
d2012Aout <- d2012[c(32:62),]

# 2.1
# Summary of year

sum2007 = summary(d2007)[c(1:6),2]
sum2007 = c(sum2007,paste("Variance:",round(var(d2007[,2]),2)))
sum2007 = c(sum2007,paste("Ecart Type:",round(sd(d2007[,2]),2)))
sum2007 = c(sum2007,paste("Mode:",getmode(d2007[,2])))
sum2007

sum2012 = summary(d2012)[c(1:6),2]
sum2012 = c(sum2012,paste("Variance:",round(var(d2012[,2]),2)))
sum2012 = c(sum2012,paste("Ecart Type:",round(sd(d2012[,2]),2)))
sum2012 = c(sum2012,paste("Mode:",getmode(d2012[,2])))
sum2012

sum2017 = summary(d2017)[c(1:6),2]
sum2017 = c(sum2017,paste("Variance:",round(var(d2017[,2]),2)))
sum2017 = c(sum2017,paste("Ecart Type:",round(sd(d2017[,2]),2)))
sum2017 = c(sum2017,paste("Mode:",getmode(d2017[,2])))
sum2017

sumAnnee<-data.frame(sum2007,sum2012,sum2017)
sumAnnee

# Summary Juillet

sum2007Juillet = summary(d2007Juillet)[c(1:6),2]
sum2007Juillet = c(sum2007Juillet,paste("Variance:",round(var(d2007Juillet[,2]),2)))
sum2007Juillet = c(sum2007Juillet,paste("Ecart Type:",round(sd(d2007Juillet[,2]),2)))
sum2007Juillet = c(sum2007Juillet,paste("Mode:",getmode(d2007Juillet[,2])))
sum2007Juillet

sum2012Juillet = summary(d2012Juillet)[c(1:6),2]
sum2012Juillet = c(sum2012Juillet,paste("Variance:",round(var(d2012Juillet[,2]),2)))
sum2012Juillet = c(sum2012Juillet,paste("Ecart Type:",round(sd(d2012Juillet[,2]),2)))
sum2012Juillet = c(sum2012Juillet,paste("Mode:",getmode(d2012Juillet[,2])))
sum2012Juillet

sum2017Juillet = summary(d2017Juillet)[c(1:6),2]
sum2017Juillet = c(sum2017Juillet,paste("Variance:",round(var(d2017Juillet[,2]),2)))
sum2017Juillet = c(sum2017Juillet,paste("Ecart Type:",round(sd(d2017Juillet[,2]),2)))
sum2017Juillet = c(sum2017Juillet,paste("Mode:",getmode(d2017Juillet[,2])))
sum2017Juillet

sumJuillet<-data.frame(sum2007Juillet, sum2012Juillet, sum2017Juillet)
sumJuillet

# Summary Aout
sum2007Aout = summary(d2007Aout)[c(1:6),2]
sum2007Aout = c(sum2007Aout,paste("Variance:",round(var(d2007Aout[,2]),2)))
sum2007Aout = c(sum2007Aout,paste("Ecart Type:",round(sd(d2007Aout[,2]),2)))
sum2007Aout = c(sum2007Aout,paste("Mode:",getmode(d2007Aout[,2])))

sum2012Aout = summary(d2012Aout)[c(1:6),2]
sum2012Aout = c(sum2012Aout,paste("Variance:",round(var(d2012Aout[,2]),2)))
sum2012Aout = c(sum2012Aout,paste("Ecart Type:",round(sd(d2012Aout[,2]),2)))
sum2012Aout = c(sum2012Aout,paste("Mode:",getmode(d2012Aout[,2])))

sum2017Aout = summary(d2017Aout)[c(1:6),2]
sum2017Aout = c(sum2017Aout,paste("Variance:",round(var(d2017Aout[,2]),2)))
sum2017Aout = c(sum2017Aout,paste("Ecart Type:",round(sd(d2017Aout[,2]),2)))
sum2017Aout = c(sum2017Aout,paste("Mode:",getmode(d2017Aout[,2])))

sumAout<-data.frame(sum2007Aout, sum2012Aout, sum2017Aout)
sumAout

# plot the tempratures
# A little bug for the dates, but no influence on data. 
# Year
plot(d2007, type="p", main="Temp¨¦rature 2007, 2012, 2017")
points(d2012, col="blue")
points(d2017, col="green")
legend(1, 87, legend=c("2007", "2012", "2017"),col=c("black", "blue", "green"), lty=1:2, cex=0.8)

# Juillet
plot(d2007Juillet, type="p", main="Temp¨¦rature Juillet 2007, 2012, 2017")
points(d2012Juillet, col="blue")
points(d2017Juillet, col="green")
legend(1, 87, legend=c("07Juillet", "12Juillet", "17Juillet"),col=c("black", "blue", "green"), lty=1:2, cex=0.8)

# Aout
plot(d2007Aout, type="p", main="Temp¨¦rature Aout 2007, 2012, 2017")
points(d2012Aout, col="blue")
points(d2017Aout, col="green")
legend(1, 84, legend=c("07Aout", "12Aout", "17Aout"),col=c("black", "blue", "green"), lty=1:2, cex=0.8)

# Boxplot
# boxplot year
boxplot(d2007$Temp¨¦rature, d2012$Temp¨¦rature, d2017$Temp¨¦rature, main="Temp¨¦rature ann¨¦e", 
        names=c("2007","2012","2017"),col=c("purple","blue","green"))

# boxplot Juillet
boxplot(d2007Juillet$Temp¨¦rature, d2012Juillet$Temp¨¦rature, d2017Juillet$Temp¨¦rature, main="Temp¨¦rature Juillet", 
        names=c("2007Juillet","2012Juillet","2017Juillet"),col=c("purple","blue","green"))

# boxplot Aout
boxplot(d2007Aout$Temp¨¦rature, d2012Aout$Temp¨¦rature, d2017Aout$Temp¨¦rature, main="Temp¨¦rature Aout", 
        names=c("2007Aout","2012Aout","2017Aout"),col=c("purple","blue","green"))

# Histogram year

hist(d2007[,2], breaks=30, freq=FALSE, main= "Histogram of 2007",xlab="Temperature")
lines(density(d2007[,2]),col = "black")
hist(d2012[,2], breaks=30, freq=FALSE, main= "Histogram of 2012",xlab="Temperature")
lines(density(d2012[,2]),col = "black")
hist(d2017[,2], breaks=30, freq=FALSE, main= "Histogram of 2017",xlab="Temperature")
lines(density(d2017[,2]),col = "black")

# Histogram juillet

hist(d2007Juillet[,2], breaks=30, freq=FALSE, main= "Histogram of juillet 2007",xlab="Temperature")
lines(density(d2007Juillet[,2]),col = "black")
hist(d2012Juillet[,2], breaks=30, freq=FALSE, main= "Histogram of juillet 2012",xlab="Temperature")
lines(density(d2012Juillet[,2]),col = "black")
hist(d2017Juillet[,2], breaks=30, freq=FALSE, main= "Histogram of juillet 2017",xlab="Temperature")
lines(density(d2017Juillet[,2]),col = "black")

# Histogram aout

hist(d2007Aout[,2], breaks=30, freq=FALSE, main= "Histogram of aout 2007",xlab="Temperature")
lines(density(d2007Aout[,2]),col = "black")
hist(d2012Aout[,2], breaks=30, freq=FALSE, main= "Histogram of aout 2012",xlab="Temperature")
lines(density(d2012Aout[,2]),col = "black")
hist(d2017Aout[,2], breaks=30, freq=FALSE, main= "Histogram of aout 2017",xlab="Temperature")
lines(density(d2017Aout[,2]),col = "black")

# 2.2
# Estimation ponctel de l'esperance

Y2007= d2007[,2]
Y2017= d2017[,2]
Y2012= d2012[,2]
size2007=length(Y2007)
size2012=length(Y2012)
size2017=length(Y2017)

J2007= d2007[c(1:31),2]
J2012= d2012[c(1:31),2]
J2017= d2017[c(1:31),2]
sizeJ2007=length(J2007)
sizeJ2012=length(J2012)
sizeJ2017=length(J2017)

A2007= d2007[c(32:62),2]
A2012= d2012[c(32:62),2]
A2017= d2017[c(32:62),2]
sizeA2007=length(A2007)
sizeA2012=length(A2012)
sizeA2017=length(A2017)

sum=sum(Y2007)
Esperance2007=sum/size2007
Esperance2007

sum7=sum(Y2012)
Esperance2012= sum7/size2012
Esperance2012

sum2=sum(Y2017)
Esperance2017=sum2/size2017
Esperance2017

sum3=sum(J2007)
EsperanceJ2007=sum3/sizeJ2007
EsperanceJ2007


sum4=sum(J2017)
EsperanceJ2017=sum4/sizeJ2017
EsperanceJ2017


sum5=sum(A2007)
EsperanceA2007=sum5/sizeA2007
EsperanceA2007

sum6=sum(A2017)
EsperanceA2017=sum6/sizeA2017
EsperanceA2017

sum8=sum(J2012)
EsperanceJ2012= sum8/sizeJ2012
EsperanceJ2012

sum9=sum(A2012)
EsperanceA2012= sum9/sizeA2012
EsperanceA2012

# 2.3
# Intervalle de confiance year
# Intervalle de confiance 2007
t.test(d2007[,2], conf.level = 0.95)$conf.int
t.test(d2007[,2], conf.level = 0.99)$conf.int

# La facon manuelle 
# alpha = 5%
Intervalle_de_confiance_2007_95 = (mean(d2007[,2])- 1.96 *(sd(d2007[,2])/sqrt(62))) # min
Intervalle_de_confiance_2007_95 = paste(Intervalle_de_confiance_2007,(mean(d2007[,2])+ 1.96 *(sd(d2007[,2])/sqrt(62)))) # max
Intervalle_de_confiance_2007_95
# alpha = 1%
Intervalle_de_confiance_2007_99 = (mean(d2007[,2])- 2.576 *(sd(d2007[,2])/sqrt(62))) # min
Intervalle_de_confiance_2007_99 = paste(Intervalle_de_confiance_2007,(mean(d2007[,2])+ 2.576 *(sd(d2007[,2])/sqrt(62)))) # max
Intervalle_de_confiance_2007_99

# Intervalle de confiance 2012
t.test(d2012[,2], conf.level = 0.95)$conf.int
t.test(d2012[,2], conf.level = 0.99)$conf.int

# Intervalle de confiance 2017
t.test(d2017[,2], conf.level = 0.95)$conf.int
t.test(d2017[,2], conf.level = 0.99)$conf.int

#Interavlle de confiance pour chaque mois
#Juillet
#intervalle pour juillet 2007
t.test(d2007Juillet[,2], conf.level = 0.95)$conf.int
t.test(d2007Juillet[,2], conf.level = 0.99)$conf.int

#intervalle pour juillet 2012
t.test(d2012Juillet[,2], conf.level = 0.95)$conf.int
t.test(d2012Juillet[,2], conf.level = 0.99)$conf.int

#intervalle pour juillet 2017
t.test(d2017Juillet[,2], conf.level = 0.95)$conf.int
t.test(d2017Juillet[,2], conf.level = 0.99)$conf.int

#Aout
#intervalle pour Aout 2007
t.test(d2007Aout[,2], conf.level = 0.95)$conf.int
t.test(d2007Aout[,2], conf.level = 0.99)$conf.int

#intervalle pour Aout 2012
t.test(d2012Aout[,2], conf.level = 0.95)$conf.int
t.test(d2012Aout[,2], conf.level = 0.99)$conf.int

#intervalle pour Aout 2017
t.test(d2017Aout[,2], conf.level = 0.95)$conf.int
t.test(d2017Aout[,2], conf.level = 0.99)$conf.int

# 2.4
# tests d'hypothese 
# 2007 et 2017
testhypothese07_17 <- t.test(d2007[,2], mu=Esperance2017)
testhypothese07_17
testhypothese17_07 <- t.test(d2017[,2], mu=Esperance2007)
testhypothese17_07

# 2007 et 2012
testhypothese07_12 <- t.test(d2007[,2], mu=Esperance2012)
testhypothese07_12
testhypothese12_07 <- t.test(d2012[,2], mu=Esperance2007)
testhypothese12_07

# 2012 et 2017
testhypothese17_12 <- t.test(d2017[,2], mu=Esperance2012)
testhypothese17_12
testhypothese12_17 <- t.test(d2012[,2], mu=Esperance2017)
testhypothese12_17

#2.5
# test de comparaison
t.test(d2007[,2],d2012[,2])
t.test(d2007[,2],d2017[,2])
t.test(d2012[,2],d2017[,2])


# Conclusion
# Test de normality
shapiro.test(d2007[,2]) 
shapiro.test(d2012[,2]) 
shapiro.test(d2017[,2]) 

shapiro.test(d2007Juillet[,2]) 
shapiro.test(d2012Juillet[,2]) 
shapiro.test(d2017Juillet[,2]) 

shapiro.test(d2007Aout[,2]) 
shapiro.test(d2012Aout[,2]) 
shapiro.test(d2017Aout[,2]) 

# Test non parametric
wilcox.test(d2007[,2], d2017[,2])
wilcox.test(d2007[,2], d2012[,2])
wilcox.test(d2012[,2], d2017[,2])


#functions
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



