setwd("C:/Users/ASUS/Downloads/R Practical")
load("killersandmotives.Rdata")
killersandmotives
createsample(72)
mysample
dim(mysample)
write.csv(mysample,file = 'C:/Users/ASUS/Downloads/R Practical/Mysample.csv',row.names = FALSE)
mean(mysample$AgeFirstKill)
mean(killersandmotives$AgeFirstKill)
sd(mysample$AgeFirstKill)
sd(killersandmotives$AgeFirstKill)
var(killersandmotives$AgeFirstKill)
var(mysample$AgeFirstKill)

sum(killersandmotives$AgeFirstKill == 99999)
sum(mysample$AgeFirstKill == 99999)

motive_NA_killers <- killersandmotives[killersandmotives$Motive == 'NA',]
dim(motive_NA_killers)
motive_NA <- mysample[mysample$Motive == 'NA',]
dim(motive_NA)

sum(killersandmotives$YearBorn < 1900)
sum(mysample$YearBorn <1900)

sum((mysample$YearBorn + mysample$AgeFirstKill)<1900)

bar <- subset(mysample, ((mysample$YearBorn + mysample$AgeFirstKill) < 1900))
bar

#Filtering of data
testDB <- subset(mysample,!(mysample$AgeFirstKill == 99999))
testDB <- subset(testDB, !(testDB$Motive == 'NA'))
testDB <- subset(testDB,!((testDB$YearBorn + testDB$AgeFirstKill) < 1900))
dim(testDB)

#Add variable 'CareerDuartion' to the dataset -defined as number of years between first and last kill.
y <- testDB$AgeLastKill - testDB$AgeFirstKill
testDB$CareerDuration <- y
dim(testDB)
testDB

#write to csv - for ref
write.csv(testDB,file = 'C:/Users/ASUS/Downloads/R Practical/newsample.csv',row.names = FALSE)

#Data eXPLORATION
#Age first kill
agefirstkill <- testDB$AgeFirstKill
mean(agefirstkill)
sd(agefirstkill)
max(agefirstkill)
min(agefirstkill)
quantile(agefirstkill)
boxplot(agefirstkill)

#age last kill
agelastkill <- testDB$AgeLastKill
mean(agelastkill)
sd(agelastkill)
max(agelastkill)
min(agelastkill)
quantile(agelastkill, type = 1)
boxplot(agelastkill)

careerduration <- testDB$CareerDuration
mean(careerduration)
sd(careerduration)
max(careerduration)
min(careerduration)
quantile(careerduration, type = 1)
boxplot(careerduration)

table(testDB$YearBorn)

table(testDB$Motive)
table(testDB$Sex)
table(testDB$Race)
table(testDB$Sentence)
table((testDB$InsanityPlea))

hist(agefirstkill)
hist(agelastkill)
hist(careerduration)

par(mfrow = c(1, 1))
hist(agefirstkill)
hist(agelastkill)
hist(careerduration)

str(testDB)
plot(testDB$AgeFirstKill, testDB$AgeLastKill)
cor(testDB$AgeFirstKill, testDB$AgeLastKill)
plot(testDB$AgeFirstKill,testDB$CareerDuration)
cor(testDB$AgeFirstKill,testDB$CareerDuration)
plot(testDB$AgeLastKill,testDB$CareerDuration)
cor(testDB$AgeLastKill,testDB$CareerDuration)

par(mfrow = c(1, 4))
#Exploring relation with other variables
plot(testDB$AgeFirstKill,testDB$Motive)
plot(testDB$AgeFirstKill,testDB$InsanityPlea)
plot(testDB$AgeFirstKill,testDB$Sex)
plot(testDB$AgeFirstKill,testDB$Race)

plot(testDB$AgeLastKill,testDB$Motive)
plot(testDB$AgeLastKill,testDB$InsanityPlea)
plot(testDB$AgeLastKill,testDB$Sex)
plot(testDB$AgeLastKill,testDB$Race)