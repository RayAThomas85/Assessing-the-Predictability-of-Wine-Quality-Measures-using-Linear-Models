#MATH6357 Project
#Carolina Garza, Michael Saenz, Ray Thomas

#Import the data
labels = c("Fixed Acidity","Volatile Acidity","Citric Acid","Residual Sugar","Chlorides","Free Sulfur Dioxide","Total Sulfur Dioxide","Density","pH","Sulphates","Alcohol","Quality")
whiteWine = read.csv('C:/Users/Caro/Documents/School/MATH 6357/winequality-white.csv', sep = ';', col.names = labels)

summary(whiteWine)

head(whiteWine)

#Explore the data
summary(whiteWine$fixed.acidity)
pairs(whiteWine)
hist(whiteWine$Quality, main = 'White Wine Quality' ,xlab = 'Quality')
qqnorm(whiteWine$Quality)
qqline(whiteWine$Quality)


#Correlation matrix
corrW = cor(whiteWine)
round(corrW, 2)

#Plot most heavily correlated
plot(whiteWine$residual.sugar, whiteWine$density,main = 'Residual Sugar vs Density', xlab = 'Residual Sugar (g/dm3)', ylab = 'Density (g/cm3)')
plot(whiteWine$alcohol, whiteWine$density,main = 'Alcohol vs Density', xlab = 'Alcohol (vol%)', ylab = 'Density (g/cm3)')
plot(whiteWine$free.sulfur.dioxide, whiteWine$total.sulfur.dioxide,main = 'Free Sulfur Dioxide vs Total Sulfur Dioxide', xlab = 'Free Sulfur Dioxide (mg/dm3)', ylab = 'Total Sulfur Dioxide (mg/dm3)')

#Get models for stepwise selection
library(MASS)
full.model = lm(whiteWine$Quality ~ ., data = whiteWine)
base.model = lm(whiteWine$Quality ~ 1, data = whiteWine)

#Try backwards stepwise
step.model = step(full.model, direction = 'backward')
#backwards model acc
pred = predict(step.model)
predR = round(pred)
predR

predRMatrix = predR==whiteWine$Quality
numCorrect = sum(predRMatrix)
acc = numCorrect/length(predRMatrix)*100
acc #gave 51.92%


#Try forwards stepwise
forward.model = step(base.model, direction = 'forward', scope = formula(full.model))


#Forward model Acc
#Prediction is a decimal, so going to round them for the Quality
pred = predict(forward.model)
predR = round(pred)
predR

predRMatrix = predR==whiteWine$Quality
numCorrect = sum(predRMatrix)
acc = numCorrect/length(predRMatrix)*100
acc #Gave 51.92%


#Try both stepwise
both.model = step(base.model, direction = 'both', scope = formula(full.model))

#Both model Acc
pred = predict(both.model)
predR = round(pred)
predR

predRMatrix = predR==whiteWine$Quality
numCorrect = sum(predRMatrix)
acc = numCorrect/length(predRMatrix)*100
acc #Gave 51.92%


#Summary of lm model
summary(step.model)
step.model$anova

#Boxplots of final features
data = cbind(whiteWine$Fixed.Acidity,whiteWine$Alcohol)
boxplot.matrix(data, names = c('Fixed Acidity', 'Alcohol'))
data2 = cbind(whiteWine$Free.Sulfur.Dioxide )
boxplot.matrix(data2, xlab = 'Free Sulfur Dioxide')
data3 = cbind(whiteWine$Volatile.Acidity,whiteWine$Sulphates)
boxplot.matrix(data3, names = c('Volatile Acidity', 'Sulphates'))
data4 = cbind(whiteWine$pH)
boxplot.matrix(data4, xlab = 'pH')
boxplot(whiteWine$Density, xlab = 'Density')
boxplot(whiteWine$Residual.Sugar, xlab = 'Residual Sugar')

#Fixed Acidity, Free Sulfur Dioxide, Volatile Acidity, Suplhates, pH have a lot of outliers 
#Calcluations to get number of outliers
outValsFixedAcid = boxplot(whiteWine$Fixed.Acidity, plot=FALSE)$out
length(outValsFixedAcid)

outValsVolAcid = boxplot(whiteWine$Volatile.Acidity, plot=FALSE)$out
length(outValsVolAcid)

outValsFreeSul = boxplot(whiteWine$Free.Sulfur.Dioxide, plot=FALSE)$out
length(outValsFreeSul)

outValsSul= boxplot(whiteWine$Sulphates, plot=FALSE)$out
length(outValsSul)

outValspH = boxplot(whiteWine$pH, plot=FALSE)$out
length(outValspH)

outValsAlc = boxplot(whiteWine$Alcohol, plot=FALSE)$out
length(outValsAlc)

outValsResidSug = boxplot(whiteWine$Residual.Sugar, plot=FALSE)$out
length(outValsResidSug)

outValsDensity = boxplot(whiteWine$Density, plot=FALSE)$out
length(outValsDensity)


#Maybe try truncate instead of rounding?
predT = trunc(pred)
predT

predTMatrix = predT==whiteWine$Quality
numCorrectT = sum(predTMatrix)
accT = numCorrect/length(predTMatrix)*100
accT #Didnt help




#Square our model to get 2nd degree model
full2.model = lm(whiteWine$Quality ~ (Fixed.Acidity + Volatile.Acidity + Citric.Acid + 
                                        Residual.Sugar + Chlorides + Free.Sulfur.Dioxide + Total.Sulfur.Dioxide + 
                                        Density + pH + Sulphates + Alcohol)^2, data = whiteWine)
#2nd degree stepwise selection
step2.model = step(full2.model, direction = 'both')
pred2 = round(predict(step2.model))
#2nd degree acc
pred2Matrix = pred2==whiteWine$Quality
numCorrect2 = sum(pred2Matrix)
acc2 = numCorrect/length(pred2Matrix)*100
acc2


#Get the 3rd model
full3.model = lm(whiteWine$Quality ~ (Fixed.Acidity + Volatile.Acidity + Citric.Acid + 
                                        Residual.Sugar + Chlorides + Free.Sulfur.Dioxide + Total.Sulfur.Dioxide + 
                                        Density + pH + Sulphates + Alcohol)^3, data = whiteWine)
#3rd degree stepwise selection
step3.model = step(full3.model, direction = 'both')
pred3 = round(predict(step3.model))
#3rd degree acc
pred3Matrix = (pred3 == whiteWine$Quality)
numCorrect3 = sum(pred3Matrix)
acc3 = (numCorrect3/length(pred3Matrix))*100
acc3

#Summary of 3rd degree
summary(full3.model)
summary(step3.model)
step3.model$call

