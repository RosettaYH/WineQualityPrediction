library(caret)
library(corrplot)
library(reshape)
# Import data
wine=read.csv("./winequality-white.csv", sep = ";")
head(wine)
summary(wine)

# Wine quality distribution 
hist(wine$quality, breaks = 20, main = "Distribution of Quality Data", xlab = "Quality", ylab = "Frequency")


# Correlation plot
wine_cor = cor(wine)
corrplot(wine_cor, method = 'number')

# Individual box-plots of variables
par(mfrow=c(4, 3))
for (i in 1:(length(wine)-1)) {
  boxplot(wine[,i], main=names(wine[i]), type="l")
  
}

# box-plots
par(mfrow=c(1, 1))
meltData=melt(wine)
boxplot(data=meltData, value~variable)

# Model
model=lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, 
                 data = wine)
summary(model)
vif(model)

# Residuals
hist(model$residuals, breaks = 30)

# Check variable Importance
importance=varImp(model, scale=FALSE)
print(importance)

