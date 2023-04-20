library(caret)
library(corrplot)
library(reshape)
library(leaps)
library(MASS)
library(car)
# Import data
wine=read.csv("./winequality-white.csv", sep = ";")
head(wine)
summary(wine)
sapply(wine, sd)

# Wine quality distribution 
par(mfrow=c(1, 1))
hist(wine$quality, main = "Distribution of Quality", xlab = "Quality", ylab = "Frequency", xlim=c(0, 10))

# Individual box-plots of variables
par(mfrow=c(2, 3))
for (i in 1:(length(wine)-1)) {
  boxplot(wine[,i], main=names(wine[i]), type="l")
  
}

# box-plots
par(mfrow=c(1, 1))
meltData=melt(wine)
boxplot(data=meltData, value~variable)

# Correlation plot
wine_cor = cor(wine)
corrplot(wine_cor, method = 'circle', tl.col="black")

# Full Model
model=lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,
                 data = wine)
summary(model)
vif(model)

# Use subsets
res=regsubsets(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + density + pH + sulphates + alcohol + proportion.sulfur, data=wine,nbest = 1,nvmax=11)
best.res=summary(res)

results=cbind(best.res$outmat,round(best.res$rsq,3),round(best.res$adjr2,3),round(best.res$cp,2))

colnames(results) = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "density", "pH", "sulphates", "alcohol", "proportion.sulfur", "R2", "R2_adj", "Mallow's CP")
results

write.table(results, file = "models.csv", sep = ",", quote = FALSE, row.names = F)

# Fit New Model
newModel = lm(quality ~ fixed.acidity+volatile.acidity+density+pH+sulphates+alcohol+proportion.sulfur, data=wine)

summary(newModel)
vif(newModel)

# Residuals
hist(newModel$residuals, breaks = 30)

# Check variable Importance
importance=varImp(newModel, scale=FALSE)
print(importance)

