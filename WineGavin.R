library(car)
library(leaps)
library(MASS)

wine=winequality.white

wine=within(wine,{
  proportion.sulfur = free.sulfur.dioxide/total.sulfur.dioxide
})

head(wine)
#########################
#LIT

#Wine quality depends on the vinification process and the 
#geographical origin of the grapes but also highly relies 
#on the varietal composition of the grape must.

#Negative quality factors, such as off-odors, are generally 
#easier to identify and control. Positive quality factors 
#tend to be more elusive. Wine quality often is defined in 
#incredibly diverse ways. It may be evaluated in terms of 
#subtlety and complexity, aging potential, stylistic purity, 
#varietal expression, ranking by experts, or consumer acceptance.

### BEST SUBSETS

res=regsubsets(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + density + pH + sulphates + alcohol + proportion.sulfur ,data=wine,nbest = 1,nvmax=11)
best.res=summary(res)

results=cbind(best.res$outmat,round(best.res$rsq,3),round(best.res$adjr2,3),
              round(best.res$cp,2))

colnames(results) = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "density", "pH", "sulphates", "alcohol", "proportion.sulfur", "R2", "R2_adj", "Mallow's CP")
results

write.table(results, file = "models.csv", sep = ",", quote = FALSE, row.names = F)

newModel = lm(quality ~ fixed.acidity+volatile.acidity+density+pH+sulphates+alcohol+proportion.sulfur, data=wine)

summary(newModel)
vif(newModel)

######################
#analysis from Rosetta
summary(wine)

# Wine quality distribution 
hist(wine$quality, breaks = 20, main = "Distribution of Quality Data", xlab = "Quality", ylab = "Frequency")
cor(wine$quality, wine$citric.acid)

# Correlation plot
wine_cor = cor(wine) # analysis on github
corrplot(wine_cor, method = 'number') #^^

# Individual box-plots of variables
par(mfrow=c(2, 2))
for (i in 1:(length(wine)-1)) {
  boxplot(wine[,i], main=names(wine[i]), type="l")
}
# box-plots
par(mfrow=c(1, 1))
meltData=melt(wine)
boxplot(data=meltData, value~variable)



res = 

######################################
### CREATE FULL MODEL W/O ALTERCATIONS

fullModel = lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, 
                     data = wine)
summary(fullModel) #r^2_adj = 0.2631
vif(fullModel) #vif > 2: total.sulfer.dioxide = 2.20, density = 3.20, alcohol = 3.00
plot(fullModel) #centered @ 0, constant variance, few concerns with cook's d




###################################
### MODEL WITHOUT VIF > 2 VARIABLES
vifModel = lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + pH + sulphates, 
              data = wine)
summary(vifModel) #r^2_adj = 0.0913
vif(vifModel) #no vif > 2
plot(vifModel) #some abnormality with variance (no HUGE concern), QQ plot is good, few concerns with cook's d




##################################################
### MODEL BASED ON MC AND CORRELATION WITH QUALITY
mcModel = lm(quality~alcohol+volatile.acidity+free.sulfur.dioxide+density+fixed.acidity, data=wine)
summary(mcModel) #0.2605
vif(mcModel) #vif > 2: alcohol = 2.667, density = 2.947
plot(mcModel)#variance and mean good, qq plot good, a couple leverage points




#######################################################
### ROBUST REGRESSION MODEL WITH HUBER AND MC VARIABLES
hubModel = rlm(quality~alcohol+volatile.acidity+free.sulfur.dioxide+density+fixed.acidity, data=wine, maxit=50)
summary(hubModel)  #VERY different from normal models
vif(hubModel) #not sure if VIF is useful with robust, but: alcohol = 2.667, density = 2.947
plot(hubModel) #mean + variance good, qq plot good, a few leverage, 




##########################################################
### ROBUST REGRESSION MODEL WITH BISQUARE AND MC VARIABLES
bisqModel = rlm(quality~alcohol+volatile.acidity+free.sulfur.dioxide+density+fixed.acidity, data=wine, maxit=50, psi=psi.bisquare)
summary(bisqModel)
vif(bisqModel)


#Note how many outliers in the model, influence points, leverage points, 
#and proportion of the data that is leverage/influece/etc.
#Note the difference with least square and robust regression
#Cook's d will be useful in this case

