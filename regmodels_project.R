# Coursera Regression Models
# project mtcars

# classify variables correctly
mtcars$cyl=factor(mtcars$cyl)
mtcars$vs=factor(mtcars$vs)
mtcars$gear=factor(mtcars$gear)
mtcars$carb=factor(mtcars$carb)
mtcars$am=factor(mtcars$am,labels=c('Automatic','Manual'))
auto=subset(mtcars, am=="Automatic")
man=subset(mtcars, am=="Manual")

# create boxplot with data
color1="#F8766D75"
color2="#00BFC475"
col1legend="#F8766D"
col2legend="#00BFC4"
boxplot(auto$mpg,man$mpg,col=c(color1,color2),varwidth=TRUE,xlab="transmission type",ylab="mpg",main="Comparison of MPG of Automatic vs. Manual Transmission",names=c("automatic","manual"))
stripchart(mpg ~ am, data = mtcars, vertical = TRUE,method = "jitter", 
           jitter = 0.15, pch = 16, col = c("salmon","darkblue"),
           bg = "green", add = TRUE) 

# density plot
a=density(auto$mpg)
m=density(man$mpg)
plot(m,ylim=c(0,.1),col="darkblue",lwd=1,xlab="mpg",main="Density of MPG by Transmission Type for MtCars")
lines(a,col="red",lwd=1)
polygon(a, col=color1, border="red") 
polygon(m,col=color2,border="darkblue")
legend("topright",c("automatic","manual"),lty=c(1,1),lwd=c(2,2),col=c("salmon","darkblue"),bty="")


# mean of transmission type
library(plyr)
Transmission=ddply(mtcars,"am",summarize, avg=mean(mpg))
test=t.test(mpg ~am, data = mtcars)
t=t.test(mpg ~am, data = mtcars)$p.value

# paris plot
library(ggplot2)
library(GGally)

pairsGG=mtcars[,c(1,2,3,4,6,9)]
ggpairs(pairsGG,lower = list(continuous="smooth",theme_set(theme_bw())),title="mtcars",colour = "am")

# building the linear regression model
library(MASS)
fit= lm(mpg ~ cyl + disp + hp+drat+wt+vs+am+gear+carb, data=mtcars)
step = stepAIC(fit, direction="both")

fitAIC= lm(mpg ~ cyl  + hp+wt+am, data=mtcars)
sumAIC=summary(fitAIC)
round(fitAIC$coefficients,3)

p.values=round(summary(fitAIC)$coefficients[,4],5)
sumAIC=summary(fitAIC)
p.values

fitNOam=lm(mpg~ cyl+hp+wt,data=mtcars)
sumNOam=summary(fitNOam)
round(fitNOam$coefficients,3)
p.valuesNOam=round(sumNOam$coefficients[,4],5)
p.valuesNOam
pANOVA1=round(anova(fitAIC,fitNOam)$"Pr(>F)"[2],3)
fitNOhp=lm(mpg~cyl+wt,data=mtcars)
sumNOhp=summary(fitNOhp)
pANOVA2=round(anova(fitNOam,fitNOhp)$"Pr(>F)"[2],3)
pANOVA3=round(anova(fitAIC,fitNOhp)$"Pr(>F)"[2],3)
summary(fitNOhp) # final model

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitNOhp,pch=16)

# model with only transmission type
fitT=lm(mpg~am,data=mtcars) # this R^2 vs. fithp R^2
sumfitT=summary(fitT)

pANOVA4=round(anova(fitT,fitNOhp)$"Pr(>F)"[2],10)

# bootstrap relative importance of variables
library(relaimpo)

boot <- boot.relimp(fitAIC, b = 1000, type = c("lmg"), rank = TRUE,diff = TRUE, rela = TRUE)
boot2=booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 



