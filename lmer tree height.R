

#The basicidea is that when you have measurements on the same individuals (or plots, or some other unit) over
#time, you cannot treat the measurements as independent because that would be pseudo-replication,
#in2ation of your sample size, and anti-conservative conclusions about signi1cant effects.



#In this experiment, sixteen plots of 72 Eucalyptus saligna trees
# were remeasured 20 times for height and diameter (although on a number of dates, not all trees were
#measured). Four treatments were applied (control, irrigated, fertilised, irrigated + fertilised). We ask in
#the following example whether tree height differs by treatment

path<- file.path("C:\\Users\\Amita\\Downloads\\HFEIFbytree.csv")
tree<- read.csv(path, header=T,sep=",")



View(tree)

str(tree)

tree$ID<- factor(tree$ID)
tree$plotnr <- factor(tree$plotnr)

bwplot(height ~ Date|treat,data=tree) # the height seems to be linearly increasing with time . the effect of treatment is not clear at this point



tree$Date<- as.Date(tree$Date)
#days since the start of the experiment
tree$time<- as.numeric(as.Date(tree$Date)- min(as.Date(tree$Date)))



install.packages("scales")
require(scales)

palette(alpha(c("blue","red","forestgreen","darkorange"),0.5))
with(tree[sample(nrow(tree)),],plot(jitter(time),height,col=treat,pch=19,xlab="Time(Days)",ylab="Height(m)"))

legend("topleft",levels(tree$treat),pch=19,col=palette())

#"(1|plotnr)"	.What	this	is	saying	is	"assume	an	intercept	that's	different	for	each	
#plot"	.	and	"1"	stands	for	the	intercept	here.	You	can	think	of	this	formula	as	
#telling	 your	 model	 that	 it	 should	 expect	 that	 there's	 going	 to	 be	 multiple	
#responses	 per	 plot,	 and	 these	 responses	 will	 depend	 on	 each	 plot's	
#baseline	 level. This	 effectively	 resolves	 the	 non-independence	 that	 stems	 from	
#having	multiple	responses	by	the	same	plot

#Here's	a	visual	representation	of	the	by-plot	variability:
boxplot(height ~ plotnr,data=tree) # The	variation between	plots isn't big	- but	
#there	 are	 still	 noticeable	 differences,	 and	 we	 better	 account	 for	 them	 in	 our	
#model!





mod <- lmer(height ~ time + treat + (1|plotnr),data=tree) # without int

mod.int <- lmer(height ~ time * treat + (1|plotnr),data=tree) # with int

anova(mod,mod.int) # compare the two. mod.int wins!



require(car)
Anova(mod.int) 


#the p values are only computed if we fit the model AFTER loading lmerTest package.YAY!

install.packages("lmerTest")
library(lmerTest)
mod.int1 <- lmer(height ~ time * treat + (1|plotnr),data=tree)
summary(mod.int1)
summary(mod.int)
Anova(mod.int)



#You	 see	 that	 each	 plot	 is	 assigned	 a	 different	 intercept.	
#That's	what	we	would	expect,	given	that	we've	told	the	model	with	"(1|plotnr)"	
#to take the by-plot	variability	into	account.
#But	 not	also	 that	the	 fixed	effects	 (time and treat)	are	all	 the	 same	 for	all	
#plots.	Our	model	is	what	is	called	a	random	intercept	model.	In	this	
#model,	we	account	for	baseline-differences	in	height,	but	we	assume	that	whatever	
#the	effect	of	treatment and time	is,	it's	going	to	be	the	same	for	all	plots

#> coef(mod.int)
#$plotnr
#(Intercept)        time     treatF     treatI    treatIL  time:treatF time:treatI time:treatIL
#1     3.271267 0.006921244 0.09307408 -0.5353595 -0.2463226 -0.000141759 0.003514427   0.00364596
#2     2.884342 0.006921244 0.09307408 -0.5353595 -0.2463226 -0.000141759 0.003514427   0.00364596
#3     2.330081 0.006921244 0.09307408 -0.5353595 -0.2463226 -0.000141759 0.003514427   0.00364596


mod.int.r <- lmer(height ~ time * treat + (treat|plotnr),data=tree)



Anova(mod.int.r)

anova(mod.int,mod.int.r) # we don't need the random slope . reject mod.int.r

# Visualized effect of Time on height, by treatment for the HFE IxF dataset, 1tted with a linear
# mixed-effects model.

require(visreg)
visreg(mod.int,"time",by="treat",overlay=T)
visreg(mod.int,"treat",by="time",overlay=T) # lame coz time is not a factor



#---------------------------------------------------------------------------------
#pairwise comparisons of the treatments
#-----------------------------------------------------------------------------------

install.packages("lsmeans")
library(lsmeans)


lsmeans(mod.int,pairwise ~ treat)







# PLOTTING DIAGNOSTIC PLOTS :CHECK THE DATA FOR ADHERANCE TO THE MODEL ASSUMPTIONS.
#-----------------------------------------------------------------------------------------------

#Plots of residuals versus fitted values and versus each of the predictors in
#turn are the most basic diagnostic graph
# model validation centers around the residuals 
#(essentially the distance of the data points from the fitted regression line).


# fitted vs. residuals, coloured according to the 'treat' variable

mod.int2<- lmer(height ~ time * treat + (treat|plotnr),data=tree)
plot(mod.int,col=tree$treat) # linearity is not violated.




#check the assumption of normality of residuals
plot(fitted(mod.int), residuals(mod.int), xlab="Predicted values", ylab="Residuals")
abline(h=0)
qqmath(residuals(mod.int))
hist(residuals(mod.int),xlim = c(-5,5))
qqnorm(residuals(mod.int))


# check for collinearity

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

max(vif.mer(mod.int))


#-----------------------------------------------------------------------------------------------
#Explore multilevel models faster with the new merTools R package
#-----------------------------------------------------------------------------------------------

class(mod.int)
fixef(mod.int) #get fixed effects
ranef(mod.int) #get random effects 
#OR  ranef(mod.int, drop = TRUE) # 'drop' changes the output format from matrix to vector
fitted(mod.int) # get predicted values



# check the assumption of Normality of random effects by drawing a qqplot of the random effects with their variances(a straight line indicates normality)
qqnorm(unlist(ranef(mod.int)))
qqmath(ranef(mod.int, condVar = TRUE), strip = FALSE)$plotnr # fancy
#OR
require(lattice)
dotplot(ranef(mod.int, condVar = TRUE)) # # same as REsim from merTools package.

dotplot(fixef(mod.int,condVar=TRUE))   # same as FEsim from merTools package.

#plot fixed effect coefficients # same as FEsim from merTools package.

require(reshape2)
install.packages("coefplot2",
                 repos="http://www.math.mcmaster.ca/bolker/R",
                 type="source")

library(coefplot2)

install.packages("coda")
coefplot2(mod.int)


#-------------------------------------------------------------------------------
#PLOTTING RANDOM AND FIXED EFFECT DISTRIBUTIONS
#-------------------------------------------------------------------------------

# merTools also provides functionality for inspecting merMod objects visually.
#The easiest are getting the posterior distributions of both fixed and random effect parameters.
# NOTE : WILL NOT WORK IF LMERTEST IS LOADED.
require(lme4)
require(merTools)


feSims <- FEsim(merMod=mod.int, n.sims = 100)
feSims

plotFEsim(feSims, level = 0.9, stat = 'median', intercept = FALSE)

reSims <- REsim(mod.int, n.sims = 100)
plotREsim(reSims,stat = 'median', sd = TRUE)




#S is known both as the standard error of the regression and as the standard error of the estimate.
#S represents the average distance that the observed values fall from the regression line. 
#Conveniently, it tells you how wrong the regression model is on average using the units of
#the response variable. 
#Smaller values are better because it indicates that the observations are closer to the fitted line.









#-------------------------------------------------------------------------------------
#prediction interval
#-------------------------------------------------------------------------------------
#prediction intervals for the data selected by user are calculated using the predictInterval function
#within the package. This function calculates prediction intervals quickly by sampling from the simulated 
#distribution of the fixed effect and random effect terms and combining these simulated estimates to produce 
#a distribution of predictions for each observation. This allows prediction intervals
#to be generated from very large models where the use of bootMer would not be feasible computationally.


install.packages("merTools")
require(merTools)
require(ggplot2)
library("lme4")
install.packages("devtools")
install.packages("foreach")
devtools::install_github("rstudio/foreach")
#predInt <- predictInterval(merMod=mod.int1, newdata=tree[sample(nrow(tree),10),],n.sims=999)

predInt <- predictInterval(merMod=mod.int1, newdata=tree,n.sims=999,level =0.95,stat = "median", type="linear.prediction",include.resid.var = TRUE)

head(cbind("observed"=tree$height,predInt))


ggplot(aes(x=1:30, y=fit, ymin=lwr, ymax=upr), data=predInt[1:30,]) +
  geom_point() + 
  geom_linerange() +
  labs(x="Index", y="Prediction w/ 95% PI") + theme_bw()


#This looks pretty familiar, the prediction interval being always bigger than the confidence interval.
#Finally, let's plot the averages with 95% CIs and PIs. Notice that the PIs are much wider than the CIs. 
#That means we're much more confident in predicting the average than a single value.

#	

#predictInterval includes the error/uncertainty for both the fixed and random effect terms.
#In dotplot you are only seeing the uncertainty due to the random portion of the prediction, 
#essentially the uncertainty around the estimate of the fish specific intercepts.
#If your model has a lot of uncertainty in the fixed parameter fishWt and this parameter 
#drives most of the predicted value, then the uncertainty around any specific fish intercept is 
#trivial and you won't see a big difference in the width of the intervals. 



#--------------------------------------------------------------------------------------------------
# CONFIDENCE INTERVALS
#-------------------------------------------------------------------------------------------------------



# Make dataframe for which we want predictions 

#newdat1
#newdat <- data.frame(time=c(rep(0,1),rep(60,1),rep(91,1)), treat= rep(c("C","IL","I"),1))


#newdat2
#inter<- tree[tree$time==c(0,60,91),]
#newdat<- inter[order(inter$time),]

newdat <- subset(tree,treat=="C")

View(newdat)



bb <- bootMer(mod.int, FUN=function(x)predict(x, newdat, re.form=NA),
                    nsim=999)

## These represent the confidence interval of the mean at any value of Time.
lci <- apply(bb$t, 2, quantile, 0.025)
uci <- apply(bb$t, 2, quantile, 0.975)
pred <- predict(mod.int,newdat,re.form=NA)

tail(cbind(pred,lci,uci),10)

library(scales)
palette(alpha(c("blue","red","forestgreen","darkorange"),0.5))
plot(height~jitter(time),col=treat,data=tree[tree$treat=="C",],pch=16)

lines(pred~time,newdat,lwd=2,col="orange",alpha=0.5)
lines(lci~time,newdat,lty=2,col="orange")
lines(uci~time,newdat,lty=2,col="orange")





