library(ggplot2)
response(budworm.lg)
fitted(budworm.lg)


ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20-numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
head(budworm.lg$model)
str(budworm.lg)
?density

model_data = budworm.lg$model 
ids = names(budworm.lg$model )

source(file="//cht-gs1/clinicaloutcomes$/Tom/R/predictshine/predictshine.r")

predictshine(budworm.lg, se.fit = TRUE )

class(budworm.lg)
a = data.frame(predict(budworm.lg,  se.fit = TRUE))[1,]


school <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
library(predictshine)
data(school)

school$rank <- factor(school$rank)
school$admit <- factor(school$admit)
mylm <- lm(gre ~ admit + gpa + rank, data = school)
predictshine(mylm, main = 'Linear Model example', variable_descriptions = c('Admission (0 = yes, 1 = no)', 'Grade point average', 'Class rank'))



mylogit <- glm(admit ~ gre + gpa + rank, data = school, family = "binomial")
predictshine(mylogit)

str(mylogit)
mylogit$family$linkinv
# Create the simplest test data set 
library(survival)

# Fit model using lung dataset
lung$sex = factor(lung$sex )
lung$ph.ecog = factor(lung$ph.ecog )

#note model must be set to TRUE
fit_cox = coxph(Surv(time, status) ~ age + sex + ph.ecog , lung, model = TRUE) 

predictshine(fit_cox )


summary(fit_cox)
fit_cox$model
test = survfit(fit,newdata = test1[1,])
plot(test)
test


source(file="//cht-gs1/clinicaloutcomes$/Tom/R/predictshine/1.functions.r")
source(file="//cht-gs1/clinicaloutcomes$/Tom/R/predictshine/predictshine.r")
predictshine(mylogit)
test = predictshine(mylm)

str(test)
dplyr::glimpse(school)
str(test)
test$ serverFuncSource
test[[1]]
test
str(mylogit)
mp()
p = 0.98
hist(mylogit$model[[1]], col = 3, axes = FALSE, xlab = 'Probability')
points(x  = p, y = 0, cex = 4, col = 2)
segments(y0 = 0, y1 = 0, x0 = p - 0.1, x1 = p + 0.1, lwd = 6, col = 2)

response = colnames(mylogit$model)[1]

library(ggplot2)
b = ggplot(mylogit$model, aes_string (response))
b + geom_histogram()



class(lm.D9)
data.frame(predict(lm.D9 ))[1,]
a = data.frame(predict(lm.D9 , interval =  "confidence"))[1,]
data.frame(predict(lm.D9 , interval =  "prediction"))[1,]


	k <- ggplot(a, aes(y = fit,x = 1, ymin = fit, ymax = fit, label=fit))

if( "se.fit" %in% names(a))
	k <- ggplot(a, aes(y = fit,x = 1, ymin = fit - 2 * se.fit, ymax = fit - 2 * se.fit, label=fit))
	
if( "lwr" %in% names(a))
	k <- ggplot(a, aes(y = fit,x = 1, ymin = lwr, ymax = upr, label=fit))
	
k + geom_pointrange(size = 2) + coord_flip() +  geom_text( vjust=-2)
	




[c('fit', 'se.fit')])


ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
range(lm.D9$model[[1]]) * c(0.9, 1.1)
?predict.lm
predict(lm.D9 ,  interval = 'prediction')[,'fit']


predictshine(lm.D9 )
