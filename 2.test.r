library(ggplot2)
response(budworm.lg)
fitted(budworm.lg)

model_data = budworm.lg$model 
ids = names(budworm.lg$model 
source(file="//cht-gs1/clinicaloutcomes$/Tom/R/predictshine/predictshine.r"); 
predictshine(budworm.lg, se.fit = TRUE )

class(budworm.lg)
a = data.frame(predict(budworm.lg,  se.fit = TRUE))[1,]

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



predictshine(lm.D9 )
