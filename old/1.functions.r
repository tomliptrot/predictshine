#setwd('C:\\Users\\Tom\\Documents\\R\\shinyModels')
setwd("//cht-gs1/ClinicalOutcomes$/Tom/R/predictshine")
library(shiny)
library(plyr)

model_input <- function (x, ...) {
	UseMethod("model_input")
}

model_input.factor <- function(x, id = NULL){
	if(is.null(id)) id = deparse(substitute(x))
	l = levels(x)
	radioButtons(id, label = id,
			choices = as.list(l), 
			selected = l[1])
	}
	
model_input.numeric <- function(x , id = NULL){
	if(is.null(id)) id = deparse(substitute(x))
	sliderInput(id, label = id, min = min(x), max = max(x), value = median(x))
	}
	
get_new_data <- function(model_data, ids, input ){

		data_new = alply(1:length(ids), 1, function(i){
			input[[ ids[i] ]]
			})
			
		data_new = as.data.frame(data_new)	
		
		names(data_new) <- names(model_data)
	
		
		for(i in 1:ncol(model_data)){
			if( is.factor(model_data[[i]] )){
				data_new[[i]] <- factor(data_new[[i]], levels = levels(model_data[[i]]))
				}
			}
			
		data_new
}	


get_prediction  <- function (model, ...) {
	UseMethod("get_prediction")
}


get_prediction.glm <- function(fit, newdata ){

	p = predict(fit  , newdata = newdata,  type = "response", se.fit = FALSE)
	se = predict(fit , newdata = newdata,  type = "link", se.fit = TRUE)
	lower_ci = inverse.logit(se$fit + 2 * se$se.fit)
	upper_ci = inverse.logit(se$fit - 2 * se$se.fit)
	res = list(p = p, lower = lower_ci , upper = upper_ci, fit = fit )
	class(res) <- 'prediction_glm'
	res
	}
	
get_prediction.lm <- function(fit, newdata ){

	p = predict(fit  , newdata = newdata, interval = 'prediction')
	res = list(p = p[,'fit'], lower =p[,'lwr'] , upper = p[,'upr']  , fit = fit)
	class(res) <- 'prediction_lm'
	res
	}
	
get_prediction.coxph <- function(fit, newdata ){
	p = survfit(fit , newdata = newdata)
	p
	}	

plot.prediction_glm = function(pred,  ...){
	p = signif(pred$p, 2)
	lower = pred$lower
	upper = pred$upper
	plot(p, xlim = c(0,1),ylim = c(0.4, 0.6), type = 'n', axes = FALSE , ylab = '', xlab = 'Probability', ...)
	points(x = p, y = 0.5, cex = 3)
	segments(x0 = lower , y0 =0.5, x1 = upper, y1 = 0.5, lwd = 3)
	abline(v = 0:1, , h = 0.5, lty = 2)
	axis(1)
	text(p, x = p, y = 0.5, pos = 3, cex = 1.5)
	}
	
plot.prediction_lm = function(pred,  ...){
	p = signif(pred$p, 2)
	lower = pred$lower
	upper = pred$upper
	response_name = names(pred$fit$model)[1]
	plot(p, xlim = range(pred$fit$model[[1]]) * c(0.9, 1.1) , ylim = c(0.4, 0.6), type = 'n', axes = FALSE , ylab = '', xlab = response_name, ...)
	points(x = p, y = 0.5, cex = 3)
	segments(x0 = lower , y0 =0.5, x1 = upper, y1 = 0.5, lwd = 3)
	abline( h = 0.5, lty = 2)
	axis(1)
	text(p, x = p, y = 0.5, pos = 3, cex = 1.5)
	}

