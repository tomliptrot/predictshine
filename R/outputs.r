
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

