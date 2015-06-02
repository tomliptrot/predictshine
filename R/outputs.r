
get_prediction  <- function (model, ...) {
	UseMethod("get_prediction")
}


get_prediction.glm <- function(fit, newdata ){

	p = predict(fit  , newdata = newdata,  type = "response", se.fit = FALSE)
	se = predict(fit , newdata = newdata,  type = "link", se.fit = TRUE)
	lower_ci = fit$family$linkinv(se$fit + 2 * se$se.fit)
	upper_ci = fit$family$linkinv(se$fit - 2 * se$se.fit)
	res = list(p = p, lower = lower_ci , upper = upper_ci, fit = fit )
	class(res) <- 'prediction_glm'
	res
	}
	
get_prediction.lm <- function(fit, newdata ){

	p = predict(fit  , newdata = newdata, interval = 'prediction', se.fit = TRUE)
	res = list(p = p$fit[,'fit'], lower =p$fit[,'lwr'] , upper = p$fit[,'upr']  , fit = fit, sd = p$residual.scale)
	class(res) <- 'prediction_lm'
	res
	}
	
get_prediction.coxph <- function(fit, newdata ){
	#todo makes it show survplot3 instead
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
	
plot.prediction_lm = function(pred,xlab = NULL,  ...){

	if(is.null(xlab)) xlab = response_name
	p = signif(pred$p, 2)
	plot_norm(mu = p, sd = pred$sd, 
		xlim = range(pred$fit$model[[1]]) * c(0.9, 1.1), 
		from = p - 2.5 * pred$sd, 
		to = p + 2.5 * pred$sd,
		xlab = xlab,
		...)
	
	# lower = pred$lower
	# upper = pred$upper
	# response_name = names(pred$fit$model)[1]
	# if(is.null(xlab)) xlab = response_name
	# plot(p, xlim = range(pred$fit$model[[1]]) * c(0.9, 1.1) , ylim = c(0.4, 0.6), type = 'n', axes = FALSE , 
		# ylab = '', xlab = xlab, ...)
	# points(x = p, y = 0.5, cex = 3)
	# segments(x0 = lower , y0 =0.5, x1 = upper, y1 = 0.5, lwd = 3)
	# abline( h = 0.5, lty = 2)
	# axis(1)
	# text(p, x = p, y = 0.5, pos = 3, cex = 2)
	}
	
mp = function (pch = 20, mgp = c(2, 0.5, 0), mar = c(3, 3, 3, 1), 
    tck = -0.01, las = 1, bty = "l", cex.axis = 0.7, ...) 
{
    par(pch = pch, mgp = mgp, mar = mar, tck = tck, las = las, 
        bty = bty, cex.axis = cex.axis, ...)
}

plot_norm <- function(mu, sd,from, to, ...){		
	coords = curve(dnorm(x,mu,sd),type = 'n',from = from, to = to, ylab = 'Density',...)
	polygon(coords,col=makeTransparent(2),  border = NA)
	segments(x0 = mu, x1 = mu, y0 = 0, y1 = dnorm(mu,mu,sd), col = 2, lty = 2, lwd = 3)
	par(xpd = TRUE)
	text(mu, x = mu, y = dnorm(mu,mu,sd), pos = 3, cex = 2)
	}
	
makeTransparent<-function(someColor, alpha=100)
{
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}	
	



