setwd('C:\\Users\\Tom\\Documents\\R\\shinyModels')
setwd("//cht-gs1/ClinicalOutcomes$/Tom/R/predictshine")
library(shiny)
runApp()
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

	
sliderInput("cycle_1_dose", label = h3("cycle 1 dose"), min = 10, 
				max = 25, value = 15)
	
	
	
model_input(budworm.lg$model$ldose	)
model_input(n	)
n = 1:10 * 0.1
as.list(levels(x))
list("TRUE" , 'FALSE')
	
		  radioButtons("hist_a", label = h3("Alveolar favourable"),
			choices = list("TRUE" , 'FALSE'), 
			selected = 'TRUE')
			
model_input (budworm.lg$model$sex)


model_input (1:10)

?predict.glm

ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20-numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
str(budworm.lg$model)

summary(budworm.lg)

plot(c(1,32), c(0,1), type = "n", xlab = "dose",
     ylab = "prob", log = "x")
text(2^ldose, numdead/20, as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.

a = radioButtons("hist_a", label = h3("Alveolar favourable"),
			choices = list("TRUE" , 'FALSE'), 
			selected = 'TRUE')
			
str(a)
print.shiny.tag