model_input <- function (x, ...) {
	UseMethod("model_input")
}

model_input.factor <- function(x, id = NULL){
	if(is.null(id)) id = deparse(substitute(x))
	l = levels(x)
	shiny::radioButtons(id, label = id,
			choices = as.list(l), 
			selected = l[1])
	}
	
model_input.numeric <- function(x , id = NULL){
	if(is.null(id)) id = deparse(substitute(x))
	shiny::sliderInput(id, label = id, min = min(x), max = max(x), value = median(x))
	}
	
get_new_data <- function(model_data, ids, input ){

		data_new = plyr::alply(1:length(ids), 1, function(i){
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