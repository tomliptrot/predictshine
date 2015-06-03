model_input <- function (x, ...) {
	UseMethod("model_input")
}

model_input.factor <- function(x, id = NULL){
	if(is.null(id)) id = deparse(substitute(x))
	l = levels(x)
	if(length(l) > 5) {
		return(
			shiny::selectInput(id, label = id,
			choices = as.list(l), 
			selected = l[1])
			)
		}
	else {
	return(
		shiny::radioButtons(id, label = id,
				choices = as.list(l), 
				selected = l[1])
				)
		}
	}
	
model_input.logical <- function(x, id = NULL){
	#logical not working - test
	if(is.null(id)) id = deparse(substitute(x))
	l = c('FALSE', 'TRUE')
	shiny::radioButtons(id, label = id,
			choices = as.list(l), 
			selected = l[1])
	}
	
	
model_input.numeric <- function(x , id = NULL){
	if(is.null(id)) id = deparse(substitute(x))
	shiny::sliderInput(id, label = id, min = min(x), max = max(x), value = median(x))
	}
	
model_input.poly <- function(x , id = NULL){
	#to do - need to extract name of variable
	if(is.null(id)) id = deparse(substitute(x))
	shiny::sliderInput(id, label = id, min = min(x), max = max(x), value = median(x))
	}
	
#todo: model_input.default	
	
get_new_data <- function(model_data, ids, input ){

		
		data_new = plyr::alply(1:length(ids), 1, function(i){
			input[[ ids[i] ]]
			})
			
		data_new = as.data.frame(data_new)	
		
		#an error is occuring here for some reason
		if(length(names(data_new)) == length(names(model_data)))
		names(data_new) <- names(model_data)
	
		
		for(i in 1:ncol(model_data)){
			if( is.factor(model_data[[i]] )){
				data_new[[i]] <- factor(data_new[[i]], levels = levels(model_data[[i]]))
				}
			if( is.logical(model_data[[i]] )){
				data_new[[i]] <- as.logical(data_new[[i]])
				}
			}
			
		data_new
}	
