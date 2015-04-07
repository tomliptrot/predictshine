library(plyr)
model_data = budworm.lg$model 
ids = names(budworm.lg$model )
	
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


shinyServer(function(input, output) {


	output$ui <- renderUI({
		alply(1:ncol(model_data ), 1, function(i){
				model_input(model_data [[i]], id = ids[i])
				})
		})
		
		
	a_ply(1:length(ids), 1, function(i){
			output[[ ids[i] ]] <- renderPrint({
				str(input[[ ids[i] ]])
			})
		})
		
	output$df <-	renderPrint({
	
		data_new = get_new_data(model_data, ids, input)
			
		str(data_new)
		predict(budworm.lg, newdata = data_new)
		
		})
		
		
	})
	
