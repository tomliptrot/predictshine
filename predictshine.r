predictshine = function(fit, main = NULL, ...){

	if(is.null(main)) main = deparse(substitute(fit))

	model_data = fit$model[-1]
	ids = names(fit$model)[-1]

	shinyApp(
	# ui -----------------------------------------------------------------------------------------------
		ui =  fluidPage(
				titlePanel(main),
				sidebarLayout(
					sidebarPanel(
						uiOutput("ui")
						),
					mainPanel(						
						h4(textOutput('df'))
						)
					)	
			),
		
		
		# server-----------------------------------------------------------------------------------------------
		server = function(input, output) {
		
			inputs_list = alply(1:ncol(model_data ), 1, function(i){
							model_input(model_data [[i]], id = ids[i])
							})
		
			output$ui <- renderUI({	inputs_list	})
			
			predictions = reactive({
				data_new = get_new_data(model_data, ids, input)	
				predict(fit, newdata = data_new, ...)
				})
				
			a_ply(1:length(ids), 1, function(i){
					output[[ ids[i] ]] <- renderPrint({
						str(input[[ ids[i] ]])
					})
				})
				
			output$df <- renderText({
				predictions()
				})
				
			
		}
		)
	}
		
