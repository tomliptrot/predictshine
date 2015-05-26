predictshine = function(fit, main = NULL, ...){
	library(shiny)
	library(plyr)

	if(is.null(main)) main = deparse(substitute(fit))

	model_data = fit$model[-1]
	ids = names(fit$model)[-1]

	shinyApp(
	# ui -----------------------------------------------------------------------------------------------
		ui =  fluidPage(
				titlePanel(main),
				sidebarLayout(
					sidebarPanel(
						textOutput("call"),
						h4(),
						uiOutput("ui")
						),
					mainPanel(	
							tabsetPanel(
								tabPanel("Prediction", plotOutput('pred_plot')),
								tabPanel("Model Summary", verbatimTextOutput("model"))
								)
						)
					)	
			),
		
		
		# server-----------------------------------------------------------------------------------------------
		server = function(input, output) {
		
			inputs_list = alply(1:ncol(model_data ), 1, function(i){
							model_input(model_data [[i]], id = ids[i])
							})
			
			output$call = renderPrint({fit$call})
			output$ui <- renderUI({	inputs_list	})
			
			predictions = reactive({
				data_new = get_new_data(model_data, ids, input)	
				get_prediction(fit, newdata = data_new)
				})
				
			a_ply(1:length(ids), 1, function(i){
					output[[ ids[i] ]] <- renderPrint({
						str(input[[ ids[i] ]])
					})
				})
				
			output$df <- renderText({
				predictions()
				})
				
			 output$pred_plot <- renderPlot({ 
					pred = predictions()
					mp(cex = 1)
					plot(pred, ... )
					})
				
			output$model = renderPrint({summary(fit)})
				
			
		}
		)
	}
		
