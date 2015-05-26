#' Make interactive model predictions with shiny
#' 
#' @param fit a model object from either lm, glm or coxph.
#' @param main a main title for the app, defaults to the name of \code{fit}
#' @param ... arguments to be passed to plot()
#' @return An object that represents the app. Printing the object or passing it to runApp() will run the app.
#' @examples
#' # linear regression
#' # create demo linear model using school dataset
#'
#' mylm <- lm(gre ~ admit + gpa + rank, data = school)
#' predictshine(mylm)
#' 
#' # Logistic regression
#' mylogit <- glm(admit ~ gre + gpa + rank, data = school, family = "binomial")
#'
#' predictshine(mylogit)
#' 
#' # Survival
#'library(survival)
#'
#' # Variables must be set to correct type outside of model call
#' lung$sex = factor(lung$sex )
#' lung$ph.ecog = factor(lung$ph.ecog )
#' 
#' # model must be set to TRUE
#' fit_cox = coxph(Surv(time, status) ~ age + sex + ph.ecog , lung, model = TRUE) 
#' 
#' predictshine(fit_cox, xscale = 365 , xlab = 'Time (years)', ylab = 'Overall Survival')
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
						uiOutput("ui"),
						p('To close window press escape in the R console')
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
		
