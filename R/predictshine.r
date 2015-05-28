#' Make interactive model predictions with shiny
#' 
#' @param fit a model object from either lm, glm or coxph.
#' @param main a main title for the app, defaults to the name of \code{fit}
#' @param variable_descriptions an optional character vector giving description of each variable in the model. Defaults to \code{NULL}, giving headings based on the variable names only
#' @param ... optional arguments to be passed to \code{plot()}
#' @return An object that represents the app. Printing the object or passing it to \code{runApp()} will run the app.
#' @examples
#' # linear regression
#' # create demo linear model using school dataset
#'
#' predictshine(mylm, main = 'Linear Model example', variable_descriptions = c('Admission (0 = yes, 1 = no)', 'Grade point average', 'Class rank'))
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
predictshine = function(fit, page_title = NULL, variable_descriptions = NULL,description = NULL, ncol_termplot = 2,...){
	library(shiny)
	library(plyr)

	if(is.null(page_title)) page_title = deparse(substitute(fit))
	#TODO could use model.frame + delete.response here - see code for predict and termplot
	model_data = fit$model[-1]
	n_vars = ncol(model_data)
	if(is.null(variable_descriptions))	ids = names(fit$model)[-1]
	else ids = variable_descriptions

	shinyApp(
	# ui -----------------------------------------------------------------------------------------------
		ui =  fluidPage(
				titlePanel(page_title),
				sidebarLayout(
					sidebarPanel(
						description,
						#textOutput("call"),
						h4(),
						uiOutput("ui"),
						p('To close window press escape in the R console')
						),
					mainPanel(	
							tabsetPanel(
								tabPanel(h4("Prediction"), plotOutput('pred_plot')),
								#tabPanel(h4("Regression Term plot"), plotOutput('term_plot', height = "800px")),
								tabPanel(h4("Model Summary"), verbatimTextOutput("model"))
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
			
			# output$term_plot = renderPlot({
				# ncol_termplot
				# mp(mfrow = c(ceiling(n_vars / ncol_termplot), ncol_termplot), cex.axis = 1)
				# termplot(fit, cex.axis = 1)
				# })
				
			
		}
		)
	}
		
