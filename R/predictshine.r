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

	if(is.null(fit$model) & class(fit) == "coxph") stop('Missing model frame from coxph() fit. Re-run coxph() setting model = TRUE and try again')
	
	model_data = model.frame(fit)[-1]
	n_vars = ncol(model_data)
	
	if(is.null(variable_descriptions))	ids = names(fit$model)[-1]
	else ids = variable_descriptions
	
	model_summary = summary(fit)

	shinyApp(
	# ui -----------------------------------------------------------------------------------------------
		ui =  fluidPage(
				titlePanel(page_title),
				sidebarLayout(
					sidebarPanel(
						description,
						h4(),
						# uiOutput creates input sliders based on the class of terms in fit
						# these object are created dynamically in the server function using model_input() and renderUI()
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
							
			#inputs_list is a list of shiny widgets corresponding to each term in fit
			# The list is created by calling model_input on each column of model_data
			inputs_list = plyr::alply(1:ncol(model_data ), 1, function(i){
							model_input(model_data [[i]], id = ids[i])
							})
			
			output$ui <- renderUI({	inputs_list	})

			#creates a reactive function that makes prediction from the model fit and newdata as inputed by the user
			# Only runs when input values change
			predictions = reactive({
				data_new = get_new_data(model_data, ids, input)	
				if(is.null(data_new)) return(NULL)
				get_prediction(fit, newdata = data_new)
				})
			
			#creates a plot of the predictions
			 output$pred_plot <- renderPlot({ 
					pred = predictions()
					if(is.null(pred)) return(NULL)
					mp(cex = 1)
					plot(pred, ... )
					})
			
			#outputs a summary of the model			
			output$model = renderPrint({model_summary})
			
			
			
		}
		)
	}
		

predictshine_multi = function(model_list, page_title = NULL, variable_descriptions = NULL,description = NULL, ncol_termplot = 2, options = list(),...){
	library(shiny)
	library(plyr)

	
	shinyApp(
	# ui -----------------------------------------------------------------------------------------------
		ui =  fluidPage(
				titlePanel(page_title),
				sidebarLayout(
					sidebarPanel(
						description,
						h4(),
                        shiny::radioButtons('fit_select', label = 'Select model',
                                    choices = names(model_list) ),
                        tags$hr() ,        
                                        # uiOutput creates input sliders based on the class of terms in fit
						# these object are created dynamically in the server function using model_input() and renderUI()
						uiOutput("ui"),
						p('To close window press escape in the R console')
						),
					mainPanel(	
							tabsetPanel(
								tabPanel(h4("Prediction"), plotOutput('pred_plot')),
								#tabPanel(h4("Regression Term plot"), plotOutput('term_plot', height = "800px")),
								tabPanel(h4("Model Summary"), verbatimTextOutput("model")),
                                tabPanel(h4("Model Summary"), verbatimTextOutput("test"))
								)
						)
					)	
			),
		
		
		# server-----------------------------------------------------------------------------------------------
		server = function(input, output) {
							
            fit = reactive({
                fit = model_list[[input$fit_select]]
				if(is.null(page_title)) page_title = deparse(substitute(fit))

                #if(is.null(fit$model) & class(fit) == "coxph") stop('Missing model frame from coxph() fit. Re-run coxph() setting model = TRUE and try again')
                
                model_data = model.frame(fit)[-1]
                n_vars = ncol(model_data)
                
                if(is.null(variable_descriptions))	ids = names(fit$model)[-1]
                else ids = variable_descriptions
                
                model_summary = summary(fit)
                
            #inputs_list is a list of shiny widgets corresponding to each term in fit
			# The list is created by calling model_input on each column of model_data
			inputs_list = plyr::alply(1:n_vars, 1, function(i){
							model_input(model_data [[i]], id = ids[i])
							})
                            
                            
                            
                                
                list(model_data = model_data,
                    n_vars = n_vars, 
                    page_title = page_title, 
                    ids = ids,
                    model_summary = model_summary, 
                    fit = fit, 
                    inputs_list = inputs_list)
				})

		
			
			output$ui <- renderUI({	fit()$inputs_list	})
            
			#creates a reactive function that makes prediction from the model fit and newdata as inputed by the user
			# Only runs when input values change
			predictions = reactive({
				data_new = get_new_data(fit()$model_data, fit()$ids, input)	
				if(is.null(data_new)) return(NULL)
				get_prediction(fit()$fit, newdata = data_new)
				})
			
			#creates a plot of the predictions
			 output$pred_plot <- renderPlot({ 
					pred = predictions()
					if(is.null(pred)) return(NULL)
					mp(cex = 1)
					plot(pred, ... )
					})
			
			#outputs a summary of the model			
			output$model = renderPrint({fit()$model_summary})
            
            output$test = renderPrint({predictions()})
			
			
			
		}
		, options = options)
	}