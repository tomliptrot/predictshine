setwd("//cht-gs1/ClinicalOutcomes$/Tom/R/predictshine")
library(predictshine)
devtools::load_all()
devtools::install()
?predictshine	
shiny::handlerManager
 getAnywhere(handlerManager)
	
lm_1 = lm(overall_sat ~   age2  * region + 
						  sex  + 
						  married + 
						  age2 * eductaion + 
						  ethnicity 
						  + health  , 
						  data = well_being)
app_1 = predictshine(lm_1, 
	page_title = 'Happiness in the UK', 
	variable_descriptions = c('Age', "Region", 'Sex','Marital status', 
		"What is the highest level of qualification?",
		"Ethnicity White/Other", 
		"How is your health in general?" ),
	main = 'Overall, how satisfied are you with your life nowadays?', 
	xlab =  'predicted score out of 10', 
	description = p('Alter variables to get predicted overall life satisfaction (out of 10). 
		This model is made using data from the 1,000 respondents of the ONS Opinions Survey, 
		Well-Being Module, April 2011'))
		
app_2 <- predictshine(lm_1)
		
saveRDS(app_1, file = 'app_1.Rdata')

?shinyApp

app_1 <- readRDS('app_1.Rdata')

  app <- shinyApp(
    ui = fluidPage(
      numericInput("n", "n", 1),
      plotOutput("plot")
    ),
    server = function(input, output) {
      output$plot <- renderPlot( plot(head(cars, input$n)) )
    }
  )	
str(app)
app[[1]]
test = as.shiny.appobj(app_1)
str(test)
library(predictshine)
library(plyr)
app_1
app_1
str(app_1)
app_1
app_1$ serverFuncSource
length(app_1$options)
app_1[[2]]
app_2[[5]]
dput(app_1)
identical(app_1[[1]], app_2)
app_2
a = list(app_1)
str(a)
runApp(app_1)
?runApp
lm_1

search()
		
library(survival)

# Variables must be set to correct type outside of model call
lung$sex = factor(lung$sex )
lung$ph.ecog = factor(lung$ph.ecog )

# model must be set to TRUE
fit_cox = coxph(Surv(time, status) ~ age + sex + ph.ecog , lung, model = TRUE)
fit_cox_missing = coxph(Surv(time, status) ~ age + sex + ph.ecog , lung)

str(delete.response(model.frame(fit_cox_missing)))
?delete.response
terms(fit_cox)
?terms
predictshine(fit_cox)
predictshine(fit_cox_missing)
class(fit_cox)
predictshine(fit_cox, xscale = 365 , xlab = 'Time (years)', ylab = 'Overall Survival')