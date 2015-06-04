setwd("//cht-gs1/ClinicalOutcomes$/Tom/R/predictshine")
library(predictshine)
devtools::load_all()
?predictshine		
lm_1 = lm(overall_sat ~   age2  * region + 
						  sex  + 
						  married + 
						  age2 * eductaion + 
						  ethnicity 
						  + health  , 
						  data = well_being)
predictshine(lm_1)
, 
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