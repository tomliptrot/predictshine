# Install the released version from CRAN:
install.packages("haven")
library(devtools)

library(RCurl)
library(httr)
set_config( config( ssl.verifypeer = 0L ) )

devtools::install_github("hadley/haven")
library(haven)
library(dplyr)
setwd('O:/Tom/R/predictshine')
well_being <- read_dta("opn_teaching_dataset.dta")
str(well_being)


as_numeric <- function(x, na_string = c(99, 98)){
	out = as.numeric(x)
	out[x %in% na_string] <- NA
	out
	}
names(well_being)	
well_being$overall_sat <- as_numeric(well_being$ MCZ_1)


well_being <- well_being %>%
	mutate(
		overall_sat = as_numeric(MCZ_1),
		sex = as_factor(rsex),
		health = as_factor(QHealthr),
		age = as_factor(agex),
		married = as_factor(marstat3r),
		eductaion = as_factor(highed4),
		ethnicity = as_factor(Ethnicity2r),
		region = as_factor(GorA),
		health = as_factor(QHealthr)
		
	)
	
well_being$ethnicity[ well_being$ethnicity == 'Refusal'] <- 'White'
well_being <- drop.levels(well_being)

well_being$age2 = NA	
well_being$age2[well_being$age == '16 to 24'] <- 20
well_being$age2[well_being$age == '25 to 44'] <- 30
well_being$age2[well_being$age == '45 to 54'] <- 50
well_being$age2[well_being$age == '55 to 64'] <- 60
well_being$age2[well_being$age == '65 to 74'] <- 70
well_being$age2[well_being$age == '75 and over'] <- 80

	
lm_1 = lm(overall_sat ~   poly(age2, degree = 2)  + health + sex + married + eductaion + ethnicity + region , data = well_being)
lm_1 = lm(overall_sat ~   age2  + health + sex + married + eductaion + ethnicity + region , data = well_being)
head(model.frame(lm_1))
head(lm_1$model)
model = lm_1

mf = model.frame(model)
test = mf[2]
names(test)
id = 
eval(names(test), envir = mf)

predict((test[[1]]), newdata = 20)


lm_test = lm(overall_sat ~   poly(age2, degree = 2) , data = well_being)
predict(lm_test, data.frame(age = 100))
predict.lm
Terms = delete.response(terms(lm_test))
delete.response(tt)
args( poly(age2, degree = 2))
?call

      m <- model.frame(Terms)

str(well_being)

str(terms)
str(eval(model$call$data, envir = environment(formula(model))))
head(terms)
    terms <- predict(model, type = "terms")
    else predict(model, type = "terms", se.fit = se, terms = terms)
    n.tms <- ncol(tms <- as.matrix(terms))

str(lm_1)
termplot(lm_1, partial.resid = TRUE)
table(well_being$ethnicity)
summary(lm_1)
devtools::load_all()

lm_1 = lm(overall_sat ~   age2  + health + sex + married + eductaion + ethnicity + region , data = well_being)
lm_1 = lm(overall_sat ~   age2  *region + sex  + married + age2*eductaion + ethnicity + health  , data = well_being)
summary(lm_1)
library(predictshine)
variable_descriptions = c('Age', "Region", 'Sex','Marital status', "What is the highest level of qualification?","Ethnicity White/Other", "How is your health in general?" ),

lm_1 = lm(overall_sat ~   age2  *region + sex  + married + age2*eductaion + ethnicity + health  , data = well_being)

predictshine(lm_1, 
	page_title = 'Happiness in the UK', 
	variable_descriptions = c('Age', "Region", 'Sex','Marital status', "What is the highest level of qualification?","Ethnicity White/Other", "How is your health in general?" ),
	main = 'Overall, how satisfied are you with your life nowadays?', 
	xlab =  'predicted score out of 10', 
	description = p('Alter variables to get predicted 
		overall life satisfaction (out of 10). This model is made using data from the 1,00 respondents of the ONS Opinions Survey, 
		Well-Being Module, April 2011'))
		
		
		?tabPanel
		
runApp(app,host="0.0.0.0",port=3168)
h4('test')		
	a = tabPanel(h4('test'), value = title, icon = NULL)	
str(a)		
model_data = fit$model[-1]
	n_vars = ncol(model_data)		
		ncol_termplot
				mp(mfrow = c(3,2), cex.axis = 1)
				termplot(lm_1, cex.axis = 1.2)

mp(mfrow = c(3,3), cex = 1)
termplot(lm_1)