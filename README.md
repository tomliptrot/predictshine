# predictshine
### interactive predictions in R with shiny
The predictshine package allows users to create interactive shiny based web-apps to make predictions about individuals. its main function `predictshine()` works similarly to the `predict()` function. First the user creates a statistical model, then calls `predictshine()` to bring up a browser based interface to predict the dependent variable based user entered changes to the independent variables. This app can the be shared with anyone via the web.

Currently, methods are implemented for linear regressions, logistic regressions and cox proportional hazards models.

### Installation
To install run:
```R
library(devtools)
install_github('tomliptrot/predictshine')
library(predictshine)
```

If you get the following error running `install_github()` (as I do because I am behind a corporate firewall)
```R
Downloading github repo tomliptrot/predictshine@master
Error in function (type, msg, asError = TRUE)  : 
  SSL certificate problem: unable to get local issuer certificate
```

 try running this first:

```R
library(RCurl)
library(httr)
set_config( config( ssl_verifypeer = 0L ) )
```

### Use
#### Linear regression
Create demo linear model using the `well_being` dataset
```R
lm_1 = lm(overall_sat ~   age2  * region + 
						  sex  + 
						  married + 
						  age2 * eductaion + 
						  ethnicity 
						  + health  , 
						  data = well_being)
```

Now call `predictshine()` to open an interactive browser window


```R
predictshine(lm_1, 
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
```
Giving you this:

![] (http://tomliptrot.github.io/lm_predictshine.JPG)

To close this window press escape in the R concole

#### Logistic regression 
```R
mylogit <- glm(admit ~ gre + gpa + rank, data = school, family = "binomial")

predictshine(mylogit)
```

#### Cox Proportional Hazards

Fit model using lung dataset from the survival package
```R
library(survival)

#Variables must be set to correct type outside of model call
lung$sex = factor(lung$sex )
lung$ph.ecog = factor(lung$ph.ecog )

#note model must be set to TRUE
fit_cox = coxph(Surv(time, status) ~ age + sex + ph.ecog , lung, model = TRUE) 

predictshine(fit_cox )
```

