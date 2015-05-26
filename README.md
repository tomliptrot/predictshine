# predictshine
### interactive predictions in R with shiny
The predictshine package allows users to create interactive shiny based web-apps to make predictions about individuals. its main function `predictshine()` functions similarly to the `predict()` function. First the user creates a statisticical model, then calls `predictshine()` to bring up a browser based interace to predict the dependenet variable based on changes to the independent variable. 

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
set_config( config( ssl.verifypeer = 0L ) )
```

### Use
#### Linear regression
create demo linear model using school dataset
```R
mylm <- lm(gre ~ admit + gpa + rank, data = school)
```

This will launch a browser window that will allow interactive exploration of the linear model created earlier

```R
predictshine(mylm)
```

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

