# predictshine
### Interactive predictions with shiny

To install run:
```R
library(devtools)

# I need to run this bit of code as I am behind a corporate firewall
library(RCurl)
library(httr)
set_config( config( ssl.verifypeer = 0L ) )


install_github('tomliptrot/predictshine')
library(predictshine)
```

### Linear regression
```R
#create demo linear model using school dataset

mylm <- lm(gre ~ admit + gpa + rank, data = school)

# This will launch a browser window that will allow 
# interactive exploration of the linear model created earlier

predictshine(mylm)
```

### Logistic regression 
```R
mylogit <- glm(admit ~ gre + gpa + rank, data = school, family = "binomial")

predictshine(mylogit)
```

###Cox Proportional Hazards
```R
library(survival)

# Fit model using lung dataset
# VAriables must be set to correct type outside of model call
lung$sex = factor(lung$sex )
lung$ph.ecog = factor(lung$ph.ecog )

#note model must be set to TRUE
fit_cox = coxph(Surv(time, status) ~ age + sex + ph.ecog , lung, model = TRUE) 

predictshine(fit_cox )
```

