# predictshine
### Interactive predictions with shiny

To install run:
```R
library(devtools)


library(RCurl)
library(httr)
set_config( config( ssl.verifypeer = 0L ) )


install_github('tomliptrot/predictshine')
```

Then run:
```R
library(predictshine)

school$rank <- factor(school$rank)
school$admit <- factor(school$admit)
mylm <- lm(gre ~ admit + gpa + rank, data = school)

#This will launch a browser window that will allow interactive exploration of the linear model created earlier
predictshine(mylm)
```

