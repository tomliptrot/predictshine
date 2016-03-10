library(devtools)
install_github("devtools", "hadley")
install_github('tomliptrot/predictshine')

appDependencies("//cht-gs1/clinicaloutcomes$/Tom/R/predictshine/happiness")

runApp("//cht-gs1/clinicaloutcomes$/Tom/R/predictshine/happiness")
library(shinyapps)
options(shinyapps.service_url = "http://api.shinyapps.io/v1")
deployApp("//cht-gs1/clinicaloutcomes$/Tom/R/predictshine/happiness", appName = 'happiness_demo')