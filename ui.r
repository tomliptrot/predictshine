 shinyUI(fluidPage(
		uiOutput("ui"),
		verbatimTextOutput(ids[1]),
		verbatimTextOutput(ids[2]),
		verbatimTextOutput(ids[3]),
		verbatimTextOutput('df')
		)
		
	)