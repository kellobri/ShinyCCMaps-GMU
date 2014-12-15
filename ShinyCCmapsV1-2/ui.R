rdata <- read.csv("data/DiabetesData.csv",header=TRUE,stringsAsFactors=FALSE)

shinyUI(fluidPage(
	h3("CC Maps Test Project v.1.2 : Diabetes Prevelence Data (CDC)"),
	plotOutput("map"),
	 
	fluidRow(
		hr(),
		
		column(3, offset = 2,
	    h4("Leisure Time Physical"),
		h4("Inactivity Prevalence"),
	    sliderInput("LArange", "Range:", min = min(rdata$LeisureInactivity), max = max(rdata$LeisureInactivity), value = c(18,31))
	    ),
		
		#column(4, offset = 1,
	    #h3("Diagnosed Diabetes Prevalence"),
	    #sliderInput("DIrange", "Range:", min = min(rdata$Diabetes), max = max(rdata$Diabetes), value = c(5,15))
	    #),
		
		column(3, offset = 1,
	    h4("Obesity Prevalence"),
	    sliderInput("OBrange", "Range:", min = min(rdata$Obesity), max = max(rdata$Obesity), value = c(20,40))
	    )
  
						  
	)
))

