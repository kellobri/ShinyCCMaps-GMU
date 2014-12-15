library(shiny)

shinyUI(fluidPage(    
	
	titlePanel("NCES Data Explorer: Time Series of Scaled Scores for 2007-2013"),

    sidebarLayout(      
    	sidebarPanel(
        	selectInput("dataset", "Select a Dataset:", 
                    choices=c("All Students Grade 8 Math","All Students Grade 8 Reading")),
        	hr(),
        	helpText("Data from The National Center for Education Statistics (nces.ed.gov)"),
			hr(),
			h4("Differences between 2007 and 2009: inprogress"),
			#textOutput("DiffList1")
			h4("Differences between 2009 and 2011: inprogress"),
			h4("Differences between 2011 and 2013: inprogress")
      ),
      
      mainPanel(
		conditionalPanel(align="center",
		    condition = "input.dataset == 'All Students Grade 8 Math'",
		    sliderInput("mSlider", "Choose the Math Score Thresholds:", 
		                 min=248, max=301, value=c(270,280))
		),
		
		conditionalPanel(align="center",
			condition = "input.dataset == 'All Students Grade 8 Reading'",
		    sliderInput("rSlider", "Choose the Reading Score Thresholds:", 
		                 min=241, max=277, value=c(255,265))  
		),
		
		fluidRow(
			column(3,align="middle",h4("2007")),
			column(3,align="middle",h4("2009")),
			column(3,align="middle",h4("2011")),
			column(3,align="middle",h4("2013"))
		),
		
        plotOutput("timeseriesPlot",height='200px'),
		
		fluidRow(
			column(2,h2(" ")),
			column(8,align="middle",plotOutput("differences",height='150px')),
			column(2,h2(" "))
		) 
      )
      
    )
  )
)