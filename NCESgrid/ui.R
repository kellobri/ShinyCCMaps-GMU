library(shiny)

shinyUI(navbarPage(

title = 'Data View Options',  
tabPanel('Maps Visualization',
	  
  fluidPage(	
  titlePanel("NAEP/NCES Grade 8 Math Scores by State and Parent Education"),
  
  fluidRow(
	  column(4,align="center",h5("Update the Test Score Difference Slider to view the difference in state average math scores from the national average.")),
	  column(4,align="center",
	  		sliderInput("color", "Data Masking Slider: Show states with a score difference from National Average (285 Points) equal to or greater than the slider value.",  min=0, max=33, value=0)),
	  column(4,align="center",h5("States in green have higher scores than the national average. States in purple have lower scores than the national average. States with nuetral color did not have data to satisfy reporting standards."))
	  ),
	  
  hr(),
  
  fluidRow(
	  column(3,align="center",h3("Parent Education:")),
	  column(2,align="center",h5("Father Did Not Finish High School")),
	  column(2,align="center",h5("Father Graduated High School")),
	  column(2,align="center",h5("Father had Education After High School")),
	  column(2,align="center",h5("Father Graduated College")),
	  column(1,h5(""))
	  ),
  
  fluidRow(
	  column(3,align="right",h5("Mother Did Not"),h5("Finish High School")),
      column(8,
        plotOutput("dfhs_Plot", height = "150px")),
	  column(1,h5(""))
	  ),
	  
  fluidRow(
	  column(3,align="right",h5("Mother Graduated"),h5("High School")),	  
      column(8,
        plotOutput("ghs_Plot", height = "150px"))
	  ),
	  
  fluidRow(
	  column(3,align="right",h5("Mother Had Some"),h5("Education After High School")),
      column(8,
        plotOutput("sedhs_Plot", height = "150px"))
	  ),
	  
  fluidRow(
	  column(3,align="right",h5("Mother Graduated"),h5("from College")),
      column(8,
        plotOutput("gcoll_Plot", height = "150px"))
	  )
  )),
  
  tabPanel('Data Table: Mother Ed Level 1', dataTableOutput('dataTab.gfhs')),
  tabPanel('Data Table: Mother Ed Level 2', dataTableOutput('dataTab.ghs')),
  tabPanel('Data Table: Mother Ed Level 3', dataTableOutput('dataTab.sedhs')),
  tabPanel('Data Table: Mother Ed Level 4', dataTableOutput('dataTab.gcoll'))
	  
))