library(maps)
library(dplyr)
library(maptools)
library(ggplot2)
library(ggmap)

diabetesData <- read.csv("data/DiabetesData.csv",header=TRUE,stringsAsFactors=FALSE)

county.df <- map_data("county")
names(county.df)[5:6] <- c("state","county")

state.df=map_data("state")
state.df$region <- gsub(" ","",state.df$region)

diabetesData$County<-tolower(diabetesData$County)
diabetesData$State<-tolower(diabetesData$State)
names(diabetesData)[1:2] <- c("state","county")

county.df$state <- gsub(" ","",county.df$state)
county.df$county <- gsub(" ","",county.df$county)

D.data <- merge(county.df,diabetesData,by=c("state","county"))
D.data <- D.data[order(D.data$order),]

D.data$DiabetesRate=cut(D.data$Diabetes,breaks=c(0,4,8,10,12,14,16,18,20,100), include.lowest=T)

shinyServer(
  	function(input, output) {
	
	#Filter Data, Return Data Frame
	diabetesP <- reactive({
		minLAct <- input$LArange[1]
		maxLAct <- input$LArange[2]
		minOb <- input$OBrange[1]
		maxOb <- input$OBrange[2]

		#Apply Physical Activity and Obesity Prevalence Filters
		F.Data <- D.data %>%
			filter(
				LeisureInactivity >= minLAct,
				LeisureInactivity <= maxLAct,
				Obesity >= minOb,
				Obesity <= maxOb
			)
			 	
		F.Data

	})
	
	
	
	output$value <- renderPrint({ diabetesP() })
	
    output$map <- renderPlot({
		ggplot(diabetesP(), aes(long, lat, group = group)) +
		     geom_polygon(aes(fill = DiabetesRate), colour = "white", size = 0.2) + 
		     geom_polygon(data = state.df, colour = "gray", fill = NA) +
		     scale_fill_brewer(palette = "Greens") +
			 theme_bw() +
			 labs(x="Percentage of Diabetes Prevelence by US County")+
	 		 theme(plot.background = element_blank(),
	    	 			panel.grid.major = element_blank(),
	    			 	panel.grid.minor = element_blank(),
	    			 	panel.border = element_blank(),
	 					axis.line = element_blank(),
	 					axis.ticks = element_blank(), 
	 					axis.text.y = element_blank(),
	 					axis.text.x = element_blank(),
	 					axis.title.y=element_blank())
	
    })
  }
)
