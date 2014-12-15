library(maps)
library(ggplot2)
library(dplyr)
library(shiny)
library(gridExtra)

math <- read.csv("data/grade8math.csv",header=TRUE,stringsAsFactors=FALSE)
reading <- read.csv("data/grade8reading.csv",header=TRUE,stringsAsFactors=FALSE)

states = c("alabama","arizona","arkansas","california",
"colorado","connecticut","delaware","district of columbia",
"florida","georgia","idaho","illinois",
"indiana","iowa","kansas","kentucky",
"louisiana","maine","maryland","massachusetts",
"michigan","minnesota","mississippi","missouri",
"montana","nebraska","nevada","new hampshire",
"new jersey","new mexico","new york","north carolina",
"north dakota","ohio","oklahoma","oregon",
"pennsylvania","rhode island","south carolina","south dakota",
"tennessee","texas","utah","vermont",
"virginia","washington","west virginia","wisconsin",
"wyoming")

shinyServer(function(input, output) {

	
	dataR <- reactive({
		
		if (input$dataset == 'All Students Grade 8 Math'){
			data = math
			minT <- input$mSlider[1]
			maxT <- input$mSlider[2]
		} 
		if (input$dataset == 'All Students Grade 8 Reading'){
			data = reading
			minT <- input$rSlider[1]
			maxT <- input$rSlider[2]
		}
		
		lev <- c("L","M","H")
		
		cbreaks07 <- c(min(data$year2007),minT,maxT,max(data$year2007))
		grpCol07 <- cut(data$year2007,breaks=cbreaks07,inc=TRUE,lab=FALSE)
		lv07 <- factor(lev[grpCol07],levels=lev)
		data$lv07 <- lv07
		
		cbreaks09 <- c(min(data$year2009),minT,maxT,max(data$year2009))
		grpCol09 <- cut(data$year2009,breaks=cbreaks09,inc=TRUE,lab=FALSE)
		lv09 <- factor(lev[grpCol09],levels=lev)
		data$lv09 <- lv09
		
		cbreaks11 <- c(min(data$year2011),minT,maxT,max(data$year2011))
		grpCol11 <- cut(data$year2011,breaks=cbreaks11,inc=TRUE,lab=FALSE)
		lv11 <- factor(lev[grpCol11],levels=lev)
		data$lv11 <- lv11
		
		cbreaks13 <- c(min(data$year2013),minT,maxT,max(data$year2013))
		grpCol13 <- cut(data$year2013,breaks=cbreaks13,inc=TRUE,lab=FALSE)
		lv13 <- factor(lev[grpCol13],levels=lev)
		data$lv13 <- lv13
		
		#Calculate the differences
		D1 = seq(1:49)
		for (a in 1:49){
			if (lv09[a] != lv07[a]){
				D1[a] = TRUE
				} else D1[a] = FALSE
		}
		Diff1 = ifelse(D1,"Y","N")
		data$Diff1 <- Diff1
		
		D2 = seq(1:49)
		for (b in 1:49){
			if (lv11[b] != lv09[b]){
				D2[b] = TRUE
				} else D2[b] = FALSE
		}
		Diff2 = ifelse(D2,"Y","N")
		data$Diff2 <- Diff2
		
		D3 = seq(1:49)
		for (c in 1:49){
			if (lv13[c] != lv11[c]){
				D3[c] = TRUE
				} else D3[c] = FALSE
		}
		Diff3 = ifelse(D3,"Y","N")
		data$Diff3 <- Diff3

		
		#Compile Data Frame
		frm <- data.frame(data)
		
		us_state_map <- map_data('state')
		mdata <- merge(us_state_map, frm, by='region', all=T)
		mdata <- mdata[order(mdata$order), ]
		
		mlist = c(mdata,frm)
		mdata
	
	})	
	
	#Print Differences
	output$DiffList1 <- renderPrint({
		dat = dataR()
		me = dat[[2]]
		
		DL1 = numeric()
		for (d1 in 1:49) {
			if (me$lv07[d1]==me$lv09[d1]){
				DL1[d1] = FALSE
				} else DL1[d1] = TRUE
		}
		DL1 <- ifelse(DL1,states,"N")
		print1 = numeric()
		w1=1
		for (e1 in 1:49){
			if(DL1[e1]!="N"){
				print1[w1] = DL1[e1]
				w1 = w1+1
			}
		}
		if(length(print1)==0){
			print1[1]="No Differences"
		}
		print1
	})
	
	
	bdr <- c("L" = "white","M" = "white","H" = "white")
	cols <- c("L" = "tomato2","M" = "royalblue2","H" = "olivedrab")
	
	dbdr <- c("Y" = "black","N" = "black")
	dcols <- c("Y" = "mediumorchid4","N" = "white")
	
    output$timeseriesPlot <- renderPlot({
		datapl = dataR()
		
		pY1<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=lv07, fill=lv07)
		+ theme_bw()
		+ theme(plot.background = element_blank(),
   	 			panel.grid.major = element_blank(),
   			 	panel.grid.minor = element_blank(),
   			 	panel.border = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(), 
				axis.text.y = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y= element_blank(),
				axis.title.x=element_blank(),
				legend.position = "none")
		+ scale_fill_manual(values = cols)
		+ scale_colour_manual(values = bdr)
		)
	
		pY2<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=lv09, fill=lv09)
		+ theme_bw()
		+ theme(plot.background = element_blank(),
   	 			panel.grid.major = element_blank(),
   			 	panel.grid.minor = element_blank(),
   			 	panel.border = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(), 
				axis.text.y = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y=element_blank(),
				axis.title.x=element_blank(),
				legend.position = "none")
		+ scale_fill_manual(values = cols)
		+ scale_colour_manual(values = bdr)
		)

		pY3<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=lv11, fill=lv11)
		+ theme_bw()
		+ theme(plot.background = element_blank(),
   	 			panel.grid.major = element_blank(),
   			 	panel.grid.minor = element_blank(),
   			 	panel.border = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(), 
				axis.text.y = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y=element_blank(),
				axis.title.x=element_blank(),
				legend.position = "none")
		+ scale_fill_manual(values = cols)
		+ scale_colour_manual(values = bdr)
		)
		
		pY4<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=lv13, fill=lv13)
		+ theme_bw()
		+ theme(plot.background = element_blank(),
   	 			panel.grid.major = element_blank(),
   			 	panel.grid.minor = element_blank(),
   			 	panel.border = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(), 
				axis.text.y = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y=element_blank(),
				axis.title.x=element_blank(),
				legend.position = "none")
		+ scale_fill_manual(values = cols)
		+ scale_colour_manual(values = bdr)
		)
	
		grid.arrange(pY1,pY2,pY3,pY4,ncol=4)
	
    })
	
    output$differences <- renderPlot({
		datapl = dataR()
	
		pD1<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=Diff1, fill=Diff1)
		+ theme_bw()
		+ theme(plot.background = element_blank(),
   	 			panel.grid.major = element_blank(),
   			 	panel.grid.minor = element_blank(),
   			 	panel.border = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(), 
				axis.text.y = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y= element_blank(),
				#axis.title.x=element_blank(),
				legend.position = "none")
		+ scale_fill_manual(values = dcols)
		+ scale_colour_manual(values = dbdr)
		+ labs(x="2007-2009")
		)
	
		pD2<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=Diff2, fill=Diff2)
		+ theme_bw()
		+ theme(plot.background = element_blank(),
   	 			panel.grid.major = element_blank(),
   			 	panel.grid.minor = element_blank(),
   			 	panel.border = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(), 
				axis.text.y = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y=element_blank(),
				#axis.title.x=element_blank(),
				legend.position = "none")
		+ scale_fill_manual(values = dcols)
		+ scale_colour_manual(values = dbdr)
		+ labs(x="2009-2011")
		)

		pD3<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=Diff3, fill=Diff3)
		+ theme_bw()
		+ theme(plot.background = element_blank(),
   	 			panel.grid.major = element_blank(),
   			 	panel.grid.minor = element_blank(),
   			 	panel.border = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(), 
				axis.text.y = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y=element_blank(),
				#axis.title.x=element_blank(),
				legend.position = "none")
		+ scale_fill_manual(values = dcols)
		+ scale_colour_manual(values = dbdr)
		+ labs(x="2011-2013")
		)
	
		grid.arrange(pD1,pD2,pD3,ncol=3)
	
    })
	
})