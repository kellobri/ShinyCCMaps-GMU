library(maps)
library(ggplot2)
library(dplyr)
library(shiny)
library(gridExtra)

crosst <- read.csv("data/parentEd.csv",header=TRUE,stringsAsFactors=FALSE)


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

dfhs.m<-crosst[crosst$med == 1,]
ghs.m<-crosst[crosst$med == 2,]
sedhs.m<-crosst[crosst$med == 3,]
gcoll.m<-crosst[crosst$med == 4,]

shinyServer(
	function(input,output) {
		
		#Filter Data, Return Data Frame
		dataR <- reactive({
			
			dnum <- input$color
			
			data <- dfhs.m
			
			#Calculate the differences
			
			C1 = seq(1:49)
			C1m = seq(1:49)
			for (a in 1:49) {
				if (abs(dfhs.m$dfhs[a]-285)>=dnum){
					C1[a] = TRUE
					} else C1[a] = FALSE
				if (dfhs.m$dfhs[a]==0){
					C1m[a] = "W"
					} else C1m[a] = "L"
				if (dfhs.m$dfhs[a]-285>0){
					C1m[a] = "H"
				} 
			}
			PrintC1 <- ifelse(C1,states,"N")
			DiffC1 = ifelse(C1,"Y","N")
			ColorC1 = ifelse(C1,C1m,"N")
			
			data$DiffC1 <- DiffC1
			data$ColorC1 <- ColorC1
			
			C2 = seq(1:49)
			C2m = seq(1:49)
			for (b in 1:49) {
				if (abs(dfhs.m$ghs[b]-285)>=dnum){
					C2[b] = TRUE
					} else C2[b] = FALSE
				if (dfhs.m$ghs[b]==0){
					C2m[b] = "W"
					} else C2m[b] = "L"
				if (dfhs.m$ghs[b]-285>0){
					C2m[b] = "H"
				} 
			}
			PrintC2 <- ifelse(C2,states,"N")
			DiffC2 = ifelse(C2,"Y","N")
			ColorC2 = ifelse(C2,C2m,"N")
			
			data$DiffC2 <- DiffC2
			data$ColorC2 <- ColorC2
			
			C3 = seq(1:49)
			C3m = seq(1:49)
			for (c in 1:49) {
				if (abs(dfhs.m$sedhs[c]-285)>=dnum){
					C3[c] = TRUE
					} else C3[c] = FALSE
				if (dfhs.m$sedhs[c]==0){
					C3m[c] = "W"
					} else C3m[c] = "L"
				if (dfhs.m$sedhs[c]-285>0){
					C3m[c] = "H"
				} 
			}
			PrintC3 <- ifelse(C3,states,"N")
			DiffC3 = ifelse(C3,"Y","N")
			ColorC3 = ifelse(C3,C3m,"N")
			
			data$DiffC3 <- DiffC3
			data$ColorC3 <- ColorC3
			
			C4 = seq(1:49)
			C4m = seq(1:49)
			for (d in 1:49) {
				if (abs(dfhs.m$gcoll[d]-285)>=dnum){
					C4[d] = TRUE
					} else C4[d] = FALSE
				if (dfhs.m$gcoll[d]==0){
					C4m[d] = "W"
					} else C4m[d] = "L"
				if (dfhs.m$gcoll[d]-285>0){
					C4m[d] = "H"
				}
			}
			
			PrintC4 <- ifelse(C4,states,"N")
			DiffC4 = ifelse(C4,"Y","N")
			ColorC4 = ifelse(C4,C4m,"N")
			
			data$DiffC4 <- DiffC4
			data$ColorC4 <- ColorC4
			
			D1 = seq(1:49)
			D1m = seq(1:49)
			for (a in 1:49) {
				if (abs(ghs.m$dfhs[a]-285)>=dnum){
					D1[a] = TRUE
					} else D1[a] = FALSE
				if (ghs.m$dfhs[a]==0){
					D1m[a] = "W"
					} else D1m[a] = "L" 
				if (ghs.m$dfhs[a]-285>0){
					D1m[a] = "H"
				}
			}
			PrintD1 <- ifelse(D1,states,"N")
			DiffD1 = ifelse(D1,"Y","N")
			ColorD1 = ifelse(D1,D1m,"N")
			
			data$DiffD1 <- DiffD1
			data$ColorD1 <- ColorD1
			
			D2 = seq(1:49)
			D2m = seq(1:49)
			for (b in 1:49) {
				if (abs(ghs.m$ghs[b]-285)>=dnum){
					D2[b] = TRUE
					} else D2[b] = FALSE
				if (ghs.m$ghs[b]==0){
					D2m[b] = "W"
					} else D2m[b] = "L" 
				if (ghs.m$ghs[b]-285>0){
					D2m[b] = "H"
				}
			}
			PrintD2 <- ifelse(D2,states,"N")
			DiffD2 = ifelse(D2,"Y","N")
			ColorD2 = ifelse(D2,D2m,"N")
			
			data$DiffD2 <- DiffD2
			data$ColorD2 <- ColorD2
			
			D3 = seq(1:49)
			D3m = seq(1:49)
			for (c in 1:49) {
				if (abs(ghs.m$sedhs[c]-285)>=dnum){
					D3[c] = TRUE
					} else D3[c] = FALSE 
				if (ghs.m$sedhs[c]==0){
					D3m[c] = "W"
					} else D3m[c] = "L" 
				if (ghs.m$sedhs[c]-285>0){
					D3m[c] = "H"
				}
			}
			PrintD3 <- ifelse(D3,states,"N")
			DiffD3 = ifelse(D3,"Y","N")
			ColorD3 = ifelse(D3,D3m,"N")
			
			data$DiffD3 <- DiffD3
			data$ColorD3 <- ColorD3
			
			D4 = seq(1:49)
			D4m = seq(1:49)
			for (d in 1:49) {
				if (abs(ghs.m$gcoll[d]-285)>=dnum){
					D4[d] = TRUE
					} else D4[d] = FALSE
				if (ghs.m$gcoll[d]==0){
					D4m[d] = "W"
					} else D4m[d] = "L" 
				if (ghs.m$gcoll[d]-285>0){
					D4m[d] = "H"
				} 
			}
			PrintD4 <- ifelse(D4,states,"N")
			DiffD4 = ifelse(D4,"Y","N")
			ColorD4 = ifelse(D4,D4m,"N")
			
			data$DiffD4 <- DiffD4
			data$ColorD4 <- ColorD4
			
			E1 = seq(1:49)
			E1m = seq(1:49)
			for (a in 1:49) {
				if (abs(sedhs.m$dfhs[a]-285)>=dnum){
					E1[a] = TRUE
					} else E1[a] = FALSE 
				if (sedhs.m$dfhs[a]==0){
					E1m[a] = "W"
					} else E1m[a] = "L" 
				if (sedhs.m$dfhs[a]-285>0){
					E1m[a] = "H"
				}
			}
			PrintE1 <- ifelse(E1,states,"N")
			DiffE1 = ifelse(E1,"Y","N")
			ColorE1 = ifelse(E1,E1m,"N")
			
			data$DiffE1 <- DiffE1
			data$ColorE1 <- ColorE1
			
			E2 = seq(1:49)
			E2m = seq(1:49)
			for (b in 1:49) {
				if (abs(sedhs.m$ghs[b]-285)>=dnum){
					E2[b] = TRUE
					} else E2[b] = FALSE
				if (sedhs.m$ghs[b]==0){
					E2m[b] = "W"
					} else E2m[b] = "L" 
				if (sedhs.m$ghs[b]-285>0){
					E2m[b] = "H"
				} 
			}
			PrintE2 <- ifelse(E2,states,"N")
			DiffE2 = ifelse(E2,"Y","N")
			ColorE2 = ifelse(E2,E2m,"N")
			
			data$DiffE2 <- DiffE2
			data$ColorE2 <- ColorE2
			
			E3 = seq(1:49)
			E3m = seq(1:49)
			for (c in 1:49) {
				if (abs(sedhs.m$sedhs[c]-285)>=dnum){
					E3[c] = TRUE
					} else E3[c] = FALSE
				if (sedhs.m$sedhs[c]==0){
					E3m[c] = "W"
					} else E3m[c] = "L" 
				if (sedhs.m$sedhs[c]-285>0){
					E3m[c] = "H"
				} 
			}
			PrintE3 <- ifelse(E3,states,"N")
			DiffE3 = ifelse(E3,"Y","N")
			ColorE3 = ifelse(E3,E3m,"N")
			
			data$DiffE3 <- DiffE3
			data$ColorE3 <- ColorE3
			
			E4 = seq(1:49)
			E4m = seq(1:49)
			for (d in 1:49) {
				if (abs(sedhs.m$gcoll[d]-285)>=dnum){
					E4[d] = TRUE
					} else E4[d] = FALSE
				if (sedhs.m$gcoll[d]==0){
					E4m[d] = "W"
					} else E4m[d] = "L" 
				if (sedhs.m$gcoll[d]-285>0){
					E4m[d] = "H"
				}
			}
			PrintE4 <- ifelse(E4,states,"N")
			DiffE4 = ifelse(E4,"Y","N")
			ColorE4 = ifelse(E4,E4m,"N")
			
			data$DiffE4 <- DiffE4
			data$ColorE4 <- ColorE4
			
			F1 = seq(1:49)
			F1m = seq(1:49)
			for (a in 1:49) {
				if (abs(gcoll.m$dfhs[a]-285)>=dnum){
					F1[a] = TRUE
					} else F1[a] = FALSE
				if (gcoll.m$dfhs[a]==0){
					F1m[a] = "W"
					} else F1m[a] = "L" 
				if (gcoll.m$dfhs[a]-285>0){
					F1m[a] = "H"
				} 
			}
			PrintF1 <- ifelse(F1,states,"N")
			DiffF1 = ifelse(F1,"Y","N")
			ColorF1 = ifelse(F1,F1m,"N")
			
			data$DiffF1 <- DiffF1
			data$ColorF1 <- ColorF1
			
			F2 = seq(1:49)
			F2m = seq(1:49)
			for (b in 1:49) {
				if (abs(gcoll.m$ghs[b]-285)>=dnum){
					F2[b] = TRUE
					} else F2[b] = FALSE
				if (gcoll.m$ghs[b]==0){
					F2m[b] = "W"
					} else F2m[b] = "L" 
				if (gcoll.m$ghs[b]-285>0){
					F2m[b] = "H"
				} 
			}
			PrintF2 <- ifelse(F2,states,"N")
			DiffF2 = ifelse(F2,"Y","N")
			ColorF2 = ifelse(F2,F2m,"N")
			
			data$DiffF2 <- DiffF2
			data$ColorF2 <- ColorF2
			
			F3 = seq(1:49)
			F3m = seq(1:49)
			for (c in 1:49) {
				if (abs(gcoll.m$sedhs[c]-285)>=dnum){
					F3[c] = TRUE
					} else F3[c] = FALSE
				if (gcoll.m$sedhs[c]==0){
					F3m[c] = "W"
					} else F3m[c] = "L" 
				if (gcoll.m$sedhs[c]-285>0){
					F3m[c] = "H"
				}
			}
			PrintF3 <- ifelse(F3,states,"N")
			DiffF3 = ifelse(F3,"Y","N")
			ColorF3 = ifelse(F3,F3m,"N")
			
			data$DiffF3 <- DiffF3
			data$ColorF3 <- ColorF3
			
			F4 = seq(1:49)
			F4m = seq(1:49)
			for (d in 1:49) {
				if (abs(gcoll.m$gcoll[d]-285)>=dnum){
					F4[d] = TRUE
					} else F4[d] = FALSE
				if (gcoll.m$gcoll[d]==0){
					F4m[d] = "W"
					} else F4m[d] = "L" 
				if (gcoll.m$gcoll[d]-285>0){
					F4m[d] = "H"
				}
			}
			PrintF4 <- ifelse(F4,states,"N")
			DiffF4 = ifelse(F4,"Y","N")
			ColorF4 = ifelse(F4,F4m,"N")
			
			data$DiffF4 <- DiffF4
			data$ColorF4 <- ColorF4
			
			#Compile Data Frame
			us_state_map <- map_data('state')
			mdata <- merge(us_state_map, data, by='region', all=T)
			mdata <- mdata[order(mdata$order), ]
			
			mdata
			 
		})
		
		output$dataTab.dfhs <- renderDataTable(dfhs.m[,2:6], options = list(pageLength = 49))
		output$dataTab.ghs <- renderDataTable(ghs.m[,2:6], options = list(pageLength = 49))
		output$dataTab.sedhs <- renderDataTable(sedhs.m[,2:6], options = list(pageLength = 49))
		output$dataTab.gcoll <- renderDataTable(gcoll.m[,2:6], options = list(pageLength = 49))
		
		dbdr <-c("N" = "wheat3", "Y" = "wheat3")
		dcols <- c("L"="mediumorchid4","H"="springgreen4","N" = "white","W"="wheat2")
		
	    output$dfhs_Plot <- renderPlot({
			datapl = dataR()
		
			pC1<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffC1, fill=ColorC1)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			+ labs(y="Mother did not finish High School")
			)
		
			pC2<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffC2, fill=ColorC2)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
	
			pC3<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffC3, fill=ColorC3)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
			
			pC4<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffC4, fill=ColorC4)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
		
			grid.arrange(pC1,pC2,pC3,pC4,ncol=4)
		
	    })
		
	    output$ghs_Plot <- renderPlot({
			datapl = dataR()
		
			pD1<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffD1, fill=ColorD1)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			+ labs(y="Mother graduated High School")
			)
		
			pD2<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffD2, fill=ColorD2)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
	
			pD3<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffD3, fill=ColorD3)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
			
			pD4<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffD4, fill=ColorD4)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
		
			grid.arrange(pD1,pD2,pD3,pD4,ncol=4)
		
	    })
		
	    output$sedhs_Plot <- renderPlot({
			datapl = dataR()
		
			pE1<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffE1, fill=ColorE1)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			+ labs(y="Mother - some education post High School")
			)
		
			pE2<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffE2, fill=ColorE2)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
	
			pE3<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffE3, fill=ColorE3)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
			
			pE4<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffE4, fill=ColorE4)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
		
			grid.arrange(pE1,pE2,pE3,pE4,ncol=4)
		
	    })
		
	    output$gcoll_Plot <- renderPlot({
			datapl = dataR()
		
			pF1<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffF1, fill=ColorF1)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			+ labs(y="Mother Graduated College")
			)
		
			pF2<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffF2, fill=ColorF2)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
	
			pF3<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffF3, fill=ColorF3)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
			
			pF4<-(qplot(long, lat, data=datapl, geom="polygon", group=group, colour=DiffF4, fill=ColorF4)
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
			+ scale_fill_manual(values = c(dcols,"black","black"))
			+ scale_colour_manual(values = c(dbdr,"black","black"))
			)
		
			grid.arrange(pF1,pF2,pF3,pF4,ncol=4)
		
	    })
		
})