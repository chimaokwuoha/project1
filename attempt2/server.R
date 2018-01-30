library(leaflet)
library(dplyr)
server <- function(input, output, session) {
  
  
  #the next 2 output plots will be called for the 2nd tab in the UI

  output$plot1 <- renderPlot({
    
    #chose items based on user input
    
    gtd1=gtd%>%arrange(.,region)
    gtd1=gtd1%>%filter(.,region_txt==input$area)
    gtd1$country_txt=droplevels(gtd1$country_txt)
    gtd1=gtd1%>%filter(.,iyear==input$slider)
    
    #organize data into a table of Death Toll and region
    
    gtd1=gtd1%>%group_by(.,country_txt)
    gtd1=gtd1%>%summarise(.,Incidents=n(), DeathT=sum(nkill))
    
    #create bar graph 
    
    gtd2 = ggplot(gtd1, aes(x = gtd1$country_txt, y = gtd1$Incidents)) + 
      geom_bar(fill="dark green",stat="identity")+coord_flip()+
      xlab("Country")+ylab("Number of Incidents")+ggtitle("Terror Attacks")
    
    
    gtd2+scale_x_discrete(drop=FALSE)+scale_y_continuous(limits=c(0, 1000))
  })  
  
  #line graph on tab2
  output$plotyr<-renderPlot({
    
    #filter by users input
    gtdy=gtd%>%filter(.,region_txt==input$area)
    
    #Organize data by year
    gtdy=gtdy%>%group_by(.,iyear)
    gtdy=gtdy%>%filter(.,iyear!=2005)#delete extraneous messy datapoints 
    
    
    #create line graph of Death toll by year
    gtdy=gtdy%>%summarise(.,DeathToll=sum(nkill,na.rm = TRUE))
    g <- ggplot(data = gtdy, aes(x = iyear,y=DeathToll)) + geom_line()
    g
  })
#The next graphs will be in tab 3
  
  output$tableTerror <- renderTable({
    
    #collect data based on user input
    gtdm3=gtd%>%filter(.,region_txt==input$reg3)
    gtdm3=gtdm3%>%filter(.,iyear==input$format)
    
    #create table of Group/DeathToll and corresponding color
    gtdm3=gtdm3%>%group_by(.,Group)
    gtdm3=gtdm3%>%summarise(.,DeathToll=sum(nkill,na.rm = TRUE))
    
    gtdm3=head(gtdm3%>%arrange(.,desc(DeathToll)))
    
    gtdm3=gtdm3%>%mutate(.,Color=c("green","blue","orange","pink","purple","red")[1:nrow(gtdm3)])
    gtdm3
  })
  
  
  #this is the object that creates the world map on tab3
  output$plotmp2 <- renderLeaflet({
    
    #this object creates a loading bar indicting to the user to be patient
    
    withProgress(message = 'Idntifying Terrorist Groups', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      for (i in 1:n-5) {
        incProgress(1/n, detail = paste("Doing part", i))
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.7)
      }
    })
    
    
    #filter data based in user input
    
    gtdm3=gtd%>%filter(.,region_txt==input$reg3)
    gtdm3=gtdm3%>%filter(.,iyear==floor(input$format))
    
    
    
    gtdm3=gtdm3%>%group_by(.,Group)
    gtdm3=gtdm3%>%summarise(.,DeathToll=sum(nkill,na.rm = TRUE))
    gtdm3=head(gtdm3%>%arrange(.,desc(DeathToll)))
    
    #create an gtdm2 obgect that contains a list of all the terroris incidents
    gtdm2=gtd%>%filter(.,gtd$Group==gtdm3$Group)
    gtdm2=gtdm2%>%filter(.,gtdm2$region_txt==input$reg3)
    
    head_Group <<- gtdm3$Group
    #this object is needed in the color coder

    
    #this object is designed to determinr the icon color based on the terrorist group
    getColor <- function(df) {
      sapply(df$Group, function(Group) {
        if(Group == head_Group[1]) {
          "green"
        } else if(Group == head_Group[2]) {
          "blue"
        }else if(Group == head_Group[3]) {
          "orange"
        }else if(Group == head_Group[4]) {
          "pink"
        }else if(Group == head_Group[5]) {
          "purple"
        }else {
          "red"
        }
      }
      )
    }
    
    
    #this is the icon ba
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(gtdm2)
    )
    
    #puts a progress bar to show user calculations are still being made
    
    withProgress(message = 'Locating Incidents', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      for (i in 1:n-5) {
        incProgress(1/n, detail = paste("Doing part", i))
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    # This places markers based location of terrorist incidents
    
    m = leaflet(gtdm2) %>% addTiles() %>%#setView(zoom = 14)%>%
      addAwesomeMarkers(lng=~longitude, lat=~latitude, 
                        icon=icons, label=as.character(gtdm2$Group))
    m
    
    
  })

  
  #this observer object allows the region input change the nations options that the user can select from  
  observe({
    nat1 = gtd%>%filter(.,region_txt==input$reg2)
    #filter based on picked reion
    
    
    nat2 <-unique(nat1$country_txt,na.rm=TRUE)
    #get a list of countries in that region
    
    #update choices in for countries
    updateSelectizeInput(
      session, "Nat",
      choices = nat2,
      selected = nat2[1])
  })
  
    output$plotcountry<-renderTable({
      
    #filter based on user input choice
    gtdm4=gtd%>%filter(.,region_txt==input$reg2)
    gtdm4=gtdm4%>%filter(.,country_txt==input$Nat)
    
    
    #create table of Group and country
    gtdm4=gtdm4%>%group_by(.,Group)
    gtdm4=gtdm4%>%summarise(.,DeathToll=sum(nkill,na.rm = TRUE))
    gtdm4=head(gtdm4%>%arrange(.,desc(DeathToll)))
    
    gtdm4 
  })
  
  output$perp<-renderTable({
    

    gtdm5=gtd%>%filter(.,region_txt==input$reg2)
    gtdm5=gtdm5%>%filter(.,country_txt==input$Nat)
    
    #create table of Nationality of Perpatrator and Deathtoll
    gtdm5=gtdm5%>%group_by(.,Nationality=natlty1_txt)
    gtdm5=gtdm5%>%summarise(.,DeathToll=sum(nkill,na.rm = TRUE))
    gtdm5=head(gtdm5%>%arrange(.,desc(DeathToll)))
    
    
    gtdm5 
    
  })

  
  output$targ<-renderTable({
    
    #filter based on user input choice
    gtdm5=gtd%>%filter(.,region_txt==input$reg2)
    gtdm5=gtdm5%>%filter(.,country_txt==input$Nat)
    
    
    #create a table target type and corresponding deathtoll
    gtdm5=gtdm5%>%group_by(.,Target=targtype1_txt)
    gtdm5=gtdm5%>%summarise(.,DeathToll=sum(nkill,na.rm = TRUE))
    gtdm5=head(gtdm5%>%arrange(.,desc(DeathToll)))
    gtdm5 
    
  })

  output$plotyrnat<-renderPlot({
    
    #filter by users input
    gtdn=gtd%>%filter(.,region_txt==input$reg2)
    gtdn=gtdn%>%filter(.,country_txt==input$Nat)
    
    #Organize data by year
    gtdn=gtdn%>%group_by(.,iyear)
    gtdn=gtdn%>%filter(.,iyear!=2005)#delete extraneous messy datapoints 
    
    
    #create line graph of Death toll by year
    gtdn=gtdn%>%summarise(.,DeathToll=sum(nkill,na.rm = TRUE))
    g <- ggplot(data = gtdn, aes(x = iyear,y=DeathToll)) + geom_line()
    g
  })
  

}