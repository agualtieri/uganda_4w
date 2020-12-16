# load the required packages

require(shinydashboard)
library(ggplot2)
library(shinythemes)
library(classInt)
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(rgdal)
library(rmapshaper)


# load data
Dist_actors<- read.csv("UGA_COVID_Actors.csv")    #DATA BY PILLAR (TAB 1)
activ_dat<- read.csv("UGA_COVID_Activities.csv")  #DATA BY ACTIVITY (TAB 2)

#DERIVE TOTAL ACTORS PER DISTRICT FOR TAB 1##########################################################################
#the dataset by default has some of the same actor in several pillars

Dist_actors0<- as.data.frame(t(Dist_actors[,c(1,13:23)])) #subset district and actor names columns, then flip axis
df_dist = NULL

## Coerce all column of the dataset into characters
cols <- colnames(Dist_actors0)
Dist_actors0[cols] <- lapply(Dist_actors0[cols], as.character)


#For loop to iterate through all pillar actors per district and sum total actors per district 
for(i in names(Dist_actors0)){ #iterate through all district columns
  tryd<- as.character(unlist(strsplit(Dist_actors0[-1,i] , "\\|"))) #separate text strings by | for column i
  activA<-as.data.frame(unique(trimws(tryd))) #create dataframe of each actor, removing duplicates
  activA_p<-paste(activA[,1], collapse = ", ") 
  nospc<-count(as.data.frame(activA[activA!="None",])) #count all actors, exclude "None" entries
  df_dist = rbind(df_dist, data.frame(Dist_actors0[1,i],activA_p,nospc))
}


#DERIVE TOTAL ACTORS PER Pillar FOR TAB 1####################################
Dist_actors1<- Dist_actors[,c(13:23)]
df_pillar = NULL

## Coerce all column of the dataset into characters
cols <- colnames(Dist_actors1)
Dist_actors1[cols] <- lapply(Dist_actors1[cols], as.character)

#For loop to iterate through all pillar actors per district and sum total actors per district 
for(i in names(Dist_actors1)){ #iterate through all district columns
  Pillar<-colnames(Dist_actors1[i])
  tryd<-unlist( strsplit( Dist_actors1[,i] , "\\|" ) ) #separate text strings by | for column i
  activA<-as.data.frame(unique(trimws(tryd)))
  activA_p<-paste(activA[,1], collapse = ", ")
  nospc<-count(as.data.frame(activA[activA!="None",])) #count all actors, exclude "None" entries
  df_pillar = rbind(df_pillar, data.frame(Pillar,activA_p,nospc))
}

#DERIVE TOTAL DISTRICTS PER Pillar FOR TAB 1####################################
Dist_actors2<- Dist_actors[,c(1:12)]
df_pillarDist = NULL

## Coerce all column of the dataset into characters
cols <- colnames(Dist_actors2)
Dist_actors2[cols] <- lapply(Dist_actors2[cols], as.character)


#For loop to iterate through all pillar actors per district and sum total actors per district 
for(i in names(Dist_actors2[-1])){ #iterate through all district columns
  Pillar<-colnames(Dist_actors2[i])
  non0 <- Dist_actors2[Dist_actors2[i] != 0,]
  dists<-nrow(non0)
  df_pillarDist = rbind(df_pillarDist, data.frame(Pillar,dists))
  #print(nospc)
}

df_pillarDist<-cbind(df_pillarDist, actors=df_pillar$n)
##############################################################################################################################

districts <- st_read("shapefiles/DName_2019.shp")
districts <- ms_simplify(districts)
districts <- st_transform(districts, "+init=epsg:4326")


ALLmaxledg<- as.numeric(max(df_dist[,3]))

#districts<-districts[,c(1,3)]

## Join datasets
names(df_dist)[names(df_dist) == "Dist_actors0.1..i."] <- "District"
names(Dist_actors)[names(Dist_actors) == "district"] <- "District"
names(districts)[names(districts) == "DName2019"] <- "District"

list <- list(districts, Dist_actors, df_dist)
districts <- purrr::reduce(list, left_join)

Dist_actors2[2:12] <- lapply(Dist_actors2[2:12], as.numeric)
maxledg<-max(Dist_actors2[2:12]) #obtain max number of actors for legend


regions <- st_read("shapefiles/subregions.shp")
regions <- st_transform(regions, "+init=epsg:4326")

country <- st_read("shapefiles/country_boundary_revised.shp")
country <- st_transform(country, "+init=epsg:4326")

################################################################################################
# DATA PROCESSING: ====
################################################################################################
# add new columns for popup windows

districts <-districts %>% mutate( popup_Coordination_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","COORDINATION PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", Coordination_n, "<br>",
  "<b>","Actors present: ","</b>",Coordination, "<br>",
  "</div>"
)) %>% mutate(popup_PreventionControl_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","INFECTION PREVENTION CONTROL PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$PreventionControl_n, "<br>",
  "<b>","Actors present: ","</b>",districts$PreventionControl, "<br>",
  "</div>"
)) %>% mutate(popup_Surveillance_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","SURVEILLANCE PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$Surveillance_n, "<br>",
  "<b>","Actors present: ","</b>",districts$Surveillance, "<br>",
  "</div>"
)) %>% mutate(popup_CaseManagement_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","CASE MANAGEMENT PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$CaseManagement_n, "<br>",
  "<b>","Actors present: ","</b>",districts$CaseManagement, "<br>",
  "</div>"
)) %>%  mutate(popup_WASH_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","WASH PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$WASH_n, "<br>",
  "<b>","Actors present: ","</b>",districts$WASH, "<br>",
  "</div>"
)) %>% mutate(popup_ICTInnovation_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","ICT AND INNOVATION PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$ICTInnovation_n, "<br>",
  "<b>","Actors present: ","</b>",districts$ICTInnovation, "<br>",
  "</div>"
)) %>% mutate(popup_RiskCommunication_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","RISK COMMUNICATION PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$RiskCommunication_n, "<br>",
  "<b>","Actors present: ","</b>",districts$RiskCommunication, "<br>",
  "</div>"
)) %>% mutate( popup_MentalHealth_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","MENTAL HEALTH PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$MentalHealth_n, "<br>",
  "<b>","Actors present: ","</b>",districts$MentalHealth, "<br>",
  "</div>"
)) %>% mutate( popup_Logistics_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","LOGISTIC PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$Logistics_n, "<br>",
  "<b>","Actors present: ","</b>",districts$Logistics, "<br>",
  "</div>"
)) %>% mutate(popup_HumanResources_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","HUMAN RESOURCES PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$HumanResources_n, "<br>",
  "<b>","Actors present: ","</b>",districts$HumanResources, "<br>",
  "</div>"
)) %>% mutate(popup_Laboratory_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","LABORATORY PILLAR","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Refugee hosting district: ","</b>", districts$Ref_host, "<br>",
  "<b>","Actors active: ","</b>", districts$Laboratory_n, "<br>",
  "<b>","Actors present: ","</b>",districts$Laboratory, "<br>",
  "</div>"
)) %>% mutate(popup_n = paste(
  "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:100px'>",
  "<b>","ALL PILLARS","</b>","<br>",
  #"<b>","Sub Region: ","</b>", districts$F15Regions, "<br>",
  "<b>","District: ","</b>", districts$District, "<br>",
  "<b>","Actors active: ","</b>", districts$n, "<br>",
  "<b>","Actors present: ","</b>",districts$activA_p, "<br>",
  "</div>"
))



# Define UI 
ui <- fluidPage(theme = shinytheme("sandstone"), 
                tags$head(includeHTML("google-analytics.html")), 
                    # fluidRow(column(2,img(src="MoH_logoNEW2.jpg", height = 160)),
                    #         column(8,style = "margin-top: 42px;",
                    #                strong(HTML("<span style='font-size:27px'>Uganda Ministry of Health COVID-19 Response <br> Partner Activity Mapping</span>")))),
        
                   navbarPage(" ",
                              windowTitle = "Uganda 4W COVID-19 Response",
                              tabPanel(strong("Pillars"),
                                       icon = icon("map-marker"),
                                       # Sidebar layout with input and output definitions ----
                                       sidebarLayout(
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(width=4,
                                                       h5("This dashboard provides 4Ws  information (who, what, where, when) on actor presence and programmatic focus to support enhanced coordination under 
                                                          each pillar and across the COVID-19 response."),
                                                      
                                                       h5("This information was gathered from partners supporting the COVID-19 response via a survey that was circulated by the Ministry of Health. Partners submitted information based on program documents and other materials to 
                                                          represent their activities as accurately as possible."),
                                                      
                                                       h5("While the results presented here are a first baseline, efforts will be made to capture 4W information 
                                                          from a broader set of actors moving forward."),
                                                      
                                                       h5("If you are a response actor and would like to submit your information or update previously submitted information, please do so via this online ",
                                                          a("questionnaire", target="_blank", href="https://ee.humanitarianresponse.info/x/bh8qlOf8")),
                                                       
                                                       br(),
                                                      
                                                       h3("Pillar information at district level"),
                                                      
                                                       helpText("Click a district on the map to view actors that are active in the selected district"),
                                                       radioButtons("select", label = h4("Pillar information at district level:"),
                                                                    # Next we define the choices. The first argument always corresponds to what is displayed, the second to what the corresponding value is. Make sure you define these values such that they correspond with the columns you want to refer to.
                                                                    choices = list("ALL PILLARS"= "n", "Surveillance pillar" = "Surveillance_n", "Coordination pillar" = "Coordination_n","Infection Prevention and Control pillar " = "PreventionControl_n","Case Management pillar" = "CaseManagement_n",
                                                                                   "WASH pillar" = "WASH_n","ICT and Innovation pillar" = "ICTInnovation_n","Mental Health pillar" = "MentalHealth_n","Risk Communication pillar" = "RiskCommunication_n",
                                                                                   "Logistics pillar" = "Logistics_n","Human Resources pillar" = "HumanResources_n", "Laboratory pillar" = "Laboratory_n" ), 
                                                                    selected = "n"),
                                                       br(),
                                                       tags$h5("With support from:"),
                                                       div(img(src="ULEARN2.jpg", height = 80)), #img(src="UKAid.bmp", height = 80),
                                                       
                                         ),
                                         
                                         # Main panel for displaying outputs ----
                                         mainPanel(# tags$head(tags$style(HTML("a {color: #107AA7}"))),
                                                    width=8,
                                                    #tags$style(type = "text/css", "#map {height: calc(100vh) !important;"), # we add a CSS tag to make sure the map fills the entire area
                                                    h3(strong(textOutput("text_sel1"))),
                                                    h5("Total # of Actors: ", strong(textOutput("text_sel2", inline = TRUE))),
                                                    h5("Total # of Districts: ", strong(textOutput("text_sel3", inline = TRUE))),
                                                    leafletOutput("map", height=700),
                                                    h6("Please contact info@ulearn-uganda.org with feedback and questions about this application")
                                                    
                                         ),
                                         
                                       )),
                              tabPanel(strong("Activities"),
                                        icon= icon("th-list"),
                                        
                                       h3("Actor Activity Explorer "),
                                      helpText("Select an actor from the list to view their projects and other information"),
                                        selectizeInput(inputId =  "actorSel", label = h4("Actor:"), width = "50%",
                                                   choices = sort(activ_dat$Partner)),
                                       dataTableOutput("activityTable"))
                  )
)
 
# create the server functions for the dashboard  
server <- function(input, output) {  
  
  ## SELECTED PILLAR TEXT #####################
  pillarS <- reactive({
    switch(input$select, 
           "n" = "All Pillars", "Surveillance_n" =  "Surveillance pillar","Coordination_n" = "Coordination pillar", "PreventionControl_n" = "Infection Prevention and Control pillar", "CaseManagement_n" = "Case Management pillar", 
           "WASH_n" = "WASH pillar", "ICTInnovation_n" = "ICT and Innovation pillar", "MentalHealth_n" = "Mental Health pillar", "RiskCommunication_n" = "Risk Communication pillar",
           "Logistics_n"= "Logistics pillar", "HumanResources_n" = "Human Resources pillar", "Laboratroy_n" = "Laboratory pillar")
    
  })
  output$text_sel1<-renderText({
    pillarS()
  })
  
  ## SELECTED PILLAR TOTAL # ACTORS #####################
  actorsN <- reactive({
    switch(input$select, 
           "n" = as.character(count(as.data.frame(unique(activ_dat$Partner)))), 
           "Surveillance_n" =  df_pillarDist[df_pillarDist$Pillar=="Surveillance_n",3],
           "Coordination_n" = df_pillarDist[df_pillarDist$Pillar=="Coordination_n",3], 
           "PreventionControl_n" = df_pillarDist[df_pillarDist$Pillar=="PreventionControl_n",3], 
           "CaseManagement_n" = df_pillarDist[df_pillarDist$Pillar== "CaseManagement_n",3], 
           "WASH_n" = df_pillarDist[df_pillarDist$Pillar=="WASH_n",3], 
           "ICTInnovation_n" = df_pillarDist[df_pillarDist$Pillar=="ICTInnovation_n",3], 
           "MentalHealth_n" = df_pillarDist[df_pillarDist$Pillar=="MentalHealth_n",3], 
           "RiskCommunication_n" = df_pillarDist[df_pillarDist$Pillar=="RiskCommunication_n",3],
           "Logistics_n"= df_pillarDist[df_pillarDist$Pillar=="Logistics_n",3], 
           "HumanResources_n" = df_pillarDist[df_pillarDist$Pillar=="HumanResources_n",3],
           "Laboratory_n" = df_pillarDist[df_pillarDist$Pillar=="Laboratory_n",3])
    
  })
  output$text_sel2<-renderText({
    actorsN()
  })
  
  ## SELECTED PILLAR TOTAL # DISTRICTS #####################
  
  distN <- reactive({
    switch(input$select, 
           "n" = "135", 
           "Surveillance_n" =  df_pillarDist[df_pillarDist$Pillar=="Surveillance_n",2],
           "Coordination_n" = df_pillarDist[df_pillarDist$Pillar=="Coordination_n",2], 
           "PreventionControl_n" = df_pillarDist[df_pillarDist$Pillar=="PreventionControl_n",2], 
           "CaseManagement_n" = df_pillarDist[df_pillarDist$Pillar== "CaseManagement_n",2], 
           "WASH_n" = df_pillarDist[df_pillarDist$Pillar=="WASH_n",2], 
           "ICTInnovation_n" = df_pillarDist[df_pillarDist$Pillar=="ICTInnovation_n",2], 
           "MentalHealth_n" = df_pillarDist[df_pillarDist$Pillar=="MentalHealth_n",2], 
           "RiskCommunication_n" = df_pillarDist[df_pillarDist$Pillar=="RiskCommunication_n",2],
           "Logistics_n"= df_pillarDist[df_pillarDist$Pillar=="Logistics_n",2], 
           "HumanResources_n" = df_pillarDist[df_pillarDist$Pillar=="HumanResources_n",2],
           "Laboratory_n" = df_pillarDist[df_pillarDist$Pillar=="Laboratory_n",2])
    
  })
  
  output$text_sel3<-renderText({
    distN()
  })
  
  
  #change legend if All Pillars selected
  observeEvent(input$select,{
    pal2 <- colorNumeric(palette = c("YlGn"), domain = c(0,ALLmaxledg))
    pal <- colorNumeric(palette = c("#FAFCFD","#E8F1F5","#1AACEA","#149CD6","#107AA7","#094761"), domain = c(0,maxledg))
    map <- leafletProxy("map")
    map %>% clearControls()
    if (input$select == "n") {
      map %>% addLegend("bottomright", pal = pal2, values = c(0,ALLmaxledg),
                        title = "Total Number of Actors Present",
                        opacity = 1) %>%
        addPolygons(data = districts,
                    label = ~District,
                    stroke = TRUE, color = "grey",weight = 1, smoothFactor = 0.2, fillOpacity = 1,
                    fillColor = ~pal2(districts[[input$select]]), # here we dynamically define which column to use for the fill color, it takes the column we selected in the UI
                    popup = as.formula(paste0("~popup_",input$select)), #here we dynamically define which popup to display
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 1
                    )
        )
    }
    else {
      map %>% addLegend("bottomright", pal = pal, values = c(0,maxledg),
                        title = "Number of Actors Present",
                        opacity = 1)%>%
        addPolygons(data = districts,
                    label = ~District,
                    stroke = TRUE, color = "grey",weight = 1, smoothFactor = 0.2, fillOpacity = 1,
                    fillColor = ~pal(districts[[input$select]]), # here we dynamically define which column to use for the fill color, it takes the column we selected in the UI
                    popup = as.formula(paste0("~popup_",input$select)), #here we dynamically define which popup to display
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 1
                    )
        )
    }})

  
  # data_selected <- reactive({
  #   districts_df <- districts %>%
  #     dplyr::select(input$select, 
  #                   paste0(sub(input$select, pattern = "_n", replacement = "")))
  # })
  
  output$map <- renderLeaflet({
    

    
    leaflet(options=leafletOptions(zoomControl = FALSE)) %>%  
      fitBounds(28.230183082, -2,  35.289081871 , 4.0) %>% 
      #addTiles() %>% 
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      # addPolygons(data = districts,
      #             label = ~District,
      #             stroke = TRUE, color = "grey",weight = 1, smoothFactor = 0.2, fillOpacity = 1,
      #             fillColor = ~pal(districts[[input$select]]), # here we dynamically define which column to use for the fill color, it takes the column we selected in the UI
      #             popup = as.formula(paste0("~popup_",input$select)), #here we dynamically define which popup to display
      #             highlight = highlightOptions(
      #               weight = 5,
      #               color = "#666",
      #               fillOpacity = 1
      #             )
      # ) %>%
      addPolygons(data = regions, group = "Sub-regions", fill = FALSE, stroke = TRUE, color = "grey", weight = 1.5, opacity = 1) %>%
      #addPolygons(data = country, group = "Country", fill = FALSE, stroke = TRUE, color = "#000000", weight = 3, opacity = 1) %>%
      # addLegend(position = "bottomright", pal = pal, values = c(0,maxledg),  #here we dynamically define which column to build the legend with
      #           title = "Number of Actors Present",
      #           opacity = 1) %>%
      
      setMapWidgetStyle(style = list(background = "transparent"))
  })
  
  output$activityTable<-renderDataTable({
    activity_data<-activ_dat %>% 
      filter(Partner == input$actorSel) %>% 
      select(-ProjectID) %>% 
      rename("Status" = Project_Status, 
             "Supported Pillars" = Pillars_supported_by_project,
             "Activities" = Activities_conducted2, 
             #"Activities (non-pillar)"= Other_nonpPillar_activities,
             "Implementing Partner"= Implementing_partner)
    datatable(activity_data, extensions = 'Buttons', 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'pdf'),
                             paging=FALSE))
    
    
   #filter = "top"
    
  })
  
  
}


shinyApp(ui, server)
