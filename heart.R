rm(list=ls())

# Packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(dlnm)
library(splines)
library(sf)
library(reshape2)
library(leaflet)
library(sp)
library(gtools)
library(shinycssloaders)
library(rgdal)

setwd(".../HEART-main") # To change


# Databse dlnm
pat_dlnm <- readRDS("pat_dlnm.rds")

# Environmental data by zones
area_polmet <- readRDS("area_polmet.rds")

# Select variable
data <- area_polmet[,c("date","cp","max_NO2max","max_O3max","moy_PM10avg","max_delta_P","max_delta_T2","max_Pmax","max_T2max")]
# The best variables which represent the original variables are :
# NO2max : maximum
# O3max : maximum
# PM10avg : mean
# delta_P : maximum
# delta_T2 : maximum
# Pmax : maximum
# T2max : maximum

# Borders
areaPACA <- read_sf("areaPACA/areaPACA.shp")
areaPACA <- st_transform(areaPACA,crs=2154) # WGS84 --> lambert93, compatible with data pollution

data <- merge(data,areaPACA,by="cp")
data <- st_as_sf(data)

# Zip code with name
zip_code2018 <- read_sf("codes_postaux_laposte/laposte_hexasmal.shp")

zip_code2018PACA <- zip_code2018 %>% subset(substr(code_commun,1,2)=="06" | 
                                              substr(code_commun,1,2)=="05" |
                                              substr(code_commun,1,2)=="04" |
                                              substr(code_commun,1,2)=="13" | 
                                              substr(code_commun,1,2)=="84" | 
                                              substr(code_commun,1,2)=="83" )

zip_code2018PACA$insee <- zip_code2018PACA$code_commun
zip_code2018PACA <- as.data.frame(zip_code2018PACA)
zip_code2018PACA <- zip_code2018PACA[,c(7,2,3,4)]
zip_code2018PACA <- unique(zip_code2018PACA) # remove rows repeated

# Add Marseille to data zip_code2018PACA
om <- c("13055","MARSEILLE","13000","MARSEILLE")# missing Marseille with insee code "13055", coded by districts
zip_code2018PACA <- rbind(zip_code2018PACA,om)

zip_code2018PACA <- zip_code2018PACA[,c(3,4)]
colnames(zip_code2018PACA) <- c("cp","name")

zipcodePACA <- as.data.frame(data[,1])
zipcodePACA <- unique(zipcodePACA)

# Association zip codes of zones to their names
nameszip <- merge(zipcodePACA,zip_code2018PACA,by="cp",all.x=T) 
nameszip <- nameszip %>% mutate(name_cp=ifelse(cp=="06000_06100_06200_06300","NICE",
                                               ifelse(cp=="06130_06520","GRASSE",
                                                      ifelse(cp=="06150_06400","CANNES",
                                                             ifelse(cp=="06160_06600","ANTIBES",
                                                                    ifelse(cp=="13080_13090_13100_13290_13540","AIX EN PROVENCE/BEAURECUEIL/ST MARC JAUMEGARDE/LE THOLONET/ST ANTONIN SUR BAYON",
                                                                           ifelse(cp=="13104_13123_13129_13200_13280","ARLES",
                                                                                  ifelse(cp=="13117_13500","MARTIGUES",
                                                                                         ifelse(cp=="13118_13800","ISTRES",
                                                                                                ifelse(cp=="83000_83100_83200","TOULON/LE REVEST LES EAUX",
                                                                                                       ifelse(cp=="83380_83520","ROQUEBRUNE SUR ARGENS",
                                                                                                              ifelse(cp=="83530_83700","SAINT-RAPHAEL",
                                                                                                                     ifelse(cp=="84000_84140","AVIGNON",
                                                                                                                            ifelse(cp=="04160_04600","L ESCALE/CHATEAU ARNOUX ST AUBAN/MONTFORT",
                                                                                                                                   ifelse(cp=="04350_04510","MALIJAI/MIRABEAU/MALLEMOISSON/AIGLUN/LE CHAFFAUT ST JURSON",
                                                                                                                                          ifelse(cp=="13700_13730","ST VICTORET/MARIGNANE",
                                                                                                                                                 ifelse(cp=="83370_83600","	FREJUS/BAGNOLS EN FORET/LES ADRETS DE L ESTEREL",
                                                                                                                                                        ifelse(cp=="04000_04270_04330_04420","DIGNE LES BAINS/LA ROBINE SUR GALABRE/LA JAVIE/MAJASTRES/ST JEANNET/BEYNES/MEZEL/ESTOUBLON/BRAS D ASSE/SENEZ/ENTRAGES/ST JULIEN D ASSE/CHATEAUREDON/CLUMANC/BLIEUX/BARREME/TARTONNE/ST LIONS/ST JACQUES/CHAUDON NORANTE/ARCHAIL/LE BRUSQUET/MARCOUX/PRADS HAUTE BLEONE/PRADS HAUTE BLEONE/DRAIX/BEAUJEU ",name)
                                                                                                                                                 )))))))))))))))))

test <- dcast(nameszip, cp ~ name_cp)
test$names <- apply(test[,c(2:906)],1,paste,collapse="/")
test <- test %>% 
  mutate(names = stringr::str_remove_all(names, c("/NA"))) %>% 
  mutate(names = stringr::str_remove_all(names, c("NA/")))
test <- test[,c("cp","names")]

nameszip <- merge(nameszip,test)
nameszip$cp2 <- paste(nameszip$cp,nameszip$names,sep=" : ")
nameszip <- nameszip[,c(1,6)]
nameszip <- unique(nameszip)

data <- merge(data,nameszip,by="cp")

data <- data %>% arrange(cp,date)

pat_dlnm <- merge(pat_dlnm,nameszip,by="cp")

# remove zones with less 18 cardiac dyspnea
pat_dlnm <- pat_dlnm %>% group_by(cp) %>% mutate(tot=sum(Y))
pat_dlnm <- pat_dlnm %>% filter(tot>=18)

# remove zones with biased results
pat_dlnm2 <- pat_dlnm %>% subset(cp=="84860"|cp=="84800"|cp=="84700"|cp=="84600"|cp=="84530"|cp=="84500"|cp=="84400"|cp=="84310"|cp=="84300"|cp=="84290"|cp=="84270"|cp=="84160"|cp=="84130"|cp=="84120"|cp=="84110"|cp=="84100"|cp=="84000_84140"|
                                 cp=="83980"|cp=="83890"|cp=="83830"|cp=="83720"|cp=="83690"|cp=="83570"|cp=="83560"|cp=="83550"|cp=="83530_83700"|cp=="83510"|cp=="83500"|cp=="83490"|cp=="83480"|cp=="83470"|cp=="83460"|cp=="83440"|cp=="83400"|cp=="83390"|cp=="83380_83520"|cp=="83370_83600"|cp=="83340"|cp=="83320"|cp=="83300"|cp=="83270"|cp=="83260"|cp=="83250"|cp=="83230"|cp=="83220"|cp=="83210"|cp=="83190"|cp=="83170"|cp=="83160"|cp=="83150"|cp=="83140"|cp=="83136"|cp=="83130"|cp=="83110"|cp=="83000_83100_83200"|
                                 cp=="13700_13730"|cp=="13680"|cp=="13600"|cp=="13560"|cp=="13520"|cp=="13450"|cp=="13330"|cp=="13310"|cp=="13300"|cp=="13250"|cp=="13240"|cp=="13230"|cp=="13220"|cp=="13170"|cp=="13160"|cp=="13150"|cp=="13140"|cp=="13130"|cp=="13127"|cp=="13120"|cp=="13118_13800"|cp=="13117_13500"|cp=="13110"|cp=="13104_13123_13129_13200_13280"|cp=="13080_13090_13100_13290_13540"|cp=="13000"|
                                 cp=="06800"|cp=="06700"|cp=="06670"|cp=="06580"|cp=="06550"|cp=="06530"|cp=="06500"|cp=="06440"|cp=="06390"|cp=="06370"|cp=="06340"|cp=="06250"|cp=="06210"|cp=="06190"|cp=="06150_06400"|cp=="06130_06520"|cp=="06110"|cp=="06000_06100_06200_06300"|
                                 cp=="05300"|cp=="05000"|
                                 cp=="04860"|cp=="04800"|cp=="04700"|cp=="04300"|cp=="04220"|cp=="04200"|cp=="04160_04600"|cp=="04100"|cp=="04000_04270_04330_04420")

####### Onglet n1 : Maps

# Variables to factor
data$NO2 <- cut(data$max_NO2max,breaks = c(0,60,120,140,200,Inf),labels=c("Very good","Good","Moderate","Lightly","Heavily")) 
data$O3 <- cut(data$max_O3max,breaks = c(0,54,108,126,180,Inf),labels=c("Very good","Good","Moderate","Lightly","Heavily")) 
data$PM10 <- cut(data$moy_PM10avg,breaks = c(0,15,30,35,50,Inf),labels=c("Very good","Good","Moderate","Lightly","Heavily")) 
data$T2 <- cut(data$max_T2max,breaks=c(-Inf,0,10,20,30,Inf),labels =c("Very cold", "Cold","Temperate","Hot","Very hot"))
data$T2delta <- cut(data$max_delta_T2,breaks=c(0,5,11,17,22,28),labels=c("Very small","Small","Average","High","Very high"))
data$P <- cut(data$max_Pmax,breaks=c(-Inf,1001,1018,1035,1052,1069),labels=c("Very Low","Low","Average","High","Very high"))
data$Pdelta <- cut(data$max_delta_P,breaks=c(0,9,19,29,39,46),labels=c("Very small","Small","Average","High","Very high"))

# Borders
areaPACA <- areaPACA %>% arrange(cp)
pacapoly <- sf::as_Spatial(areaPACA)
pacapoly <- spTransform(pacapoly, CRS("+init=epsg:4326"))

######## Onglet n2 : Map

cp2 <- unique(data[,c("cp","cp2")])
front <- left_join(data.frame(areaPACA),cp2,by="cp") 

#table
level_pol <- data.frame(c("Reference (very good)","Good","Moderate","Lightly","Heavily"),c("50","61-120","121-140","141-200",">200"),
                        c("40","55-108","109-126","127-180",">180"),c("10","16-30","31-35","36-50",">50"))
colnames(level_pol) <- c("Levels of pollution","NO2(μg/m3)","O3(μg/m3)","PM10(μg/m3)")

# UI
#####################################################################################################################################################

############################# HEADER ###################
addResourcePath(prefix = 'img', directoryPath = 'image')

header <- dashboardHeader(title=a(img(src = "img/heart_logo1.png",height = "60px")),titleWidth = 250,
                          tags$li(class = "dropdown",
                                  tags$style(".main-header {max-height: 60px}"),
                                  tags$style(".main-header .logo {height: 60px}")
                          ),
                          tags$li(a(img(src = "img/uca.jpg",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown"),
                          tags$li(a(img(src = "img/uca_jedi.png",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown"),
                          tags$li(a(img(src = "img/mdlab2.png",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown"),
                          tags$li(a(img(src = "img/msi2.jpg",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown"),
                          tags$li(a(img(src = "img/CHU.jpg",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown"),
                          tags$li(a(img(src = "img/orupaca.png",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown"),
                          tags$li(a(img(src = "img/atmosud.png",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown"),
                          tags$li(a(img(src = "img/inria.png",height = "38px"),
                                    style = "padding-top:0px; padding-bottom:0px;"),
                                  class = "dropdown")
                          
)

############################ SIDEBAR ###################

sidebar <- dashboardSidebar(width=250,
                            sidebarMenu(menuItem("Exposition map",tabName="Maps",icon=icon("globe")),
                                        menuItem("Effect on health",tabName ="Graphlag",icon=icon("chart-line")),
                                        tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
                                        tags$div(tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                                 tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                                 tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                                 "Auteurs :",tags$br(),
                                                 "Fanny Simões", tags$br(), "Charles Bouveyron", tags$br(),
                                                 "Damien Piga", tags$br(),"Damien Borel", tags$br(),
                                                 "Stéphane Descombes", tags$br(), "Véronique Paquis-Flucklinger",
                                                 tags$br(),"Jaques Levraut",tags$br(),"Pierre Gibelin",tags$br(),"Silvia Bottini")
                            ))

############################ BODY ######################
body <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "Maps",
            
            fluidRow(
              box(selectInput(inputId = "input_polmet", choices=c("Nitrogen dioxide (NO2)"="NO2","Ozone (O3)"="O3", "Fine particle (PM10)"="PM10", "Temperature"="T2", "Pressure"="P", "Temperature amplitude"="T2delta", "Pressure amplitude"="Pdelta"),label="Select pollutant or meteorological variable :"),
                  dateInput(inputId = "input_date", label = "Select a day :",value = "2013-01-01", min = "2013-01-01", max = "2018-12-31",weekstart = 1),
                  status="primary",width = 2),
              
              box(mainPanel(leafletOutput("map",width = 700,height = 800) %>% withSpinner(type = 6,size = 2)),status="primary",width=8),
              
              box(title="Légende",
                  "Nitrogen dioxide - NO2 (µg/m3):",tags$br(),"0 - 60 = Very good",tags$br(),"61 - 120 = Good",tags$br(),"121 - 140 = Moderate",tags$br(),"141 - 200 = Lightly",tags$br(),"201 - 300 = Heavily",tags$br(),tags$br(),
                  "Ozone - O3 (µg/m3):",tags$br(),"0 - 54 = Very good",tags$br(),"55 - 108 = Good",tags$br(),"109 - 126 = Moderate",tags$br(),"127 - 180 = Lightly",tags$br(),"181 - 300 = Heavily",tags$br(),tags$br(),
                  "Fine particle - PM10 (µg/m3):",tags$br(),"0 - 15 = Very good",tags$br(),"16 - 30 = Good",tags$br(),"31 - 35 = Moderate",tags$br(),"36 - 50 = Lightly",tags$br(),"51 - 100 = Heavily",tags$br(),tags$br(),
                  "Temperature (°C):",tags$br(),"-15 - 0 = Very cold",tags$br(),"1 - 10 = Cold",tags$br(),"11 - 20 = Temperate",tags$br(),"21 - 30 = Hot",tags$br(),"31 - 39 = Very hot",tags$br(),tags$br(),
                  "Temperature amplitude (°C):",tags$br(),"0 - 5 = Very small",tags$br(),"6 - 11 = Small",tags$br(),"12 - 17 = Average",tags$br(),"18 - 23 = High",tags$br(),"24 - 28 = Very high",tags$br(),tags$br(),
                  "Pressure (hPa):",tags$br(),"985 - 1001 = Very Low",tags$br(),"1002 - 1018 = Low",tags$br(),"1019 - 1035 = Average",tags$br(),"1036 - 1052 = High",tags$br(),"1053 - 1069 = Very high",tags$br(),tags$br(),
                  "Pressure amplitude (hPa):",tags$br(),"0 - 9 = Very low",tags$br(),"10 - 19 = Low",tags$br(),"20 - 29 = Average",tags$br(),"30 - 39 = High",tags$br(),"40 - 46 = Very high",tags$br(),tags$br(),
                  status = "primary",width = 2, solidHeader = TRUE))
            
    ),
    
    tabItem(tabName="Graphlag",
            
            fluidRow(
              box(mainPanel(leafletOutput("city",height = 300,width=700) %>% withSpinner(type =  7,size  = 2)),width=8,status="primary"),
              box(mainPanel(tableOutput("table")),width=4,status="primary"),
              
              box(selectInput(inputId = "input_city",choices=levels(as.factor(pat_dlnm2$cp2)),label="Select one zone :",selected = "13000 : MARSEILLE"),
                  selectInput(inputId = "input_var", choices=c("Nitrogen dioxyde - NO2 (reference = 50 µg/m3)"="pred_NO2","Ozone - O3 (reference = 40 µg/m3)"="pred_O3", "Fine particle - PM10 (reference = 10 µg/m3)"="pred_PM10", "Temperature"="pred_maxT", "Pressure"="pred_maxP", "Temperature amplitude"="pred_deltaT", "Pressure amplitude"="pred_deltaP"),label="Select pollutant or meteorological variable:"), 
                  conditionalPanel(condition = "input.input_var == 'pred_NO2'",uiOutput("sliderNO2A"),uiOutput("sliderNO2B"),uiOutput("sliderNO2C"),uiOutput("sliderNO2D")), 
                  conditionalPanel(condition = "input.input_var == 'pred_O3'",uiOutput("sliderO3A"),uiOutput("sliderO3B"),uiOutput("sliderO3C"),uiOutput("sliderO3D")),
                  conditionalPanel(condition = "input.input_var == 'pred_PM10'",uiOutput("sliderPM10A"),uiOutput("sliderPM10B"),uiOutput("sliderPM10C"),uiOutput("sliderPM10D")),
                  mainPanel(plotOutput("prediction2",height = 400,width=500) %>% withSpinner(type =  7,size  = 2),plotOutput("prediction",height = 400,width=500)%>% withSpinner(type =  7,size  = 2)),status="warning",width=6,title="Zone 1", solidHeader = TRUE),
              
              box(selectInput(inputId = "input_city2",choices=levels(as.factor(pat_dlnm2$cp2)),label="Select one zone :",selected ="06000_06100_06200_06300 : NICE"),
                  selectInput(inputId = "input_var2", choices=c("Nitrogen dioxyde - NO2 (reference = 50 µg/m3)"="pred_NO2","Ozone - O3 (reference = 40 µg/m3)"="pred_O3", "Fine particle - PM10 (reference = 10 µg/m3)"="pred_PM10", "Temperature"="pred_maxT", "Pressure"="pred_maxP", "Temperature amplitude"="pred_deltaT", "Pressure amplitude"="pred_deltaP"),label="Select pollutant or meteorological variable:"), 
                  conditionalPanel(condition = "input.input_var2 == 'pred_NO2'",uiOutput("sliderNO2AA"),uiOutput("sliderNO2BB"),uiOutput("sliderNO2CC"),uiOutput("sliderNO2DD")), 
                  conditionalPanel(condition = "input.input_var2 == 'pred_O3'",uiOutput("sliderO3AA"),uiOutput("sliderO3BB"),uiOutput("sliderO3CC"),uiOutput("sliderO3DD")),
                  conditionalPanel(condition = "input.input_var2 == 'pred_PM10'",uiOutput("sliderPM10AA"),uiOutput("sliderPM10BB"),uiOutput("sliderPM10CC"),uiOutput("sliderPM10DD")),
                  mainPanel(plotOutput("prediction3",height = 400,width=500)%>% withSpinner(type =  7,size  = 2),plotOutput("prediction4",height = 400,width=500)%>% withSpinner(type =  7,size  = 2)),status="success",width=6,title="Zone 2", solidHeader = TRUE),
            ))
    
    
  ))

ui <- dashboardPage(header,sidebar,body,skin="black")

##################################################################################################################################
#serveur 

server <- function(input, output) {
  
  # Onglet n1
  
  dateselect <- reactive({
    subset(data, date == input$input_date)
  })
  
  inputdata <- reactive({
    switch(input$input_polmet,
           "NO2"=dateselect()$NO2,
           "O3"=dateselect()$O3,
           "PM10"=dateselect()$PM10,
           "P"=dateselect()$P,
           "Pdelta"=dateselect()$Pdelta,
           "T2"=dateselect()$T2,
           "T2delta"=dateselect()$T2delta)
  })
  
  output$map <- renderLeaflet({ 
    
    lab <- sprintf("<strong>%s</strong>",dateselect()$cp2) %>% lapply(htmltools::HTML)
    
    leaflet(pacapoly)%>%
      addProviderTiles(providers$OpenStreetMap)%>%
      addPolygons(color="black",# color borders
                  weight=0.7, # size of borders
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity = 1),
                  smoothFactor = 0.5,
                  label=lab,
                  opacity = 0.5, # opacity of borders
                  fillOpacity = 1, # opacity color of zones
                  fillColor = ~colorFactor(c("skyblue3","skyblue1","tan1","brown1","brown4"),inputdata())(inputdata())) %>%  
      addLegend("bottomright",
                pal = colorFactor(c("skyblue3","skyblue1","tan1","brown1","brown4"),inputdata()),
                values = inputdata(),
                title = "Légende",
                opacity = 1)
  })
  
  
  
  # Onglet n2
  
  dataselect2 <- reactive({
    subset(pat_dlnm2, cp2==input$input_city)
  })
  
  output$sliderNO2A <- renderUI({
    sliderInput("NO2A", "First level (dark blue)", min=0, max=max(round(dataselect2()$max_NO2max),0), value=50)
  })
  
  output$sliderNO2B <- renderUI({
    sliderInput("NO2B", "Second level (cyan)", min=0, max=max(round(dataselect2()$max_NO2max),0), value=50)
  })
  
  output$sliderNO2C <- renderUI({
    sliderInput("NO2C", "Third level (orange)", min=0, max=max(round(dataselect2()$max_NO2max),0), value=50)
  })
  
  output$sliderNO2D <- renderUI({
    sliderInput("NO2D", "Fourth level (red)", min=0, max=max(round(dataselect2()$max_NO2max),0), value=50)
  })
  
  output$sliderO3A <- renderUI({
    sliderInput("O3A", "First level (dark blue)", min=0, max=max(round(dataselect2()$max_O3max),0), value=40)
  })
  
  output$sliderO3B <- renderUI({
    sliderInput("O3B", "Second level (cyan)", min=0, max=max(round(dataselect2()$max_O3max),0), value=40)
  })
  
  output$sliderO3C <- renderUI({
    sliderInput("O3C", "Third level (orange)", min=0, max=max(round(dataselect2()$max_O3max),0), value=40)
  })
  
  output$sliderO3D <- renderUI({
    sliderInput("O3D", "Fourth level (red)", min=0, max=max(round(dataselect2()$max_O3max),0), value=40)
  })
  
  output$sliderPM10A <- renderUI({
    sliderInput("PM10A", "First level (dark blue)", min=0, max=max(round(dataselect2()$moy_PM10avg),0), value=10)
  })
  
  output$sliderPM10B <- renderUI({
    sliderInput("PM10B", "Second level (cyan)", min=0, max=max(round(dataselect2()$moy_PM10avg),0), value=10)
  })
  
  output$sliderPM10C <- renderUI({
    sliderInput("PM10C", "Third level (orange)", min=0, max=max(round(dataselect2()$moy_PM10avg),0), value=10)
  })
  
  output$sliderPM10D <- renderUI({
    sliderInput("PM10D", "Fourth level (red)", min=0, max=max(round(dataselect2()$moy_PM10avg),0), value=10)
  })
  
  output$prediction <- renderPlot({
    
    dlnm_db <- dataselect2()
    dlnm_db <- dlnm_db %>% arrange(date)
    dlnm_db$t <- c(1:nrow(dlnm_db)) #time
    summary(dlnm_db)
    
    ####*********************************************************** Crossbasis 
    timebasis <- ns(dlnm_db$t,df=2*6) 
    
    basis_NO2 <- crossbasis(dlnm_db$max_NO2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_PM10 <- crossbasis(dlnm_db$moy_PM10avg,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    basis_O3 <- crossbasis(dlnm_db$max_O3max,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    
    basis_maxT <- crossbasis(dlnm_db$max_T2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_maxP <- crossbasis(dlnm_db$max_Pmax,lag=14,argvar=list(fun="ns",df=4),arglag=list(fun="ns",df=4))
    basis_deltaT <- crossbasis(dlnm_db$max_delta_T2,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_deltaP <- crossbasis(dlnm_db$max_delta_P,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    
    ####*********************************************************** Model 
    mod <- glm(Y ~ basis_NO2 + basis_O3 + basis_PM10 + basis_maxT + basis_maxP + basis_deltaT + basis_deltaP + timebasis + log(poptotal) + dow ,family = quasipoisson(),dlnm_db)
    
    ############################################# Prediction
    
    # Pollution
    pred_NO2 <- crosspred(basis_NO2,mod,cen=50,by=1)
    pred_O3 <- crosspred(basis_O3,mod,cen=40,by=1)
    pred_PM10 <- crosspred(basis_PM10,mod,cen=10,by=1)
    
    # Weather
    ####### maxT
    a <- round(quantile(dlnm_db$max_T2max, c(.01,.10,.90,.99)),1)
    pred_maxT <- crosspred(basis_maxT,mod,cen=median(dlnm_db$max_T2max),at=c(a))
    ####### maxP
    b <- round(quantile(dlnm_db$max_Pmax, c(.01,.10,.90,.99)),1)
    pred_maxP <- crosspred(basis_maxP,mod,cen=median(dlnm_db$max_Pmax),at=c(b))
    ####### deltaT
    c <- round(quantile(dlnm_db$max_delta_T2, c(.01,.10,.90,.99)),1)
    pred_deltaT <- crosspred(basis_deltaT,mod,cen=median(dlnm_db$max_delta_T2),at=c(c))
    ####### deltaP
    d <- round(quantile(dlnm_db$max_delta_P, c(.01,.10,.90,.99)),1)
    pred_deltaP <- crosspred(basis_deltaP,mod,cen=median(dlnm_db$max_delta_P),at=c(d))
    
    var <- reactive({
      switch(input$input_var,
             "pred_NO2"=pred_NO2,
             "pred_O3"=pred_O3,
             "pred_PM10"=pred_PM10,
             "pred_maxT"=pred_maxT,
             "pred_maxP"=pred_maxP,
             "pred_deltaT"=pred_deltaT,
             "pred_deltaP"=pred_deltaP)
    })
    
    if (input$input_var=="pred_NO2") {
      plot(var(),"slices",var=c(input$NO2A,input$NO2B,input$NO2C,input$NO2D),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var=="pred_O3") {
      plot(var(),"slices",var=c(input$O3A,input$O3B,input$O3C,input$O3D),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var=="pred_PM10") {
      plot(var(),"slices",var=c(input$PM10A,input$PM10B,input$PM10C,input$PM10D),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var=="pred_maxT") {
      plot(var(),"slices",var=c(a),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var=="pred_maxP") {
      plot(var(),"slices",var=c(b),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var=="pred_deltaT") {
      plot(var(),"slices",var=c(c),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else {
      plot(var(),"slices",var=c(d),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    }
    
})
  
  output$prediction2 <- renderPlot({
    
    dlnm_db <- dataselect2()
    dlnm_db <- dlnm_db %>% arrange(date)
    dlnm_db$t <- c(1:nrow(dlnm_db)) #time
    summary(dlnm_db)
    
    ####*********************************************************** Crossbasis 
    timebasis <- ns(dlnm_db$t,df=2*6) 
    
    basis_NO2 <- crossbasis(dlnm_db$max_NO2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_PM10 <- crossbasis(dlnm_db$moy_PM10avg,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    basis_O3 <- crossbasis(dlnm_db$max_O3max,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    
    basis_maxT <- crossbasis(dlnm_db$max_T2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_maxP <- crossbasis(dlnm_db$max_Pmax,lag=14,argvar=list(fun="ns",df=4),arglag=list(fun="ns",df=4))
    basis_deltaT <- crossbasis(dlnm_db$max_delta_T2,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_deltaP <- crossbasis(dlnm_db$max_delta_P,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    
    ####*********************************************************** Model 
    mod <- glm(Y ~ basis_NO2 + basis_O3 + basis_PM10 + basis_maxT + basis_maxP + basis_deltaT + basis_deltaP + timebasis + log(poptotal) + dow ,family = quasipoisson(),dlnm_db)
    
    ############################################# Prediction
    
    # Pollution
    pred_NO2 <- crosspred(basis_NO2,mod,cen=50,by=1)
    pred_O3 <- crosspred(basis_O3,mod,cen=40,by=1)
    pred_PM10 <- crosspred(basis_PM10,mod,cen=10,by=1)
    
    # Weather
    ####### maxT
    a <- round(quantile(dlnm_db$max_T2max, c(.01,.10,.90,.99)),1)
    pred_maxT <- crosspred(basis_maxT,mod,cen=median(dlnm_db$max_T2max),at=c(a))
    ####### maxP
    b <- round(quantile(dlnm_db$max_Pmax, c(.01,.10,.90,.99)),1)
    pred_maxP <- crosspred(basis_maxP,mod,cen=median(dlnm_db$max_Pmax),at=c(b))
    ####### deltaT
    c <- round(quantile(dlnm_db$max_delta_T2, c(.01,.10,.90,.99)),1)
    pred_deltaT <- crosspred(basis_deltaT,mod,cen=median(dlnm_db$max_delta_T2),at=c(c))
    ####### deltaP
    d <- round(quantile(dlnm_db$max_delta_P, c(.01,.10,.90,.99)),1)
    pred_deltaP <- crosspred(basis_deltaP,mod,cen=median(dlnm_db$max_delta_P),at=c(d))
    
    var <- reactive({
      switch(input$input_var,
             "pred_NO2"=pred_NO2,
             "pred_O3"=pred_O3,
             "pred_PM10"=pred_PM10,
             "pred_maxT"=pred_maxT,
             "pred_maxP"=pred_maxP,
             "pred_deltaT"=pred_deltaT,
             "pred_deltaP"=pred_deltaP)
    })
    
    if (input$input_var=="pred_NO2") {
      plot(var(),"slices",ci="n",var=input$NO2A,lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var(),"slices",var=input$NO2B,col=c("turquoise1"),lwd=3)
      lines(var(),"slices",var=input$NO2C,col=c("darkorange"),lwd=3)
      lines(var(),"slices",var=input$NO2D,col=c("firebrick3"),lwd=3)
    } else if (input$input_var=="pred_O3") {
      plot(var(),"slices",ci="n",var=input$O3A,lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var(),"slices",var=input$O3B,col=c("turquoise1"),lwd=3)
      lines(var(),"slices",var=input$O3C,col=c("darkorange"),lwd=3)
      lines(var(),"slices",var=input$O3D,col=c("firebrick3"),lwd=3)
    } else if (input$input_var=="pred_PM10") {
      plot(var(),"slices",ci="n",var=input$PM10A,lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var(),"slices",var=input$PM10B,col=c("turquoise1"),lwd=3)
      lines(var(),"slices",var=input$PM10C,col=c("darkorange"),lwd=3)
      lines(var(),"slices",var=input$PM10D,col=c("firebrick3"),lwd=3)
    } else if (input$input_var=="pred_maxT") {
      plot(var(),"slices",ci="n",var=a[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var(),"slices",var=a[2],col=c("turquoise1"),lwd=3)
      lines(var(),"slices",var=a[3],col=c("darkorange"),lwd=3)
      lines(var(),"slices",var=a[4],col=c("firebrick3"),lwd=3)
      legend("topright",paste("",c(a),c("°C (1st percentile)","°C (10th percentile)","°C (90th percentile)","°C (99th percentile)")),col=c("darkblue","turquoise1","darkorange","firebrick3"),lwd = 1.5,cex=0.9)
    } else if (input$input_var=="pred_maxP") {
      plot(var(),"slices",ci="n",var=b[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var(),"slices",var=b[2],col=c("turquoise1"),lwd=3)
      lines(var(),"slices",var=b[3],col=c("darkorange"),lwd=3)
      lines(var(),"slices",var=b[4],col=c("firebrick3"),lwd=3)
      legend("topright",paste("",c(b),c("hPa (1st percentile)","hPa (10th percentile)","hPa (90th percentile)","hPa (99th percentile)")),col=c("darkblue","turquoise1","darkorange","firebrick3"),lwd = 1.5,cex=0.9)
    } else if (input$input_var=="pred_deltaT") {
      plot(var(),"slices",ci="n",var=c[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var(),"slices",var=c[2],col=c("turquoise1"),lwd=3)
      lines(var(),"slices",var=c[3],col=c("darkorange"),lwd=3)
      lines(var(),"slices",var=c[4],col=c("firebrick3"),lwd=3)
      legend("topright",paste("",c(c),c("°C (1st percentile)","°C (10th percentile)","°C (90th percentile)","°C (99th percentile)")),col=c("darkblue","turquoise1","darkorange","firebrick3"),lwd = 1.5,cex=0.9)
    } else {
      plot(var(),"slices",ci="n",var=d[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var(),"slices",var=d[2],col=c("turquoise1"),lwd=3)
      lines(var(),"slices",var=d[3],col=c("darkorange"),lwd=3)
      lines(var(),"slices",var=d[4],col=c("firebrick3"),lwd=3)
      legend("topright",paste("",c(d),c("hPa (1st percentile)","hPa (10th percentile)","hPa (90th percentile)","hPa (99th percentile)")),col=c("darkblue","turquoise1","darkorange","firebrick3"),lwd = 1.5,cex=0.9)
    }
    
  })
  
  dataselect3 <- reactive({
    subset(pat_dlnm2, cp2==input$input_city2)
  })
  
  output$sliderNO2AA <- renderUI({
    sliderInput("NO2AA", "First level (dark blue)", min=0, max=max(round(dataselect3()$max_NO2max),0), value=50)
  })
  
  output$sliderNO2BB <- renderUI({
    sliderInput("NO2BB", "Second level (cyan)", min=0, max=max(round(dataselect3()$max_NO2max),0), value=50)
  })
  
  output$sliderNO2CC <- renderUI({
    sliderInput("NO2CC", "Third level (orange)", min=0, max=max(round(dataselect3()$max_NO2max),0), value=50)
  })
  
  output$sliderNO2DD <- renderUI({
    sliderInput("NO2DD", "Fourth level (red)", min=0, max=max(round(dataselect3()$max_NO2max),0), value=50)
  })
  
  output$sliderO3AA <- renderUI({
    sliderInput("O3AA", "First level (dark blue)", min=0, max=max(round(dataselect3()$max_O3max),0), value=40)
  })
  
  output$sliderO3BB <- renderUI({
    sliderInput("O3BB", "Second level (cyan)", min=0, max=max(round(dataselect3()$max_O3max),0), value=40)
  })
  
  output$sliderO3CC <- renderUI({
    sliderInput("O3CC", "Third level (orange)", min=0, max=max(round(dataselect3()$max_O3max),0), value=40)
  })
  
  output$sliderO3DD <- renderUI({
    sliderInput("O3DD", "Fourth level (red)", min=0, max=max(round(dataselect3()$max_O3max),0), value=40)
  })
  
  output$sliderPM10AA <- renderUI({
    sliderInput("PM10AA", "First level (dark blue)", min=0, max=max(round(dataselect3()$moy_PM10avg),0), value=10)
  })
  
  output$sliderPM10BB <- renderUI({
    sliderInput("PM10BB", "Second level (cyan)", min=0, max=max(round(dataselect3()$moy_PM10avg),0), value=10)
  })
  
  output$sliderPM10CC <- renderUI({
    sliderInput("PM10CC", "Third level (orange)", min=0, max=max(round(dataselect3()$moy_PM10avg),0), value=10)
  })
  
  output$sliderPM10DD <- renderUI({
    sliderInput("PM10DD", "Fourth level (red)", min=0, max=max(round(dataselect3()$moy_PM10avg),0), value=10)
  })
  
  
  output$prediction3 <- renderPlot({
    
    dlnm_db <- dataselect3()
    dlnm_db <- dlnm_db %>% arrange(date)
    dlnm_db$t <- c(1:nrow(dlnm_db)) #time
    summary(dlnm_db)
    
    ####*********************************************************** Crossbasis 
    timebasis <- ns(dlnm_db$t,df=2*6) 
    
    basis_NO2 <- crossbasis(dlnm_db$max_NO2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_PM10 <- crossbasis(dlnm_db$moy_PM10avg,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    basis_O3 <- crossbasis(dlnm_db$max_O3max,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    
    basis_maxT <- crossbasis(dlnm_db$max_T2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_maxP <- crossbasis(dlnm_db$max_Pmax,lag=14,argvar=list(fun="ns",df=4),arglag=list(fun="ns",df=4))
    basis_deltaT <- crossbasis(dlnm_db$max_delta_T2,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_deltaP <- crossbasis(dlnm_db$max_delta_P,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    
    ####*********************************************************** Model 
    mod <- glm(Y ~ basis_NO2 + basis_O3 + basis_PM10 + basis_maxT + basis_maxP + basis_deltaT + basis_deltaP + timebasis + log(poptotal) + dow ,family = quasipoisson(),dlnm_db)
    
    ############################################# Prediction
    
    # Pollution
    pred_NO2 <- crosspred(basis_NO2,mod,cen=50,by=1)
    pred_O3 <- crosspred(basis_O3,mod,cen=40,by=1)
    pred_PM10 <- crosspred(basis_PM10,mod,cen=10,by=1)
    
    # Weather
    ####### maxT
    a <- round(quantile(dlnm_db$max_T2max, c(.01,.10,.90,.99)),1)
    pred_maxT <- crosspred(basis_maxT,mod,cen=median(dlnm_db$max_T2max),at=c(a))
    ####### maxP
    b <- round(quantile(dlnm_db$max_Pmax, c(.01,.10,.90,.99)),1)
    pred_maxP <- crosspred(basis_maxP,mod,cen=median(dlnm_db$max_Pmax),at=c(b))
    ####### deltaT
    c <- round(quantile(dlnm_db$max_delta_T2, c(.01,.10,.90,.99)),1)
    pred_deltaT <- crosspred(basis_deltaT,mod,cen=median(dlnm_db$max_delta_T2),at=c(c))
    ####### deltaP
    d <- round(quantile(dlnm_db$max_delta_P, c(.01,.10,.90,.99)),1)
    pred_deltaP <- crosspred(basis_deltaP,mod,cen=median(dlnm_db$max_delta_P),at=c(d))
    
    var2 <- reactive({
      switch(input$input_var2,
             "pred_NO2"=pred_NO2,
             "pred_O3"=pred_O3,
             "pred_PM10"=pred_PM10,
             "pred_maxT"=pred_maxT,
             "pred_maxP"=pred_maxP,
             "pred_deltaT"=pred_deltaT,
             "pred_deltaP"=pred_deltaP)})
    
    
    if (input$input_var2=="pred_NO2") {
      plot(var2(),"slices",ci="n",var=input$NO2AA,lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var2(),"slices",var=input$NO2BB,col=c("turquoise1"),lwd=3)
      lines(var2(),"slices",var=input$NO2CC,col=c("darkorange"),lwd=3)
      lines(var2(),"slices",var=input$NO2DD,col=c("firebrick3"),lwd=3)
    } else if (input$input_var2=="pred_O3") {
      plot(var2(),"slices",ci="n",var=input$O3AA,lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var2(),"slices",var=input$O3BB,col=c("turquoise1"),lwd=3)
      lines(var2(),"slices",var=input$O3CC,col=c("darkorange"),lwd=3)
      lines(var2(),"slices",var=input$O3DD,col=c("firebrick3"),lwd=3)
    } else if (input$input_var2=="pred_PM10") {
      plot(var2(),"slices",ci="n",var=input$PM10AA,lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var2(),"slices",var=input$PM10BB,col=c("turquoise1"),lwd=3)
      lines(var2(),"slices",var=input$PM10CC,col=c("darkorange"),lwd=3)
      lines(var2(),"slices",var=input$PM10DD,col=c("firebrick3"),lwd=3)
    } else if (input$input_var2=="pred_maxT") {
      plot(var2(),"slices",ci="n",var=a[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var2(),"slices",var=a[2],col=c("turquoise1"),lwd=3)
      lines(var2(),"slices",var=a[3],col=c("darkorange"),lwd=3)
      lines(var2(),"slices",var=a[4],col=c("firebrick3"),lwd=3)
    } else if (input$input_var2=="pred_maxP") {
      plot(var2(),"slices",ci="n",var=b[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var2(),"slices",var=b[2],col=c("turquoise1"),lwd=3)
      lines(var2(),"slices",var=b[3],col=c("darkorange"),lwd=3)
      lines(var2(),"slices",var=b[4],col=c("firebrick3"),lwd=3)
    } else if (input$input_var2=="pred_deltaT") {
      plot(var2(),"slices",ci="n",var=c[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var2(),"slices",var=c[2],col=c("turquoise1"),lwd=3)
      lines(var2(),"slices",var=c[3],col=c("darkorange"),lwd=3)
      lines(var2(),"slices",var=c[4],col=c("firebrick3"),lwd=3)
    } else {
      plot(var2(),"slices",ci="n",var=d[1],lwd=3,col="darkblue",ylab="RR",cex.axis=1.5,font.lab=2,cex.lab=1.5,xlab="Lag (day)",main="",cex.main=1.5)
      lines(var2(),"slices",var=d[2],col=c("turquoise1"),lwd=3)
      lines(var2(),"slices",var=d[3],col=c("darkorange"),lwd=3)
      lines(var2(),"slices",var=d[4],col=c("firebrick3"),lwd=3)
    }
    
  })
  
  output$prediction4 <- renderPlot({
    
    dlnm_db <-  dataselect3()
    dlnm_db <- dlnm_db %>% arrange(date)
    dlnm_db$t <- c(1:nrow(dlnm_db)) #time
    summary(dlnm_db)
    
    ####*********************************************************** Crossbasis 
    timebasis <- ns(dlnm_db$t,df=2*6) 
    
    basis_NO2 <- crossbasis(dlnm_db$max_NO2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_PM10 <- crossbasis(dlnm_db$moy_PM10avg,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    basis_O3 <- crossbasis(dlnm_db$max_O3max,lag=14,argvar=list(fun="ns",df=3),arglag=list(fun="ns",df=3))
    
    basis_maxT <- crossbasis(dlnm_db$max_T2max,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_maxP <- crossbasis(dlnm_db$max_Pmax,lag=14,argvar=list(fun="ns",df=4),arglag=list(fun="ns",df=4))
    basis_deltaT <- crossbasis(dlnm_db$max_delta_T2,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    basis_deltaP <- crossbasis(dlnm_db$max_delta_P,lag=14,argvar=list(fun="ns",df=2),arglag=list(fun="ns",df=2))
    
    ####*********************************************************** Model 
    mod <- glm(Y ~ basis_NO2 + basis_O3 + basis_PM10 + basis_maxT + basis_maxP + basis_deltaT + basis_deltaP + timebasis + log(poptotal) + dow ,family = quasipoisson(),dlnm_db)
    
    ############################################# Prediction
    
    # Pollution
    pred_NO2 <- crosspred(basis_NO2,mod,cen=50,by=1)
    pred_O3 <- crosspred(basis_O3,mod,cen=40,by=1)
    pred_PM10 <- crosspred(basis_PM10,mod,cen=10,by=1)
    
    # Weather
    ####### maxT
    a <- round(quantile(dlnm_db$max_T2max, c(.01,.10,.90,.99)),1)
    pred_maxT <- crosspred(basis_maxT,mod,cen=median(dlnm_db$max_T2max),at=c(a))
    ####### maxP
    b <- round(quantile(dlnm_db$max_Pmax, c(.01,.10,.90,.99)),1)
    pred_maxP <- crosspred(basis_maxP,mod,cen=median(dlnm_db$max_Pmax),at=c(b))
    ####### deltaT
    c <- round(quantile(dlnm_db$max_delta_T2, c(.01,.10,.90,.99)),1)
    pred_deltaT <- crosspred(basis_deltaT,mod,cen=median(dlnm_db$max_delta_T2),at=c(c))
    ####### deltaP
    d <- round(quantile(dlnm_db$max_delta_P, c(.01,.10,.90,.99)),1)
    pred_deltaP <- crosspred(basis_deltaP,mod,cen=median(dlnm_db$max_delta_P),at=c(d))
    
    var2 <- reactive({
      switch(input$input_var2,
             "pred_NO2"=pred_NO2,
             "pred_O3"=pred_O3,
             "pred_PM10"=pred_PM10,
             "pred_maxT"=pred_maxT,
             "pred_maxP"=pred_maxP,
             "pred_deltaT"=pred_deltaT,
             "pred_deltaP"=pred_deltaP)})
    
    if (input$input_var2=="pred_NO2") {
      plot(var2(),"slices",var=c(input$NO2AA,input$NO2BB,input$NO2CC,input$NO2DD),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var2=="pred_O3") {
      plot(var2(),"slices",var=c(input$O3AA,input$O3BB,input$O3CC,input$O3DD),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var2=="pred_PM10") {
      plot(var2(),"slices",var=c(input$PM10AA,input$PM10BB,input$PM10CC,input$PM10DD),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var2=="pred_maxT") {
      plot(var2(),"slices",var=c(a),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var2=="pred_maxP") {
      plot(var2(),"slices",var=c(b),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else if (input$input_var2=="pred_deltaT") {
      plot(var2(),"slices",var=c(c),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    } else {
      plot(var2(),"slices",var=c(d),type="p",ci="bars",pch=19,cex=1.5,ylab="RR")
    }
    
  })
  
  output$city<- renderLeaflet({
    
    front <- front %>% mutate(code=ifelse(front$cp2 == input$input_city,1,
                                          ifelse(front$cp2 == input$input_city2,2,0)))
    front$code <- as.factor(front$code)
    
    leaflet(pacapoly)%>%
      addProviderTiles(providers$OpenStreetMap)%>%
      addPolygons(color="black",
                  weight=0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity = 1),
                  smoothFactor = 0.5,
                  #label=lab2,
                  opacity = 0.5, 
                  fillOpacity = 0.7, 
                  fillColor = ~colorFactor(c("transparent","orange","springgreen4"),front$code)(front$code)) 
  })
  
  output$table <- renderTable(level_pol)

}

##########################################
# Create Shiny app ----
shinyApp(ui, server)
