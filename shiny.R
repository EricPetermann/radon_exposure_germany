library(shiny)
library(shinyjs)
library(raster)
library(tmap)
library(tmaptools)
library(plyr)
library(dplyr)
library(rriskDistributions)
library(EnvStats)
library(sf)
library(htmlwidgets)
library(leaflet)
library(shinycssloaders)
library(terra)
library(stringi)
library(ggplot2)
library(partykit)

# Specify the application port                                                                            
options(shiny.host = "0.0.0.0")                                                            
options(shiny.port = 8080)    

if (Sys.getenv("DEV")=="Docker") { 
setwd("/shiny/data/")
load("2024_ircmodel.RData") 
} else { 
setwd("W:/UR_intern/UR2/Mitarbeitende/Eric/Projekte/IRC und weitere Karten 2022/R/shiny/data/")
load("IRC model.R") 
}

#load spatial predictor data
#load predictor data; assign CRS, reproject to common CRS if needed
Rn_S<-rast("RnSoil_2_0.tif")
crs(Rn_S)<-"epsg:25832"
Perm<-rast("Permeability_2_0.tif")
crs(Perm)<-"epsg:25832"
Temp<-rast("Temperature_1981to2010.tif")
Temp<-terra::project(Temp,crs(Rn_S))
Prec<-rast("Precipitation_1981to2010.tif")
Prec<-terra::project(Prec,crs(Rn_S))
SM<-rast("SoilMoisture_1981to2010.tif")
SM<-terra::project(SM,crs(Rn_S))
slope<-rast("Slope250.tif")
crs(slope)<-"epsg:25832"
faults<-rast("tectonicfaultdensity_100.tif")
crs(faults)<-"epsg:25832"
WindExp<-rast("WindExp250.tif")
crs(WindExp)<-"epsg:25832"
WindExp<-terra::project(WindExp,crs(Rn_S))
OutRn<-rast("OutdoorRn_idw_nmax10_idp06.tif")

#shp files
border<-st_read("border germany.shp")
water<-st_read("water.shp")
mining<-st_read("mining deposits etc.shp")

#options for mapping
icon<-tmap_icons("icon_building.png")
tmap_mode("view")

# #define desired percentiles
percentile<- c(0.1, 0.25, 0.5, 0.75,0.8,0.85,0.9,0.95,0.98)
myQuantile <- function(y, w) quantile(rep(y, w), probs = percentile)

#defintion for colour bar
GRP_bar<-data.frame(c(0,seq(2,50,1),seq(50,100,5)),seq(1,61,1))
colnames(GRP_bar)<-c("GRP","rank")

#map zoom added to bbox in m
zoom <- 250

#supress warnings
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

#settings for spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)

ui <-fluidPage(
  titlePanel(
    fluidRow(
      column(4,div(p(strong("Indoor radon prediction"),style="font-size:50px;color:#3474A7",class = "verticalcenter")))  #vertical alignment of title??
      )
    ),
                fluidRow(
                   column(3,
                   wellPanel(
                   tags$h1("Input", style = "font-size:30px;color:#3474A7"),
                   tags$h3("1) Select a location", style = "font-size:20px;color:#3474A7"),
                   p("For example:", style = "font-size:14px"),
                   tags$div(
                     HTML("<ul>
            <li>Munich [Municipality]</li>
            <li>Hamburg, Bartelsstr 7 [Municipality + street] </li>
            <li>Friedrichstraße 22, 10969 Berlin [Municipality + street + postal code] </li>
          </ul>")
                   ),
                   textInput(inputId = "addresse", label = "Enter city or address (works only within Germany)", value = ""),
                   actionButton(inputId = "Adress_button", label = "Search", class = "btn-success"),
                   hr(),
                   tags$h3("2) Dwelling characteristics", style = "font-size:20px;color:#3474A7"),
                   selectInput(inputId ="etage", "On which floor is the living space located?", 
                               choices = c("Please select"="","Basement","Groundfloor",
                                           "1st floor","2nd floor","3rd floor or higher")),
                   selectInput(inputId ="baujahr", "When was the building constructed?", 
                               choices = c("Please select"="","Before 1945","1945 - 1980","1981 - 1995",
                                           "1996 - 2005","2006 or later","unknown")),
                   selectInput(inputId ="wohneinheiten", "How many apartments does the building have",
                               choices=c("Please select"="","1","2","3 - 6","7 - 12","13 or more")),
                   selectInput(inputId = "type","What is the building type?",
                               choices = c("Please select"="","Single family house","Townhouse","Multi family house",
                                           "Apartment block","High-rise apartment block","Terrace house","Farmhouse","Office building")),
                   actionButton(inputId = "Gebaeude_button", label = "Confirm", class = "btn-success"),
                   hr(),
                  p("Go back to main ", a("Indoor Radon Exposure Germany",href = "https://indoor.radonmap.info/")))),
                  column(4,
                   br(),
                   tags$h1("Operation", style = "font-size:24px;color:#3474A7"),
                   p("1) Enter address"),
                   p("2) Enter building and dwelling characteristics"),
                   p("3) Start prediction"),
                   br(),
                   tags$h1("How does the radon prediction works?", style = "font-size:24px;color:#3474A7"),
                   p("The radon calculator computes the probability distribution for indoor radon concentration in your household. The calculation utilizes the building and household characteristics entered by the user and natural characteristics (radon concentration in the soil, soil gas permeability, radon concentration in the outdoor air, long-term mean annual precipitation sum, long-term mean temperature, long-term mean soil moisture, slope inclination, tectonic fault density, wind exposure) that are to be expected at the entered address. A statistical model (quantile regression forest) from the field of machine learning is used for the calculation. The model was trained with approximately 14,000 indoor measurements from a recent national survey in Germany."),
                   br(),
                   p("This is exclusively supporting material to our recent preprint 'A new high-resolution residential radon map for Germany using a machine learning based probabilistic exposure model'."),
                   br(),  
                   htmlOutput("meinort"),
                   tags$head(tags$style("#meinort{font-size:24px;color:#3474A7}")),
                   tmapOutput(outputId ="map", width = 600, height=250),
                   htmlOutput("check_Dtl"),
                   tags$head(tags$style("#check_Dtl{font-size:14px;color:red}")),
                   htmlOutput("check_water"),
                   tags$head(tags$style("#check_water{font-size:14px;color:red}")),
                   htmlOutput("check_mining"),
                   tags$head(tags$style("#check_mining{font-size:14px;color:red}")),
                   br(),
                   br(),
                   br(),
                   htmlOutput("geb_settings"),
                   tags$head(tags$style("#geb_settings{font-size:24px;color:#3474A7}")),
                   tableOutput(outputId="newdata"),
                   htmlOutput("check_datanew"),
                   tags$head(tags$style("#check_datanew{font-size:14px;color:red}")),
                   htmlOutput("logic"),
                   tags$head(tags$style("#logic{font-size:14px;color:red}"))),
                  column(4,
                   br(),
                   br(),
                   htmlOutput("start_calc"),
                   tags$head(tags$style("#start_calc{font-size:24px;color:#3474A7}")),
                   br(),
                   useShinyjs(),
                   shinyjs::hidden(actionButton(inputId = "ML_button", label = "Start prediction", class = "btn-success")),
                   #ationButton(inputId = "ML_button", label = "Start prediction", class = "btn-success"),
                   br(),
                   htmlOutput("pred_setting"),
                   br(),
                   br(),
                   tags$head(tags$style("#pred_setting{color: black;font-size: 14px}")),
                   htmlOutput("prob100"),
                   tags$head(tags$style("#prob100{color: black;font-size: 18px}}")),
                   br(),
                   withSpinner(plotOutput(outputId="pdf", width = "35%", inline = TRUE),type=6),
                   br(),
                   htmlOutput("info")
                   )))
             
server <- function(input, output,session) {
  
  update_newdata <- reactive({
    sum(input$Gebaeude_button,input$Adress_button)
  })
  
  geocode <- eventReactive(input$Adress_button,{
      address<-stringi::stri_replace_all_fixed(
      input$addresse, 
      c("ä", "ö", "ü", "Ä", "Ö", "Ü","ß"), 
      c("ae", "oe", "ue", "Ae", "Oe", "Ue","ss"), 
      vectorize_all = FALSE)
     geocode_OSM(q=address,projection=25832)
  })

  Adresse <- reactive({
    pnt <- data.frame(
      "x" = geocode()$coords[1],
      "y" = geocode()$coords[2]
    )
    st_as_sf(pnt,coords=c("x","y"),crs=st_crs(25832))
  })
  
  check<- eventReactive(input$Adress_button,{
    check <- Adresse()
    check[1,"border"] <- st_join(check,border)$ADE
    check[1,"water"] <- st_join(check,water)$class5
    check[1,"mining"] <- st_join(check,mining)$class5
    check
  })
  
  output$check_Dtl<-reactive({
    if (is.na(check()$border))
      print("WARNING! No prediction possible. Entered location is not in Germany.")
  })
  
  output$check_water<-reactive({
    if (!is.na(check()$water))
      print("WARNING! No prediction possible. The radon concentration in soil cannot be estimated for the selected location.")
  })
 
   output$check_mining<-reactive({
    if (!is.na(check()$mining))
      print("WARNING! No prediction possible. The radon concentration in soil cannot be estimated for the selected location.")
   })
  
  Rnout <- eventReactive(input$Adress_button,{
    as.numeric(format(round(terra::extract(OutRn,Adresse())[,2], 0),nsmall=0))
  })
  

  output$meinort <- eventReactive(input$Adress_button,{
   paste("1) Selected location")
  })
  
  output$map <- renderTmap({
    if(is.null(Adresse()))
      return()
     tm_shape(Adresse()) + 
      tm_symbols(shape=icon,size=2) +
      tm_view(bbox = geocode()$bbox+(zoom*c(-1,-1,1,1)),alpha=0.8) +
      tm_basemap(leaflet::providers$OpenStreetMap)
    })

  output$geb_settings <- eventReactive(input$Gebaeude_button,{
    paste("2) Building and dwelling characteristics")
  })
  
  
  newdata <- reactive({
    if(is.null(Adresse()))
      return()
    
      newdata <- data.frame(matrix(ncol = 14, nrow = 1))
      x <- c("type","Baujahr_Klasse","Wohneinheiten","Etage","RnS","Perm","Temp","Prec","SM","slope",
             "faults","WindExp","RnOut","address")
      colnames(newdata) <- x
      newdata[,1] <- mapvalues(as.factor(input$type), 
                                 from = c("Single family house","Townhouse","Multi family house",
                                          "Apartment block","High-rise apartment block","Terrace house","Farmhouse","Office building"), 
                                 to = c("EFH/ZFH", "RH/DHH","MFH","WB","WHH","TH","BH","Büro"))
      newdata[,2] <- mapvalues(as.factor(input$baujahr), 
                               from = c("Before 1945","1945 - 1980","1981 - 1995",
                                        "1996 - 2005","2006 or later","unknown"), 
                               to = c("<1945", "1945-1980","1981-1995","1996-2005",">2006","NA"))
      
      newdata[,3] <- mapvalues(as.factor(input$wohneinheiten), 
                               from = c("1","2","3 - 6","7 - 12","13 or more"), 
                               to = c("1 Wohneinheit","2 Wohneinheiten","3 - 6 Wohneinheiten",      
                                      "7 - 12 Wohneinheiten","13 und mehr Wohneinheiten"))
      newdata[,4] <- mapvalues(as.factor(input$etage), 
                               from = c("Basement","Groundfloor","1st floor","2nd floor","3rd floor or higher"), 
                               to = c("Keller","Erdgeschoss","1. Etage","2. Etage","3. Etage oder höher"))
      newdata[,5] <- as.numeric(format(round(terra::extract(Rn_S,Adresse())[,2],0),nsmall=0))
      newdata[,6] <- as.numeric(format(round(terra::extract(Perm,Adresse())[,2], 1),nsmall=1))
      newdata[,7] <- as.numeric(format(round(terra::extract(Temp,Adresse())[,2], 1),nsmall=1))
      newdata[,8] <- as.numeric(format(round(terra::extract(Prec,Adresse())[,2], 0),nsmall=0))
      newdata[,9] <- as.numeric(format(round(terra::extract(SM,Adresse())[,2], 1),nsmall=1))
      newdata[,10] <- as.numeric(format(round(terra::extract(slope,Adresse())[,2], 0),nsmall=0))
      newdata[,11] <- as.numeric(format(round(terra::extract(faults,Adresse())[,2], 0),nsmall=0))
      newdata[,12] <- as.numeric(format(round(terra::extract(WindExp,Adresse())[,2], 2),nsmall=2))
      newdata[,13] <- Rnout()
      newdata[,14]<- Adresse()     
      newdata
    })

  observeEvent(input$Gebaeude_button, {
    shinyjs::show("ML_button")
  })

  check_newdata<-reactive({
    sum(is.na(as.vector(newdata())))>0
  })

  output$check_datanew <- reactive({
    if (check_newdata())
      print("WARNING! Your input data is incomplete. No prediction possible!")
  })
  
  output$newdata <- renderTable({
    data.df<-newdata()
    data.show<-data.df[,1:13]
    data.show[,1] <- as.factor(input$type)
    data.show[,2] <- as.factor(input$baujahr)
     data.show[,3] <- as.factor(input$wohneinheiten)
     data.show[,4] <- as.factor(input$etage)
    data.show[,7]<-data.show[,7]/10
    colnames(data.show)<-c("Building type","Building age","Number of appartments","Floor level","Radon in soil [kBq/m³]",
                           "Permeability [log m²]","Temperature [°C]","Precipitation [mm/a]","Soil moisture [% nFK]",
                           "Slope [°]","Faults [length/km²]","Wind Exposure Index [-]","Rn outdoor [Bq/m³]")
    data.show
  })


  output$start_calc <- eventReactive(input$Gebaeude_button,{
    paste("3) Radon prediction")
  })
  
  data.for.pred <-  eventReactive(input$ML_button,{
    newdata()
  })
  
  output$pred_setting <- eventReactive(input$ML_button,{
    req(probs())
    HTML(paste("<b>","Prediction for a residential building in: ", 
              input$addresse,". ",
          "Floor level: ",as.character(input$etage),"; building type: ",as.character(input$type),
          "; built:  ",as.character(input$baujahr),"; number of appartments: ",as.character(input$wohneinheiten)," .",sep=""))
  })
  
  output$info<- renderText({
    req(newdata())
    req(probs())
    HTML(paste0("<b>","A prediction is not a replacement of a measurement!","</b>",p("Other relevant factors which are affecting indoor radon concentration are existing. These factors comprise dwelling specific 
                 construction design (e.g., tightness of the foundation), the frequency and intensity of ventilation by the residents and local characteristics (e.g. location in the (former) mining area).")))
          })
  
  dist.par <- eventReactive(input$ML_button,{
    pred = predict(mod,newdata=newdata(),type="response",FUN=myQuantile)  #make prediction
    RnOut<-Rnout()
    if (RnOut>=pred[1])
      RnOut <- pred[1]-1 #Outdoor Rn  -> check if RnOut is lower than 10 %ile, then set Rnout at 1 Bq/m³ lower than OutdoorRn
    quantile_cor<-pred-RnOut #substract threshold value
    lnorm.par<-get.lnorm.par(p=percentile,q=quantile_cor,fit.weights=c(2,2,2,3,3,3,4,4,4),show.output = FALSE) #fit 3 parameter lognorm distribution
    AM<-exp(lnorm.par[1]+1/2*lnorm.par[2])+RnOut
    pars<-data.frame(as.numeric(lnorm.par[1]),as.numeric(lnorm.par[2]),RnOut,AM,pred[7])
    colnames(pars)<-c("GM","GSD","thresh","AM","P90")
    pars
    })
  

  probs <- eventReactive(input$ML_button,{
    probs<-(1-plnorm3(q=100,mean=dist.par()$GM,sd=dist.par()$GSD,threshold=dist.par()$thresh))*100  
    P100<-probs[1]
    probs<-data.frame(P100)
  })

  output$pdf <- renderPlot({
    quant<-seq(0,1000,0.1)
    pdf<-dlnorm3(x=quant, meanlog = dist.par()$GM, sdlog = dist.par()$GSD, threshold = dist.par()$thresh)
    pdf.data<-data.frame(quant,pdf)
    colnames(pdf.data)<-c("quant","probs")
    
    limit_x<-500
    if (dist.par()$GM<4.1)
      limit_x<-300
    if (dist.par()$GM<3.2)
      limit_x<-100
    if (dist.par()$GM>4.6)
      limit_x<-1000

      ggplot(pdf.data,aes(quant,probs))+
      geom_line(show.legend = FALSE,linewidth=2)+
      labs(x="Indoor radon concentration [Bq/m³]",y="Low  <-- Probability -->  High",
           title="Probability distribution")+
      scale_x_continuous(limits=c(0,limit_x))+
      scale_y_continuous()+
      theme_bw()+
      theme(axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 22,hjust=0.5))
  },
  height = 300, width = 600)
  

  output$prob100 <- renderText({
    ifelse(probs()$P100 <1,
           paste0("The best estimate (conditional mean) for your dwelling is an indoor radon concentration of ", strong(round(dist.par()$AM,digits=0)," Bq/m³."),
                  "In 10 out of 100 cases (90th percentile) for dwellings with similar environmental and dwelling characteristics, indoor radon concentrations are greater than ",
                  round(dist.par()$P90,digits=0)," Bq/m³.",
                  paste0("The probability of ",strong("exceeding 100 Bq/m³")," for your household is ",strong("<1 %."))),
           paste0("The best estimate (conditional mean) for your dwelling is an indoor radon concentration of ", strong(round(dist.par()$AM,digits=0)," Bq/m³."),
                  "In 10 out of 100 cases (90th percentile) for dwellings with similar environmental and dwelling characteristics, indoor radon concentrations are greater than ",
                  round(dist.par()$P90,digits=0)," Bq/m³.",
                  paste0("The probability of ",strong("exceeding 100 Bq/m³")," for your household is ",strong(round(probs()$P100,digits=0)," %."))))
    
  })
  
}
  
shinyApp(ui = ui, server = server)
