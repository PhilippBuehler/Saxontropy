# Area of hydrological regions under which they become a special "others" region
# 0 -> no threshold
melt_hyd_region_below_x_skm <<- 500       #Square kilometer



#load required packages and install if nessesary
req_packages=c("shiny",#Load Shiny Package
               "dismo", "rJava", #Load Maxent Packages
               "ggplot2", "ggmap","ggrepel","png","grid", #Load plotting packages
               "raster","rgdal","rgeos","sp")  #Geoscientific Packages

Install_And_Load <- function(packages) {
  k <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(k)){   install.packages(k, repos="https://cran.rstudio.com/")  }
  for(package_name in packages){     library(package_name,character.only=TRUE, quietly = TRUE)  }
}

Install_And_Load(req_packages)



#Check if all data is already imported, if yes, skip the importing
check_vars <<- c("EXT","DEM","map_Google","map_OSM","Shapes_Region_DF","River_fs","Region_border_fs","hydr_regions_fs")
if (any(c(sapply(check_vars,function(x) !exists(x)),
          ifelse(exists("old_grenz2"), melt_hyd_region_below_x_skm-old_grenz2 != 0,TRUE))))  {
  
  # Koordinatensysteme
  EXT <<- c(11.80, 15.20, 50.05, 51.75)
  Coord_WGS <<- CRS("+init=epsg:4326")
  
  #Sachsen DGM
  DEM <<- raster(x = "data/DEM/Raster_Sachsen_HYDRO_1K.grd")
  DEM <<- projectRaster(from = DEM,crs = Coord_WGS)
  DEM <<- crop(DEM,extent(c(11.4,15.6,49.5,52.2)))
  DGM_DF <<- data.frame(rasterToPoints(DEM))
  colnames(DGM_DF) <<- c("lon", "lat", "alt")
  DGM_DF$alt <<- round(DGM_DF$alt)
  
  #Google Maps
  map_Google <<- readRDS(file = "data/my_map.rds")
  #get OpenStreetMaps
  map_OSM  <<- get_map(location = c(11.5,50,15.5,52.2), source = "osm")
  #get Catchment
  Shapes_Region <<- readOGR(dsn="data/Teileinzugsgebiete",layer="teileinzugsgebiete")
  Shapes_Region <<- spTransform(Shapes_Region, Coord_WGS)
  Shapes_Region_DF <<- data.frame(as.data.frame(gCentroid(Shapes_Region,byid=TRUE)),GRIDCODE=Shapes_Region$GKZ)
  Shapes_Region_DF$GRIDCODE <<- as.character(Shapes_Region_DF$GRIDCODE)
  Shapes_Region_DF$marked <<- factor(rep(0,nrow(Shapes_Region_DF)),levels = c(0,1))
  Shapes_Region_DF$Pegel <<- factor(rep(0,nrow(Shapes_Region_DF)),levels = c(0,1))
  
  #get Rivers
  Rivers <<- readOGR(dsn="data/DE_rivers",layer="river_DE")
  Rivers <<- spTransform(Rivers,Coord_WGS)
  River_fs <<- fortify(Rivers)
  
  #get Region border
  Region_border <<- readOGR(dsn="data/Border",layer="Border")
  Region_border <<- spTransform(Region_border,Coord_WGS)
  Region_border_fs <<- fortify(Region_border)
  
  
  #get hydr_regions
  hydr_regions <<- readOGR(dsn="data/hydr_regions",layer="Natur_ISO8859",stringsAsFactors = FALSE,
                     use_iconv = TRUE, encoding = "UTF-8")
  hydr_regions <<- aggregate(hydr_regions,by="NATURRAUMT")
  if(melt_hyd_region_below_x_skm>0){
    hydr_regions_Area <<- area(hydr_regions)/1000^2
    hydr_regions[hydr_regions_Area<melt_hyd_region_below_x_skm,"NATURRAUMT"]  <<-  "Other"
    hydr_regions <<- aggregate(hydr_regions,by="NATURRAUMT")
  }
  hydr_regions <<- spTransform(hydr_regions,Coord_WGS)
  hydr_regions_fs <<- fortify(hydr_regions)
  hydr_regions_fs$id <<- as.numeric(hydr_regions_fs$id)
  hydr_regions_fs$id <<- factor(hydr_regions$NATURRAUMT[hydr_regions_fs$id])
  hydr_regions_fac <<- levels(hydr_regions_fs$id)
  hydr_regions_fac_named <<- setNames(hydr_regions_fac,1:length(hydr_regions_fac))
  
  
  #Here, all data from all catchments with all identifiers and all variables are in one file
  Alle_Daten <<- read.table("data/Data_catchments_en_public.csv",sep=';',header = TRUE,stringsAsFactors = FALSE)
  names_no_predictor <<- make.names(c("Watershed","Watershed_norm","Informationextension","River","No_from","No_to","Profile","Basin",
                                      "To_From","No","Gauge","Area"))
  Alle_Pred <<- names(Alle_Daten)[!names(Alle_Daten) %in% names_no_predictor]
  
  predictor_by_class <<- scan("data/Tabellen/en/relevant_variables.txt", what="", sep="\n")
  predictor_by_class <<- strsplit(predictor_by_class, ";")
  names(predictor_by_class) <<- sapply(predictor_by_class, head, n=1)
  predictor_by_class <<- sapply(predictor_by_class, function(x) x[2:length(x)])
  
  #Here the subsamples from Alle_Daten can be divided into smaller parts with only their relevant variable in each file 
  All_files <<- list.files(path="data/Tabellen/en/",pattern="*.csv",)
  All_files <<- All_files[order(as.numeric(sub("^.*CC_min([0-9]+).*", "\\1", All_files)))]
  old_grenz2 <<- melt_hyd_region_below_x_skm
}



ui <- fluidPage(
  fluidRow(
    column(8,
           plotOutput("map", click = "clickMap", width = 1100, height = 800)
    ),
    column(4,
           fluidRow(
             column(7,
                    fluidRow(
                      selectInput("select", label = "Watershed Area", 
                                  choices = All_files, 
                                  selected = "CC_300-350.csv")
                    )
             ),
             column(3,
                    fluidRow(
                      actionButton("do", label = "Calculation")     
                    ),
                    fluidRow(
                      actionButton("Mod", label = "Maxent statistics")     
                    )
             ),
             column(2,
                    fluidRow(downloadButton("downloadImg",label="Figure")
                    ),
                    fluidRow(downloadButton("downloadData",label="Daten")
                    )
             )
           ),
           fluidRow(
             column(3,radioButtons("radioZusatz",label ="Watershed with 10% deviation in area",
                                   choices=list("On","Off"),selected = "On")
             ),
             column(3,radioButtons("radiomap",label ="Map background",
                                   choices=list("DEM","OSM", "Google" ,
                                                "Hydr. Region","none"),selected = "DEM")
             ),
             column(3,radioButtons("Names",label ="Watershed Label",
                                   choices=list("Off", "No","Gauge","Both"),selected = "Off")
             )
           ),
           
           fluidRow(
             column(7,
                    h4("Selected Watershed"),
                    tableOutput("selected_var")
             ),
             column(5,
                    h4("Stat. characteristics of all watersheds"),
                    htmlOutput("Med")
             )
           ),
           fluidRow(plotOutput("me", width = 500, height = 300)
           ),
           fluidRow(
             plotOutput("pngs", width = 500, height = 100)
           )
    )
  ),
  fluidRow(
    
    column(3, 
           fluidRow(
             actionLink("selectall_N","All/None"),
             checkboxGroupInput("Natur_choices",label ="Hydr. region restriction",
                                choices=hydr_regions_fac,selected = hydr_regions_fac)
           )
    ),
    column(3, 
           fluidRow(
             checkboxGroupInput("Pred_choices",label ="Predictor restriction",
                                choices=Alle_Pred,selected = Alle_Pred)
           )
    ),
    column(3,#style = "margin-top: -40px;",
           h1("Information"),
           p("This R-Shiny App is developed to claculate the maximum entropy of watersheds using the ",
             a("Maxent software for modeling species niches and distributions",
               href = "http://biodiversityinformatics.amnh.org/open_source/maxent/"),
             " applied to watershed characteristics. To determine the location of a new gauge in an  
                    unobserved watershed, the similarity to existing gauges is calculated and a 
                    probability of non-similarity is calculated to decide whether or not the new gauge 
                    delivers new information.",
             br(),
             h3("Data Sources: "),
             a("Sächsisches Landesamt für Umwelt, Landwirtschaft und Geologie",
               href = "https://www.umwelt.sachsen.de/umwelt/infosysteme/ida/index.xhtml"),
             br(),
             a("Elevation Model and River Network HYDRO 1K DEM",
               href = "https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-hydro1k?qt-science_center_objects=0#qt-science_center_objects"),
             br(),
             a("Google Maps",
               href = "https://console.cloud.google.com/home/dashboard"),
             br(),
             a("OpenStreetMap (OSM)",
               href = "https://www.openstreetmap.org")
           )
    )
  )
) 


server <- function(input, output,session) {
  RDF <- reactiveValues()
  RDF$Shape<-Shapes_Region_DF
  output$selected_var <- renderTable(data.frame(GRIDCODE="Not selected"))
  
  
  observeEvent(input$select,{
    Datengrundlage_min <- read.table(paste("data/Tabellen/en/",input$select,sep=""),sep=';',header = TRUE,stringsAsFactors = FALSE)
    Datengrundlage_min$Informationextension[is.na(Datengrundlage_min$Informationextension)] <- FALSE
    
    class_name <- substr(input$select,7,(nchar(input$select)-4))
    updateCheckboxGroupInput(session,"Pred_choices",selected=predictor_by_class[[class_name]])
    Datengrundlage_min <- Datengrundlage_min[,c("Watershed","Informationextension")] #,"Pegel"
    RDF$D <- Datengrundlage_min
  })
  
  
  
  
  observeEvent({input$radioZusatz
    input$Natur_choices
    input$Pred_choices},{
      
      Datengrundlage_min <- RDF$D 

      Datengrundlage <- merge(Datengrundlage_min,Alle_Daten , by="Watershed")
      Datengrundlage <- Datengrundlage[!duplicated(Datengrundlage$Watershed),]
      
      
      RDF$Shape <- Shapes_Region_DF
      RDF$Shape$Gaugename  <- ""
      RDF$Shape$GRIDCODE <- as.numeric(RDF$Shape$GRIDCODE)
      
      if(input$radioZusatz=="Aus"){
        RDF$Data <- Datengrundlage[Datengrundlage$Informationextension==FALSE,]
      }else{
        RDF$Data <- Datengrundlage
      }
      
      
      skipp=FALSE
      if(any(c(length(input$Natur_choices))==0)){
        skipp=TRUE
      }
      
      if(!skipp){
        RDF$Shape <- RDF$Shape[RDF$Shape$GRIDCODE %in% RDF$Data$Watershed,]
        
        #subselect Naturräume
        coord_temp_sp <- SpatialPoints(RDF$Shape[,c("x","y")],proj4string=Coord_WGS)
        
        over_op=over(coord_temp_sp, hydr_regions[hydr_regions$NATURRAUMT %in% input$Natur_choices,])
        RDF$Shape <- RDF$Shape[which(!is.na(over_op$NATURRAUMT)),]
        RDF$Data=RDF$Data[RDF$Data$Watershed %in% RDF$Shape$GRIDCODE,]
        if(nrow(RDF$Shape)==0)  skipp=TRUE
      }
      

      
      if(!skipp){
        # sel_vars=!names(RDF$Data) %in% names_no_predictor
        # RDF$predictors = RDF$Data[,sel_vars]
        
        
        RDF$predictors = RDF$Data[,input$Pred_choices]
        RDF$predictors =sapply(RDF$predictors,as.numeric)
        if(is.null(nrow(RDF$predictors))){
          RDF$predictors = as.data.frame(t(RDF$predictors))
        }else{
          RDF$predictors= as.data.frame(RDF$predictors)
        } 
        
        noNA=!apply(RDF$predictors,1,function(x) any(is.na(x)))
        RDF$predictors=RDF$predictors[noNA,]
        RDF$Data=RDF$Data[noNA,]
        
        
        RDF$Occ=rep(0,nrow(RDF$Data))
        RDF$Occ[RDF$Data$Gauge!=""]=1
        
        
        RDF$Shape=RDF$Shape[RDF$Shape$GRIDCODE %in% RDF$Data$Watershed,]
        
        RDF$Shape$Gauge[]=0
        RDF$Shape$Gauge[RDF$Shape$GRIDCODE %in% RDF$Data$Watershed[RDF$Occ==1]]=1
        
        RDF$Shape$Gaugename=sub("\\ -.*", "", RDF$Data$Gauge[RDF$Occ==1][match(RDF$Shape$GRIDCODE,
                                                                               RDF$Data$Watershed[RDF$Occ==1])])
        
        RDF$Shape$Beide_Namen=sapply(1:nrow(RDF$Shape), function(x) 
          ifelse(RDF$Shape$Gauge[x]==1,RDF$Shape$Gaugename[x],RDF$Shape$GRIDCODE[x]))
        
        
        RDF$me=NULL
        RDF$r=NULL
      }else{
        RDF$predictors <- NULL
        RDF$Shape <- RDF$Shape[NULL,]
      }
    })
  
  
  
  observeEvent(input$do, {
    req(RDF$predictors, RDF$Occ)
    if(sum(RDF$Occ==0)>=2 & sum(RDF$Occ==1)>=2){
      RDF$me <- maxent(RDF$predictors, RDF$Occ)
      RDF$r <- 1-predict(RDF$me, RDF$predictors) 
      
      RDF$Shape$Entropy=0
      RDF$Shape$Entropy=sapply(1:nrow(RDF$Shape),
                                function(x) max(RDF$r[RDF$Data$Watershed==RDF$Shape$GRIDCODE[x]]))
    }
  })
  
  
  observeEvent(input$Mod, {
    req(RDF$me)
    browseURL(RDF$me@html)
  })
  
  
  output$Med <- renderUI({
    req(RDF$r)
    str1 <- "Probability of non-similarity:"
    str2 <- paste("Observed (Median):",round(median(RDF$r[which(RDF$Occ==1)]),2))
    str3 <- paste("Not observed (Median):",round(median(RDF$r[which(RDF$Occ==0)]),2))
    HTML(paste(str1, str2, str3,sep = '<br/>'))
  })
  
  
  output$me <- renderPlot({
    if(is.null(RDF$me)){
      plot.new()
    }else{
      plot(RDF$me,main=NA,xlab="Contribution of each characteristic to entropy [%]")
    }
  })
  
  
  
  observeEvent(input$clickMap,{
    if(nrow(RDF$Shape)>0){
      P <- data.frame(Longitude = input$clickMap$x, Latitude =input$clickMap$y,names = c("Point"))
      coordinates(P) <- ~  Longitude + Latitude
      P@proj4string <- Coord_WGS
      
      RDF$Shape$marked[]=0
      RDF$Shape$marked[which.min(pointDistance(P,RDF$Shape[,c("x","y")] ,lonlat=TRUE))]=1
    }
  })
  
  
  observeEvent(RDF$Shape,{
    if(nrow(RDF$Shape)>0){
      if(any(names(RDF$Shape)=="Entropy")){
        xx=RDF$Shape[which(RDF$Shape$marked==1)[1],c("GRIDCODE","Gaugename","Entropy")]
        names(xx)<-c("Watershed", "Gaugename","Non-similarity")
        xx$Watershed=as.integer(xx$Watershed)
      }else{
        xx=data.frame(RDF$Shape[which(RDF$Shape$marked==1)[1],c("GRIDCODE","Gauge")])
        names(xx) <- c("Watershed","Gauge")
        xx$Watershed <-as.integer(xx$Watershed)
      }
      output$selected_var <- renderTable(xx)
    }
  })
  
  
  PlotFun<-reactive({
    if(input$radiomap=="DEM"){
      GG<-ggplot()+
        geom_raster(data=DGM_DF,aes(x=lon,y=lat,fill=alt))+
        scale_fill_gradientn(colours = terrain.colors(10),name="Elevation [m. a.s.l.]")+
        guides(fill = guide_colourbar(barwidth = unit(3 , "in" ),barheight =  unit(0.2 , "in" ),ticks.colour = "black",
                                      ticks.linewidth = 1,frame.colour = "black",frame.linewidth = 1))
    }else if (input$radiomap=="OSM"){
      GG<-ggmap(map_OSM)
    }else if (input$radiomap=="Google"){
      GG<-ggmap(map_Google)
    }else if (input$radiomap=="Hydr. Region"){
      temps <- hydr_regions_fs[hydr_regions_fs$id %in% input$Natur_choices,]
      GG<-ggplot()+
        geom_polygon(data=temps,aes(x=long,y=lat,fill=id,group=group))+
        scale_fill_discrete(name = NULL)+
        guides(fill = guide_legend(ncol = 2))
    }else if (input$radiomap=="None"){
      GG<-ggplot()
    }
    
    GG<-GG+
      geom_path(data=Region_border_fs,aes(x=long,y=lat,group=group),colour="black",size=1.2)+
      geom_path(data=River_fs,aes(x=long,y=lat,group=id),colour="blue",size=0.5)+
      scale_x_continuous(limits =  EXT[1:2], expand = c(0, 0)) +
      scale_y_continuous(limits = EXT[3:4], expand = c(0, 0))+
      labs(x="Longitude",y="Latitude")+
      theme(legend.justification =c(1, 0),legend.position = c(1, 0),
            legend.direction = "horizontal",legend.text.align = 1,legend.text=element_text(size=12,family = ))
    
    
    size=7
    if(any(names(RDF$Shape)=="Entropy")){
      GG<- GG+geom_point(data=RDF$Shape,aes(x=x,y=y,colour=Entropy),size=size)+
        scale_colour_gradient2(low="green",mid="blue", high="red",midpoint = 0.5,limit=c(0,1),name="Non-similarity",guide = FALSE)
      # guides(colour = guide_colourbar(barwidth = unit(4 , "in" ),barheight =  unit(0.3 , "in" ),
      # ticks.colour = "black",ticks.linewidth = 1,frame.colour = "black",frame.linewidth = 1))
    }else{
      GG=GG+geom_point(data=RDF$Shape,aes(x=x,y=y),colour="black",size=size)
    }
    
    
    
    if(input$Names=="No"){
      GG<-GG+
        geom_label_repel(data=RDF$Shape,aes(x=x,y=y,label=GRIDCODE),hjust=0,nudge_x = 0.03,nudge_y = 0.03)
    }else if(input$Names=="Gauge"){
      GG<-GG+
        geom_label_repel(data=RDF$Shape,aes(x=x,y=y,label=Gaugename),hjust=0,nudge_x = 0.03,nudge_y = 0.03)
      
    }else if(input$Names=="Both"){
      GG<-GG+
        geom_label_repel(data=RDF$Shape,aes(x=x,y=y,label=Beide_Namen),hjust=0,nudge_x = 0.03,nudge_y = 0.03)
    }
    
    GG=GG+geom_point(data = RDF$Shape, aes(x=x, y=y,alpha=marked),shape=4, size = size*2,colour="red",show.legend = FALSE)+
      scale_alpha_manual(values = setNames(c(0,1),c(0,1)))
    
    return(GG)
  })
  
  output$map <- renderPlot({
    PlotFun()
  })
  
  
  output$downloadImg <- downloadHandler(
    filename = function() {
      "Saxontropy.png"
    },
    content = function(file) {
      pngg <- rasterGrob(readPNG("data/colour_bar_en.PNG"), interpolate=TRUE)
      dp <- PlotFun()+
        theme(plot.margin = unit(c(1,1,5,1), "lines"))+
        annotation_custom(pngg, xmin=12, xmax=15, ymin=49.8, ymax=49.95)+ 
        coord_cartesian(clip = "off")
      ggsave(file, dp, width = 12, height = 10)
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Daten_Entropie_",input$select,sep="")
    },
    content = function(file) {
      if(is.null(RDF$r)){
        write.table(RDF$Data,file = file,sep = ";",row.names = FALSE)
      }else{
        write.table(cbind(RDF$Data, data.frame(Unaehnlichkeit=RDF$r)),file = file,sep = ";",row.names = FALSE)
      }
    }
  )
  
  output$pngs <- renderPlot(
    if(any(names(RDF$Shape)=="Entropy")){
      grid.raster(readPNG("data/colour_bar_en.PNG"))
    }    
  )


  
  observeEvent(input$selectall_N,{
    if(input$selectall_N == 0) return(NULL) 
    else if (input$selectall_N%%2 == 0){
      updateCheckboxGroupInput(session,"Natur_choices",choices=hydr_regions_fac)
    }else{
      updateCheckboxGroupInput(session,"Natur_choices",choices=hydr_regions_fac,selected=hydr_regions_fac)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
