# Load required libraries
library(gdalUtilities)
library(shiny)
library(leaflet)
library(sf)
library(terra)
library(aws.s3)
library(shinydashboard)
library(leaflet.extras)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(reshape2)
library(DT)
library(rworldmap)
library(sp)
library(shinyjs)

# Set working directory
#setwd('C:/cshiny/app')
source('zz_tiles.R')
source('Netflux.R')

# Set AWS environment variables
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "",
           "AWS_DEFAULT_REGION" = "")

options(shiny.maxRequestSize = 50 * 1024^2)

# Define land cover class names
lc_names <- c(
  "0" = "No Data",
  "10" = "Cropland, rainfed",
  "11" = "Herbaceous cover",
  "12" = "Tree or shrub cover",
  "20" = "Cropland, irrigated or postflooding",
  "30" = "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)",
  "40" = "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)",
  "50" = "Tree cover, broadleaved, evergreen, closed to open (>15%)",
  "60" = "Tree cover, broadleaved, deciduous, closed to open (>15%)",
  "61" = "Tree cover, broadleaved, deciduous, closed (>40%)",
  "62" = "Tree cover, broadleaved, deciduous, open (15to40%)",
  "70" = "Tree cover, needleleaved, evergreen, closed to open (>15%)",
  "71" = "Tree cover, needleleaved, evergreen, closed (>40%)",
  "72" = "Tree cover, needleleaved, evergreen, open (15to40%)",
  "80" = "Tree cover, needleleaved, deciduous, closed to open (>15%)",
  "81" = "Tree cover, needleleaved, deciduous, closed (>40%)",
  "82" = "Tree cover, needleleaved, deciduous, open (15to40%)",
  "90" = "Tree cover, mixed leaf type (broadleaved and needleleaved)",
  "100" = "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
  "110" = "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)",
  "120" = "Shrubland",
  "121" = "Evergreen shrubland",
  "122" = "Deciduous shrubland",
  "130" = "Grassland",
  "140" = "Lichens and mosses",
  "150" = "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)",
  "152" = "Sparse shrub (<15%)",
  "153" = "Sparse herbaceous cover (<15%)",
  "160" = "Tree cover, flooded, fresh or brackish water",
  "170" = "Tree cover, flooded, saline water",
  "180" = "Shrub or herbaceous cover, flooded, fresh/saline/brackish water",
  "190" = "Urban areas",
  "200" = "Bare areas",
  "201" = "Consolidated bare areas",
  "202" = "Unconsolidated bare areas",
  "210" = "Water bodies",
  "220" = "Permanent snow and ice"
)

world_map <- st_as_sf(getMap(resolution = "low"))  # Load world map

ui <- navbarPage(
  title = div(img(src="country.png"),"CPools Viewer"),
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  tabPanel("Single Epochs",
           fluidRow(
             div( class = "card",
                  leaflet::leafletOutput('map',height = "550px"),
                  absolutePanel(top = 100,right = 20, width = 200,
                                class = "custom-absolute-panel",
                                selectInput("accounting_year", "Select Accounting Year", 
                                            choices = c("2010", "2015", "2016", "2017", "2018", "2019", "2020", "2021")),
                                radioButtons("aoi_type", "Choose AOI Type:", choices = c("Administrative Area", "User-Defined Area"), selected = "User-Defined Area"),
                                conditionalPanel(
                                  condition = "input.aoi_type == 'User-Defined Area'",
                                  fileInput("shapefile_zip", "Upload Shapefile (ZIP)", multiple = FALSE, accept = c('.zip')),
                                  checkboxInput("show_polygon", "Show Polygon", value = TRUE)
                                ),
                                conditionalPanel(
                                  condition = "input.aoi_type == 'Administrative Area'",
                                  selectInput("country_select", "Select Country", choices = unique(world_map$NAME), selected = "Kenya")
                                ),
                                actionButton("get_tiles", "Download C Pools", class = "btn-primary btn-block"),
                                selectInput("c_pool_display", "Select C Pool Layer", choices = NULL, width = "100%"),
                                verbatimTextOutput("tile_names"),
                                verbatimTextOutput("raster_status")
                  )
             )
           ),
           
           fluidRow(
             column(3,
                    div(
                      class='card_grey_left',
                      div(class='card_grey-title',h5('Land Use/Ecosystem Selection',style="text-align: center;")),
                      conditionalPanel(
                        condition = "input.aoi_type == 'Administrative Area'",
                        radioButtons("land_use_option", "Choose Land Use/Ecosystem Data:", choices = "Use CCI-LC", selected = "Use CCI-LC")
                      ),
                      
                      conditionalPanel(
                        condition = "input.aoi_type == 'User-Defined Area'",
                        radioButtons("land_use_option", "Choose Land Use/Ecosystem Data:", choices = c("Upload Own", "Use CCI-LC"), selected = "Use CCI-LC")
                      ),
                      
                      conditionalPanel(
                        condition = "input.land_use_option == 'Upload Own'",
                        fileInput("land_use_zip", "Choose a Land Use ZIP file", accept = c('.zip')),
                        uiOutput("land_use_column_ui")
                      )
                      
                    )    
                    
             ),
             column(9,
                    div(
                      class='card_grey_right', 
                      
                      div(class='card_grey-title',h5('Ecosystem Analysis', style='text-align: center;')),
                      
                      actionButton("analyze", "Analyze"),
                      br(),
                      br(),
                      br(),
                      hidden( 
                        div( id='tables',
                          fluidRow(
                             column(6,
                             DTOutput("analysis_results"),
                             ),
                             column(6, 
                             DTOutput("ecosystem_analysis_results")
                             )
                          )
                        )
                      )
                    )
             )
           )
           
  ),
  tabPanel("Accounting Periods", 
           fluidRow(
             div( class = "card",
                  leafletOutput("map_accounting", height = 550),
                  
                  absolutePanel(top = 100,right = 20, width = 250,
                                class = "custom-absolute-panel",
                                selectInput("start_year", "Select Start Year", choices = c("2010", "2015", "2016", "2017", "2018", "2019", "2020", "2021")),
                                selectInput("end_year", "Select End Year", choices = c("2010", "2015", "2016", "2017", "2018", "2019", "2020", "2021")),
                                radioButtons("aoi_type_accounting", "Choose AOI Type:", choices = c("Administrative Area", "User-Defined Area"), selected = "User-Defined Area"),
                                conditionalPanel(
                                  condition = "input.aoi_type_accounting == 'User-Defined Area'",
                                  fileInput("shapefile_zip_accounting", "Upload Shapefile (ZIP)", multiple = FALSE, accept = c('.zip')),
                                  checkboxInput("show_polygon_accounting", "Show Polygon", value = TRUE)
                                ),
                                conditionalPanel(
                                  condition = "input.aoi_type_accounting == 'Administrative Area'",
                                  selectInput("accounting_country_select", "Select Country", choices = unique(world_map$NAME), selected = "Kenya")
                                ),
                                actionButton("get_tiles_accounting", "Download C Pools", class = "btn-primary btn-block", style = "width: 100%;"),
                                selectInput("c_pool_display_accounting", "Select C Pool Layer", choices = NULL, width = "100%"),
                                verbatimTextOutput("tile_names_accounting"),
                                verbatimTextOutput("raster_status_accounting")
                  )
                  
             )
     ),
     
     fluidRow(
              div(
                class='card', 
                
                div(class='card_grey-title',h5('Accounting Analysis', style='text-align: center;')),
                
                actionButton("analyze_accounting", "Analyze"),
                br(),
                br(),
                hidden( 
                  div( id='tables_accounting',
                       fluidRow(
                         column(6,
                                DTOutput("short_table_results")
                         ),
                         column(6, 
                                DTOutput("moderate_table_results")
                         ),
                         br(),
                         br(),
                         column(12, 
                                DTOutput("detailed_table_results")
                         )
                       )
                  )
                )
              )
     )
  )
  
)

server <- function(input, output, session) {
  reactive_aoi <- reactiveVal(NULL)
  reactive_selected_raster <- reactiveVal(NULL)
  download_progress <- reactiveVal(0)
  reactive_shp_data <- reactiveVal(NULL)
  
  reactive_aoi_accounting <- reactiveVal(NULL)
  reactive_selected_raster_accounting <- reactiveVal(NULL)
  download_progress_accounting <- reactiveVal(0)
  reactive_shp_data_accounting <- reactiveVal(NULL)
  cpools_popup_data <- reactiveVal(NULL)
  shp_centroid_reactive <- reactiveVal(NULL)
  
  world_map <- st_as_sf(getMap(resolution = "low"))  # Load world map
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(
        targetGroup = 'drawnItems',
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        rectangleOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'blue'))
      )
  })
  
  observe({
    if (input$aoi_type == "User-Defined Area") {
      leafletProxy("map") %>%
        addDrawToolbar(
          targetGroup = 'drawnItems',
          polylineOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          rectangleOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'blue'))
        ) %>%
        clearShapes()
    } else {
      leafletProxy("map") %>% removeDrawToolbar()
    }
  })
  
  
  observeEvent(list(input$country_select,input$map_shape_click,input$accounting_year), {
    req(input$aoi_type == "Administrative Area")
    country_shape <- world_map[world_map$NAME == input$country_select, ]
    
    # load the data for pop up when Country is selected
    popup_data <- readRDS(paste0('./data/Sum_',country_shape$ISO3[2],'_',input$accounting_year,'.Rdata'))
    
    country_shape$popupd <- popup_data # add the pop up info to the shapefile
    
    centroid <- sf::st_centroid(country_shape)
    
    reactive_aoi(country_shape)
    leafletProxy("map") %>%
      flyTo(lng = centroid$geometry[[2]][1],lat = centroid$geometry[[2]][2],zoom = 6) %>%
      clearTiles() %>%
      clearShapes() %>%
      clearPopups() %>%
      addTiles() %>%
      addPolygons(data = country_shape, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItems")%>%
      addPopups(lng = centroid$geometry[[2]][1],lat = centroid$geometry[[2]][2],popup = paste(popup_data))
  })
  
  observeEvent(input$shapefile_zip, {
    req(input$shapefile_zip)
    tmp_dir <- tempdir()
    unzip(input$shapefile_zip$datapath, exdir = tmp_dir)
    shp_file <- list.files(tmp_dir, pattern = "*.shp", full.names = TRUE)
    
    if (length(shp_file) > 0) {
      aoi <- st_read(shp_file[1])
      bbox <- st_bbox(aoi)
      
      cp <- readRDS(paste0('./data/Pools_',aoi$ISO3,'_',input$accounting_year,'.Rdata'))
      cpools_popup_data(cp)
      
      shp_centroid <- sf::st_centroid(aoi)
      
      shp_centroid_reactive(shp_centroid) # Feed the centroids into reactive object for use on the map
      
      reactive_aoi(aoi)
      leafletProxy("map") %>%
        flyTo(lng = shp_centroid$geometry[[1]][1],lat = shp_centroid$geometry[[1]][2],zoom = 7) %>% # for zooming in to AOI
        clearShapes() %>%
        clearPopups() %>%
        addPolygons(data = aoi, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItems")
      
    }
  })
  
  # Polygon drawing
  observeEvent(input$map_draw_new_feature, {
    coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    coords <- lapply(coords, function(coord) unlist(coord))
    coords <- do.call(rbind, coords)
    aoi <- st_polygon(list(coords))
    aoi <- st_sfc(aoi, crs = st_crs(4326))
    reactive_aoi(aoi)
    bbox <- st_bbox(aoi)
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = aoi, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItems")
  })
  
  # Land use shapefile upload
  observeEvent(input$land_use_zip, {
    req(input$land_use_zip)
    tmp_dir <- tempdir()
    unzip(input$land_use_zip$datapath, exdir = tmp_dir)
    shp_file <- list.files(tmp_dir, pattern = "*.shp", full.names = TRUE)
    if (length(shp_file) > 0) {
      land_use_data <- st_read(shp_file[1])
      reactive_shp_data(land_use_data)
      
      output$land_use_column_ui <- renderUI({
        selectInput("land_use_column", "Select Land Use Column", choices = names(land_use_data))
      })
    }
  })
  
  # Tile downloading
  downloadTilesBasedOnAOI <- reactive({
    if (!is.null(reactive_aoi())) {
      year <- input$accounting_year
      overlapping_files <- TilesInAOI(reactive_aoi(), year)
      
      output$tile_names <- renderPrint({
        print(overlapping_files)
      })
      
      if (length(overlapping_files) > 0) {
        local_files <- list()
        download_dir <- getwd()
        num_files <- length(overlapping_files)
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Downloading tiles", value = 0)
        
        for (i in seq_along(overlapping_files)) {
          file <- overlapping_files[i]
          s3_path <- paste0("s3://carbonpools/", file)
          local_path <- file.path(download_dir, file)
          if (!file.exists(local_path)) {
            save_object(object = file, bucket = "carbonpools", file = local_path)
          }
          local_files <- c(local_files, local_path)
          progress$inc(1/num_files, detail = paste("Downloaded", i, "of", num_files, "files"))
        }
        
        reactive_selected_raster(local_files)
        updateSelectInput(session, "c_pool_display", choices = local_files)
        
        if (length(local_files) > 0) {
          vrt_path <- file.path(download_dir, "output.vrt")
          message("Creating VRT file at: ", vrt_path)
          
          # Create the VRT file using gdalbuildvrt
          gdalbuildvrt(gdalfile = unlist(local_files), output.vrt = vrt_path)
          
          if (file.exists(vrt_path)) {
            message("VRT file created successfully.")
            return(vrt_path)
          } else {
            message("Failed to create VRT file.")
          }
        }
      }
    }
    return(NULL)
  })
  
  observeEvent(input$get_tiles, {
    downloadTilesBasedOnAOI()
  })
  
  reactive_raster <- reactive({
    raster_path <- downloadTilesBasedOnAOI()
    message("Raster path from get_tiles event: ", raster_path)
    if (!is.null(raster_path) && file.exists(raster_path)) {
      raster_data <- rast(raster_path)
      lc <- raster_data[[1]]
      
      names(raster_data) <- c("agc", "bgc", "deadwood", "litter","soc")
      aoi <- reactive_aoi()
      if (!is.null(aoi)) {
        aoi_vect <- vect(aoi)  # Convert to SpatVector
        raster_data <- crop(raster_data, aoi_vect)
        raster_data <- mask(raster_data, aoi_vect)
      }
      return(list(raster_data = raster_data))
    } else {
      message("Raster path is NULL or does not exist.")
      return(NULL)
    }
  })
  
  observe({
    req(reactive_raster())
    carbon_pools <- names(reactive_raster()$raster_data)
    updateSelectInput(session, "c_pool_display", choices = carbon_pools)
  })
  
  observe({
    req(reactive_raster())
    observeEvent(input$c_pool_display, {
      selected_layer <- input$c_pool_display
      raster_data <- reactive_raster()$raster_data
      
      if (!is.null(raster_data) && selected_layer %in% names(raster_data)) {
        layer_data <- raster_data[[selected_layer]]
        bins <- c(0, 5, 10, 50, 100, 150, 200, 300, Inf)
        pal <- colorBin(palette = "viridis", domain = terra::values(layer_data), bins = bins, na.color = "transparent")
        
        cpools_sel <- cpools_popup_data() %>%
          filter(tolower(selected_layer)==tolower(Pool))
        
        leafletProxy("map") %>%
          clearImages() %>%
          clearControls() %>%
          clearPopups() %>%
          addRasterImage(layer_data, colors = pal, opacity = 0.9, layerId = "raster") %>%
          addLegend(position = "bottomleft", pal = pal, values = terra::values(layer_data), title = "C Mg/ha", opacity = 1) %>%
          addPopups(lng = shp_centroid_reactive()$geometry[[1]][1],lat = shp_centroid_reactive()$geometry[[1]][2],popup = HTML(paste(cpools_sel$Pool,'<br>',cpools_sel$Sum_Gg)))
        
      }
    })
  })
  
  # Clear previous raster and legend when new layer is selected
  observeEvent(input$c_pool_display, {
    leafletProxy("map") %>%
      clearControls() %>%
      removeImage("raster")
  })
  
  # Polygon visibility control
  observe({
    proxy <- leafletProxy("map")
    if (input$show_polygon) {
      proxy %>% showGroup("drawnItems")
      if (!is.null(reactive_aoi())) {
        leafletProxy("map") %>% clearShapes()
        leafletProxy("map") %>% addPolygons(data = reactive_aoi(), color = "blue", weight = 2, fillOpacity = 0, group = "drawnItems")
      }
    } else {
      proxy %>% hideGroup("drawnItems")
    }
  })
  
  # Ecosystem analysis
  observeEvent(input$analyze, { #list(input$analyze,input$country_select)
    # req(reactive_raster(), reactive_shp_data())
    # 
    # # Ensure the selected column exists in the shapefile
    # if (!input$land_use_column %in% names(reactive_shp_data())) {
    #   stop("Selected land use column not found in the uploaded shapefile.")
    # }
    # 
    # # Extract unique land use types from the selected column
    # land_use_types <- unique(na.omit(reactive_shp_data()[[input$land_use_column]]))
    # 
    # # Initialize results dataframe
    # results <- data.frame(land_use = character(), stringsAsFactors = FALSE)
    # carbon_pools <- c("agc", "bgc", "deadwood", "litter", "soc", "total")
    # 
    # for (pool_name in carbon_pools) {
    #   results[[pool_name]] <- numeric()
    # }
    # 
    # # Compute the statistics for each land use type and carbon pool
    # for (lu_type in land_use_types) {
    #   lu_type_clean <- make.names(as.character(lu_type))  # Ensure valid column names
    #   new_row <- data.frame(land_use = lu_type_clean, stringsAsFactors = FALSE)
    #   for (pool_name in carbon_pools) {
    #     new_row[[pool_name]] <- NA
    #   }
    #   results <- rbind(results, new_row)
    # }
    # 
    # for (lu_type in land_use_types) {
    #   lu_polygons <- reactive_shp_data()[reactive_shp_data()[[input$land_use_column]] == lu_type, ]
    #   lu_type_clean <- make.names(as.character(lu_type))
    #   for (pool_name in carbon_pools) {
    #     masked_raster <- mask(reactive_raster()$raster_data[[pool_name]], vect(lu_polygons))
    #     mean_value <- round(global(masked_raster, fun = mean, na.rm = TRUE)[[1]], 2)
    #     results[results$land_use == lu_type_clean, pool_name] <- mean_value
    #   }
    # }
    # 
    # # Rename columns to include units
    # colnames(results)[2:length(colnames(results))] <- paste0(colnames(results)[2:length(colnames(results))], " (C Mg/ha)")
    
    country_analyze <- world_map[world_map$NAME == input$country_select, ]
    
    # make a reactive object to hold the data for display on the table - Ecosys_Pools.. data
    ecosy_pools.tbl <- reactive({
      tbl <- readRDS(paste0('./data/Ecosys_Pools_',country_analyze$ISO3[2],'_',input$accounting_year,'.Rdata'))
    })
    
    # Data table with the reactive object
    output$analysis_results <- renderDT({
      datatable(ecosy_pools.tbl(),
                options = list(dom = 'Bfrtip',
                               buttons = c('csv', 'excel', 'pdf'),
                               pageLength = 5,
                  scrollX=TRUE,scrollCollapse=TRUE
                )
                  ) %>%
        formatStyle(columns = names(ecosy_pools.tbl()),
                    backgroundColor = '#E4E8EB',
                    border = '1px solid #2E8B57')
                    
          
    })
    
    # make a reactive object to hold the data for display on the table - Ecosys_Sum.. data
    ecosy_sum.tbl <- reactive({
      tbl <- readRDS(paste0('./data/Ecosys_Sum_',country_analyze$ISO3[2],'_',input$accounting_year,'.Rdata'))
    })
    
    # Data table with the reactive object
    output$ecosystem_analysis_results <- renderDT({
      datatable(ecosy_sum.tbl(),
                options = list(dom = 'Bfrtip',
                               buttons = c('csv', 'excel', 'pdf'),
                               pageLength = 5,
                  scrollX=TRUE,scrollCollapse=TRUE
                )) %>%
        formatStyle(columns = names(ecosy_sum.tbl()),
                    backgroundColor = '#D9E2DC',
                    border = '1px solid #2E8B57')
                    
          
    })
    
    shinyjs::show(id='tables')
    
  })
  

  ########***** Accounting Periods
  ########*
  
  output$map_accounting <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(
        targetGroup = 'drawnItemsAccounting',
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        rectangleOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'blue'))
      )
  })
  
  ## Accounting Adminstrative AOI selection
  
  observeEvent(list(input$accounting_country_select,input$map_accounting_shape_click,input$start_year,input$end_year), {
    req(input$aoi_type_accounting == "Administrative Area")
    accounting.country <- world_map[world_map$NAME == input$accounting_country_select, ]
    
    # Load the data to be displayed on popups
    start_year.popup <- readRDS(paste0('./data/Sum_',accounting.country$ISO3[2],'_',input$start_year,'.Rdata'))
    end_year.popup <- readRDS(paste0('./data/Sum_',accounting.country$ISO3[2],'_',input$end_year,'.Rdata'))
    
    accounting.centroid <- sf::st_centroid(accounting.country)
    
    #reactive_aoi(accounting.country)
    leafletProxy("map_accounting") %>%
      flyTo(lng = accounting.centroid$geometry[[2]][1],lat = accounting.centroid$geometry[[2]][2],zoom = 6) %>%
      clearTiles() %>%
      clearShapes() %>%
      clearPopups() %>%
      clearControls() %>%
      addTiles() %>%
      addPolygons(data = accounting.country, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItems")%>%
      addPopups(lng = accounting.centroid$geometry[[2]][1],
                lat = accounting.centroid$geometry[[2]][2],popup = HTML(paste0(start_year.popup,'<br>',end_year.popup)))
  })
  
  
  # Handle AOI type changes for accounting
  observe({
    if (input$aoi_type_accounting == "User-Defined Area") {
      leafletProxy("map_accounting") %>% addDrawToolbar(
        targetGroup = 'drawnItemsAccounting',
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        rectangleOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'blue'))
      )
    } else {
      leafletProxy("map_accounting") %>% removeDrawToolbar()
    }
  })
  
  # Shapefile upload for AOI for accounting
  observeEvent(input$shapefile_zip_accounting, {
    req(input$shapefile_zip_accounting)
    tmp_dir <- tempdir()
    unzip(input$shapefile_zip_accounting$datapath, exdir = tmp_dir)
    shp_file <- list.files(tmp_dir, pattern = "*.shp", full.names = TRUE)
    if (length(shp_file) > 0) {
      aoi <- st_read(shp_file[1])
      accounting_shp_centroid <- sf::st_centroid(aoi)
      bbox <- st_bbox(aoi)
      reactive_aoi_accounting(aoi)
      leafletProxy("map_accounting") %>%
        flyTo(lng = accounting_shp_centroid$geometry[[1]][1],lat = accounting_shp_centroid$geometry[[1]][2],zoom = 7) %>% # for zooming in to AOI
        clearShapes() %>%
        clearPopups() %>%
        addPolygons(data = aoi, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItemsAccounting") 
    }
  })
  
  # Polygon drawing for accounting
  observeEvent(input$map_accounting_draw_new_feature, {
    coords <- input$map_accounting_draw_new_feature$geometry$coordinates[[1]]
    coords <- lapply(coords, function(coord) unlist(coord))
    coords <- do.call(rbind, coords)
    aoi <- st_polygon(list(coords))
    aoi <- st_sfc(aoi, crs = st_crs(4326))
    reactive_aoi_accounting(aoi)
    bbox <- st_bbox(aoi)
    leafletProxy("map_accounting") %>%
      clearShapes() %>%
      clearPopups() %>%
      addPolygons(data = aoi, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItemsAccounting")
  })
  
  # Tile downloading for accounting
  downloadTilesBasedOnAOIAccounting <- reactive({
    if (!is.null(reactive_aoi_accounting())) {
      start_year <- input$start_year
      end_year <- input$end_year
      overlapping_files_start <- TilesInAOI(reactive_aoi_accounting(), start_year)
      overlapping_files_end <- TilesInAOI(reactive_aoi_accounting(), end_year)
      
      output$tile_names_accounting <- renderPrint({
        print(overlapping_files_start)
        print(overlapping_files_end)
      })
      
      if (length(overlapping_files_start) > 0 && length(overlapping_files_end) > 0) {
        local_files_start <- list()
        local_files_end <- list()
        download_dir <- getwd()
        num_files_start <- length(overlapping_files_start)
        num_files_end <- length(overlapping_files_end)
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Downloading tiles", value = 0)
        
        for (i in seq_along(overlapping_files_start)) {
          file <- overlapping_files_start[i]
          s3_path <- paste0("s3://carbonpools/", file)
          local_path <- file.path(download_dir, file)
          if (!file.exists(local_path)) {
            save_object(object = file, bucket = "carbonpools", file = local_path)
          }
          local_files_start <- c(local_files_start, local_path)
          progress$inc(1/num_files_start, detail = paste("Downloaded", i, "of", num_files_start, "files"))
        }
        
        for (i in seq_along(overlapping_files_end)) {
          file <- overlapping_files_end[i]
          s3_path <- paste0("s3://carbonpools/", file)
          local_path <- file.path(download_dir, file)
          if (!file.exists(local_path)) {
            save_object(object = file, bucket = "carbonpools", file = local_path)
          }
          local_files_end <- c(local_files_end, local_path)
          progress$inc(1/num_files_end, detail = paste("Downloaded", i, "of", num_files_end, "files"))
        }
        
        if (length(local_files_start) > 0 && length(local_files_end) > 0) {
          vrt_path_start <- file.path(download_dir, "output_start.vrt")
          vrt_path_end <- file.path(download_dir, "output_end.vrt")
          message("Creating VRT files at: ", vrt_path_start, " and ", vrt_path_end)
          
          # Create the VRT files using gdalbuildvrt
          gdalbuildvrt(gdalfile = unlist(local_files_start), output.vrt = vrt_path_start)
          gdalbuildvrt(gdalfile = unlist(local_files_end), output.vrt = vrt_path_end)
          
          if (file.exists(vrt_path_start) && file.exists(vrt_path_end)) {
            message("VRT files created successfully.")
            return(list(start = vrt_path_start, end = vrt_path_end))
          } else {
            message("Failed to create VRT files.")
          }
        }
      }
    }
    return(NULL)
  })
  
  observeEvent(input$get_tiles_accounting, {
    downloadTilesBasedOnAOIAccounting()
  })
  
  reactive_raster_accounting <- reactive({
    raster_paths <- downloadTilesBasedOnAOIAccounting()
    message("Raster paths from get_tiles_accounting event: ", raster_paths)
    if (!is.null(raster_paths) && file.exists(raster_paths$start) && file.exists(raster_paths$end)) {
      raster_data_start <- rast(raster_paths$start)
      raster_data_end <- rast(raster_paths$end)
      
      names(raster_data_start) <- c("agc", "bgc", "deadwood", "litter", "soc")
      names(raster_data_end) <- c("agc", "bgc", "deadwood", "litter", "soc")
      
      lc_start <- raster_data_start[[1]]
      lc_end <- raster_data_end[[1]]
      
      aoi <- reactive_aoi_accounting()
      if (!is.null(aoi)) {
        aoi_vect <- vect(aoi)  # Convert to SpatVector
        raster_data_start <- crop(raster_data_start, aoi_vect)
        raster_data_start <- mask(raster_data_start, aoi_vect)
        
        raster_data_end <- crop(raster_data_end, aoi_vect)
        raster_data_end <- mask(raster_data_end, aoi_vect)
      }
      return(list(start = raster_data_start, end = raster_data_end))
    } else {
      message("Raster paths are NULL or do not exist.")
      return(NULL)
    }
  })
  
  observe({
    req(reactive_raster_accounting())
    carbon_pools <- names(reactive_raster_accounting()$start)
    updateSelectInput(session, "c_pool_display_accounting", choices = carbon_pools)
  })
  
  observe({
    req(reactive_raster_accounting())
    observeEvent(input$c_pool_display_accounting, {
      selected_layer <- input$c_pool_display_accounting
      raster_data_start <- reactive_raster_accounting()$start
      raster_data_end <- reactive_raster_accounting()$end
      
      if (!is.null(raster_data_start) && selected_layer %in% names(raster_data_start)) {
        layer_data_start <- raster_data_start[[selected_layer]]
        layer_data_end <- raster_data_end[[selected_layer]]
        
        bins <- c(0, 5, 10, 50, 100, 150, 200, 300, Inf)
        pal <- colorBin(palette = "viridis", domain = c(terra::values(layer_data_start), terra::values(layer_data_end)), bins = bins, na.color = "transparent")
        
        leafletProxy("map_accounting") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(layer_data_start, colors = pal, opacity = 0.9, layerId = "raster_start") %>%
          addRasterImage(layer_data_end, colors = pal, opacity = 0.9, layerId = "raster_end") %>%
          addLegend(position = "bottomright", pal = pal, values = c(terra::values(layer_data_start), terra::values(layer_data_end)), title = "C Mg/ha", opacity = 1) %>%
          fitBounds(lng1 = terra::ext(layer_data_start)[1], lat1 = terra::ext(layer_data_start)[3], lng2 = terra::ext(layer_data_start)[2], lat2 = terra::ext(layer_data_start)[4])
      }
    })
  })
  
  # Clear previous raster and legend when new layer is selected for accounting
  observeEvent(input$c_pool_display_accounting, {
    leafletProxy("map_accounting") %>%
      clearControls() %>%
      removeImage("raster_start") %>%
      removeImage("raster_end")
  })
  
  # Polygon visibility control for accounting
  observe({
    proxy <- leafletProxy("map_accounting")
    if (input$show_polygon_accounting) {
      proxy %>% showGroup("drawnItemsAccounting")
      if (!is.null(reactive_aoi_accounting())) {
        leafletProxy("map_accounting") %>% clearShapes()
        leafletProxy("map_accounting") %>% addPolygons(data = reactive_aoi_accounting(), color = "blue", weight = 2, fillOpacity = 0, group = "drawnItemsAccounting")
      }
    } else {
      proxy %>% hideGroup("drawnItemsAccounting")
    }
  })
  
  # Accounting analysis
  observeEvent(input$analyze_accounting, {
    #req(reactive_raster_accounting())
    
    country_analyze <- world_map[world_map$NAME == input$accounting_country_select, ]
    
    # rasters <- reactive_raster_accounting()
    # period1_ras <- rasters$start
    # period2_ras <- rasters$end
    
    # accounting_results <- calculate_net_fluxes(period1_ras, period2_ras)
    accounting_results <- readRDS(paste0('./data/',country_analyze$ISO3[2],'_',country_analyze$ISO3[2],'_',input$start_year,'_',input$end_year,'.Rdata'))
    
    output$short_table_results <- renderDT({
      results <- accounting_results[[1]]
      
      results <- results %>%
        mutate_if(is.numeric,~round(.,2))
      
      datatable(results, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        pageLength = 5,
        scrollX=TRUE
      )) %>%
        formatStyle(columns = names(results),
                    backgroundColor = '#E4E8EB',
                    border='1px solid #3498db')
      
    })
    
    output$moderate_table_results <- renderDT({
      results <- accounting_results[[2]]
      
      results <- results %>%
        mutate_if(is.numeric,~round(.,2))
      
      datatable(results, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        pageLength = 5,
        scrollX=TRUE
      )) %>%
        formatStyle(columns = names(results),
                    backgroundColor = '#D9E2DC',
                    border='1px solid #3498db')
    })
    
    output$detailed_table_results <- renderDT({
      results <- accounting_results[[3]]
      
      results <- results %>%
        mutate_if(is.numeric,~round(.,2))
      
      datatable(results, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        pageLength = 5,
        scrollX=TRUE
      )) %>%
        formatStyle(columns = names(results),
                    backgroundColor = '#FFFFFF',
                    border='1px solid #3498db')
    })
    
    shinyjs::show('tables_accounting')
    
  })
  

}

shinyApp(ui = ui, server = server)
