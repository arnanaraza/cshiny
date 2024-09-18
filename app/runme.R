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

# Set working directory
setwd('C:/cshiny/app')
source('zz_tiles.R')
source('Netflux.R')

# Set AWS environment variables
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "",
           "AWS_DEFAULT_REGION" = "eu-central-1")

options(shiny.maxRequestSize = 50 * 1024^2)
world_map <- st_as_sf(getMap(resolution = "low"))  # Load world map
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

ui <- dashboardPage(
  dashboardHeader(title = "C pools Viewer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Single Epochs", tabName = "aoi_selection", icon = icon("globe")),
      menuItem("Accounting Periods", tabName = "accounting", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "aoi_selection",
        fluidRow(
          box(
            title = "Select Year", status = "primary", solidHeader = TRUE,
            selectInput("accounting_year", "Select Accounting Year", choices = c("2010", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))
          ),
          box(
            title = "Select AOI Type", status = "primary", solidHeader = TRUE,
            radioButtons("aoi_type", "Choose AOI Type:", choices = c("Administrative Area", "User-Defined Area"), selected = "User-Defined Area"),
            conditionalPanel(
              condition = "input.aoi_type == 'User-Defined Area'",
              fileInput("shapefile_zip", "Upload Shapefile (ZIP)", multiple = FALSE, accept = c('.zip')),
              checkboxInput("show_polygon", "Show Polygon", value = TRUE)
            ),
            conditionalPanel(
              condition = "input.aoi_type == 'Administrative Area'",
              selectInput("country_select", "Select Country", choices = unique(world_map$NAME), selected = "Uganda")
            )
          ),
          box(
            title = "Draw AOI", status = "primary", solidHeader = TRUE, width = 12,
            leafletOutput("map", height = 600),
            actionButton("get_tiles", "Download C Pools", class = "btn-primary btn-block", style = "width: 100%;"),
            selectInput("c_pool_display", "Select C Pool Layer", choices = NULL, width = "100%"),
            verbatimTextOutput("tile_names"),
            verbatimTextOutput("raster_status")
          )
        ),
        fluidRow(
          box(
            title = "Land Use/Ecosystem Selection", status = "primary", solidHeader = TRUE, width = 12,
            radioButtons("land_use_option", "Choose Land Use/Ecosystem Data:", choices = c("Upload Own", "Use CCI-LC"), selected = "Use CCI-LC"),
            conditionalPanel(
              condition = "input.land_use_option == 'Upload Own'",
              fileInput("land_use_zip", "Choose a Land Use ZIP file", accept = c('.zip')),
              uiOutput("land_use_column_ui")
            )
          ),
          box(
            title = "Ecosystem Analysis", status = "primary", solidHeader = TRUE, width = 12,
            actionButton("analyze", "Analyze", class = "btn-primary btn-block"),
            dataTableOutput("analysis_results"),
            dataTableOutput("ecosystem_analysis_results") # Second table
          )
        )
      ),
      tabItem(
        tabName = "accounting",
        fluidRow(
          box(
            title = "Select Years", status = "primary", solidHeader = TRUE,
            selectInput("start_year", "Select Start Year", choices = c("2010", "2015", "2016", "2017", "2018", "2019", "2020", "2021")),
            selectInput("end_year", "Select End Year", choices = c("2010", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))
          ),
          box(
            title = "Select AOI Type", status = "primary", solidHeader = TRUE,
            radioButtons("aoi_type_accounting", "Choose AOI Type:", choices = c("Administrative Area", "User-Defined Area"), selected = "User-Defined Area"),
            conditionalPanel(
              condition = "input.aoi_type_accounting == 'User-Defined Area'",
              fileInput("shapefile_zip_accounting", "Upload Shapefile (ZIP)", multiple = FALSE, accept = c('.zip')),
              checkboxInput("show_polygon_accounting", "Show Polygon", value = TRUE)
            )
          ),
          box(
            title = "Draw AOI", status = "primary", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "input.aoi_type_accounting == 'User-Defined Area'",
              leafletOutput("map_accounting", height = 600)
            ),
            actionButton("get_tiles_accounting", "Download C Pools", class = "btn-primary btn-block", style = "width: 100%;"),
            selectInput("c_pool_display_accounting", "Select C Pool Layer", choices = NULL, width = "100%"),
            verbatimTextOutput("tile_names_accounting"),
            verbatimTextOutput("raster_status_accounting")
          )
        ),
        fluidRow(
          box(
            title = "Accounting Analysis", status = "primary", solidHeader = TRUE, width = 12,
            actionButton("analyze_accounting", "Analyze", class = "btn-primary btn-block"),
            dataTableOutput("short_table_results"),
            dataTableOutput("moderate_table_results"),
            dataTableOutput("detailed_table_results")
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
  
  observeEvent(input$country_select, {
    req(input$aoi_type == "Administrative Area")
    country_shape <- world_map[world_map$NAME == input$country_select, ]
    bbox <- st_bbox(country_shape)
    reactive_aoi(country_shape)
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = country_shape, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItems") %>%
      fitBounds(bbox[1], bbox[3], bbox[2], bbox[4])
  })
  
  observeEvent(input$shapefile_zip, {
    req(input$shapefile_zip)
    tmp_dir <- tempdir()
    unzip(input$shapefile_zip$datapath, exdir = tmp_dir)
    shp_file <- list.files(tmp_dir, pattern = "*.shp", full.names = TRUE)
    if (length(shp_file) > 0) {
      aoi <- st_read(shp_file[1])
      bbox <- st_bbox(aoi)
      reactive_aoi(aoi)
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data = aoi, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItems") %>%
        fitBounds(bbox[1], bbox[3], bbox[2], bbox[4])
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
          print(local_files)
          gdalbuildvrt(gdalfile = unlist(local_files), output.vrt = vrt_path, overwrite=T)
          
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
    print(raster_path)
    message("Raster path from get_tiles event: ", raster_path)
    if (!is.null(raster_path) && file.exists(raster_path)) {
      raster_data <- rast(raster_path)
      lc <- raster_data[[1]]
      names(raster_data) <- c("lc", "agc", "bgc", "deadwood", "litter", "soc", "total")
      raster_data <- raster_data[[-1]]  # Exclude the first band "lc"
      aoi <- reactive_aoi()
      if (!is.null(aoi)) {
        aoi_vect <- vect(aoi)  # Convert to SpatVector
        raster_data <- crop(raster_data, aoi_vect)
        raster_data <- mask(raster_data, aoi_vect)
      }
      return(list(raster_data = raster_data, lc = lc))
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
        
        leafletProxy("map") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(layer_data, colors = pal, opacity = 0.9, layerId = "raster") %>%
          addLegend(position = "bottomright", pal = pal, values = terra::values(layer_data), title = "C Mg/ha", opacity = 1) %>%
          fitBounds(lng1 = terra::ext(layer_data)[1], lat1 = terra::ext(layer_data)[3], lng2 = terra::ext(layer_data)[2], lat2 = terra::ext(layer_data)[4])
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
  observeEvent(input$analyze, {
    req(reactive_raster(), reactive_shp_data())
    
    # Ensure the selected column exists in the shapefile
    if (!input$land_use_column %in% names(reactive_shp_data())) {
      stop("Selected land use column not found in the uploaded shapefile.")
    }
    
    # Extract unique land use types from the selected column
    land_use_types <- unique(na.omit(reactive_shp_data()[[input$land_use_column]]))
    
    # Initialize results dataframe
    results <- data.frame(land_use = character(), stringsAsFactors = FALSE)
    carbon_pools <- c("agc", "bgc", "deadwood", "litter", "soc", "total")
    
    for (pool_name in carbon_pools) {
      results[[pool_name]] <- numeric()
    }
    
    # Compute the statistics for each land use type and carbon pool
    for (lu_type in land_use_types) {
      lu_type_clean <- make.names(as.character(lu_type))  # Ensure valid column names
      new_row <- data.frame(land_use = lu_type_clean, stringsAsFactors = FALSE)
      for (pool_name in carbon_pools) {
        new_row[[pool_name]] <- NA
      }
      results <- rbind(results, new_row)
    }
    
    for (lu_type in land_use_types) {
      lu_polygons <- reactive_shp_data()[reactive_shp_data()[[input$land_use_column]] == lu_type, ]
      lu_type_clean <- make.names(as.character(lu_type))
      for (pool_name in carbon_pools) {
        masked_raster <- mask(reactive_raster()$raster_data[[pool_name]], vect(lu_polygons))
        mean_value <- round(global(masked_raster, fun = mean, na.rm = TRUE)[[1]], 2)
        results[results$land_use == lu_type_clean, pool_name] <- mean_value
      }
    }
    
    # Rename columns to include units
    colnames(results)[2:length(colnames(results))] <- paste0(colnames(results)[2:length(colnames(results))], " (C Mg/ha)")
    
    output$ecosystem_analysis_results <- renderDataTable({
      results
    })
  })
  
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
      bbox <- st_bbox(aoi)
      reactive_aoi_accounting(aoi)
      leafletProxy("map_accounting") %>%
        clearShapes() %>%
        addPolygons(data = aoi, color = "blue", weight = 2, fillOpacity = 0, group = "drawnItemsAccounting") %>%
        fitBounds(bbox[1], bbox[3], bbox[2], bbox[4])
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
      
      names(raster_data_start) <- c("lc", "agc", "bgc", "deadwood", "litter", "soc", "total")
      names(raster_data_end) <- c("lc", "agc", "bgc", "deadwood", "litter", "soc", "total")
      
      lc_start <- raster_data_start[[1]]
      lc_end <- raster_data_end[[1]]
      
      #  raster_data_start <- raster_data_start[[-1]]  # Exclude the first band "lc"
      # raster_data_end <- raster_data_end[[-1]]  # Exclude the first band "lc"
      
      aoi <- reactive_aoi_accounting()
      if (!is.null(aoi)) {
        aoi_vect <- vect(aoi)  # Convert to SpatVector
        raster_data_start <- crop(raster_data_start, aoi_vect)
        raster_data_start <- mask(raster_data_start, aoi_vect)
        
        raster_data_end <- crop(raster_data_end, aoi_vect)
        raster_data_end <- mask(raster_data_end, aoi_vect)
      }
      return(list(start = raster_data_start, end = raster_data_end, lc_start = lc_start, lc_end = lc_end))
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
    req(reactive_raster_accounting())
    
    rasters <- reactive_raster_accounting()
    period1_ras <- rasters$start
    period2_ras <- rasters$end
    
    accounting_results <- calculate_net_fluxes(period1_ras, period2_ras)
    
    output$short_table_results <- renderDataTable({
      results <- accounting_results[[1]]
      results <- cbind(Epoch = paste(input$start_year, "-", input$end_year), results) # Add epochs
      datatable(results, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        pageLength = 10
      ))
    })
    
    output$moderate_table_results <- renderDataTable({
      results <- accounting_results[[2]]
      results <- cbind(Epoch = paste(input$start_year, "-", input$end_year), results) # Add epochs
      datatable(results, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        pageLength = 10
      ))
    })
    
    output$detailed_table_results <- renderDataTable({
      results <- accounting_results[[3]]
      results <- cbind(Epoch = paste(input$start_year, "-", input$end_year), results) # Add epochs
      datatable(results, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        pageLength = 10
      ))
    })
    
 
  })
}

shinyApp(ui = ui, server = server)
