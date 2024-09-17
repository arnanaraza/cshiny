

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

years <- c(2010,2015,2016,2017, 2018,2019, 2020,2021)
year_combinations <- expand.grid(start_year = years, end_year = years)
valid_combinations <- subset(year_combinations, start_year < end_year)

gigagrams_value <- 1 / 1000

# Function to calculate zonal statistics for a given accounting period
calculate_zonal_stats <- function(raster_stack, unit=gigagrams_value, unique_lc) {
  lc <- raster_stack[["lc"]]
  
  # Initialize an empty data frame to store results
  results <- data.frame(LandCover = lc_names[as.character(unique_lc)])
  lc_list <- lapply(unique_lc, function(x) ifel(lc == x, 1, NA))
  
  # Calculate statistics for each carbon pool
  for (pool in c('lc', "agc", "bgc", "deadwood", "litter", "soc", 'total')) {
    pool_data <- raster_stack[[pool]]
    masked_rasters <- lapply(lc_list, function(x) terra::mask(pool_data, x, maskvalue = NA))
    gc()
    print('tapos mga mask')
    #    pool_sums <- sapply(masked_rasters, function(x) round(base::sum(terra::values(x), na.rm = TRUE), 1))
    
    pool_sums <- sapply(masked_rasters, function(x) {
      raster_sum <- global(x, fun="sum", na.rm=TRUE)
      rounded_sum <- round(as.numeric(raster_sum[["sum"]]), 1)
      return(rounded_sum)
    })
    gc()
    results[[pool]] <- pool_sums
    print('tapos mga pool')
  }
  gc()
  results[,3:length(results)] <- round(results[,3:length(results)] * unit ,2)
  results
}
calculate_net_fluxes <- function(period1_ras, period2_ras) {
  lc <- period1_ras[["lc"]]
  unique_lc <- freq(lc)[[2]]
  print(unique_lc)
  
  str0 <- deparse(substitute(period1_ras)) 
  str0 <- substr(str0, 5, 7)
  str1 <- deparse(substitute(period1_ras)) 
  str1 <- as.numeric(gsub("[^0-9]", "", str1))
  str2 <- deparse(substitute(period2_ras)) 
  str2 <- as.numeric(gsub("[^0-9]", "", str2))
  str <- paste0(str1,'_',str2)
  
  # Period 1 stats
  period1 <- calculate_zonal_stats(period1_ras, gigagrams_value, unique_lc)
  # Period 1 stats
  period2 <- calculate_zonal_stats(period2_ras, gigagrams_value, unique_lc)
  
  # emission-removal
  net_flux <- period2_ras$total - period1_ras$total
  seq <- (ifel(net_flux >= 0, net_flux, 0))
  emi <- (ifel(net_flux < 0, net_flux, 0))
  emi_rem <- c(emi,seq)
  names(emi_rem) <- c('Emission' ,'Sequestration')
  
  results <- data.frame(LandCover = lc_names[as.character(unique_lc)])
  
  lc_list <- lapply(unique_lc, function(x) ifel(lc == x, 1, NA))
  gc()
  print(emi_rem)
  for (pool in c("Emission", "Sequestration")) {
    pool_data <- emi_rem[[pool]]
    masked_rasters <- lapply(lc_list, function(x) terra::mask(pool_data, x, maskvalue = NA))
    gc()
    #pool_sums <- sapply(masked_rasters, function(x) round(base::sum(terra::values(x), na.rm = TRUE),1))
    
    
    pool_sums <- sapply(masked_rasters, function(x) {
      raster_sum <- global(x, fun="sum", na.rm=TRUE)
      rounded_sum <- round(as.numeric(raster_sum[["sum"]]), 1)
      return(rounded_sum)
    })
    
    results[[pool]] <- pool_sums
  }
  gc()
  # Ensure land cover classes are in the same order
  period2 <- period2[match(period1$LandCover, period2$LandCover), ]
  # Calculate net fluxes
  net_fluxes <- period2[, -1] - period1[, -1] # Exclude the first column (LandCover)
  colnames(net_fluxes) <- paste0("NetFlux_", colnames(net_fluxes))
  print(head(net_fluxes))
  
  # Combine results
  combined_results <- cbind(period1, period2[, -1], net_fluxes[,-1], results[,-1])
  new_column_names <- c('lc_opening','agc_opening', 'bgc_opening', 'deadwood_opening', 'litter_opening', 'soc_opening', 'OpeningStocks',
                        'lc_closing','agc_closing', 'bgc_closing', 'deadwood_closing', 'litter_closing', 'soc_closing', 'ClosingStocks') 
  
  colnames(combined_results)[2:15] <- new_column_names
  combined_results$Period <- str
  combined_results$Country <- str0
  combined_results$defo_frac <- combined_results$lc_closing / combined_results$lc_opening
  combined_results$defo_frac  <- ifelse( combined_results$defo_frac >1, 1,  combined_results$defo_frac )
  combined_results$deg_frac <- 1-combined_results$defo_frac
  combined_results$emission_defo <- round(combined_results$Emission * combined_results$defo_frac,1)
  combined_results$emission_deg <-  round(combined_results$Emission * combined_results$deg_frac,1)
  names(combined_results)
  short_table <- data.frame(combined_results$LandCover, combined_results$Period,combined_results$Country,
                            combined_results$OpeningStocks,round(combined_results$NetFlux_total, 2),
                            combined_results$ClosingStocks)
  names(short_table) <- sapply(names(short_table), function(x) sub(".*\\.", "", x))
  moderate_table <- combined_results[,c(1:17,26,27)]
  names(moderate_table) <- sapply(names(moderate_table), function(x) sub(".*\\.", "", x))
  list(short_table, moderate_table, combined_results)
}

