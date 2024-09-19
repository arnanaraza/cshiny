# Define the years you want to process
years <- c(2010, 2017:2020)  # Add more years as needed

# Iterate over each year
for (year in years) {
  
  # Set the working directory for the specific year
  input_dir <- paste0('C:/ESACCI-BIOMASS-L4-Cpools-MERGED-10km-', year, '-fv4.0')
  setwd(input_dir)
  
  # Get list of .tif files for the current year
  f <- list.files(getwd(), pattern = '\\.tif$', full.names = TRUE)
  
  # Initialize an empty list to store the raster layers
  l <- list()
  
  # Loop through each file and read it as a raster object
  for (i in f) {
    l[[length(l) + 1]] <- rast(i)  # Use a numeric index to avoid named elements
  }
  
  # Merge all the rasters in the list
  r <- do.call(terra::merge, l)
  
  # Set output directory and save the merged raster for the current year
  setwd('C:/cshiny/app/')
  output_file <- paste0('ESACCI-BIOMASS-L4-Cpools-MERGED-10km-', year, '-fv4.0.tif')
  writeRaster(r, output_file, overwrite = TRUE)
  
  # Optional: print a message to track progress
  print(paste0("Merging and saving completed for year ", year))
}
