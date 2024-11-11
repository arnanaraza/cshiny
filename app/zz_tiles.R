
# Function to get overlapping tiles
TilesInAOI <- function(aoi, year) {
  
  bbox <- st_bbox(aoi)
  xmin <- floor(bbox$xmin / 10) * 10
  xmax <- ceiling(bbox$xmax / 10) * 10
  ymin <- floor(bbox$ymin / 10) * 10
  ymax <- ceiling(bbox$ymax / 10) * 10
  
  overlapping_files <- c()
  
  for (lat in seq(ymin, ymax - 10, by = 10)) {
    for (lon in seq(xmin, xmax - 10, by = 10)) {
      lat_prefix <- ifelse(lat >= 0, sprintf("N%02d", lat+10), sprintf("S%02d", abs(lat)))
      lon_prefix <- ifelse(lon >= 0, sprintf("E%03d", lon), sprintf("W%03d", abs(lon)))
      #tile_name <- paste0(lat_prefix, lon_prefix, "_ESACCI-BIOMASS-L4-Cpools-MERGED-100m-", year, "-fv5.0.tif")
      tile_name <- paste0(lat_prefix, lon_prefix,"_ESACCI-BIOMASS-L4-Cpools-MERGED-10km-", year, "-fv4.0.tif")
      
      tile_bbox <- st_bbox(c(xmin = lon, xmax = lon + 10, ymin = lat, ymax = lat + 10), crs = 4326)
      tile_geom <- st_as_sfc(tile_bbox, crs = 4326)
      if (st_intersects(st_transform(tile_geom, 4326), st_transform(aoi, 4326), sparse = FALSE)[1, 1]) {
        overlapping_files <- c(overlapping_files, tile_name)
      }
    }
  }
  return(overlapping_files)
}
