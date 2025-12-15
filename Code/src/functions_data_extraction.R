# Code/src/functions_data_extraction.R

library(terra)      # For handling the Yield Raster (NetCDF/GeoTIFF)
library(sf)         # For handling field boundaries
library(data.table)
library(stringr)


# --- Helper 1: Extract Yield History for Single Field ---
get_neighbor_history <- function(bdry_sf, raster_stack) {
  
  # bdry_sf: The simple feature object of the field boundary
  # raster_stack: The terra object containing SCYM yield history
  
  # 1. Get the Centroid
  # We extract the pixel value where the field center is located
  # This represents the "Neighbor/Regional" signal for that location
  centroid <- st_centroid(bdry_sf)
  
  # 2. Match CRS (Coordinate Reference System)
  # Critical: Transform the field point to match the map projection of the SCYM data
  centroid_trans <- st_transform(centroid, crs(raster_stack))
  
  # 3. Extract Values
  # Extract the vector of yields across all layers (years)
  # ID = FALSE ensures we just get the values, not the spatial ID
  # method = "simple" picks the pixel the point falls into
  yield_values <- extract(raster_stack, centroid_trans, ID = FALSE, method = "simple")
  
  # 4. Format as Data Table
  # Assumes layer names in raster_stack represent years (e.g., "yield_2000")
  # We clean the names later in the main script if needed
  
  # Check if extraction returned NA (e.g., field outside map coverage)
  if (all(is.na(yield_values))) {
    return(NULL)
  }
  
  # Convert 1-row data frame to a clean data table
  dt <- data.table(
    year_raw = names(raster_stack),
    neighbor_yield_bu_ac = as.numeric(yield_values[1, ])
  )
  
  return(dt)
}


get_field_history <- function(bdry_sf, yield_stack, csdl_stack) {
  
  # 1. Get Centroid
  centroid <- st_centroid(bdry_sf)
  
  # 2. Extract Yield (SCYM)
  # Transform centroid to match Yield projection
  cent_yield <- st_transform(centroid, crs(yield_stack))
  yield_vals <- extract(yield_stack, cent_yield, ID = FALSE, method = "simple")
  
  # 3. Extract Crop Type (CSDL)
  # Transform centroid to match CSDL projection (Albers)
  cent_csdl <- st_transform(centroid, crs(csdl_stack))
  crop_vals <- extract(csdl_stack, cent_csdl, ID = FALSE, method = "simple")
  
  # 4. Check for Empty Extraction
  if(all(is.na(yield_vals)) && all(is.na(crop_vals))) return(NULL)
  
  # 5. Combine into Table
  # FIX: Use str_extract to grab the FIRST 4 digits (the year)
  # This correctly handles "1999_CSDL_v04" -> "1999"
  
  dt_yield <- data.table(
    year = as.numeric(str_extract(names(yield_stack), "\\d{4}")),
    neighbor_yield = as.numeric(yield_vals[1,])
  )
  
  dt_crop <- data.table(
    year = as.numeric(str_extract(names(csdl_stack), "\\d{4}")),
    crop_code = as.numeric(crop_vals[1,])
  )
  
  # Merge on Year (Inner join keeps only years present in both)
  final_dt <- merge(dt_yield, dt_crop, by = "year", all.x = TRUE)
  
  return(final_dt)
}