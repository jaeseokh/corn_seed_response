# ============================================================
# functions_for_processing.R
# Updated for "Monthly Moments" approach (No Planting Dates)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(data.table); library(stringr)
  library(sf); library(terra); library(elevatr); library(soilDB)
  library(daymetr); library(lubridate); library(here); library(exactextractr)
})

# ----------------------------
# 1. I/O Helpers & Spatial
# ----------------------------
# (These remain the same as before)

read_boundary <- function(ffy_id, crop, base_dir = here("Data")) {
  bd_path <- file.path(base_dir, crop, "exp_bdry_data", paste0(ffy_id, "_bdry.rds"))
  if(!file.exists(bd_path)) stop(paste("Boundary not found:", bd_path))
  bd <- readRDS(bd_path)
  if (is.na(sf::st_crs(bd))) bd <- sf::st_set_crs(bd, 4326)
  return(bd)
}

read_exp_sf <- function(ffy_id, crop, base_dir = here("Data")) {
  tb_path <- file.path(base_dir, crop, "exp_tb_data", paste0(ffy_id, "_tb.rds"))
  if(!file.exists(tb_path)) stop(paste("Experiment file not found:", tb_path))
  exp_tb <- readRDS(tb_path)
  
  geom_col <- attr(exp_tb, "sf_column")
  if(is.null(geom_col)) {
    if("geom" %in% names(exp_tb)) geom_col <- "geom"
    if("geometry" %in% names(exp_tb)) geom_col <- "geometry"
  }
  
  if("obs_id" %in% names(exp_tb)) exp_tb <- exp_tb[!duplicated(exp_tb$obs_id), ]
  else exp_tb$obs_id <- 1:nrow(exp_tb)

  exp_sf <- sf::st_as_sf(exp_tb)
  if (is.na(sf::st_crs(exp_sf))) exp_sf <- sf::st_set_crs(exp_sf, 4326)
  return(exp_sf)
}

# ----------------------------
# 2. Soil & Topo (Same as before)
# ----------------------------
get_ssurgo_props <- function(boundary_sf) {
  boundary_sp <- as(boundary_sf, "Spatial")
  res <- SDA_spatialQuery(boundary_sp, what = "mupolygon", db = "SSURGO", geomIntersection = TRUE)
  if (is.null(res) || nrow(res) == 0) return(NULL)
  ssurgo_geom <- sf::st_as_sf(res)
  if(is.na(st_crs(ssurgo_geom))) ssurgo_geom <- st_set_crs(ssurgo_geom, 4326)

  vars <- c("sandtotal_r", "silttotal_r", "claytotal_r", "awc_r", "om_r")
  mukeydata <- tryCatch({
    get_SDA_property(property = vars, method = "Weighted Average", mukeys = ssurgo_geom$mukey, top_depth = 0, bottom_depth = 150)
  }, error = function(e) NULL)

  if (is.null(mukeydata)) return(NULL)
  ssurgo_geom %>% dplyr::left_join(mukeydata, by = "mukey") %>%
    dplyr::select(mukey, sand = sandtotal_r, silt = silttotal_r, clay = claytotal_r, water_storage = awc_r, om = om_r)
}

area_weight_soils <- function(exp_sf, ssurgo_sf) {
  if (is.null(ssurgo_sf)) return(data.frame(clay=NA, sand=NA, silt=NA, water_storage=NA, om=NA)[rep(1,nrow(exp_sf)),])
  ssurgo_sf <- sf::st_transform(ssurgo_sf, st_crs(exp_sf))
  inter <- suppressWarnings(sf::st_intersection(dplyr::select(exp_sf, obs_id), ssurgo_sf))
  inter$area <- as.numeric(sf::st_area(inter))
  dt <- as.data.table(sf::st_drop_geometry(inter))
  dt[, area_pct := area / sum(area), by = obs_id]
  agg <- dt[, .(
    clay = weighted.mean(clay, w = area_pct, na.rm = TRUE),
    sand = weighted.mean(sand, w = area_pct, na.rm = TRUE),
    silt = weighted.mean(silt, w = area_pct, na.rm = TRUE),
    water_storage = weighted.mean(water_storage, w = area_pct, na.rm = TRUE),
    om   = weighted.mean(om, w = area_pct, na.rm = TRUE)
  ), by = obs_id]
  base <- data.frame(obs_id = exp_sf$obs_id)
  out <- merge(base, agg, by="obs_id", all.x=TRUE)
  out$obs_id <- NULL
  return(out)
}

get_topo_features <- function(exp_sf, boundary_sf) {
  bb <- sf::st_bbox(sf::st_transform(boundary_sf, 4326)) %>% sf::st_as_sfc() %>% sf::st_as_sf()
  elev_r <- tryCatch({ elevatr::get_elev_raster(bb, clip = "locations", z = 14) }, error = function(e) return(NULL))
  if(is.null(elev_r)) return(data.frame(elev=NA, slope=NA, aspect=NA, TPI=NA)[rep(1,nrow(exp_sf)),])
  r <- terra::rast(elev_r)
  names(r) <- "elev"
  slope  <- terra::terrain(r, "slope", unit = "degrees")
  aspect <- terra::terrain(r, "aspect", unit = "degrees")
  tpi    <- terra::terrain(r, "TPI")
  topo_stack <- c(r, slope, aspect, tpi)
  names(topo_stack) <- c("elev", "slope", "aspect", "TPI")
  vals <- exactextractr::exact_extract(topo_stack, exp_sf, c('weighted_mean'), weights = 'area', progress = FALSE)
  names(vals) <- c("elev", "slope", "aspect", "TPI")
  return(vals)
}

# ----------------------------
# 3. Weather (Monthly Moments)
# ----------------------------
gdd_calc <- function(tmin, tmax) pmax(pmin((tmin + tmax)/2, 30) - 10, 0)
edd_calc <- function(tmax) pmax(tmax - 30, 0)
day_to_month <- function(yday) {
  bounds <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 366)
  labs   <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  labs[findInterval(yday, bounds)]
}

process_weather <- function(ffy_id, boundary_sf) {
  
  # 1. Get Year and Rename it to avoid conflict
  year_str <- stringr::str_extract(ffy_id, "\\d{4}$")
  if(length(year_str) == 0 || is.na(year_str)) return(data.frame(ffy_id = ffy_id))
  
  # RENAME HERE: distinct name for the target variable
  target_year <- as.numeric(year_str)
  
  # 2. Download Daymet (30 Year History)
  cent <- sf::st_coordinates(sf::st_centroid(boundary_sf))
  dm_raw <- tryCatch({
    daymetr::download_daymet(lat = cent[1,"Y"], lon = cent[1,"X"], 
                             start = target_year - 30, end = target_year, internal = TRUE)$data
  }, error = function(e) return(NULL))
  
  if(is.null(dm_raw)) return(data.frame(ffy_id = ffy_id))
  
  dm_dt <- as.data.table(dm_raw)
  dm_dt <- dm_dt[, .(year, yday, prcp = prcp..mm.day., tmax = tmax..deg.c., tmin = tmin..deg.c.)]
  dm_dt[, `:=`(gdd = gdd_calc(tmin, tmax), edd = edd_calc(tmax), month = day_to_month(yday))]
  
  # Filter to Growing Season Months (Apr - Oct)
  target_months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
  dm_dt <- dm_dt[month %in% target_months]
  
  # 3. Calculate CURRENT YEAR Monthly Sums
  # Use 'target_year' variable here
  current_monthly <- dm_dt[year == target_year, .(
    prcp = sum(prcp),
    gdd  = sum(gdd),
    edd  = sum(edd)
  ), by = month]
  
  # Check if current year data exists before pivoting
  if(nrow(current_monthly) == 0) return(data.frame(ffy_id = ffy_id))
  
  # Pivot Current Year (Wide Format)
  curr_wide <- dcast(current_monthly, . ~ month, value.var = c("prcp", "gdd", "edd"))
  if("." %in% names(curr_wide)) curr_wide$. <- NULL 
  
  # 4. Calculate HISTORICAL Moments
  # FIX: Use 'target_year' variable. 'year < target_year' works correctly.
  hist_sums <- dm_dt[year < target_year, .( 
    prcp_sum = sum(prcp)
  ), by = .(year, month)]
  
  # Safety check: if history is empty (e.g. download failed or start year is same as target)
  if(nrow(hist_sums) == 0) {
    # Return just current if history fails
    return(cbind(data.frame(ffy_id = ffy_id), curr_wide))
  }
  
  # Then, calc moments across years
  hist_moments <- hist_sums[, .(
    prcp_mean = mean(prcp_sum, na.rm=TRUE),
    prcp_var  = var(prcp_sum,  na.rm=TRUE)
  ), by = month]
  
  # Pivot Historical (Wide Format)
  hist_wide <- dcast(hist_moments, . ~ month, value.var = c("prcp_mean", "prcp_var"))
  if("." %in% names(hist_wide)) hist_wide$. <- NULL
  
  # 5. Combine
  # Ensure they have 1 row each before binding
  if (nrow(curr_wide) == 1 && nrow(hist_wide) == 1) {
    out <- cbind(data.frame(ffy_id = ffy_id), curr_wide, hist_wide)
  } else {
    out <- data.frame(ffy_id = ffy_id) # Fallback
  }
  
  return(out)
}