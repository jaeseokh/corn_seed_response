library(here)
library(data.table)
library(magrittr)
library(dplyr)

### Variable name uniformity  ##

# 1. choose all type of seed and nitrogen rate variable names
vars_to_select <- c("obs_id","yield", "seed_rate", "s_rate", "n_rate", "uan32_rate", "nh3_rate", "uan28_rate", "urea_rate")

# 2. sort out only the experimental  input variables 
vars_to_check <-  c( "seed_rate", "s_rate", "n_rate", "uan32_rate", "nh3_rate", "uan28_rate", "urea_rate")

# 3. Field specific variables to be included in the Regression ##
field_reg_vars <- c('elev','slope','aspect','tpi', 'clay', 'sand', 
                         'silt', 'water_storage')
 

### Price information (extract and calculate crop and input prices information from the raw data)

# 1. Extract the corn price-received by year
 corn_price_raw <- fread(here("Data","Raw","corn_price_received_by_year.csv"))
 
   corn_price_tab <- corn_price_raw[Period == "MARKETING YEAR",.(Year, Value)]
     
     setnames(corn_price_tab, "Value", "corn_price")
# 2. Extract Seed cost varition by year 

corn_cost_return <- fread(here("Data","Raw","corn_cost_return.csv"))

 seed_cost_tab <- corn_cost_return[Category == "Operating costs"
                      & Item == "Seed"
                      & Region == "U.S. total" 
                      & Year %in% 2016:2023
                       ,.(Year, Value)]

setnames(seed_cost_tab, "Value", "seed_cost")

# 3. Merge the corn price and seed cost information by year
price_tab <- merge(corn_price_tab, seed_cost_tab, , by = "Year")

# 4. Generate the seed price
price_tab [, `:=`(
  seed_price = round(seed_cost*3 / seed_cost[Year == 2016],2)
)][, seed_cost := NULL][, ratio_rank := rank(seed_price/corn_price)  ]

# 5. Make corn_price_low(high) and seed_price_low(high) which is the combination of the price 
#  where the seed_price/conr_price ratio is the highest(lowest)
corn_price_high <-  price_tab[Year == 2016, .(corn_price)]$corn_price

seed_price_high <-  price_tab[Year == 2016, .(seed_price)]$seed_price
     
corn_price_low <-  price_tab[Year == 2021, .(corn_price)]$corn_price
seed_price_low <-  price_tab[Year == 2021, .(seed_price)]$seed_price
   

### Extract USDA reported annual average corn seed rates by state and year

# 1. read the US county map shape file
  us_map_sf <- st_read(here("Data","Raw","us_state_2023.shp"))  %>%
         st_transform(4326)
 
  # 2. sort out the state fips code for the states where experimental(ofpe) field are located
   ofpe_fips <- c(17, 18, 19, 20, 21, 26, 27, 29, 31, 38, 39, 46, 55)

    ofpe_sf <- us_map_sf %>%
               setnames(names(.), tolower(names(.))) %>%
               filter(statefp %in% ofpe_fips) %>% 
               mutate(fips = as.numeric(statefp)) %>%
               dplyr::select(fips,stusps)


# 3. add the USDA reported seed rate information by state and year

  seed_usda_raw <- fread(here("Data","Raw","corn_annual_plant_pop.csv"))

   seed_usda <-seed_usda_raw %>%
    setnames(names(.), tolower(names(.))) %>%        
     setnames("state ansi", "fips") %>%
     .[period=='YEAR',.(year, fips, s_rate =as.numeric(gsub("[^0-9.]", "", value))/1000)] 

