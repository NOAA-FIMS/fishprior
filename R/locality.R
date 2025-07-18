  # locality
  library(rfishbase)
  library(tidyverse)

  # fishbase species codes
  common_names_list <- c(
    "Atlantic Cod", "Atlantic Mackerel",
    "Black Sea Bass", "Haddock",
    "Scup", "Yellowtail Flounder")
  
  sp_df <- common_to_sci(common_names_list)
  
  rows_keep <- c(1, 3, 4, 9, 16, 17)
  
  sp_df <- sp_df |>
    slice(rows_keep) |>
    dplyr::select(-Language) |>
    rename(species_code = SpecCode,
           species = Species,
           com_name = ComName)
  
  # edit taxonomy
  sp_df$species[sp_df$species == "Myzopsetta ferruginea"] <- "Limanda ferruginea"
  
  spec_codes <- unique(sp_df$species_code)
  
  # locality df
  s <- fb_tbl("stocks") |>
    filter(SpecCode %in% spec_codes) |>
    rename(species_code = SpecCode) |>
    select(StockCode, species_code, StockDefs, 
           StockDefsGeneral, Level, LocalUnique,
           contains(c("North", "South", "West", "East")))
  
  glimpse(s)
  
  unique(s$StockDefs)
  
  # merge with country code?
  o <- fb_tbl("occurrence") |>
    filter(SpecCode %in% spec_codes) |>
    select(SpecCode, Stockcode, C_Code) |>
    rename(StockCode = Stockcode,
           species_code = SpecCode,
           country_code = C_Code)
  
  glimpse(o)
  
  d <- left_join(s, o) |>
    left_join(sp_df)
  
  glimpse(d)

  # do any observations not have geographic info?
  col <- 'StockDefs'
  
  n <- all(!is.na(d[[col]]))
  
  if (n) {
    print("no rows contain NA")
  } else {
    print("rows with NAs exist")
  }
  
  
  # pick a species just for an example 

  cod <- d |>
   filter(com_name == "Atlantic Cod")

  n <- all(!is.na(cod[[col]]))

  if (n) {
    print("no rows contain NA")
  } else {
    print("rows with NAs exist")
  }
  
  unique(cod$StockDefs)

  cod <- d |>
    filter(com_name == "Atlantic cod")

  unique(cod$StockDefs)
