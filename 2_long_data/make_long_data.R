
# Step 1 to convert HRS to long format 

suppressPackageStartupMessages({
  library(tidyverse)
  library(dtplyr)
  library(furrr)
  library(haven)
})

# Set this to false if you don't want to rewrite old files that take a 
# long time to run
overwrite <- TRUE

# Define paths
path_raw <- file.path("1_raw_data")
out_path <- file.path("2_long_data")
in_files <- list.files(path_raw)
in_files <- in_files[grepl(".dta", in_files)]

# Read in secondary metadata info path
metadata <- read_csv(file.path(path_raw, "var_data.csv"), col_types = cols())
metadata <- metadata[,c(1,4)]
names(metadata) <- c('varname', 'keep')
removers <- metadata %>%
  filter(keep == 0) %>%
  .$varname
keepers <- metadata %>%
  filter(keep == 1) %>%
  .$varname

# Functions to clean specific countries.
# Should only have cleaning code for before making data long

prep_hrs <- function(data) {
  
  # Grab variables from the longitudinal, nonharmonized dataset
  message("Reading in the HRS longitudinal file... slooww.....")
  vars_to_merge <- read_csv(
    file.path(path_raw, "hrs_longitudinal/var_merge.csv"),
    col_types = cols()  
  ) %>%
    .$varname
  hrs_long <- read_dta(file.path(path_raw, 'hrs_longitudinal/randhrs1992_2018v1.dta'))  
  hrs_long <- hrs_long %>%
    mutate(across(everything(), ~ as.character(ifelse(is.na(.), na_tag(.), .))))
  hrs_long <- hrs_long %>%
    select(
      hhidpn, 
      starts_with('inw'), 
      ends_with("agem_e"),
      contains("iwend"),
      contains(vars_to_merge)
    )
  
  # Merge the longitudinal variables into this one
  data <- data %>%
    lazy_dt() %>%
    left_join(hrs_long, by = 'hhidpn') %>%
    as_tibble()
  
  return(data)
  
}

prep_mhas <- function(data) {
  
  data <- data %>%
    rename(
      'hhidpn' = 'rahhidnp',
      'pn' = 'np'
    )
  return(data)  
  
}

walk(
  in_files,
  function(ff) {
    
    # Which dataset is this?
    if (grepl("HRS", ff)) {
      country <- 'hrs'
      n_waves <- 14
    } else if(grepl("MHAS", ff)) {
      country <- 'mhas'
      n_waves <- 4
    } else {
      message("Found an invalid dataset!")
      return()
    }
    
    message("Country: ", country)
    message("N waves: ", n_waves)
    
    # Get the outpath filename
    outfile <- file.path(out_path, paste0(country, '_long.rds'))
    
    # Check if the dataset has been written already, or overwrite == TRUE
    if (file.exists(outfile) & overwrite == FALSE) {
      message("Skipping this since overwrite == FALSE") 
    } else { 
      
      message("Reading in data")
      
      # Read in the data
      in_file_path <- file.path(path_raw, ff)
      data <- read_dta(in_file_path)
      
      # Zap off dta labels
      #data <- data %>%
      #  zap_labels()
      
      # Keep labels, convert everything to character
      data <- data %>%
        mutate(across(everything(), ~ as.character(ifelse(is.na(.), na_tag(.), .))))
      
      # Do any additional country-specific early processing
      message("Doing country specific processing")
      if (country == 'hrs') {
        data <- prep_hrs(data)
      } else if(country == 'mhas') {
        data <- prep_mhas(data)
      }
      
      # Remove unwanted variables
      data <- data %>%
        select(
          any_of(c('hhidpn', 'hhid', 'pn')), # ids
          starts_with("inw"),                # wave indicators
          starts_with("ra"),                 # time-invariant variables
          contains("iwend"),                 # interview date vars
          contains("age"),                   # all age vars
          contains(keepers)                  # all other vars to keep
        )
      
      
      # Now convert the data to long format
      message("Pivoting data to long format")
      varnames <- names(data)
      # To catch errors caused by other double underscores
      varnames <- gsub("__", "_", varnames)
      for (i in n_waves:1) {
        target_r <- paste0("^r", i)
        target_s <- paste0("^s", i)
        target_h <- paste0("^h", i)
        target_hh <- paste0("^hh", i)
        varnames <- gsub(target_r, paste0('w', i, '__r_'), varnames)
        varnames <- gsub(target_s, paste0('w', i, '__s_'), varnames)
        varnames <- gsub(target_h, paste0('w', i, '__h_'), varnames)
        varnames <- gsub(target_hh, paste0('w', i, '__hh_'), varnames)
      }
      names(data) <- varnames
      
      # Save out data as temp to reread within each loop
      temp_path <- file.path(out_path, 'temp.rds')
      saveRDS(data, temp_path)
      rm(data)
      
      plan(multisession, workers = 3, gc = TRUE)
      data_long <- future_map(
        1:n_waves,
        function(ii) {
          message(ii)
          data <- readRDS(temp_path)
          var_condition <- paste0('^w', ii, "__")
          data_long <- data %>%
            lazy_dt() %>%
            select(
              matches('^[a-z][a-z]'),
              matches(var_condition)
            ) %>%
            mutate(across(everything(), ~ as.character(.))) %>%
            pivot_longer(
              cols = matches('^w[0-9]'),
              names_to = c('wave', 'var'),
              names_sep = '__',
              values_to = 'value'
            ) %>%
            pivot_wider(
              id_cols = -any_of(c('var', 'value')),
              names_from = var,
              values_from = value
            ) %>%
            mutate(wave = as.numeric(str_sub(wave, 2, str_length(wave)))) %>%
            as_tibble()
          
          # Remove excess rows
          inw_col <- paste0('inw', ii)
          data_long <- data_long %>% 
            filter(!!as.name(inw_col) == "1")
          
          return(data_long)
          
        }
      ) %>%
        bind_rows()
      
      # Save out the file
      saveRDS(data_long, outfile)
      
    }
    
    
  }
)
