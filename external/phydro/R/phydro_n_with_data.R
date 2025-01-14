get_response_description <- function(a, response_variable) {
  # Check if the required data frame exists in a
  if (!("mesi_response_variable_abbreviations" %in% names(a))) {
    stop("The 'mesi_response_variable_abbreviations' data frame is not present in the provided object.")
  }
  
  # Get the description
  description <- a$mesi_response_variable_abbreviations$description[
    a$mesi_response_variable_abbreviations$response == response_variable
  ]
  
  # Check if a description was found
  if (length(description) == 0) {
    warning(paste("No description found for response variable:", response_variable))
    return(NULL)
  } else if (length(description) > 1) {
    warning(paste("Multiple descriptions found for response variable:", response_variable))
    return(description)
  } else {
    return(description)
  }
}

mesi_import <- function() {
  ### Libraries
  library(dbplyr)
  library(dplyr)
  library(tidyr)
  
  ### MESI Import
  mesi_path = "/home/josimms/Documents/Austria/MESI_data/data"
  
  a <- lapply(list.files(mesi_path, pattern = "mesi", full.names = TRUE), data.table::fread)
  names(a) <- gsub(".csv", "", list.files(mesi_path, pattern = "mesi"))
  
  a$mesi_main <- a$mesi_main %>%
    group_by(db, citation, site, study, lat, lon, elevation, ecosystem_type, vegetation_type) %>%
    mutate(index = cur_group_id()) %>%
    ungroup()
  
  # The units aren't standardised for leaf n mass, so change this first!
  unique(a$mesi_main$x_units[a$mesi_main$response == "leaf_n_mass"])
  
  convert_nitrogen_to_g_g <- function(value, unit) {
    # Molar mass of nitrogen in g/mol
    molar_mass_nitrogen <- 14.01
    
    # Define a list of conversion factors for nitrogen per leaf mass
    conversion_factors <- list(
      "g_100g" = 1 / 100,            # grams per 100 grams to grams per gram
      "g_kg" = 1 / 1000,             # grams per kilogram to grams per gram
      "ug_mg" = 1 / 1000,            # micrograms per milligram to grams per gram
      "mg_g" = 1 / 1000,             # milligrams per gram to grams per gram
      "mg_kg" = 1 / 1e6,             # milligrams per kilogram to grams per gram
      "mgn_g" = 1 / 1000,            # milligrams of nitrogen per gram to grams per gram
      "g_g" = 1,                     # grams per gram (no conversion needed)
      "mmol_g" = molar_mass_nitrogen / 1000,  # millimoles of nitrogen per gram to grams per gram
      "umol_g" = molar_mass_nitrogen / 1e6    # micromoles of nitrogen per gram to grams per gram
    )
    
    # Check if the unit is in the list of known conversions
    if (unit %in% names(conversion_factors)) {
      factor <- conversion_factors[[unit]]
      return(value * factor)
    } else {
      # There are other units here, so don't need to convert the ones that are not in the list!
      return(value)
    }
  }
  
  # Add a new column 'x_corrected' based on the 'x_c' values and corresponding units
  a$mesi_main$x_corrected <- mapply(function(value, unit) {
    tryCatch({
      # Apply the conversion function for nitrogen per leaf mass
      convert_nitrogen_to_g_g(value, unit)
    }, error = function(e) {
      NA  # If there's an error (e.g., unknown unit), set value as NA
    })
  }, a$mesi_main$x_c, a$mesi_main$x_units)
  
  # Transform the data so the response variables are columns
  df <- a$mesi_main[a$mesi_main$ecosystem_type == "forest",] %>%
    pivot_wider(names_from = response, values_from = x_corrected, id_cols = index, values_fn = function(x) {mean(x, na.rm = T)})
  
  ### I would like to extract
  # leaf_nitrogen_concentration_by_mass (leaf_n_mass)
  # (jmax)
  # (vcmax)

  plot(df$leaf_n_mass, df$jmax)
  plot(df$leaf_n_mass, df$vcmax)
  plot(df$vcmax, df$jmax)
  
  plot(df$jmax)
  
  return(df)
}

try_data <- function() {
  library(dbplyr)
  library(ggplot2)
  
  ###
  # Read in the data
  ###
  direct = "/home/josimms/Documents/Austria/TRY_data/"
  data_try_jmax <- data.table::fread(paste0(direct, "35495.txt"))
  data_try_vcmax <- data.table::fread(paste0(direct, "35516.txt"))
  data_try_leaf_n <- data.table::fread(paste0(direct, "35525.txt"))
  
  # Combine the data searches
  data_try <- rbind(data_try_jmax, data_try_vcmax)
  data_try <- rbind(data_try, data_try_leaf_n)
  
  ###
  # Retrieve string data
  #
  # Some of the string data that is numbers is not included in the Standard Values for some reason. TODO; why?
  # Created a column that takes the missing data and units
  ###
  
  data_try$ValuesFromString <-  as.numeric(iconv(data_try$OrigValueStr, to = "ASCII//TRANSLIT", sub = "NA"))
  
  data_try$ValuesFromString[data_try_jmax$TraitName == "Photosynthesis electron transport capacity (Jmax) per leaf nitrogen (N) content (Farquhar model)"]
  # Not always the same as the standard values so need to be careful wtih this!
  
  ###
  # Subset for the IIASA programme (boreal forest values)
  #
  # NOTE: change this later!
  ###
  
  ## Boreal forest data
  data_try_boreal <- data_try[data_try$DataName == 'Vegetation type / Biome' & data_try$OrigValueStr %in% c('Boreal'),]
  data_try_scots_pine <- data_try_boreal[data_try_boreal$AccSpeciesName == "Pinus sylvestris",]
  
  boreal_pivot_std <- data_try_boreal %>%
    dplyr::mutate(DataName = stringi::stri_trans_general(DataName, "Latin-ASCII")) %>%
    tidyr::pivot_wider(names_from = DataName, 
                       values_from = StdValue, 
                       id_cols = ObservationID, 
                       values_fn = ~mean(.x, na.rn = T))
  boreal_pivot_sting <- data_try_boreal %>%
    dplyr::mutate(DataName = stringi::stri_trans_general(DataName, "Latin-ASCII")) %>%
    tidyr::pivot_wider(names_from = DataName, 
                       values_from = ValuesFromString, 
                       id_cols = ObservationID, 
                       values_fn = ~mean(.x, na.rn = T))
  scots_pine_pivot_std <- data_try %>%
    dplyr::mutate(DataName = stringi::stri_trans_general(DataName, "Latin-ASCII")) %>%
    tidyr::pivot_wider(names_from = DataName, 
                       values_from = StdValue, 
                       id_cols = ObservationID, 
                       values_fn = ~mean(.x, na.rn = T))
  scots_pine_pivot_string <- data_try %>%
    dplyr::mutate(DataName = stringi::stri_trans_general(DataName, "Latin-ASCII")) %>%
    tidyr::pivot_wider(names_from = DataName, 
                       values_from = ValuesFromString, 
                       id_cols = ObservationID, 
                       values_fn = ~mean(.x, na.rn = T))
  
  check_var_combinations(names(scots_pine_pivot_std)[-c(1:3)], scots_pine_pivot_std)
  
  ###
  # Similar variable names should be aggregated!
  ###
  scots_pine_pivot_std_var_aggregated <- merge_multiple_columns(scots_pine_pivot_std,
                                                                c("Leaf nitrogen content per total area", "Leaf nitrogen content per area (Narea)"),
                                                                new_col_names = )
  scots_pine_pivot_string_var_aggregated
  
  
  ###
  # Plot the values against leaf N content
  ###
  
  # Assuming your dataset includes a column for Vcmax or Vmax similar to Jmax
  combined_data_vcmax <- scots_pine_pivot_std %>%
    pivot_longer(cols = c(`Leaf nitrogen content per dry mass (Nmass)`,
                          `Leaf nitrogen content per area (Narea)`),
                 names_to = "Nitrogen_Content",
                 values_to = "Value")
  
  # Add a label column to identify which plot it corresponds to
  combined_data_vcmax$Nitrogen_Content_Label <- ifelse(combined_data_vcmax$Nitrogen_Content == "Leaf nitrogen content per dry mass (Nmass)",
                                                       "Nmass", "Narea")
  
  # Create a new variable to distinguish between Jmax and Vcmax plots
  combined_data_vcmax$Variable <- "Jmax"
  combined_data_vcmax_vcmax <- combined_data_vcmax
  combined_data_vcmax_vcmax$Variable <- "Vcmax"
  combined_data_vcmax_vcmax$Value <- scots_pine_pivot_std$`Photosynthetic carboxylation capacity (Vcmax, Vmax) per leaf area at leaf temperature`
  
  # Combine the datasets
  final_combined_data <- rbind(combined_data_vcmax, combined_data_vcmax_vcmax)
  
  # Now create the plot
  ggplot(final_combined_data, aes(x = Value, 
                                  y = ifelse(Variable == "Jmax", 
                                             `Jmax per leaf area at ambient or leaf temperature`, 
                                             `Photosynthetic carboxylation capacity (Vcmax, Vmax) per leaf area at leaf temperature`))) +
    geom_point() +
    facet_grid(Variable ~ Nitrogen_Content_Label, scales = "free_x") +
    labs(x = "Leaf Nitrogen Content", 
         y = "Photosynthetic Parameter per Leaf Area") +
    coord_cartesian(ylim = c(min(scots_pine_pivot_std$`Photosynthetic electron transport capacity per leaf area at 25C (Jmax25area)`), 
                             max(scots_pine_pivot_std$`Photosynthetic electron transport capacity per leaf area at 25C (Jmax25area)`))) + 
    geom_line(data = dat, aes(y = jmax, x = n_leaf, linetype = "Nitrogen Version, Allocation = 1"), color = "red") +
    geom_line(data = dat_2, aes(y = jmax, x = n_leaf, linetype = "Nitrogen Version, Allocation = 2"), color = "blue") +
    geom_line(data = dat_1.5, aes(y = jmax, x = n_leaf, linetype = "Nitrogen Version, Allocation = 1.5"), color = "green") +
    geom_line(data = dat_0.75, aes(y = jmax, x = n_leaf, linetype = "Nitrogen Version, Allocation = 0.75"), color = "orange") +
    scale_linetype_manual(name = "Nitrogen Allocation", 
                          values = c("Nitrogen Version, Allocation = 0.75" = "solid",
                                     "Nitrogen Version, Allocation = 1" = "solid",
                                     "Nitrogen Version, Allocation = 1.5" = "solid",
                                     "Nitrogen Version, Allocation = 2" = "solid")) +
    theme_minimal() +
    theme(strip.background = element_rect(color = "white", size = 1),
          legend.title = element_blank(),
          legend.position = "right")
  
  
  ###
  # Plot the root biomass against the photosynthesis
  ###
  
  ###
  # Plot the photosynthesis against leaf N
  ###
  
  ###
  # Leaf nitrogen content per dry mass (Nmass) & Leaf nitrogen content per area (Narea)
  ###
  
  ###
  # Leaf respiration per dry mass and area
  ###
  
}

n_content_data <- function() {
  direct = "/home/josimms/Documents/Austria/extra_data/"
  
  # https://zenodo.org/records/11066705
  data <- readxl::read_excel(paste0(direct, "Muhonen et al article metadata file_ UEF Peltola Heli 25042024.xlsx"), sheet = 1)
  data_scots_pine = data[data$`Main tree species` == "Scots pine",]
  
  par(mfrow = c(1, 2))
  plot(data_scots_pine$`Fertilization dose (kg N/ha)`, data_scots_pine$`N needles (%) in 2021`, xlab = "Fertilization Dose", ylab = "N Needles %")
  plot(data_scots_pine$`N humus (%) in 2022`, data_scots_pine$`N needles (%) in 2021`, pch = "x", col = "blue", xlab = "N humus (%) in 2022", ylab = "N needles (%) in 2021")
  
  ### TODO: Missing / would be useful
  # Nitrogen per area to nitrogen per mass
  
  data_collation <- data.table::data.table(
    site = c("Hyytiälä",
             "France", 
             "France",
             "Mekrijärvi Research Station ",
             "Mekrijärvi Research Station ",
             "Mekrijärvi Research Station ",
             "Mekrijärvi Research Station "),
    reference = c("Korhonen, 2007", 
                  "Warren, 2002",
                  "Warren, 2002",
                  "Kellomäki, 1999",
                  "Kellomäki, 1999",
                  "Kellomäki, 1999",
                  "Kellomäki, 1999"),
    n_content = c(1.21, NA, NA, NA, NA, NA, NA),
    a_jmax = c(NA, NA, NA, 32.45, 23.24, 21.46, 30.28),
    a_vcmax = c(NA, 11.319, 4.74, 15.36, 14.91, 12.06, 12.74),
    details = c(NA,
                "Current year, Jmax Vcmax closely and linearly related",
                "1 year old, Jmax Vcmax closely and linearly related",
                "Elevated C, nitrogen leaf per area",
                "Elevated C and T, nitrogen leaf per area",
                "Elevated T, nitrogen leaf per area",
                "Control, nitrogen leaf per area")
  )
  
}


dFdIb_plot <- function() {
  dFdIb <- function(Ib, alpha, a_jmax, N) {
    return((alpha * a_jmax * N) / Ib^2)
  }
  
  Ib_range = seq(0.75, 2, length.out = 50)
  plot(Ib_range, dFdIb(Ib_range, 0.2, 50, 2), col = "red", 
       type = "l", xlab = "Range of Ib values", ylab = "Marginal cost on F of Ib",
       ylim = c(0, max(dFdIb(Ib_range, 0.2, 50, 2))))
  lines(Ib_range, dFdIb(Ib_range, 0.2, 50, 1), col = "orange")
  lines(Ib_range, dFdIb(Ib_range, 0.2, 50, 0.5), col = "yellow")
  legend("topright", c("Leaf Nitrogen = 2 g m-2", "Leaf Nitrogen = 1 g m-2", "Leaf Nitrogen = 0.5 g m-2"), 
         col = c("red", "orange", "yellow"), bty = "n", lty = 1)
}


