###
# Merge cols
###
merge_columns <- function(df, col1, col2, new_col_name) {
  # Check if the columns have the same values
  if (all(df[[col1]] == df[[col2]], na.rm = TRUE)) {
    # If they are the same, create the new column
    df[[new_col_name]] <- df[[col1]]
  } else {
    # If they differ, create a new column and indicate which rows differ
    df[[new_col_name]] <- ifelse(is.na(df[[col1]]) & !is.na(df[[col2]]), df[[col2]], 
                                 ifelse(!is.na(df[[col1]]) & is.na(df[[col2]]), df[[col1]],
                                        ifelse(df[[col1]] == df[[col2]], df[[col1]], NA)))
    warning("Columns ", col1, " and ", col2, " have different values in some rows. Check the resulting column '", new_col_name, "' for NA values indicating mismatches.")
  }
  
  return(df)
}

###
# Check units
###

get_response_description <- function(data_try, response_variable) {
  # Get the description
  description <- data_try$UnitName[
    data_try$DataName == response_variable
  ]
  
  # Check if df description was found
  if (length(description) == 0) {
    warning(paste("No description found for DateName:", response_variable))
    return(NULL)
  } else if (length(description) > 1) {
    warning(paste("Multiple descriptions found for DataName:", response_variable))
    return(table(description))
  } else {
    return(table(description))
  }
}

###
# Check the variables which both have data
###

check_var_combinations <- function(var_vector, df) {
  # Initialize a list to store results
  results <- list()
  
  # Check if all variables exist in the dataframe
  missing_vars <- var_vector[!(var_vector %in% names(df))]
  if (length(missing_vars) > 0) {
    warning(paste("The following variables are missing from the dataset:", 
                  paste(missing_vars, collapse = ", ")))
    var_vector <- var_vector[var_vector %in% names(df)]
  }
  
  # If less than 2 variables remain, stop the function
  if (length(var_vector) < 2) {
    stop("Not enough valid variables to check combinations.")
  }
  
  # Check all possible combinations
  for (i in 1:(length(var_vector) - 1)) {
    for (j in (i + 1):length(var_vector)) {
      var1 <- var_vector[i]
      var2 <- var_vector[j]
      
      # Check if there are non-NA values in both columns
      valid_data <- sum(!is.na(df[[var1]]) & !is.na(df[[var2]]))
      
      if (valid_data > 0) {
        combination <- paste(var1, var2, sep = " & ")
        results[[combination]] <- valid_data
        message(paste("Found", valid_data, "valid pairs of data for", combination))
      }
    }
  }
  
  if (length(results) == 0) {
    message("No valid pairs found for any variable combinations.")
    return(NULL)
  } else {
    return(results)
  }
}

###
# merge_multiple_columns
###

merge_multiple_columns <- function(df, col_pairs, new_col_names) {
  for (i in seq_along(col_pairs)) {
    col1 <- col_pairs[[i]][1]
    col2 <- col_pairs[[i]][2]
    new_col_name <- new_col_names[i]
    
    # Check if the columns have the same values
    if (all(df[[col1]] == df[[col2]], na.rm = TRUE)) {
      # If they are the same, create the new column
      df[[new_col_name]] <- df[[col1]]
    } else {
      # If they differ, create a new column and indicate which rows differ
      df[[new_col_name]] <- ifelse(is.na(df[[col1]]) & !is.na(df[[col2]]), df[[col2]], 
                                   ifelse(!is.na(df[[col1]]) & is.na(df[[col2]]), df[[col1]],
                                          ifelse(df[[col1]] == df[[col2]], df[[col1]], NA)))
      warning("Columns ", col1, " and ", col2, " have different values in some rows. Check the resulting column '", new_col_name, "' for NA values indicating mismatches.")
    }
  }
  
  return(df)
}

###
# Group and merge columns 
###

group_and_merge_columns <- function(df) {
  ###
  # Choose the most important columns
  ###
  # Define the vector of column names
  
  
  ###
  # Make these columns useful
  ###
  
  # df_key_columns$leaf_mass 
  
  
  
  return(df_out)
  
}