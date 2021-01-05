# Parameter Explanation
# 1. formula: The GAM regression formula used. 
# 2. data: The data used for regression training. 
# 3. feature_list: The list of independent variable we are interested in.
# 4. k: Permutation time.

# Output a variable importance row
# In actual setting, we can create a data frame that store the variable importance result.
# The column names of that data frame should be in the feature_list sequence so appending the row won't alter results.

permutation_imp <- function(formula, data, feature_list, k = 30) {

  data <- data.frame(data)
  
  # Bunch the lng and lat into geo_control and change accordingly
  model <- gam(formula, data = data)
  
  row <- vector(mode = "list", length = 0)
  # name can be a identification for different data.
  # For example, if we analyze land price data for New York in 2016, then name can = 'NY-2016'
  # row <- list.append(row, name)
  
  # We use adjusted R-sqaure as the original score to account for variable importance.
  original_score <- summary(model)$r.sq
  
  for (i in feature_list) {
    score <- 0
    new_data <- data
  
    # Change the denominator accordingly would probably be better for each different feature?
    # First have to find a way to get the edf for each term, probably need to split 
    # between smooth terms and parameter terms
    if (i == "landcat") {
      adj_df <- summary(model)$pTerms.df[[1]]
      denominator <- var(data$loglandprice_rm10med_cy_19 - mean(data$loglandprice_rm10med_cy_19))*(model$df.residual + adj_df)
    } else if (i == "land_source") {
      adj_df <- summary(model)$pTerms.df[[2]]
      denominator <- var(data$loglandprice_rm10med_cy_19 - mean(data$loglandprice_rm10med_cy_19))*(model$df.residual + adj_df)
    } else {
      df_table <- summary(model)$s.table
      if (i != "geo_control") {
        # format it in a paste0("s(",i,")") way to get the row index. column = "edf"
        adj_df <- df_table[paste0("s(",i,")"), "edf"]
        denominator <- var(data$loglandprice_rm10med_cy_19 - mean(data$loglandprice_rm10med_cy_19))*(model$df.residual + adj_df)
      } else {
        # get the df in a te(norm...) way
        adj_df <- df_table["te(normlng_cy,normlat_cy)", "edf"]
        denominator <- var(data$loglandprice_rm10med_cy_19 - mean(data$loglandprice_rm10med_cy_19))*(model$df.residual + adj_df)
      }
    }
    
    if (i == "geo_control") {
      for (j in 1:k) {
        new_data <- new_data %>%
          mutate(geo_control = paste0(normlat_cy, ',', normlng_cy))
        new_data[, i] <- sample(new_data[, i], replace = FALSE)
        new_data <- new_data %>%
          separate(geo_control, c("normlat_cy", "normlng_cy"), sep = ",") %>%
          mutate(normlat_cy = as.numeric(normlat_cy), normlng_cy = as.numeric(normlng_cy))
        
        prediction <- predict.gam(model, newdata = new_data)
        
        nominator <- var(data$loglandprice_rm10med_cy_19 - prediction)*(summary(model)$n - 1)
        new_score <- 1 - (nominator/denominator)
        score <- score + new_score
      }
      
    } else {
      for (j in 1:k) {
        # Check if this code actually changes the main data.
        # In principle it seems like it won't
        new_data[, i] <- sample(new_data[, i], replace = FALSE)
        
        prediction <- predict.gam(model, newdata = new_data)
        
        nominator <- var(data$loglandprice_rm10med_cy_19 - prediction)*(summary(model)$n - 1)
        new_score <- 1 - (nominator/denominator)
        score <- score + new_score
      }
    }
    
    
    score <- score/k
    imp_var <- original_score - score
    row <- list.append(row, imp_var)
  }
  result[nrow(result)+1, ] <- row
  return(result)
}
