# Parameter Explanation
# 1. formula: The GAM regression formula used. 
# 2. data: The data used for regression training. 
# 3. feature_list: The list of independent variable we are interested in.
# 4. k: Permutation time.

# Output a variable importance row
# In actual setting, we can create a data frame that store the variable importance result.
# The column names of that data frame should be in the feature_list sequence so appending the row won't alter results.
# Example formula and feature_list:
f_covar_gam <- loglandprice_rm10med_cy_19 ~ landcat + land_source + te(normlng_cy, normlat_cy) + s(logarea) + s(landlevel_num) + s(plotratio_ll) + s(plotratio_ul)
feature_list <- c("landcat", "logarea", "land_source", "landlevel_num", "plotratio_ll", "plotratio_ul", "geo_control")

# The reason for using "geo_control" as a feature is because "te(normlng_cy, normlat_cy)" should be combined into one geographical control term.
# When actually dealing with this, I combined the two columns and shuffled them together, then split them into lat, lng after shuffling.

permutation_imp <- function(formula, data, feature_list, k = 30) {

  data <- data.frame(data)
  
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
  
    # In GAM feature_list, we have to distinguish between smoothing terms and parameter terms.
    # Parameter terms effective degree of freedom will appear in pTerms (in a specific order, so we need to be careful)
    # Smoothing terms effective degree of freedom will appear in s.table under the row_index of "s(...)"
    # The reason to care about edf is because when calculating adjusted R-squares edf matters,
    # and if we shuffle one column of observations, we should not take that columns edf into account.
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
    
    # This part
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
  
  return(row)
}
