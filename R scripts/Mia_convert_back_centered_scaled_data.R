
new_data <- scale(original_data, center = TRUE, scale = TRUE) #This centers and scales your data.

saveRDS(new_data, file = paste0(getwd(),"/0. Data/CenteredScaledData.RDS")) # save this dataframe you will use this again later to unscale your data. The data frame contains information on means etc that is needed to back transform.

#This is the function to use later to convert centered and scaled data back
convert_back <- function(scale_center_info,coeff,var){
  orig_value <- coeff*attr(scale_center_info,'scaled:scale')[var] + attr(scale_center_info, 'scaled:center')[var]
  return(orig_value)
}