#' plots stuff
#' @param the_data - the dataset to be used
#' @param the_mod - the model (limited to randomForest objects right now)
#' @param var_name - vector of strings of variable names to plot
#' @param var_level - vector of numbers indicating levels (1 or 2 only)
#' @param cluster - string of variable name that represents cluster
#' @param type - plot raw or predicted values
#' @param interact - boolean indicating if an interaction should be plotted (limited to two variables)
#' @param reference - reference category in the case of classification. Default is most endorsed category
#' @return plot of main effects/interactions
#' @examples
#' # to do
#' @references [1] How to add numbers
#' @import dplyr ggplot2 grid gridExtra
#' @export

plot_ml <- function(the_data, the_mod, var_name, var_level, cluster, type = "raw", interact = FALSE, reference){
  
  outcome <- detect_type(the_mod, the_data)[1]
  mod_type <- detect_type(the_mod, the_data)[2]
  
  if(length(var_name) != length(var_level)) stop("Please make sure var_name and var_level are of matching lengths.")
  
  if(interact == TRUE & length(var_name) > 2) stop("Interactions are limited to two variables.")
  
  if(interact == TRUE & length(var_name) == 1) stop("Interactions require more than one variable.")
  
  if(sum(!(var_level %in% c(1, 2))) > 0) stop("Please use only 1 or 2 as numeric values to indicate levels.")
  
  if(mod_type == "classification" & missing(reference)) reference <- levels(the_data[, outcome])[1]
  
  if(!missing(reference)){
    
    if(mod_type != "classification") stop("Only use a reference group for a classification forest")
    
    if(!(reference %in% levels(the_data[, outcome]))) stop(paste(reference, "not a correct level."))
    
  }   
  
  if(sum(!(var_name %in% names(the_data))) > 0){
    
    stop(paste0("Variables (", paste(var_name[!(var_name %in% names(the_data))], collapse = ", "), ") not in the dataset."))
    
  }
  
  plot_list <- list(NA)

  for(the_plot in 1:length(var_name)){
    
    if(is.factor(the_data[, var_name[the_plot]]) & var_level[the_plot] == 1){
      
      p <- L1_cat(the_data, the_mod, var_name[the_plot], type, reference, outcome, mod_type)
      
    } else if(is.factor(the_data[, var_name[the_plot]]) & var_level[the_plot] == 2){
      
      p <- L2_cat(the_data, the_mod, var_name[the_plot], type, cluster, reference, outcome, mod_type)
      
    } else if(!is.factor(the_data[, var_name[the_plot]]) & var_level[the_plot] == 1){
      
      p <- L1_cont(the_data, the_mod, var_name[the_plot], type, reference, outcome, mod_type)
      
    } else{
      
      p <- L2_cont(the_data, the_mod, var_name[the_plot], type, cluster, reference, outcome, mod_type)
      
    }
    
    plot_list[[the_plot]] <- p
    
  }
  
  if(type == "predicted"){
    
    new_data <- rbind.data.frame(the_data, sapply(the_data, col_calc)) %>%
      filter(row_number() == n()) %>%
      convert_types(unlist(sapply(the_data, class))) 
    
    the_message <- cbind(data.frame(GRID = ""), s_select(new_data, paste0("-", cluster)))
    names(the_message)[1] <- "GRID:     "
    
    print(the_message)
    
  }
  
  if(interact == TRUE){
    
    if(sum(var_level == 1) %in% c(0, 1, 2) & sum(sapply(the_data[, var_name], is.factor)) == 2){
      
      plot_list[[length(plot_list) + 1]] <- cat_cat(the_data, the_mod, var_name, type, reference, outcome, mod_type)
      
    } else if(sum(var_level == 1) %in% c(0, 1, 2) & sum(sapply(the_data[, var_name], is.factor)) == 1){
      
      plot_list[[length(plot_list) + 1]] <- cat_cont(the_data, the_mod, var_name, type, reference, outcome, mod_type)
      
    } else if(sum(var_level == 1) %in% c(0, 1, 2) & sum(sapply(the_data[, var_name], is.factor)) == 0){
      
      plot_list[[length(plot_list) + 1]] <- cont_cont(the_data, the_mod, var_name, type, reference, outcome, mod_type)
      
    } else{
      
      stop("Error in input.")
      
    }
    
  }
  
  do.call(grid.arrange, c(plot_list, nrow = floor(sqrt(length(plot_list)))))
  
}