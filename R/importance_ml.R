#' Plot variable importance for multilevel/forest models
#'
#' Plots and compares variable importances for an arbitrary number of statistical models (limited to randomForest, cforest, and lme4
#' model objects). Calculates variable importance for forest models using the randomForest and cforest built-in procedures using
#' permutation tests. Variable importance for lme4 models are naively defined as the p-value of the respective parameter, estimated
#' via a Satterthwaite approximation from the lmerTest package. Be sure to build your lme4 model with the lmerTest package loaded
#' or else an error will be displayed. Variable importances are standardized to allow for easier comparisons.
#' Plots are ordered such that the most important variable (averaged across all models) is the furthest left on the x-axis. 
#' 
#' @param mod_list list of the models to be compared and plotted
#' @return Plots variable importance for the models given
#' @examples
#' 
#' ## Not run: 
#' 
#' # Run three models (rf, cf, and lmer)
#' 
#' rf_mod <- randomForest(MathAch ~ Minority + Sex + SES + Size + 
#'                        Sector + PRACAD + DISCLIM + HIMINTY + MEANSES,
#'                        data = HSB_data)
#' 
#' cf_mod <- cforest(MathAch ~ Minority + Sex + SES + Size + 
#'                   Sector + PRACAD + DISCLIM + HIMINTY + MEANSES,
#'                   data = HSB_data)
#' 
#' lmer_mod <- lmer(MathAch ~ Minority + Sex + SES + Size + 
#'                  Sector + PRACAD + DISCLIM + HIMINTY + 
#'                  MEANSES + (1 | School),
#'                  data = HSB_data)
#'                  
#' # Plot variable importance comparison (use explicit names in the list for labelling purposes on the plot if desired)
#' 
#' importance_ml(list(rf = rf_mod, cf = cf_mod, lmer = lmer_mod))
#'                  
#' ## End(Not run)
#' 
#' @references Martin, D. P. (2015). Efficiently exploring multilevel data with recursive partitioning (Unpublished doctoral
#' dissertation). University of Virginia, Charlottesville, VA. 
#' @importFrom reshape melt.data.frame
#' @export

importance_ml <- function(mod_list){
  
  if(length(mod_list) > 1){
    
    list_obj <- sapply(sapply(mod_list, class), "[[", 1)
    
  } else{
    
    list_obj <- sapply(mod_list, class)[1]
    
  }
  
  if(sum(grepl("randomForest|RandomForest|merMod", list_obj)) != length(mod_list)){
    
    stop("All objects must be either a randomForest, cforest, or lme4 model object.")
    
  }
  
  imp <- list(NA)
  
  pb <- txtProgressBar(min = 0, max = length(list_obj), initial = 0) 
  
  for(the_obj in 1:length(list_obj)){
    
    if(grepl("randomForest", list_obj[the_obj])){
      
      imp[[the_obj]] <- mod_list[[the_obj]]$importance
      
    } else if(grepl("RandomForest", list_obj[the_obj])){
      
      imp[[the_obj]] <- varimp(mod_list[[the_obj]])
      
    } else if(grepl("merMod", list_obj[the_obj])){
      
      anova_mod <- suppressWarnings(anova(mod_list[[the_obj]]))
      
      if(!("Pr(>F)" %in% names(anova_mod))) stop("Please build your lme4 model with the lmerTest package loaded.")
      
      imp[[the_obj]] <- -1 * as.data.frame(anova_mod)["Pr(>F)"]

    } else{
      
      stop("All objects must be either a randomForest, cforest, or lme4 model object.")
      
    }
    
    setTxtProgressBar(pb, the_obj)
    
  }
  
  var_imp <- do.call(cbind, lapply(imp, data.frame))
  
  if(is.null(names(mod_list))){
    
    names(var_imp) <- paste0("Model_", 1:length(mod_list))
    
  } else{
    
    names(var_imp) <- names(mod_list)
    
  }
  
  imp_df <- as.data.frame(apply(var_imp, 2, scale))
  imp_df$var_name <- row.names(var_imp)
  
  if(length(mod_list) == 1){
    
    imp_df$imp_mean <- imp_df[, 1]
    
  } else{
    
    imp_df$imp_mean <- rowMeans(imp_df[, 1:length(mod_list)])
    
  }
  
  imp_sorted <- imp_df[order(imp_df$imp_mean, decreasing = TRUE), ]

  var_order <- imp_sorted$var_name
  imp_long <- melt.data.frame(imp_sorted[, 1:(length(mod_list) + 1)], id.vars = "var_name")
  imp_long$var_name <- factor(imp_long$var_name, levels = imp_sorted$var_name)
  
  ggplot(aes(x = var_name, y = value, group = variable), data = imp_long) +
    geom_line(aes(color = variable), size = 1.5) +
    geom_point(size = 3, shape = 21, fill = "white") +
    scale_colour_discrete(name = "Model") +
    labs(x = "\nVariable Name", y = "Standardized Variable Importance\n(higher = more important)\n") +
    ggtitle("Variable Importance Plot") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(face = "bold"))
  
}
