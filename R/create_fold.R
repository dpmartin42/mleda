#' Create folds for model validation
#' 
#' Appends a new variable, fold, for the purposes of being used with validate_ml. This must be done as an initial step in order
#' to allow for comparisons between models, so repeated calls to validate_ml with different statistical models will be using the same
#' split structure. Folds are created at the cluster-level.
#'
#' @param the_data the data
#' @param cluster a string of the variable name representing the cluster-level
#' @param num_folds the number of folds to be used (k = 2 = split-half validation, k > 2 = k-fold cross-validation)
#' @return the_data with fold as a new variable
#' @examples
#' 
#' data_fold <- create_fold(HSB_data, "School", 2)
#' 
#' @references Martin, D. P. (2015). Efficiently exploring multilevel data with recursive partitioning (Unpublished doctoral
#' dissertation). University of Virginia, Charlottesville, VA. 
#' @export

#############################################
# create_fold
# Function to return a data frame with k-folds
# at the cluster level
# @param the_data the data set
# @param cluster_var the clustering variable in string format
# @param num_folds the number of folds to have at the cluster level

create_fold <- function(the_data, cluster, num_folds){
  
  the_data$fold <- NA
  
  shuffle_group <- sample(x = unique(the_data[, cluster]), size = length(unique(the_data[, cluster])), replace = FALSE)
  fold_label <- split(shuffle_group, cut(seq_along(shuffle_group), num_folds, labels = FALSE))
  
  for(list_element in 1:length(fold_label)){
    
    the_data[the_data[, cluster] %in% fold_label[[list_element]], ]$fold <- list_element
    
  }
  
  return(the_data)
  
}

