#' Evaluate predictive performance of multilevel/forest models 
#'
#' Performs either split-half or 5-fold cross-validation (both at the cluster level) to estimate test performance for 
#' randomForest, cforest, or lme4 models. In the continuous case, proportion of variation is reported 
#' (i.e., 1 - MSE/var(y)). In the classification case, accuracy is reported (i.e., (TP + TN)/(TP + TN + FP + FN)).
#' 
#' @param the_data the dataset to be used
#' @param formula model formula as a string for either a randomForest, cforest, or lme4 model object (runs all models with
#' defaults)
#' @param stat_method statistical method to be used ("rf" for randomForest, "cf" for cforest, "hlm" for multilevel model)
#' @param valid_method validation method of either "split-half" for split-half validation or 
#' "cv" for 5-fold cross-validation (splitting occurs at the cluster-level)
#' @param cluster string of a variable name that represents the cluster-level
#' @return Prints predictive performance of the method chosen
#' @examples
#' 
#' ## Not run: 
#' 
#' # Random forest with 5-fold cross-validation
#' 
#' validate_ml(the_data = HSB_data,
#'             formula = "MathAch ~ Minority + Sex + SES + Size + Sector +
#'                        PRACAD + DISCLIM + HIMINTY + MEANSES",
#'             stat_method = "rf",
#'             valid_method = "cv",
#'             cluster = "School")
#'             
#' # Random forest with split-half cross-validation
#' 
#' validate_ml(the_data = HSB_data,
#'             formula = "MathAch ~ Minority + Sex + SES + Size + Sector +
#'                        PRACAD + DISCLIM + HIMINTY + MEANSES",
#'             stat_method = "rf",
#'             valid_method = "split-half",
#'             cluster = "School")
#'             
#' # Multi-level model with split-half cross-validation
#' 
#' validate_ml(the_data = HSB_data,
#'             formula = "MathAch ~ Minority + Sex + SES + Size + Sector +
#'                        PRACAD + DISCLIM + HIMINTY + MEANSES + (1 | School)",
#'             stat_method = "hlm",
#'             valid_method = "split-half",
#'             cluster = "School")
#'         
#' ## End(Not run)
#' 
#' @references Martin, D. P. (2015). Efficiently exploring multilevel data with recursive partitioning (Unpublished doctoral
#' dissertation). University of Virginia, Charlottesville, VA. 
#' @import lme4 party
#' @importFrom randomForest randomForest
#' @export

validate_ml <- function(the_data, formula, stat_method, valid_method, cluster){
  
  if(!(stat_method %in% c("rf", "cf", "hlm"))) stop("Please choose either rf, cf, or hlm for stat_method.")
  if(!(valid_method %in% c("split-half", "cv"))) stop("Please choose either split-half or cv for valid_method.")
  
  outcome <- gsub(" ", "", gsub("~.*", "", formula))
  
  if(!(outcome %in% names(my_data))) stop("Please re-check your formula, your outcome does not match a variable name.")
  
  if(valid_method == "split-half"){
    
    train_ids <- sample(x = unique(the_data[, cluster]), size = floor(length(unique(the_data[, cluster]))/2), replace = FALSE)
    
    train <- the_data[the_data[, cluster] %in% train_ids, ]
    test <- the_data[!(the_data[, cluster] %in% train_ids), ]
    
    if(stat_method == "rf"){
      
      rf_train <- randomForest(as.formula(formula), data = train)
      
      test_error <- ifelse(is.factor(the_data[, outcome]),
                           sum(test[, outcome] == predict(rf_train, newdata = test))/nrow(test),
                           1 - sum((predict(rf_train, newdata = test) - test[, outcome])^2)/
                             sum((mean(test[, outcome]) - test[, outcome])^2))
      
    } else if(stat_method == "cf"){
      
      cf_train <- cforest(as.formula(formula), data = train)
      
      test_error <- ifelse(is.factor(the_data[, outcome]),
                           sum(test[, outcome] == predict(cf_train, newdata = test))/nrow(test),
                           1 - sum((predict(cf_train, newdata = test) - test[, outcome])^2)/
                             sum((mean(test[, outcome]) - test[, outcome])^2))
      
    } else if(stat_method == "hlm"){
      
      if(length(levels(the_data[, outcome])) > 2) stop("HLM models limited to two categories in the outcome")
              
      if(is.factor(the_data[, outcome])){
        
        naive_train <- glmer(as.formula(formula), family = "binomial", data = train)
        
        test_error <- sum(predict(naive_train, newdata = test, re.form = NA, type = "response") > 0.5)/nrow(the_data)
        
      } else{
        
        naive_train <- lmer(as.formula(formula), data = train)
        
        test_error <- 1 - sum((predict(naive_train, newdata = test, re.form = NA) - test[, outcome])^2)/
          sum((mean(test[, outcome]) - test[, outcome])^2)
        
      }
    
    }
    
    if(is.factor(the_data[, outcome])){
      
      print(paste("Estimated classification accuracy for", outcome, "using a", stat_method,
                  "model with split-half validation =", round(test_error, 3)))
      
    } else{
      
      print(paste("Estimated proportion of variation explained in", outcome, "using a", stat_method,
                  "model with split-half validation =", round(test_error, 3)))
      
    }
     
  } else if(valid_method == "cv"){
    
    the_data$fold <- NA
    
    shuffle_group <- sample(x = unique(the_data[, cluster]), size = length(unique(the_data[, cluster])), replace = FALSE)
    fold_label <- split(shuffle_group, cut(seq_along(shuffle_group), 5, labels = FALSE))
    
    for(list_element in 1:length(fold_label)){
      
      the_data[the_data[, cluster] %in% fold_label[[list_element]], ]$fold <- list_element
      
    }

    test_error <- c(NA)
    
    pb <- txtProgressBar(min = 0, max = 5, initial = 0) 
    
    for(i in 1:5){
      
      train <- the_data[the_data$fold != i, ]
      test <- the_data[the_data$fold == i, ]
      
      if(stat_method == "rf"){
        
        rf_train <- randomForest(as.formula(formula), data = train)
        
        test_error[i] <- ifelse(is.factor(test[, outcome]),
                             sum(test[, outcome] == predict(rf_train, newdata = test))/nrow(test),
                             1 - sum((predict(rf_train, newdata = test) - test[, outcome])^2)/
                               sum((mean(test[, outcome]) - test[, outcome])^2))
        
      } else if(stat_method == "cf"){
        
        cf_train <- cforest(as.formula(formula), data = train)
        
        test_error[i] <- ifelse(is.factor(test[, outcome]),
                             sum(test[, outcome] == predict(cf_train, newdata = test))/nrow(test),
                             1 - sum((predict(cf_train, newdata = test) - test[, outcome])^2)/
                               sum((mean(test[, outcome]) - test[, outcome])^2))
        
      } else if(stat_method == "hlm"){
        
        if(length(levels(the_data[, outcome])) > 2) stop("HLM models limited to two categories in the outcome")
        
        if(is.factor(the_data[, outcome])){
          
          naive_train <- glmer(as.formula(formula), family = "binomial", data = train)
          
          test_error[i] <- sum(predict(naive_train, newdata = test, re.form = NA, type = "response") > 0.5)/nrow(test)
          
        } else{
          
          naive_train <- lmer(as.formula(formula), data = train)
          
          test_error[i] <- 1 - sum((predict(naive_train, newdata = test, re.form = NA) - test[, outcome])^2)/
            sum((mean(test[, outcome]) - test[, outcome])^2)
          
        }
        
      }
      
      setTxtProgressBar(pb, i)
      
    }
    
    if(is.factor(the_data[, outcome])){
      
      print(paste("Estimated classification accuracy for", outcome, "using a", stat_method,
                  "model with 5-fold cross-validation =", round(mean(test_error), 3)))
      
    } else{
      
      print(paste("Estimated proportion of variation explained in", outcome, "using a", stat_method,
                  "model with 5-fold cross-validation =", round(mean(test_error), 3)))
      
    }
    
  }
  
}