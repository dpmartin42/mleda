# Helper functions for mleda plots

#############################################
# convert_types
# Converts variable types in one data frame according to
# another vector
# @param obj the data frame that needs to be altered
# @param types vector of variable types (as strings) to transform to

convert_types <- function(obj, types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],
                  character = as.character, 
                  numeric = as.numeric, 
                  integer = as.integer,
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}

#############################################
# col_calc
# Converts median values or most popular level for 
# each variable
# @param x the variable (to be used with sapply)

col_calc <- function(x){
  
  if(class(x) %in% c("numeric", "integer")) to_return <- median(x)
  else if(class(x) %in% c("factor", "ordered")) to_return <- levels(x)[which.max(table(x))]
  else if(class(x) == "character") to_return <- "character"
  else to_return <- NA
  
  to_return
  
}

#############################################
# detect_type
# Detect the type of the outcome using the model
# information
# @param the_mod the model (randomForest, cforest, or (g)lmerMod)

detect_type <- function(the_mod, the_data){
  
  if(sum(grepl("randomForest", class(the_mod))) > 0){
    
    outcome <- paste(the_mod$terms[[2]])
    the_type <- rf_mod$type
    
  } else if(sum(grepl("RandomForest", class(the_mod))) > 0) {

    outcome <- gsub("~", "", the_mod@data@formula$response)[2]
    the_type <- ifelse(is.factor(the_data[, outcome]),
                       "classification",
                       "regression")
    
  } else if(sum(grepl("lmerMod", class(the_mod))) > 0){

    outcome <- gsub("()", "", formula(the_mod)[2])
    the_type <- ifelse(class(the_mod) == "glmerMod",
                       "classification",
                       "regression")
    
  } else stop("Error. Model must be of class: randomForest, RandomForest, or (g)lmerMod.")
  
  return(c(outcome, the_type))
  
}

#############################################
# construct_grid
# Constructs a grid of predicted values
# @param the_data the dataset to be used
# @param the_mod the model
# @param var_name the variable name
# @param reference reference group in the case of classification

# Constructs grid of predicted values

construct_grid <- function(the_data, the_mod, var_name, reference, outcome, mod_type){
  
  if(mod_type == "classification"){
    
    the_data[, outcome] <- relevel(the_data[, outcome], reference)
    
  }  
  
  new_data <- rbind(the_data, sapply(the_data, col_calc)) %>%
    filter(row_number() == n()) %>%
    convert_types(unlist(sapply(the_data, class)))
  
  the_grid <- list(NA)
  
  if(sum(sapply(the_data[, var_name], is.factor)) > 0){
    
    for(the_var in 1:length(var_name)){
      
      if(is.factor(the_data[, var_name[the_var]])){
        
        the_grid[[the_var]] <- unique(the_data[, var_name[the_var]])
        
      } else{
        
        the_grid[[the_var]] <- seq(from = min(the_data[, var_name[the_var]]),
                                   to = max(the_data[, var_name[the_var]]),
                                   length.out = 20)
        
      } 
      
    }
    
    grid_total <- do.call(expand.grid, the_grid)
    names(grid_total) <- var_name
    
  } else{
    
    the_grid[[1]] <- seq(from = min(the_data[, var_name[1]]),
                         to = max(the_data[, var_name[1]]),
                         length.out = 20)
    
    if(length(var_name) == 2){
      
      the_grid[[2]] <- c(mean(the_data[, var_name[2]]) - sd(the_data[, var_name[2]]),
                         mean(the_data[, var_name[2]]),
                         mean(the_data[, var_name[2]]) + sd(the_data[, var_name[2]]))
      
      
    }
    
    grid_total <- do.call(expand.grid, the_grid)
    names(grid_total) <- var_name
    
  }
  
  new_data <- new_data[rep(1, nrow(grid_total)), ]
  
  for(the_var in 1:length(var_name)) {
    
    new_data[, var_name[the_var]] <- grid_total[, the_var]
    
  }
  
  if(mod_type == "classification"){
    
    if("glmerMod" %in% class(the_mod)){
      
      the_predictions <- predict(the_mod, newdata = new_data, re.form = NA, type = "response")
      
    } else if("randomForest" %in% class(the_mod)){
      
      the_predictions <- predict(the_mod, newdata = new_data, type = "prob")[, reference]
      
    } else if("RandomForest" %in% class(the_mod)){
      
      extract_pred <- predict(the_mod, newdata = new_data, type = "prob")
      the_predictions <- do.call(rbind, extract_pred)[, levels(new_data[, outcome]) == reference]
      
    } else stop("You can only use glmer, randomForest, or cforest for classification predictions.")
      
  } else {
    
    if("lmerMod" %in% class(the_mod)){

      the_predictions <- predict(the_mod, newdata = new_data, re.form = NA)
      
    } else if("randomForest" %in% class(the_mod)){

      the_predictions <- predict(the_mod, newdata = new_data)
      
    } else if("RandomForest" %in% class(the_mod)){

      the_predictions <- predict(the_mod, newdata = new_data)
      
    } else stop("You can only use glmer, randomForest, or cforest for classification predictions.")
    
  }

  new_data$pred <- as.vector(the_predictions)
  
  if(length(var_name) == 2 & sum(sapply(the_data[, var_name], is.factor)) == 0){
    
    new_data[, var_name[2]] <- factor(rep(c("-1SD", "0SD", "+1SD"), each = length(the_grid[[1]])),
                                      levels = c("-1SD", "0SD", "+1SD"))
    
  }
  
  return(new_data)
  
}

#############################################
# L1_cat
# Creates main effects plot for level-1 
# categorical variables
# @param the_data the dataset to be used
# @param the_mod the model (only randomForest right now)
# @param var_name the variable name
# @param type either regression or classification
# @param reference reference group in the case of classification

L1_cat <- function(the_data, the_mod, var_name, type, reference, outcome, mod_type){
    
  if(type == "raw"){
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = outcome, group = var_name[1]), data = the_data) +
      geom_jitter(position = position_jitter(width = 0.2, height = 0), color = "#545454", alpha = 0.2) +
      geom_errorbar(stat = "hline", yintercept = "mean", width = 0.8, aes(ymax = ..y.., ymin = ..y..), size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ggtitle(paste(var_name[1], "- Raw")) +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- outcome
    
  } else if(type == "predicted"){
    
    new_data <- construct_grid(the_data, the_mod, var_name, reference, outcome, mod_type)
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = "pred", group = var_name[1]), data = new_data) + 
      geom_jitter(aes_string(x = var_name[1], y = outcome), data = the_data,
                  position = position_jitter(width = .2, height = 0), color = "#545454", alpha = 0.2) +
      geom_errorbar(stat = "hline", yintercept = "mean", width = 0.8, aes(ymax = ..y.., ymin = ..y..), size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ylab(paste("Predicted", outcome)) +
      ggtitle(paste(var_name[1], "- Predicted")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- paste("Predicted", outcome)
    
  } else stop("Please enter either raw or predicted for type.")
  
  if(mod_type == "classification"){
    
    p + ylab(paste0(y_axis, " (1 = ", reference, ")"))
    
  } else{
    
    p
    
  }
  
}

#############################################
# L1_cont
# Creates main effects plot for level-1 
# continous variables
# @param the_data the dataset to be used
# @param the_mod the model (only randomForest right now)
# @param var_name the variable name
# @param type either regression or classification
# @param reference reference group in the case of classification

L1_cont <- function(the_data, the_mod, var_name, type, reference, outcome, mod_type){
  
  if(type == "raw"){
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = outcome), data = the_data) + 
      geom_point(color = "#545454", alpha = 0.2) + 
      geom_smooth(method = "loess", color = "black", se = FALSE, size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ggtitle(paste(var_name[1], "- Raw")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- outcome
    
  } else if(type == "predicted"){
    
    new_data <- construct_grid(the_data, the_mod, var_name, reference, outcome, mod_type)
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = "pred"), data = new_data) + 
      geom_point(aes_string(x = var_name[1], y = outcome), data = the_data, color = "#545454", alpha = 0.2) +
      geom_smooth(method = "loess", color = "black", linetype = "longdash", se = FALSE, size = 2) +
      geom_line(size = 1) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ylab(paste("Predicted", outcome)) +
      ggtitle(paste(var_name[1], "- Predicted")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- paste("Predicted", outcome)
    
  } else stop("Please enter either raw or predicted for type.")
  
  if(mod_type == "classification"){
    
    p + ylab(paste0(y_axis, " (1 = ", reference, ")"))
    
  } else{
    
    p
    
  }
  
}

#############################################
# L2_cat
# Creates main effects plot for level-2 
# categorical variables
# @param the_data the dataset to be used
# @param the_mod the model (only randomForest right now)
# @param var_name the variable name
# @param type either regression or classification
# @param cluster cluster variable for level-2
# @param reference reference group in the case of classification

L2_cat <- function(the_data, the_mod, var_name, type, cluster, reference, outcome, mod_type){
  
  if(mod_type == "classification"){
    
    new_agg <- s_select(the_data, cluster, outcome, var_name) %>%
      s_group_by(cluster) %>%
      s_summarise(paste0("the_mean = sum(", outcome, " == '", reference, "')/n()"),
                  paste0("the_se = sqrt((sum(", outcome, "== '", reference, "')/n() * sum(",
                         outcome, " != '", reference, "')/n())/n())"),
                  paste0(var_name, " = levels(", var_name, ")[which.max(table(", var_name, "))]"))
    
  } else{
    
    new_agg <- s_select(the_data, cluster, outcome, var_name) %>%
      s_group_by(cluster) %>%
      s_summarise(paste0("the_mean = mean(", outcome, ")"),
                  paste0("the_se = sd(", outcome, ")/length(", outcome, ")"),
                  paste0(var_name, " = levels(", var_name, ")[which.max(table(", var_name, "))]"))
    
  }
  
  if(type == "raw"){
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = "the_mean", group = var_name[1]), data = new_agg) +
      geom_pointrange(aes(ymax = the_mean + the_se, ymin = the_mean - the_se), size = 1,
                      position = position_jitter(width = 0.2, height = 0), shape = 21, fill = "white") +
      geom_errorbar(stat = "hline", yintercept = "mean", width = 0.8, aes(ymax = ..y.., ymin = ..y..), size = 2) +
      labs(y = outcome) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ggtitle(paste(var_name[1], "- Raw")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- outcome
    
  } else if(type == "predicted"){
    
    new_data <- construct_grid(the_data, the_mod, var_name, reference, outcome, mod_type)
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = "the_mean", group = var_name[1]), data = new_agg) +
      geom_pointrange(aes(ymax = the_mean + 2 * the_se, ymin = the_mean - 2 * the_se), size = 1,
                      position = position_jitter(width = .2, height = 0), shape = 21, fill = "white") +
      geom_errorbar(aes_string(x = var_name[1], y = "pred", group = var_name[1], ymax = "..y..", ymin = "..y.."), data = new_data,
                    stat = "hline", yintercept = "mean", width = 0.8, size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      labs(y = paste("Predicted", outcome)) +
      ggtitle(paste(var_name[1], "- Predicted")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- paste("Predicted", outcome)
    
  } else stop("Please enter either raw or predicted for type.")
  
  if(mod_type == "classification"){
    
    p + ylab(paste0(y_axis, " (1 = ", reference, ")"))
    
  } else{
    
    p
    
  }
  
}

#############################################
# L2_cont
# Creates main effects plot for level-2 
# continuous variables
# @param the_data the dataset to be used
# @param the_mod the model (only randomForest right now)
# @param var_name the variable name
# @param type either regression or classification
# @param cluster cluster variable for level-2
# @param reference reference group in the case of classification

L2_cont <- function(the_data, the_mod, var_name, type, cluster, reference, outcome, mod_type){
  
  if(mod_type == "classification"){
    
    new_agg <- s_select(the_data, cluster, outcome, var_name) %>%
      s_group_by(cluster) %>%
      s_summarise(paste0("the_mean = sum(", outcome, " == '", reference, "')/n()"),
                  paste0("the_se = sqrt((sum(", outcome, "== '", reference, "')/n() * sum(",
                         outcome, " != '", reference, "')/n())/n())"),
                  paste0(var_name, " = mean(", var_name, ")"))
    
  } else{
    
    new_agg <- s_select(the_data, cluster, outcome, var_name) %>%
      s_group_by(cluster) %>%
      s_summarise(paste0("the_mean = mean(", outcome, ")"),
                  paste0("the_se = sd(", outcome, ")/length(", outcome, ")"),
                  paste0(var_name[1], " = mean(", var_name, ")"))
    
  }
  
  if(type == "raw"){
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = "the_mean"), data = new_agg) +
      geom_errorbar(aes(ymax = the_mean + 2 * the_se, ymin = the_mean - 2 * the_se), width = 0, size = 1) +
      geom_smooth(method = "loess", color = "black", size = 2, se = FALSE) +
      geom_point(size = 3, shape = 21, fill = "white") +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      labs(y = outcome) +
      ggtitle(paste(var_name[1], "- Raw")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- outcome
    
  } else if(type == "predicted"){
    
    new_data <- construct_grid(the_data, the_mod, var_name, reference, outcome, mod_type)
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = "the_mean"), data = new_agg) +
      geom_errorbar(aes(ymax = the_mean + 2 * the_se, ymin = the_mean - 2 * the_se), width = 0, size = 1) +
      geom_point(size = 3, shape = 21, fill = "white") +
      geom_line(aes_string(x = var_name[1], y = "pred"), data = new_data, size = 1) +
      geom_smooth(aes_string(x = var_name[1], y = "pred"), data = new_data, size = 2,
                  method = "loess", color = "black", se = FALSE, linetype = "longdash") +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      labs(y = paste("Predicted", outcome)) +
      ggtitle(paste(var_name[1], "- Predicted")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold"))
    
    y_axis <- paste("Predicted", outcome)
    
  } else stop("Please enter either raw or predicted for type.")
  
  if(mod_type == "classification"){
    
    p + ylab(paste0(y_axis, " (1 = ", reference, ")"))
    
  } else{
    
    p
    
  }
  
}

#############################################
# cat_cat
# Creates an interaction plot for two categorical
# variables 
# @param the_data the dataset to be used
# @param the_mod the model (only randomForest right now)
# @param var_name the variable name
# @param type either regression or classification
# @param reference reference group in the case of classification

cat_cat <- function(the_data, the_mod, var_name, type, reference, outcome, mod_type){
  
  if(type == "raw"){
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    new_data <- s_group_by(the_data, var_name) %>%
      s_summarise(paste("my_mean = mean(", outcome, ")"))
    
    p <- ggplot(aes_string(x = var_name[1], y = outcome, linetype = var_name[2]), data = the_data) +
      geom_point(aes_string(x = var_name[1], y = "my_mean", shape = var_name[2]), data = new_data, size = 5) +
      geom_line(aes_string(x = var_name[1], y = "my_mean", group = var_name[2]), data = new_data, size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ggtitle(paste(var_name[1], "x", var_name[2], "Interaction - Raw")) + 
      theme_bw() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
            plot.title = element_text(face = "bold"))
    
    y_axis <- outcome
    
  } else if(type == "predicted"){
    
    new_data <- construct_grid(the_data, the_mod, var_name, reference, outcome, mod_type)
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = outcome, linetype = var_name[2]), data = the_data) +
      geom_point(aes_string(x = var_name[1], y = "pred", shape = var_name[2]), data = new_data, size = 5) +
      geom_line(aes_string(x = var_name[1], y = "pred", group = var_name[2]), data = new_data, size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ggtitle(paste(var_name[1], "x", var_name[2], "Interaction - Predicted")) + 
      theme_bw() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
            plot.title = element_text(face = "bold"))
    
    y_axis <- paste("Predicted", outcome)
    
  } else stop("Please enter either raw or predicted for type.")
  
  if(mod_type == "classification"){
    
    p + ylab(paste0(y_axis, " (1 = ", reference, ")"))
    
  } else{
    
    p
    
  }
  
}

#############################################
# cat_cont
# Creates an interaction plot for two categorical
# variables 
# @param the_data the dataset to be used
# @param the_mod the model (only randomForest right now)
# @param var_name the variable name
# @param type either regression or classification
# @param reference reference group in the case of classification

cat_cont <- function(the_data, the_mod, var_name, type, reference, outcome, mod_type){
  
  is_cat <- which.max(sapply(the_data[, var_name], is.factor))
  is_cont <- which.min(sapply(the_data[, var_name], is.factor))
  
  if(type == "raw"){
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[is_cont], y = outcome, group = var_name[is_cat]), data = the_data) +
      geom_smooth(aes_string(linetype = var_name[is_cat]), method = "loess", color = "black", se = FALSE, size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ggtitle(paste(var_name[is_cont], "x", var_name[is_cat], "Interaction - Raw")) +
      theme_bw() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
            plot.title = element_text(face = "bold"))
    
    y_axis <- outcome
    
  } else if(type == "predicted"){
    
    new_data <- construct_grid(the_data, the_mod, var_name, reference, outcome, mod_type)
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[is_cont], y = "pred", group = var_name[is_cat]), data = new_data) +
      geom_smooth(aes_string(linetype = var_name[is_cat]), method = "loess", color = "black", se = FALSE, size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ylab(paste("Predicted", outcome)) +
      ggtitle(paste(var_name[is_cont], "x", var_name[is_cat], "Interaction - Predicted")) +
      theme_bw() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
            plot.title = element_text(face = "bold"))
    
    y_axis <- paste("Predicted", outcome)
    
  } else stop("Please enter either raw or predicted for type.")
  
  if(mod_type == "classification"){
    
    p + ylab(paste0(y_axis, " (1 = ", reference, ")"))
    
  } else{
    
    p
    
  }
  
}

#############################################
# cont_cont
# Creates an interaction plot for two categorical
# variables 
# @param the_data the dataset to be used
# @param the_mod the model (only randomForest right now)
# @param var_name the variable name
# @param type either regression or classification
# @param reference reference group in the case of classification

cont_cont <- function(the_data, the_mod, var_name, type, reference, outcome, mod_type){
  
  if(type == "raw"){
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    the_data[, var_name[2]] <- cut(the_data[, var_name[2]],
                                   c(min(the_data[, var_name[2]]) - 1,
                                     median(the_data[, var_name[2]]),
                                     max(the_data[, var_name[2]]) + 1),
                                   labels = c("Low", "High"))
    
    p <- ggplot(aes_string(x = var_name[1], y = outcome, group = var_name[2]), data = the_data) +
      geom_smooth(aes_string(linetype = var_name[2]), method = "loess", color = "black", se = FALSE, size = 2) +
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ggtitle(paste(var_name[1], "x", var_name[2], "Interaction - Raw")) +
      theme_bw() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
            plot.title = element_text(face = "bold"))
    
    y_axis <- outcome
    
  } else if(type == "predicted"){
    
    new_data <- construct_grid(the_data, the_mod, var_name, reference, outcome, mod_type)
    
    if(mod_type == "classification"){
      
      the_data[, outcome] <- ifelse(the_data[, outcome] == reference, 1, 0)
      
    }
    
    p <- ggplot(aes_string(x = var_name[1], y = "pred", linetype = var_name[2]), data = new_data) +
      geom_smooth(method = "loess", color = "black", se = FALSE, size = 2) +
      scale_linetype_manual(values = c("dotted", "dashed", "solid")) + 
      ylim(min(the_data[, outcome]), max(the_data[, outcome])) +
      ylab(paste("Predicted", outcome)) +
      ggtitle(paste(var_name[1], "x", var_name[2], "Interaction - Predicted")) + 
      theme_bw() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
            legend.key.width = unit(1.5, "cm"),
            plot.title = element_text(face = "bold"))
    
    y_axis <- paste("Predicted", outcome)
    
  } else stop("Please enter either raw or predicted for type.")
  
  if(mod_type == "classification"){
    
    p + ylab(paste0(y_axis, " (1 = ", reference, ")"))
    
  } else{
    
    p
    
  }
  
}
