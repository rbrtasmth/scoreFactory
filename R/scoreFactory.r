#' This is a class for creating logistic regression scores using the LASSO algorithm.
#' 
#'  Example situation:
#'  @param data_arg data set
#'  @param var_list
#'  @param targets
#'  @param keys
#'  @param grp_by
#'  @param pct_train
#'  @param training
#'  @param tv_stratification
#'  @param lambda
#'  @param Weight
#'  @param model_label
#'  @export
#'  @import caret
#'  @import glmnet
#'  @import Matrix
#'  @import plyr
#'  @import reshape2
#'  @import stringr
scoreFactory <- function(data_arg,
                          var_list,
                          targets,
                          keys = NULL,
                          grp_by = "Career",
                          pct_train = .8,
                          training = NULL,
                          tv_stratification,
                          lambda = "1se",
                          Weight = NULL,
                          model_label = "scoreFactory"
)
{
    #  Initialize variables
    data_arg <- droplevels(data_arg)
    partitions <- levels(data_arg[,grp_by])
    n_parts <- length(partitions)
    n_targets <- length(targets)
    if(is.null(Weight)){
        data_arg$Weight <- rep(1, nrow(data_arg))
    } else {
        data_arg$Weight <- data_arg[,Weight]
    }
    
    #  define which rows could be used for training/validation
    if(is.null(training) == FALSE){
        train_ind <- with(data_arg, eval(training[[1]]))
    } else {
        train_ind <- rep(TRUE, nrow(data_arg))
    }
    
    #  save keys
    data_arg$key <- 1:nrow(data_arg)
    if(is.null(keys) == FALSE) keys_df <- data_arg[,c("key", keys[which(!(keys %in% grp_by))]) ]
    
    #  split train/valid/scoring
    training_indices <- createDataPartition(data_arg[train_ind, c(tv_stratification)], p = pct_train, list = FALSE)
    training_indices <- (data_arg[train_ind, "key"])[training_indices]
    data_arg$Data_Split <- factor(
        ifelse(train_ind,
               ifelse(data_arg$key %in% data_arg[training_indices, "key"],
                      "Training",
                      "Validation"),
               "Scoring"),
        levels = c("Training", "Validation", "Scoring")
    )
    
    #  Clean up variables list
    if(class(var_list) == "character") var_list <- list(var_list)
    if(n_parts != length(var_list)){
        print("Number variable sets is not equal to number of data partitions. Using all variables in var_list for all models")
        uniq_vars <- unique(unlist(var_list))
        var_list <- list()
        for(i in 1:n_parts){
            var_list[partitions[i]] <- list(uniq_vars)
        }
    }
    
    #drop non-modeling variables for now
    data_arg <- droplevels(data_arg[,c("key", "Weight", "Data_Split", grp_by, targets, unique(unlist(var_list)))])
    
    #  Score all targets with each partition
    coef_list <- vector("list", (n_parts * n_targets))
    score_list <- vector("list", n_parts)
    model_list <- vector("list", (n_parts * n_targets))
    label_list <- vector("list", (n_parts * n_targets))
    for(part_num in 1:n_parts){
        partial_df <- data_arg[data_arg[,grp_by] == partitions[part_num],]
        
        for(tar_num in 1:n_targets){
            cat(paste0("\nScoring target: ", targets[tar_num], "\nFor group: ", partitions[part_num], "\n"))
            #  Create each of the scores for current partition
            partial_df$target <- partial_df[,targets[tar_num]]
            vars <- c("Weight", "target", as.character(unlist(var_list[partitions[part_num]])), "key")
            n_vars <- length(vars)
            
            train_mat <- as.matrix(partial_df[partial_df$Data_Split %in% c("Training"), vars])
            holdout_mat <- as.matrix(partial_df[partial_df$Data_Split %in% c("Validation", "Scoring"), vars])
            
            #  Fit cross validated model to find ideal lambda (penalty)
            key_indice <- match("key", colnames(train_mat[,3:n_vars]))
            cv <- cv.glmnet(x = train_mat[,3:n_vars], y = train_mat[,2],
                            family = "binomial",
                            weights = train_mat[,1],
                            exclude = key_indice) #indices must match just x argument
            
            if (lambda == "min"){
                lambda_val <- cv$lambda.min
            } else if (lambda == "half se") {
                lambda_val <- max(cv$lambda[(cv$cvm[cv$lambda == cv$lambda.min] + (cv$cvsd[cv$lambda == cv$lambda.min]/2)) >= cv$cvm])
            } else if (lambda == "forth se") {
                lambda_val <- max(cv$lambda[(cv$cvm[cv$lambda == cv$lambda.min] + (cv$cvsd[cv$lambda == cv$lambda.min]/4)) >= cv$cvm])
            } else if (lambda == "eighth se") {
                lambda_val <- max(cv$lambda[(cv$cvm[cv$lambda == cv$lambda.min] + (cv$cvsd[cv$lambda == cv$lambda.min]/8)) >= cv$cvm])
            } else if (lambda == "1se") {
                lambda_val <- cv$lambda.1se
            } else {
                lambda_val <- max(cv$lambda)
            }
            
            #  Save final model
            model_list[[((part_num - 1) * n_targets) + tar_num]] <- cv$glmnet.fit
            
            #  Save scores
            valid_scores <- cbind(holdout_mat[,(key_indice+2)],
                                  predict(cv$glmnet.fit,
                                          newx = holdout_mat[,3:ncol(holdout_mat)],
                                          s = lambda_val,
                                          type = "response"))
            train_scores <- cbind(train_mat[,(key_indice+2)],
                                  predict(cv$glmnet.fit,
                                          newx = train_mat[,3:ncol(train_mat)],
                                          s = lambda_val,
                                          type = "response"))
            scores <- as.data.frame(rbind(valid_scores, train_scores))
            names(scores) <- c("key", paste(targets[tar_num], "Score", sep = "_"))
            partial_df <- merge(partial_df, scores, by = c("key"), all.x=TRUE)
            
            #  Save coefficients
            cf <- data.frame(Coefficient = row.names(as.matrix(coef(cv$glmnet.fit))),
                             Estimate = as.vector(coef(cv$glmnet.fit, s = lambda_val)),
                             row.names = NULL)
            cf <- cf[cf$Estimate != 0,]
            cf <- merge(cf,
                        coef_order(cv$glmnet.fit, lambda_val),
                        by = c("Coefficient"), all.x = TRUE)
            cf[,grp_by] <- partitions[part_num]
            cf$Target <- targets[tar_num]
            cf$Model <- model_label
            coef_list[[((part_num - 1) * n_targets) + tar_num]] <- cf
            
            #  Record which model this is
            labels <- data.frame(iteration = ((part_num - 1) * n_targets) + tar_num,
                                 group = partitions[part_num],
                                 target = targets[tar_num],
                                 lambda = lambda,
                                 lambda_val = lambda_val,
                                 dev_ratio = cv$glmnet.fit$dev.ratio[cv$glmnet.fit$lambda == lambda_val]
            )
            label_list[[((part_num - 1) * n_targets) + tar_num]]  <- labels
        }
        
        #  Save the scores for the current partition (with scores for all targets)
        score_list[[part_num]] <- partial_df
    }
    
    #  Clean output
    coef_df <- rbind.fill(coef_list)
    label_df <- rbind.fill(label_list)
    score_df <- rbind.fill(score_list)
    remove_ind <- match(c("key", "target", "Weight"), names(score_df))
    if(is.null(keys)){
        score_df <- score_df[,-(remove_ind)]
    } else {
        score_df <- merge(score_df, keys_df, by = ("key"), all.x=TRUE)[,-(remove_ind)]
    }
    
    rval <- list(
        #labels
        Meta_Info = label_df,
        Model_Name = model_label,
        
        #data
        Scores = score_df,        
        Coefficients = coef_df,
        Models = model_list,
        Variables = var_list,
        
        #scoring keys/vars/targets
        keys = keys,          
        grp_by = grp_by,
        targets = targets,
        lambda = lambda,
        Weight = Weight,
        
        #misc
        Score_Date = Sys.Date(),  
        call = sys.call()
    )
    class(rval) <- "scoreFactory"
    return(rval)
}




#' Print summary of scoreFactory object
#' 
#'  Example situation:
#'  @param x A scoreFactory object
#'  @export
#'  @import glmnet
#'  @import Matrix
print.scoreFactory <- function(x, ...){
    cat("Score factory call:\n")
    print(x$call)
    cat("\nWhich contains elements:\n")
    print(unlist(lapply(x, function(in_x) class(in_x))))
}



#' Show examples of values stored in scoreFactory object
#' 
#'  Example situation:
#'  @param x A scoreFactory object
#'  @export
#'  @import glmnet
#'  @import Matrix
head.scoreFactory <- function(x, ...){
    lapply(
        x,
        function(in_x){
            if(class(in_x) %in% c("data.frame", "numeric", "Date", "call")){
                print(head(in_x))
            } else if(class(in_x) == "character"){
                print(in_x)
            } else if(class(in_x) == "list"){
                cat("\nA list. No print method defined.\n")
            }
        }
    )
}



#' Extract coefficients of scoreFactory object
#' 
#'  Example situation:
#'  @param sf A scoreFactory object
#'  @param grp_by
#'  @param targets
#'  @export
#'  @import glmnet
#'  @import Matrix
coef.scoreFactory <- function(sf, grp_by = "all", targets = "all", ...){
    if(grp_by == "all") grp_by <- unique(sf$Coefficient[,sf$grp_by])
    if(targets == "all") targets <- unique(sf$Coefficient[,"Target"])
    rval <- droplevels(sf$Coefficient[sf$Coefficient[,sf$grp_by] %in% grp_by
                                      & sf$Coefficient[,"Target"] %in% targets,])
    return(rval)
}



#' glmnet plot of scoreFactory object
#' 
#'  Example situation:
#'  @param sf A scoreFactory object
#'  @param grp_by
#'  @param targets
#'  @export
#'  @import glmnet
#'  @import Matrix
plot.scoreFactory <- function(sf, grp_by = "all", targets = "all", ...){
    if(grp_by == "all") grp_by <- unique(sf$Meta_Info[,"group"])
    if(targets == "all") targets <-  unique(sf$Meta_Info[,"target"])
    model_indices <- sf$Meta_Info[sf$Meta_Info[,"group"] %in% grp_by &
                                      sf$Meta_Info[,"target"] %in% targets,"iteration"]
    n <- length(model_indices)
    nrows <- floor(sqrt(n))
    ncols <- n / nrows
    
    par(mfrow = c(nrows, ncols))
    for(i in model_indices){
        temp_model <- sf$Models[[i]]
        plotCoef(temp_model$beta,
                 lambda = temp_model$lambda,
                 df = temp_model$df,
                 dev = temp_model$dev.ratio,
                 label = FALSE,
                 xvar = "lambda",
                 main = paste0("Target: ",
                               sf$Meta_Info[i, "target"],
                               " for population: ",
                               sf$Meta_Info[i, "group"], "\n"))
    }
}



#' Summary of scoreFactory accuracy
#' 
#'  Example situation:
#'  @param sf A scoreFactory object
#'  @param grp_by
#'  @param targets
#'  @param data_split
#'  @param extraction
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @import reshape2
#'  @import plyr
#'  @import stringr
summary.scoreFactory <- function(object, grp_by = "all", targets = "all", data_split = "all", extraction = NULL){
    #  Create key for merging
    temp_df <- object$Scores
    
    #  Create subsets if specified
    #  1. Subset on train/validation/scoring
    if(data_split[1] != "all") temp_df <- droplevels(subset(temp_df, Data_Split %in% data_split))
    
    if(is.null(extraction) == FALSE){
        temp_df <- droplevels(subset(temp_df, eval(extraction[[1]])))
    }
    
    temp_df$key <- 1:nrow(temp_df)
    
    #  Create groups for summarizing by
    if(grp_by[1] == "all"){
        grp_by <- c(object$grp_by, "Data_Split")
    } else {
        grp_by <- unique(c(grp_by, "Data_Split"))
    }
    if(targets[1] == "all"){
        targets <-  object$targets
    }
    
    #  Pull out actual and scores from data
    a1 <- temp_df[,c("key", grp_by, targets)]
    actuals <- melt(a1, c("key", grp_by), targets)
    names(actuals) <- c("key", grp_by, "Target", "Actual")
    
    sco_vars <- paste(targets, "_Score", sep = "")
    s1 <- temp_df[,c("key", grp_by, sco_vars)]
    s2 <- melt(s1, c("key", grp_by), sco_vars)
    s2$Target <- str_replace_all(s2$variable, "_Score", "")
    scores <- s2[,-(match("variable", names(s2)))]
    names(scores) <- c("key", grp_by, "Score", "Target")
    
    tbl <- join(actuals, scores, by = c("key", grp_by, "Target"))
    tbl$Classification <- round(tbl$Score)
    tbl$Correct <- ifelse(tbl$Classification == tbl$Actual, 1, 0)
    
    round_mean <- function(x) round(mean(x), 2)
    sum_vars <- c("Actual", "Score", "Classification", "Correct")
    summary_stats <- ddply(tbl[,c(grp_by, "Target", sum_vars)], c(grp_by, "Target"), colwise(round_mean))
    names(summary_stats) <- c(grp_by, "Target", "Avg_Actual", "Avg_Score", "Pct_Predicted_Positive", "Accuracy")
    summary_ss <- ddply(tbl[,c(grp_by, "Target", "Actual")], c(grp_by, "Target"), colwise(length))
    names(summary_ss) <- c(grp_by, "Target", "Sample_Size")
    summaries <- join(summary_stats, summary_ss, by = c(grp_by, "Target"))
    
    overall_stats <- ddply(tbl[,c("Data_Split", "Target", sum_vars)], c("Data_Split", "Target"), colwise(round_mean))
    names(overall_stats) <- c("Data_Split", "Target", "Avg_Actual", "Avg_Score", "Pct_Predicted_Positive", "Accuracy")
    overall_ss <- ddply(tbl[,c("Data_Split", "Target", "Actual")], c("Data_Split", "Target"), colwise(length))
    names(overall_ss) <- c("Data_Split", "Target", "Sample_Size")
    
    overall_labels <- data.frame(rep("Overall", length(targets)))
    if(length(grp_by) >= 3){
        for(i in 3:length(grp_by)){
            overall_labels <- cbind(overall_labels, data.frame(rep("Overall", length(targets))))
        }
    }
    names(overall_labels) <- grp_by[grp_by != "Data_Split"]
    overall <- cbind(overall_labels, join(overall_stats, overall_ss, by = c("Data_Split", "Target")))
    final <- rbind(summaries, overall)
    final$Data_Split <- factor(final$Data_Split, levels = c("Training", "Validation", "Scoring"))
    rval <- list(Summary_Table = final[order(final$Target, final$Data_Split),])
    
    class(rval) <- "summary.scoreFactory"
    return(rval$Summary_Table)
}



#' Print summary of scoreFactory accuracy
#' 
#'  Example situation:
#'  @param x A scoreFactory object
#'  @export
print.summary.scoreFactory <- function(x, ...){
    print(x)
    invisible(x)
}


#' Predict method for scoreFactory object
#' 
#'  Example situation:
#'  @param object A scoreFactory object
#'  @param newdata
#'  @param targets
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @import reshape2
#'  @import plyr
predict.scoreFactory <- function(object, newdata, targets = "all", ...){
    meta <- object$Meta_Info
    if(targets == "all") targets <- object$targets
    n_targets <- length(targets)
    partitions <- as.character(unique(meta$group))
    n_parts <- length(partitions)
    newdata$key <- 1:nrow(newdata)
    
    score_list <- vector("list", (n_parts * n_targets))
    names(score_list) <- partitions
    for(p in 1:n_parts){
        vars <- c(object$Variables[[partitions[p]]], "key")
        partial_df <- newdata[newdata[,object$grp_by] == partitions[p], vars]
        scores <- c(partial_df$key)
        for(t in 1:n_targets){
            model_number <- meta$iteration[meta$group == partitions[p] & meta$target == targets[t]]
            scores <- cbind(scores,
                            c(predict(object$Models[[model_number]],
                                      newx = as.matrix(partial_df),
                                      s = meta$lambda_val[model_number],
                                      type = "response")
                            )
            )
        }
        score_list[[partitions[p]]] <- as.data.frame(scores)
    }
    
    scores_df <- rbind.fill(score_list)
    return(scores_df[order(scores_df$scores),2:ncol(scores_df)])
}



#' Densityplot method for scoreFactory object
#' 
#'  Example situation:
#'  @param object A scoreFactory object
#'  @param grp_by
#'  @param targets
#'  @param drop_scoring
#'  @param margin
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @import ggplot2
#'  @import scales
densityplot.scoreFactory <- function(object, grp_by = "all", targets = "all", drop_scoring = TRUE, margin = FALSE, ...){
    df1 <- object$Scores
    if(drop_scoring) df1 <- subset(df1, Data_Split != "Scoring")
    
    #  Create grp_by for summarizing by
    if(grp_by == "all"){
        grp_by <- c(object$grp_by, "Data_Split")
    } else if(grp_by == "none"){
        grp_by <- NULL
    }
    if(targets == "all"){
        targets <-  object$targets
    } else if((targets %in% object$targets) == FALSE) print("Error. Must pick target that was used in modeling")
    sco_vars <- paste(targets, "_Score", sep = "")
    
    df1 <- df1[,c(grp_by, sco_vars)]
    df1$key <- 1:nrow(df1)
    melt_df <- melt(df1, c(grp_by, "key"), c(sco_vars))
    
    plot_list <- list()
    for(sco in sco_vars){
        plot_data <- droplevels(subset(melt_df, variable == sco))
        sco_label <- str_replace_all(sco, "_", " ")
        the_plot <- ggplot(plot_data, aes(x = value)) +
            geom_density(fill = "#FF9999", alpha = .9) +
            geom_histogram(aes(y = ..density..), binwidth = .05, colour = "black", fill = "#999999", alpha = .5) +
            scale_x_continuous(limits = c(0,1), labels = percent) +
            labs(x = sco_label) +
            theme_bw() +
            theme(axis.text.x = element_text(size=7))
        
        if(is.null(grp_by)){
            plot_list[[sco]] <- the_plot
        } else if(length(grp_by) > 1){
            plot_by <- grp_by[-match("Data_Split", grp_by)]
            plot_data$Population <- plot_data[,plot_by]
            if(plot_by == margin) margin <- "Population"
            plot_list[[sco]] <- the_plot %+% plot_data + facet_grid(Population ~ Data_Split, scales = "free", margins = margin)
        } else if(grp_by == "Data_Split"){
            plot_list[[sco]] <- the_plot + facet_wrap(~Data_Split, scales = "free", nrow = 1)
        } else {
            plot_data$Population <- plot_data[, grp_by]
            if(grp_by == margin) margin <- "Population"
            plot_list[[sco]] <- the_plot %+% plot_data + facet_grid(Population~., scales = "free", margins = margin)
        }
    }
    class(plot_list) <- "densityplot.scoreFactory"
    return(plot_list)
}


#' Print densityplot method for scoreFactory object
#' 
#'  Example situation:
#'  @param x A densityplot.scoreFactory object
#'  @export
print.densityplot.scoreFactory <- function(x, ...){
    my_plots <- labels(x)
    for(plot in my_plots){
        print(x[[plot]])
    }
    invisible(x)
}
