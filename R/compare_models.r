#' Summarize a list of scoreFactory objects by r-squared and AUC on the validation set.
#' 
#'  Example situation: compare R-squared, and AUC across a list of scoreFactory objects.
#'  @param objects a scoreFactory object
#'  @param targets string of target names, defaults to all.
#'  @param show_rsq should r-squared be computed?
#'  @param extraction argument for only computing statistics on a subset of the validation population.
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @importFrom Hmisc rcorr.cens
compare_models <- function(objects, targets = "all", show_rsq = TRUE, extraction = NULL, ...){
    
    if(targets == "all"){
        targets <-  objects[[1]]$targets  #  targets should be same for all
    }
    
    n_objects <- length(objects)
    n_targets <- length(targets)
    summary_list <- vector("list", (n_objects * n_targets))
    
    for(obj_n in 1:n_objects){
        object <- objects[[obj_n]]
        a_df <- subset(object$Scores, Data_Split == "Validation")
        if(is.null(extraction) == FALSE){
            a_df <- droplevels(subset(a_df, eval(extraction[[1]])))
        }
        sample_size <- nrow(a_df)
        
        for(tar_n in 1:n_targets){
            a_target <- targets[tar_n]
            model_info <- object$Meta_Info
            if(sum(a_df[,a_target]) == 0 | sum(a_df[,a_target]) == nrow(a_df)){
                auc_est <- NA
                auc_se <- NA
            } else {
                model_auc <- rcorr.cens(a_df[,paste0(a_target, "_Score")],  a_df[,a_target])
                auc_est <- model_auc["C Index"]
                auc_se <- model_auc["S.D."]/2
            }
            
            if(show_rsq){                
                grad_rsq <- round(model_info[model_info$target == a_target
                                             & (model_info$group == "GRAD" | model_info$group == "Graduate"), "dev_ratio"], 3)
                ugrd_rsq <- round(model_info[model_info$target == a_target
                                             & (model_info$group == "UGRD" | model_info$group == "Undergraduate"), "dev_ratio"], 3)
                model_summary <- data.frame(
                    Target = a_target,
                    Model_Name = object$Model_Name,
                    AUC = auc_est,
                    AUC_SE = auc_se,
                    GRAD_Rsq = grad_rsq,
                    UGRD_Rsq = ugrd_rsq,
                    Sample_Size = sample_size
                )
            } else {
                model_summary <- data.frame(
                    Target = a_target,
                    Model_Name = object$Model_Name,
                    AUC = auc_est,
                    AUC_SE = auc_se,
                    Sample_Size = sample_size
                )
            }
            
            summary_list[[((obj_n - 1) * n_targets) + tar_n]] <- model_summary
        }
    }
    rval <- rbind.fill(summary_list)
    return(rval[order(rval$Target, rval$AUC, decreasing = TRUE),])
}