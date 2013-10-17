#' variable correlation
#' 
#'  Example situation:
#'  @param object a scoreFactory object
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @import polycor
#'  @import reshape2
#'  @import plyr
var_corr <- function(object, ...){
    targets <-  object$targets
    var_targets <- unique(object$Coefficients[, c("Target", object$grp_by, "Coefficient")])
    var_targets <- subset(var_targets, Coefficient != "(Intercept)" & Target %in% targets)
    var_list <- split(var_targets, var_targets[,c("Target", object$grp_by)])
    var_list_labels <- names(var_list)
    
    cor_list <- lapply(var_list,
                       function(a_df){
                           vars <- as.character(unique(a_df$Coefficient))
                           if(length(vars) < 2){
                               rval <- NA
                           } else {
                               pop_label <- unique(a_df[,object$grp_by])
                               mat <- object$Scores[object$Scores[,"Data_Split"] == "Training" & object$Scores[,object$grp_by] == pop_label, vars]
                               cor_mat <- hetcor(mat)$correlations
                               rval <- melt(cor_mat)
                               names(rval) <- c("Var1", "Var2", "Correlation")
                               return(rval)
                           }
                       }
    )
    rval <- list(cor_list = cor_list,
                 Model_Name = object$Model_Name)
    class(rval) <- "var_corr"
    return(rval)
}


#' print variable correlation
#' 
#'  Example situation:
#'  @param x a scoreFactory object
#'  @export
print.var_corr <- function(x, ...){
    print_labels <- names(x$cor_list)
    for(i in 1:length(print_labels)){
        temp_df <- x$cor_list[[i]]
        cat(paste0("\n\n  ---  ", print_labels[i], " Correlation Matrix (melted)  ---\n"))
        if(class(temp_df) == "data.frame"){
            print_val <- subset(temp_df, Correlation != 1)
            print_val <- print_val[order(abs(print_val$Correlation), decreasing = TRUE),]
            print_val <- print_val[1:nrow(print_val) %% 2 == 1,]
            print(print_val)
        }
    }
    invisible(x)
}


#' plot variable correlation
#' 
#'  Example situation:
#'  @param object a scoreFactory object
#'  @param grp_by
#'  @param target
#'  @export
#'  @import ggplot2
#'  @import scales
plot.var_corr <- function(object, grp_by, target){
    the_label <- paste(target, grp_by, sep = ".")
    the_df <- object$cor_list[[the_label]]
    if(class(the_df) != "data.frame"){
        the_plot <- NA
    } else {
        txt_cutoff <- round(quantile(abs(the_df$Correlation)[the_df$Correlation != 1], probs = seq(0,1, .1))[10], 3)
        the_df$my_label <- ifelse(abs(the_df$Correlation) < txt_cutoff | round(the_df$Correlation, 2) == 1,
                                  "",
                                  as.character(round(the_df$Correlation, 2))
        )
        if(length(unique(the_df$Var1)) >= 15){
            axis_size  = 8
            cor_size = 2
        } else {
            axis_size = 10
            cor_size = 3
        }
        my_title <- paste0(str_replace_all(object$Model_Name, "_", " "),
                           " Model Correlation Matrix\nFor Population: ",
                           grp_by,
                           " and Target: ",
                           target)
        the_plot <- ggplot(the_df, aes(x = Var1, y = Var2, fill = Correlation)) +
            geom_tile() +
            geom_text(aes(label = my_label), size = cor_size, alpha = .75) +
            scale_fill_gradient2(low = "blue",  high = "yellow", limits = c(-1,1)) +
            theme_bw() +
            theme(axis.text.x  = element_text(angle=45, hjust = 1), axis.text  = element_text(size = axis_size)) +
            labs(x = NULL, y = NULL, fill = "Correlation", title = my_title)
    }
    rval <- list(the_plot = the_plot)
    class(rval) <- "plot.var_corr"
    return(rval$the_plot)
}

#' print plot variable correlation
#' 
#'  Example situation:
#'  @param x an object
#'  @export
#'  @import ggplot2
print.plot.var_corr <- function(x){
    #  Load packages:
    require_packages(ggplot2)
    
    if(class(x) == "logical"){
        print(x)
    }
    invisible(x)
}