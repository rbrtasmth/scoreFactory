#' liftplot is a subclass of scoreFactory. Used to summarize scores by quantiles. Includes methods for plotting liftcharts. Defaults largely based on how the model was created (e.g. part_by argument, targets, etc.), but can be overridden for higher level summaries.
#' 
#'  Example situation:
#'  @param object A scoreFactory object
#'  @param grp_by
#'  @param targets
#'  @param data_split
#'  @param agg_over_grps
#'  @param n
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @import reshape2
#'  @import plyr
liftplot <- function(object, targets = "all", data_split = "Validation", agg_over_grps = TRUE, n = 10, ...){
    #  Initialize data
    df1 <- droplevels(subset(object$Scores, Data_Split == data_split))
    df1$key <- 1:nrow(df1)
    
    #  Create id_vars for summarizing by
    if(agg_over_grps){
        grp_by <- NULL
    } else {
        grp_by <- object$grp_by
    }
    
    if(targets == "all"){
        targets <-  object$targets
    }
    dec_vars <- paste(targets, "_Decile", sep = "")
    sco_vars <- paste(targets, "_Score", sep = "")
    
    #  Only save the variables we need
    df1 <- df1[c("key", grp_by, targets, sco_vars)]
    
    #  Pull out actual and scores from data
    a1 <- df1[,c("key", grp_by, targets)]
    actuals <- melt(a1, c("key", grp_by), targets)
    names(actuals) <- c("key", grp_by, "Target", "Actual")
    
    s1 <- df1[,c("key", grp_by, sco_vars)]
    s2 <- melt(s1, c("key", grp_by), sco_vars)
    s2$Target <- str_replace_all(s2$variable, "_Score", "")
    s3 <- s2[,-(match("variable", names(s2)))]
    names(s3) <- c("key", grp_by, "Score", "Target")
    
    #  need to calculate deciles within grp_by and target
    scores <- rbind.fill(
        lapply(
            split(x = s3, f = s3[,c(grp_by, "Target")]),
            function(a_df){
                transform(a_df, Quantile = ntq("Score", n, a_df, ...))
            }
        )
    )
    #  merge actual and scores
    df2 <- join(actuals, scores, by = c("key", grp_by, "Target"))
    df2$Accuracy <- ifelse(df2$Actual == round(df2$Score), 1, 0)
    
    #  Aggregate
    agg_df <- ddply(df2[,c(grp_by, "Target", "Quantile", "Actual", "Score", "Accuracy")],
                    c(grp_by, "Target", "Quantile"),
                    colwise(mean), drop = FALSE)
    agg_df$Quantile <- factor(agg_df$Quantile, levels = sort(levels(agg_df$Quantile)))
    
    rval <- list(
        Lift_Data = agg_df[do.call(order, agg_df),],
        grp_by = grp_by,
        targets = targets,
        n = n,
        Model_Name = object$Model_Name
    )
    class(rval) <- "liftplot.scoreFactory"
    return(rval)
}


#' print liftplot.
#' 
#'  Example situation:
#'  @param object A scoreFactory object
#'  @export
print.liftplot.scoreFactory <- function(object, ...){
    print(object$Lift_Data)
    invisible(object)
}


#' head method for liftplots.
#' 
#'  Example situation:
#'  @param object A scoreFactory object
#'  @export

head.liftplot.scoreFactory <- function(object, ...){
    print(head(object$Lift_Data))
    invisible(object)
}





#' plot the liftplot data.
#' 
#'  Example situation:
#'  @param object A liftplot.scoreFactory object
#'  @export
#'  @import glmnet
#'  @import Matrix
#'  @import ggplot2
#'  @import scales
plot.liftplot.scoreFactory <- function(object, ...){
    plot_data <- rbind.fill(
        lapply(
            split(object$Lift_Data, object$Lift_Data[,c(object$grp_by, "Target")]),
            function(a_df){
                transform(a_df, ntile = seq(1:nrow(a_df)))
            }
        )
    )
    
    plot_list <- list()
    for(tar in object$targets){
        plot_title <- paste0("Liftplot of ",
                             str_replace_all(object$Model_Name, "_", " "),
                             " Model\nTarget = ",
                             str_replace_all(tar, "_", " "))
        this_plot_df <- droplevels(subset(plot_data, Target == tar))
        the_plot <- ggplot(this_plot_df, aes(x = ntile, group = 1)) +
            geom_line(aes(y = Actual, colour = "Actual"), size = 1) +
            geom_line(aes(y = Score, colour = "Predicted"), size = 1) +
            scale_y_continuous(labels = percent_format(), limit = c(0,1)) +
            scale_x_continuous(breaks = seq(1, object$n, length.out = 10)) +
            scale_colour_brewer(type = "seq", palette = "Set1") +
            theme_bw() +
            labs(title = plot_title,
                 colour = NULL,
                 x = NULL,
                 y = NULL) +
            theme(legend.position = "bottom")
        if(is.null(object$grp_by)){
            plot_list[[tar]] <- the_plot
        } else {
            this_plot_df$Population <- this_plot_df[,object$grp_by]
            plot_list[[tar]] <- the_plot %+% this_plot_df + facet_wrap(~Population, scales = "free", nrow = 1)
        }
    }
    class(plot_list) <- "plot.liftplot.scoreFactory"
    return(plot_list)
}

#' print the plot of liftplot data.
#' 
#'  Example situation:
#'  @param x A plot.liftplot.scoreFactory object
#'  @export
#'  @import ggplot2
#'  @import scales
print.plot.liftplot.scoreFactory <- function(x){
    my_plots <- labels(x)
    for(a_plot in my_plots){
        print(x[[a_plot]])
    }
    invisible(x)
}