#' Augment for qcc
#'
#' @param x a qcc object
#' @param include_center should a column for the centerline be included?
#' @param include_stddev should a column for the standard deviation be included?
#' @param gr_func function to apply to group labels, useful for dates or
#'   numbers. Set this to as.numeric or as.Date when your groups are
#'   numbers/dates respectively
#' @param ...
#'
#' @return a dataframe for every row in original data along with control chart
#'   calculations
#' @export
#'
#' @examples
#' library(broom)
#'
#' data("pistonrings", package = 'qcc')
#' diameter <- qcc::qcc.groups(pistonrings$diameter, pistonrings$sample)
#' qcc_obj <- qcc::qcc(diameter[1:25,], type="xbar")
#' augment(qcc_obj)
#'
#' qcc_new <-  qcc::qcc(diameter[1:25,], newdata = diameter[24:40,], type="xbar")
#' augment(qcc_new)
augment.qcc <- function(x, include_center = TRUE, include_stddev = FALSE, gr_func = I, ...){
  
  if(!(x$type %in% c("xbar", "p", "np", "u")))
    warning("Control Chart Type ", x$type, " not directly supported by augment.qcc, use with care!")
  
  variable_limits <- nrow(x$limits)!=1
  
  calibrate_data <- data.frame(
    group = names(x$statistics),
    statistics = as.numeric(x$statistics),
    type = "calibrate",
    data_name = x$data.name,
    sizes = x$sizes,
    stringsAsFactors = FALSE
  )
  
  
  if(!is.null(x$newdata)){
    new_data <- data.frame(
      group = names(x$newstats),
      statistics = as.numeric(x$newstats),
      type = "newdata",
      data_name = x$newdata.name,
      sizes = x$newsizes,
      stringsAsFactors = FALSE
    )
    
    all_data <- rbind(calibrate_data, new_data)
    
  }else{
    all_data <- calibrate_data
  }
  
  all_data <- cbind(all_data, x$limits)
  all_data
  
  if(include_center) all_data$center <- x$center
  if(include_stddev) all_data$stddev <- x$std.dev
  
  
  all_data$violations <- "None"
  all_data$violations[x$violations$violating.runs] <- "Violating Run"
  all_data$violations[x$violations$beyond.limits] <- "Beyond Limits"
  
  # make sure all possibilites are available so that level order is predictable
  all_data$violations <- factor(all_data$violations, levels = c("None", "Violating Run", "Beyond Limits"))
  
  all_data$group <- gr_func(all_data$group)
  
  all_data
  
}

#' plot qcc using ggplot
#'
#' @param x a qcc object
#' @param add.stats currently not used
#' @param chart.all currently not used
#' @inheritParams qcc::plot.qcc
#' @inheritParams augment.qcc
#' @param ... currently not used
#'
#' @return a ggplot2 plot object
#' @export
#' @import ggplot2
#' @examples
#'
#' data("pistonrings", package = 'qcc')
#' diameter <- qcc::qcc.groups(pistonrings$diameter, pistonrings$sample)
#' qcc_obj <- qcc::qcc(diameter[1:25,], type="xbar")
#' plot_gg.qcc(qcc_obj)
#'
#' qcc_new <-  qcc::qcc(diameter[1:25,], newdata = diameter[24:40,], type="xbar")
#' plot_gg.qcc(qcc_new)
#'
plot_gg.qcc <- function(x, add.stats = TRUE, chart.all = TRUE,
                        label.limits = c("LCL ", "UCL"),
                        gr_func = as.numeric,
                        ...){
  
  augmented <- augment.qcc(x, include_center = FALSE, include_stddev = FALSE, gr_func = gr_func)
  
  calibrated <- length(unique(augmented$data_name)) == 2
  if(calibrated){
    calibration_break <- with(augmented, max(group[type=="calibrate"]))
  }
  min_grp <- min(augmented$group)
  max_grp <- max(augmented$group)
  first_lcl <- augmented$LCL[1]
  first_ucl <- augmented$UCL[1]
  
  
  g_title <- paste0(x$type, " Chart for ", x$data.name)
  
  p <- ggplot(augmented, aes(x = group, y = statistics)) +
    geom_line() +
    geom_step(aes(y = UCL), linetype = "longdash") +
    geom_step(aes(y = LCL), linetype = "longdash") +
    geom_hline(yintercept = x$center) +
    geom_point(size = 2, aes(color = violations)) +
    scale_color_manual(breaks = c("None", 'Violating Run', 'Beyond Limits'), values = c("black", "yellow", "red")) +
    labs(
      title = g_title
    )
  
  
  if(calibrated){
    p <- p + geom_vline(xintercept = as.numeric(calibration_break), linetype = "dotted")
  }
  
  
  p <- p +
    geom_text(
      x = as.numeric(min_grp),
      y = first_ucl,
      label = label.limits[2],
      hjust = -.1,
      vjust = 1.2,
      color = "grey40"
    ) +
    geom_text(
      x = as.numeric(min_grp),
      y = first_lcl,
      label = label.limits[1],
      hjust = -.1,
      vjust = -.5,
      color = "grey40"
    )
  
  p
  
}


#' Formula interface to qcc
#'
#' This function provides a simplified interface to the qcc function. The main
#' goal is to remove the need for using the qcc groups function. This allows for
#' much simpler use of the function within piping functions/map.
#'
#'
#' @param formula a formula describing the outcome variable and the grouping
#'   variable
#' @param data a data frame
#' @param cutoff a cutoff date for new data
#' @param ... further arguments passed to \code{qcc:qcc()}
#' @inheritParams qcc::qcc
#' @return a qcc object
#' @export
#'
#' @examples
#' data("pistonrings", package = 'qcc')
#' diameter <- qcc::qcc.groups(pistonrings$diameter, pistonrings$sample)
#'
#' # Without Cutoff
#' qcc_obj <- qcc::qcc(diameter[1:25,], type="xbar")
#' qcc_obj2 <- qcc.formula(diameter ~ sample, data = dplyr::filter(pistonrings, sample <= 25), type = "xbar", plot = TRUE)
#'
#' # With Cutoff
#' qcc_new <-  qcc::qcc(diameter[1:25,], newdata = diameter[24:40,], type="xbar")
#' qcc_new2 <- qcc.formula(diameter ~ sample, data = pistonrings, type = "xbar", plot = TRUE, cutoff = 25)
#'
#' # Dates and p chart
#' ## use POSIXct, not Date object to create date
#' start <- as.POSIXct('2017-01-01')
#' end <- as.POSIXct('2017-01-15')
#' samp_dates <- seq(from = start, to = end, length.out = 15)
#' samp_dates <- sample(samp_dates, 100, replace = TRUE)
#' old_process <- samp_dates < '2017-01-08'
#' vals <- ifelse(old_process,rbinom(sum(old_process), 1 , p = 0.5), rbinom(sum(!old_process), 1,  p = 0.2) )
#' d <- data.frame(samp_dates, vals)
#'
#' ## Run the graphs with plot = TRUE
#' qcc.formula(vals ~ samp_dates, data = d, type = "p", plot = TRUE)
#' qcc.formula(vals ~ samp_dates, data = d, type = "p", plot = TRUE, cutoff = '2017-01-08')
qcc.formula <- function(formula, data, type, plot = FALSE, cutoff = NULL, ...){
  data <- as.data.frame(data)
  outcome_name <- formula.tools::lhs(formula)
  group_name <- formula.tools::rhs(formula)
  
  outcome <- eval(outcome_name, envir = data)
  group <- eval(group_name, envir =  data)
  
  # standardize group to be a POSIXct
  if(inherits(group, "Date") | inherits(group, "POSIXct")) {
    group <- as.POSIXct(group)
    if(!is.null(cutoff)) cutoff <- as.POSIXct(cutoff)
  }
  
  groups <- qcc::qcc.groups(outcome, group)
  
  if(type %in% c("p", "np", "u")){
    
    data_qcc <- apply(groups, 1, sum, na.rm = TRUE)
    size_qcc <- apply(groups, 1, function(x) sum(!is.na(x)))
  }else{
    data_qcc <- groups
    size_qcc <- apply(groups, 1, function(x) sum(!is.na(x)))
  }
  
  if(!is.null(cutoff)){
    group_names <- names(data_qcc)
    if(inherits(cutoff, "POSIXct")){
      group_names <- as.POSIXct(group_names)
    }else{
      if(is.null(group_names)) group_names <- 1:nrow(data_qcc)
      else group_names <- as.numeric(group_names)
    }
    
    if(all(is.na(group_names))){
      stop("the group names could not be converted to either a number or date")
    }
    
    
    sel <- group_names < cutoff
    
  }
  
  if(!is.null(cutoff)){
    
    if(is.matrix(data_qcc)){
      calib <- data_qcc[sel, ]
      newdata <- data_qcc[!sel, ]
      
    }else{
      calib <- data_qcc[sel]
      newdata <- data_qcc[!sel]
    }
    
    qcc_obj <- qcc::qcc(data = calib,
                        sizes = size_qcc[sel],
                        type = type,
                        plot = plot,
                        newdata = newdata,
                        newsizes = size_qcc[!sel],
                        ...)
    
  }else{
    qcc_obj <- qcc::qcc(data = data_qcc,
                        sizes = size_qcc,
                        type = type,
                        plot = plot,
                        ...)
  }
  
  qcc_obj
  
  
}


