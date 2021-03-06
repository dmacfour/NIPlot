#' NIPlot
#'
#' This function allows you to make non-inferiority plots.
#' @param mean sample mean
#' @param lbound CI lower bound
#' @param ubound CI upper bound
#' @param delta non-inferiority margin
#' @param title Optional title
#' @param subtitle optional subtitle
#' @param filename optional filename - saves to current directory
#' @keywords non-inferiority
#' @export
#' @import ggplot2
#' @examples
#' NIPLOT()


NIPLOT <- function(mean,
                   lbound,
                   ubound,
                   title = "Non-Inferiority Plot",
                   subtitle = expression(paste(mu,"1 - ",mu,"2")),
                   delta = 1,
                   limit = 3,
                   filename = NULL,
                   guides = TRUE,
                   guides_direction = "positive",
                   text_size = 20){

  limit <- delta*3
  bound <- ubound
  if(abs(lbound) > ubound) bound <- abs(lbound)
  if(abs(bound) > limit) limit <- abs(bound)
  delta <- abs(delta)
  if(guides_direction == "negative"){
  nip <- ggplot() + geom_pointrange (x=0.0, y= mean, aes(ymin = lbound, ymax = ubound), size=1.0, color="red")
  nip <- nip + theme_classic()
  if(!((guides != TRUE & guides == FALSE) | (guides == TRUE & guides != FALSE))){
    warning("guides must equal TRUE or FALSE. Defaulting to TRUE")
    guides <- TRUE
  }
  if(guides == TRUE){
    nip <- nip + geom_segment(aes(x=2, xend=2, y=-(delta - delta*.999), yend=-limit), size=1.25, color="dodgerblue2", arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
    nip <- nip + geom_segment(aes(x=1.75, xend=1.75, y=-(delta*.99), yend=(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.5, xend=1.5, y=(delta*.99), yend=-limit), size=1.25, color="dodgerblue2", arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
    nip <- nip + geom_segment(aes(x=2.05, xend=1.95, y=-(delta - delta*.99), yend=-(delta - delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.80, xend=1.7, y=-(delta*.99), yend=-(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.8, xend=1.7, y=(delta*.99), yend=(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.55, xend=1.45, y=(delta*.99), yend=(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + annotate(geom = "text", x = 1.97, y = (delta*2), label = "Superiority", color="dodgerblue2", size=text_size/4)
    nip <- nip + annotate(geom = "text", x = 1.75, y = (delta*2), label = "  Equivalence", color="dodgerblue2", size=text_size/4)
    nip <- nip + annotate(geom = "text", x = 1.53, y = (delta*2), label = "    Non-inferiority", color="dodgerblue2", size=text_size/4)
  } else if(guides == FALSE) {

  }
  nip <- nip + geom_segment(aes(x=-2, xend=2.05, y=delta, yend=delta), linetype=2, size=1)
  nip <- nip + geom_segment(aes(x=-2, xend=2.05, y=-delta, yend=-delta), linetype=2, size=1)
  nip <- nip + geom_segment(aes(x=-2, xend=2.05, y=0, yend=.0), size=1)
  nip <- nip + theme(plot.title = element_text(size=20))
  nip <- nip + theme(axis.title = element_text(size=20))
  nip <- nip + theme(axis.text = element_text(size=20))
  nip <- nip + theme(legend.position = "none")
  nip <- nip + scale_y_continuous (limits = c(-limit, limit), expand = c(0, 0), breaks=c(-delta, 0.0, delta))
  nip <- nip + scale_x_continuous (limits = c(-2, 2.25), expand = c(0, 0))
  nip <- nip + theme(axis.title.x = element_text(size = text_size), axis.text.x = element_text(size = text_size*.75), axis.ticks.x = element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),plot.title = element_text(size = text_size))
  nip <- nip + labs(y=subtitle)
  nip <- nip + ggtitle(title)
  nip <- nip + coord_flip()
  nip <- nip + theme(plot.title = element_text(hjust = 0.5))
  if(!is.null(filename)) nip <- nip + ggsave(filename, width = 25, height = 25, units = "cm")
  }
  if(guides_direction == "positive"){
  nip <- ggplot() + geom_pointrange (x=0.0, y= mean, aes(ymin = lbound, ymax = ubound), size=1.0, color="red")
  nip <- nip + theme_classic()
  if(!((guides != TRUE & guides == FALSE) | (guides == TRUE & guides != FALSE))){
    warning("guides must equal TRUE or FALSE. Defaulting to TRUE")
    guides <- TRUE
  }
  if(guides == TRUE){
    nip <- nip + geom_segment(aes(x=2, xend=2, y=(delta - delta*.999), yend=limit), size=1.25, color="dodgerblue2", arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
    nip <- nip + geom_segment(aes(x=1.75, xend=1.75, y=-(delta*.99), yend=(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.5, xend=1.5, y=-(delta*.99), yend=limit), size=1.25, color="dodgerblue2", arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
    nip <- nip + geom_segment(aes(x=2.05, xend=1.95, y=(delta - delta*.999), yend=(delta - delta*.999)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.80, xend=1.7, y=-(delta*.99), yend=-(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.8, xend=1.7, y=(delta*.99), yend=(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + geom_segment(aes(x=1.55, xend=1.45, y=-(delta*.99), yend=-(delta*.99)), size=1.25, color="dodgerblue2")
    nip <- nip + annotate(geom = "text", x = 1.97, y = -(delta*2), label = "Superiority", color="dodgerblue2", size=text_size/4)
    nip <- nip + annotate(geom = "text", x = 1.75, y = -(delta*2), label = "  Equivalence", color="dodgerblue2", size=text_size/4)
    nip <- nip + annotate(geom = "text", x = 1.53, y = -(delta*2), label = "    Non-inferiority", color="dodgerblue2", size=text_size/4)
  } else if(guides == FALSE) {

  }
  nip <- nip + geom_segment(aes(x=-2, xend=2.05, y=delta, yend=delta), linetype=2, size=1)
  nip <- nip + geom_segment(aes(x=-2, xend=2.05, y=-delta, yend=-delta), linetype=2, size=1)
  nip <- nip + geom_segment(aes(x=-2, xend=2.05, y=0, yend=.0), size=1)
  nip <- nip + theme(plot.title = element_text(size=20))
  nip <- nip + theme(axis.title = element_text(size=20))
  nip <- nip + theme(axis.text = element_text(size=20))
  nip <- nip + theme(legend.position = "none")
  nip <- nip + scale_y_continuous (limits = c(-limit, limit), expand = c(0, 0), breaks=c(-delta, 0.0, delta))
  nip <- nip + scale_x_continuous (limits = c(-2, 2.25), expand = c(0, 0))
  nip <- nip + theme(axis.title.x = element_text(size = text_size), axis.text.x = element_text(size = text_size*.75), axis.ticks.x = element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),plot.title = element_text(size = text_size))
  nip <- nip + labs(y=subtitle)
  nip <- nip + ggtitle(title)
  nip <- nip + coord_flip()
  nip <- nip + theme(plot.title = element_text(hjust = 0.5))
  if(!is.null(filename)) nip <- nip + ggsave(filename, width = 25, height = 25, units = "cm")
  }
  return(nip)
}
