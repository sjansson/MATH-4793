#' Plot PCA
#'
#'
#'
#' This function creates Creates a screeplot to visualize principal components
#' @param b an object of class "PCA"
#'
#' @return a screeplot
#' @import ggplot2
#' @export

plot.PCA <- function(b){ ## PCA plotting methods function
  x <- data.frame(colnames(b$Loadings), b$Loadings[2, ])
  ggplot(data = x, aes(x = x[,1],y=  x[, 2] )) +
    geom_bar(stat="identity", fill="#00AFBB") +
    geom_text(aes(label = round(x[, 2], 4), vjust = -0.3, size = 3.5)) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(0, 1) +
    ggtitle("Principal Components") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    ylab("Proportion of Total Variance")
}

