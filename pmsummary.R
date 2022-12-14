#' The package creates a generalized summary of the variable (includes Histogram & Boxplot)
#' @export
#' @param x numeric variable
pm_summary <- function(x) {
  par(mfrow = c(1,2))
  # Boxplot
  boxplot(x, col = "steelblue1")
  # simple Histogram for the distribution
  hist(x, col = "steelblue1")
  par(mfrow = c(1,1))
  # showing the first 8 values of the variable
  head = head(x)
  # showing the quantiles
  quantile = quantile(x)
  # Summary statistics
  summary <- data.frame(min = min(x),
                        median = median(x),
                        mean = mean(x),
                        max = max(x))
  my_list <- list(summary, quantile, head)
  return(my_list)
}
