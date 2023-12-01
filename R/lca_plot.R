#' LCA-Plot
#'
#' This function creates a plot from an estimated latent class solution created with the package {poLCA}. It uses the "probs"-Variables of the object.
#' @param x the fitted object created by poLCA::poLCA()
#' @param ... other arguments to be passed to plot()
#' @return the LCA-plot
#' @export
#' @examples


lca_plot <- function(x) {

  # create an empty object to store data into
  probs_data <- NULL


  # Loop: Take the data out of the probs-variables from the poLCA-object and assign them to the probs_data-object
  for (i in 1:length(x$probs)) {
    probs_data <- rbind(probs_data, x$probs[[i]][, 2])
  }

  # if there is more than one class a matrix is created, otherwise it's only a vector and ncol(probs_data) results in NULL
  # therefore we check for that and stop the funtion if there is only one class
  # with a few tweaks you could change the function so it would plot single- and multiple-class solutions
  if (is.null(ncol(probs_data))) {
    stop("There has to be more than one class.")
  }

  if (ncol(probs_data) > 8) {
    stop("This function only supports up to eight classes.")
  }

  # create a vector of possible colors for the legend
  # as the current vector contains eight colors, the maximum number of classes possible is eight.
  col_vect <-
    c("red",
      "blue",
      "orange",
      "chartreuse4",
      "steelblue",
      "darkred")

  # create an empty plot
  plot(
    NULL,
    type = "n",
    ylim = c(0, 1),
    xlim = c(1, nrow(probs_data)),
    xaxt = "n",
    ylab = "Pr",
    xlab = "Items",
    main = "Class profiles",
    ...
  )

  # x-axis
  axis(1, at = 1:nrow(probs_data), labels = 1:nrow(probs_data))
  abline(v = 1:nrow(probs_data), col = "gray80")

  # loop: draw a line for every class
  for (i in 1:ncol(probs_data)) {
    lines(probs_data[, i], col = col_vect[i])
  }

  # create the legend
  legend(
    "topleft",
    pch = 16,
    col = col_vect[1:i],
    legend = paste("Class", 1:i)
  )

}
