GGscatterPlot <- function(data, mapping, ...,
                          method = "spearman") {

  #Get correlation coefficient
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)

  cor <- cor(x, y, method = method)
  #Assemble data frame
  df <- data.frame(x = x, y = y)
  # PCA
  nonNull <- x!=0 & y!=0
  dfpc <- prcomp(~x+y, df[nonNull,])
  df$cols <- predict(dfpc, df)[,1]
  # Define the direction of color range based on PC1 orientation:
  dfsum <- x+y
  colDirection <- ifelse(dfsum[which.max(df$cols)] <
                           dfsum[which.min(df$cols)],
                         1,
                         -1)
  #Get 2D density for alpha
  dens2D <- MASS::kde2d(df$x, df$y)
  df$density <- fields::interp.surface(dens2D ,
                                       df[,c("x", "y")])

  if (any(df$density==0)) {
    mini2D = min(df$density[df$density!=0]) #smallest non zero value
    df$density[df$density==0] <- mini2D
  }
  #Prepare plot
  pp <- ggplot(df, aes(x=x, y=y, color = cols, alpha = 1/density)) +
    ggplot2::geom_point(shape=16, show.legend = FALSE) +
    ggplot2::scale_color_viridis_c(direction = colDirection) +
    #                scale_color_gradient(low = "#0091ff", high = "#f0650e") +
    ggplot2::scale_alpha(range = c(.05, .6)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, col="darkred") +
    ggplot2::geom_label(
      data = data.frame(
        xlabel = min(x, na.rm = TRUE),
        ylabel = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)),
      mapping = ggplot2::aes(x = xlabel,
                             y = ylabel,
                             label = lab),
      hjust = 0, vjust = 1,
      size = 3, fontface = "bold",
      inherit.aes = FALSE # do not inherit anything from the ...
    ) +
    theme_minimal()

  return(pp)
}
