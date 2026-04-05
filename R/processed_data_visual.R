#' Visualize Water Quality Data
#'
#' This function generates a comprehensive set of visualizations for water quality analysis,
#' including pie chart, boxplots, histograms, and correlation matrix.
#'
#' @param df A data frame containing water quality indicators and a Potability column.
#'   The Potability column should be binary (0 = non-potable, 1 = potable).
#'
#' @return NULL. The function prints plots to the graphics device and returns nothing invisibly.
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_col geom_label coord_polar scale_fill_discrete
#' @importFrom ggplot2 theme_void ggtitle geom_boxplot labs theme_minimal
#' @importFrom ggplot2 geom_histogram theme_bw element_text
#' @importFrom GGally ggpairs wrap
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming df is your water quality data frame
#' processed_data_visual(df)
#' }
#'
#' @seealso \code{\link{Exploratory_Data_analysis}} for the EDA function


processed_data_visual = function(df){
  eda_results = Exploratory_Data_analysis(df)
  # pie chart on potability
  dist_df = eda_results$Target_Distribution
  ggplot(dist_df,aes(x="",y=Count,fill=Class))+
    geom_col()+
    geom_label(aes(label = Count),position = position_stack(vjust = 0.5),show.legend = FALSE)+
    coord_polar(theta = "y")+
    scale_fill_discrete(labels = c("Not Potable","Potable"))+
    theme_void()
  # Box plot for every data fields with class information
  indicators <- colnames(df)[colnames(df) != "Potability"]

  box_plot_indicator <- function(indicator) {
    p <- ggplot(df, aes(x = factor(Potability), y = .data[[indicator]],
                        fill = factor(Potability))) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = paste(indicator, "distribution: Potable vs Non-potable"),
        x = "Potability",
        y = indicator,
        fill = "Potability"
      ) +
      scale_fill_discrete(labels = c("Non-potable", "Potable")) +
      theme_minimal()

    return(p)
  }

  box_plots <- lapply(indicators, box_plot_indicator)
  for(p in box_plots) {
    print(p)
  }

  # distribution curves for every data field with class information
  hist_plot_indicator <- function(indicator) {
    p <- ggplot(df, aes(x = .data[[indicator]],
                        fill = factor(Potability))) +
      geom_histogram(alpha = 0.5,position = "identity") +
      labs(
        title = paste(indicator, "distribution: Potable vs Non-potable"),
        x = "Potability",
        y = indicator,
        fill = "Potability"
      ) +
      scale_fill_discrete(labels = c("Non-potable", "Potable"))

    return(p)
  }

  hist_plots <- lapply(indicators, hist_plot_indicator)
  for(p in hist_plots) {
    print(p)
  }

  # using ggally package for correlation analysis
  ggpairs(df,
          columns = 1:9,
          aes(color = factor(Potability), alpha = 0.5),
          title = "Water Quality Indicators: Potable vs Non-potable",
          lower = list(continuous = wrap("points", size = 0.8, alpha = 0.3)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.6)),
          upper = list(continuous = wrap("cor", size = 3, stars = FALSE)),
          legend = 5) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}
