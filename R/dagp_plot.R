#' Plot posterior distribution of latent parameters in \code{causact_graph} model.
#'
#' The graph object should be of class \code{causact_graph} and created using \code{dag_create()}.
#' @param drawsDataFrame one of the two posterior data frame objects created by \code{dag_greta()}.  If \code{drawsDataFrame=drawsDF}, each parameter gets its own density plot.  If \code{drawsDataFrame=tidyDrawsDF} (recommended usage), parameters are grouped into facets based on whether they share the same prior or not.  10 and 90 percent credible intervals are displayed for the posterior distributions.
#' @return a credible interval plot of all latent posterior distribution parameters.
#' @examples
#' # Create a 2 node graph
#' graph2 = dag_create() %>%
#'   dag_node("Get Card","y",
#'          rhs = bernoulli(theta),
#'          data = carModelDF$getCard) %>%
#'   dag_node(descr = "Card Probability by Car",label = "theta",
#'            rhs = beta(2,2),
#'            child = "y")
#' graph2 %>% dag_render()
#' drawsDF %>% dag_plot()
#' tidyDrawsDF %>% dag_plot()
#'
#' # multiple plate example
#' library(dplyr)
#' poolTimeGymDF = gymDF %>%
#' mutate(stretchType = ifelse(yogaStretch == 1,
#'                             "Yoga Stretch",
#'                             "Traditional")) %>%
#' group_by(gymID,stretchType,yogaStretch) %>%
#'   summarize(nTrialCustomers = sum(nTrialCustomers),
#'             nSigned = sum(nSigned))
#' graph = dag_create() %>%
#'   dag_node("Cust Signed","k",
#'            rhs = binomial(n,p),
#'            data = poolTimeGymDF$nSigned) %>%
#'   dag_node("Probability of Signing","p",
#'            rhs = beta(2,2),
#'            child = "k") %>%
#'   dag_node("Trial Size","n",
#'            data = poolTimeGymDF$nTrialCustomers,
#'            child = "k") %>%
#'   dag_plate("Yoga Stretch","x",
#'             nodeLabels = c("p"),
#'             data = poolTimeGymDF$stretchType,
#'             addDataNode = TRUE) %>%
#'   dag_plate("Observation","i",
#'             nodeLabels = c("x","k","n")) %>%
#'   dag_plate("Gym","j",
#'             nodeLabels = "p",
#'             data = poolTimeGymDF$gymID,
#'             addDataNode = TRUE)
#' graph %>% dag_render()
#' graph %>% dag_greta(mcmc=TRUE)
#' tidyDrawsDF %>% dagp_plot()
#' @importFrom dplyr bind_rows filter
#' @importFrom rlang is_empty UQ enexpr enquo expr_text quo_name eval_tidy
#' @export
#' @import ggplot2 tidyr
#' @importFrom cowplot plot_grid
#' @export

dagp_plot = function(drawsDataFrame,alphaSort = FALSE) { # case where untidy posterior draws are provided
  if (!("priorGroup" %in% colnames(drawsDataFrame))) {
    plot = drawsDataFrame %>% gather() %>%
      ggplot(aes(x = value, y = ..scaled..)) +
      geom_density(aes(fill = key)) +
      facet_wrap( ~ key, scales = "free_x") +
      theme_minimal() +
      theme(legend.position = "none")

    plot
  } else { # case where tidy posterior draws are provided
    plotList = list()
    ## filter out NA's like from LKJ prior (we do not know how to plot this)
    drawsDataFrame = drawsDataFrame %>% dplyr::filter(!is.na(priorGroup))
    priorGroups = unique(drawsDataFrame$priorGroup)
    numPriorGroups = length(priorGroups)
    for (i in 1:numPriorGroups) {
      df = drawsDataFrame %>% filter(priorGroup == priorGroups[i])
      # df$key = factor(df$key)
      # ## reorder levels using character sort from stringr
      # if (alphaSort == FALSE) {
      #   df$key = forcats::fct_reorder(df$key,df$value)
      # } else {
      #   df$key = forcats::fct_relevel(df$key,
      #                                 stringr::str_sort(levels(df$key),
      #                                                   numeric = TRUE,
      #                                                   decreasing = TRUE))
      # }

      plotList[[i]] = df %>% group_by(key) %>%
        summarize(q05 = quantile(value,0.05),
                  q25 = quantile(value,0.55),
                  q45 = quantile(value,0.45),
                  q50 = quantile(value,0.50),
                  q55 = quantile(value,0.55),
                  q75 = quantile(value,0.75),
                  q95 = quantile(value,0.95)) %>%
        mutate(credIQR = q75 - q25) %>%
        mutate(reasonableIntervalWidth = 1.5 * quantile(credIQR,0.75)) %>%
        mutate(alphaLevel = ifelse(credIQR > reasonableIntervalWidth, 0.3,1)) %>%
        arrange(alphaLevel,q50) %>%
        mutate(key = factor(key, levels = key)) %>%
        ggplot(aes(y = key, yend = key)) +
        geom_segment(aes(x = q05, xend = q95, alpha = alphaLevel), size = 4, color = "#5f9ea0") +
        geom_segment(aes(x = q45, xend = q55, alpha = alphaLevel), size = 4, color = "#11114e") +
        scale_alpha_continuous(range = c(0.6,1))  +
        guides(alpha = FALSE) +
        theme_minimal(12) +
        labs(y = element_blank(),
             x = "parameter value",
             caption = ifelse(i == numPriorGroups,"Credible Intervals - 10% (dark) & 90% (light)",""))

    }

    nCol <- ifelse(numPriorGroups==1,1,floor(1 + sqrt(numPriorGroups)))
    cowplot::plot_grid(plotlist = plotList, ncol = nCol)
  }
}
