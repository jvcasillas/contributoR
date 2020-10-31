#' Build contributor plot
#'
#' This function takes a list and generates a contributions plot.
#'
#' @param contributions A list object (defaults to NULL)
#' @keywords plot
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import forcats
#' @importFrom rlang .data
#' @export
#' @examples
#' # Create example list
#' ex_1 <- list(
#'  JVC = 1:13,
#'  JGP = 1,
#'  NR  = c(2, 4))
#'
#' # Plot contributions
#' contributor(contributions = ex_1)
#'
#' # Build list inside function
#' contributor(
#'   contributions = list(
#'   "Author 1" = seq(1, 14, by = 3),
#'   "Author 2" = c(1, 3, 5, 7, 9, 13),
#'   "Lazy person" = NA,
#'   "Advisor" = 12)
#'   )

contributor <- function(contributions = NULL) {

  contributions_df <- contributor_df_builder(contributions)

  contributor_plot <-
    ggplot(contributions_df) +
    aes(x = .data$contributors, y = .data$contributor_roles) +
    geom_point(aes(alpha = if_else(.data$val == 1, 1, 0)),
      size = 3.5, pch = 18, show.legend = F) +
    scale_x_discrete(position = "top") +
    labs(x = "Contributors", y = "Contributor roles",
         caption = "Author contributions based on CRediT taxonomy") +
    theme_minimal(base_size = 12, base_family = "Times") +
    theme(
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9), hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.15),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.15))

  return(contributor_plot)
}

