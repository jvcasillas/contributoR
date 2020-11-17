#' Build contributor plot
#'
#' This function takes a list and generates a contributions plot.
#'
#' @param contributions A list object (defaults to NULL)
#' @param weight (logical) Variable indicating whether or not to include
#' contribution weighting in the dataframe. Defaults to FALSE.
#' @param ... Pass extra parameters
#' @keywords plot
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import ggplot2
#' @import forcats
#' @importFrom rlang .data
#' @export
#' @examples
#' # A list containing named vectors
#' ex_1 <- list(
#'  JVC = 1:13,
#'  JGP = 1,
#'  NR  = c(2, 4))
#'
#' # Simple example
#' contributor(ex_1)
#'
#' # Build list inside function
#' contributor(
#'   contributions = list(
#'   "Author 1" = seq(1, 14, by = 3),
#'   "Author 2" = c(1, 3, 5, 7, 9, 13),
#'   "Lazy person" = NA,
#'   "Advisor" = 12)
#'   )
#'
#' # A list containing tibbles with 'role' and 'weight' defined
#' ex_2 <- list(
#'  p1 = tibble::tibble(role = 1:3, weight = "low"),
#'  p2 = tibble::tibble(role = 3:8, weight = "high"),
#'  p3 = tibble::tibble(role = 1:3, weight = "high"),
#'  p4 = tibble::tibble(role = 5:12, weight = rep(c("low", "high"), times = 4)))
#'
#' # Example with weights
#' contributor(ex_2, weight = TRUE)
#'
#' # The contributor function defaults to viridis colors if weight = TRUE.
#' # You can select which option and where the colors begin/end using
#' # 'option', 'begin', and 'end' arguments.
#' contributor(ex_2, weight = TRUE, begin = 0.4, end = 0.9)

contributor <- function(contributions = NULL, weight = FALSE, ...) {

  # Create contributions dataframe
  contributions_df <- contributor_df_builder(
    contributions = contributions, weight = weight, ...)

  # Get extras
  extras <- list(...)

  # Set plot defaults
  if ("option" %in% names(extras)) {
    viridis_col <- extras$option
  } else {
    viridis_col <- "A"
  }

  if ("begin" %in% names(extras)) {
    viridis_begin <- extras$begin
  } else {
    viridis_begin <- 0.2
  }

  if ("end" %in% names(extras)) {
    viridis_end <- extras$end
  } else {
    viridis_end <- 0.6
  }

  # Create contributor plot with weights
  if (weight == TRUE) {

    contributor_plot <-
      contributions_df %>%
      mutate(weight = coalesce(.data$weight, "omit")) %>%
      ggplot() +
      aes(x = .data$contributors, y = .data$contributor_roles,
        color = .data$weight, alpha = .data$val) +
      geom_point(size = 3.5, pch = 18, show.legend = F) +
      scale_x_discrete(position = "top") +
      scale_alpha_continuous(range = c(0.05, 1)) +
      scale_color_viridis_d(option = viridis_col, begin = viridis_begin,
        end = viridis_end) +
      labs(x = "Contributors", y = "Contributor roles",
           caption = "Author contributions based on CRediT taxonomy") +
      theme_minimal(base_size = 12, base_family = "Times") +
      theme(
        axis.title.y = element_text(size = rel(.9), hjust = 0.95),
        axis.title.x = element_text(size = rel(.9), hjust = 0.95),
        panel.grid.major = element_line(colour = 'grey90', size = 0.15),
        panel.grid.minor = element_line(colour = 'grey90', size = 0.15))

  } else {

    # Create contributor plot without weights
    contributor_plot <-
      ggplot(contributions_df) +
      aes(x = .data$contributors, y = .data$contributor_roles, alpha = .data$val) +
      geom_point(size = 3.5, pch = 18, show.legend = F) +
      scale_x_discrete(position = "top") +
      scale_alpha_continuous(range = c(0.05, 1)) +
      labs(x = "Contributors", y = "Contributor roles",
           caption = "Author contributions based on CRediT taxonomy") +
      theme_minimal(base_size = 12, base_family = "Times") +
      theme(
        axis.title.y = element_text(size = rel(.9), hjust = 0.95),
        axis.title.x = element_text(size = rel(.9), hjust = 0.95),
        panel.grid.major = element_line(colour = 'grey90', size = 0.15),
        panel.grid.minor = element_line(colour = 'grey90', size = 0.15))

  }

  return(contributor_plot)

}
