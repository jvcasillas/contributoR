#' Calculate individual contributions
#'
#' This function takes a list and calculates individual contributions as a
#' proportion of total work completed. The output is a dataframe.
#'
#' @param contributions A list object (defaults to NULL)
#' @keywords dataframe
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @export
#' @examples
#' ex <- list("p1" = c(1:13), "p2" = 1, "p3" = c(1, 3, 5, 9))
#' calculate_contributions(ex)

calculate_contributions <- function(contributions = NULL) {

  # Build contributor dataframe
  df_out <- contributor_df_builder(contributions) %>%
    # Group by contributors and add up contributions
    group_by(.data$contributors) %>%
    summarize(total_contributions = sum(.data$val), .groups = "drop") %>%
    # Divide individual totals by total contributions
    mutate(percent_contributed = .data$total_contributions /
                                  sum(.data$total_contributions))

  return(df_out)
}
