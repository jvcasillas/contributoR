#' Build contributor dataframe
#'
#' This function takes a list and generates a dataframe that can be sent to
#' the contributor plotting function
#'
#' @param contributions A list object (defaults to NULL)
#' @keywords dataframe
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom rlang .data
#' @export
#' @examples
#' ex_vec <- c("JVC", "JGP", "NR")
#' ex_0 <- list(
#'  "JVC",
#'  "JGP",
#'  "NR")
#' ex_1 <- list(
#'  JVC = 1:13,
#'  JGP = 1,
#'  NR  = c(2, 4))
#'
#' # Simple example
#' contributor_df_builder(ex_1)

contributor_df_builder <- function(contributions = NULL) {

  # Make sure input is a list
  if(is.list(contributions) == FALSE) {
    stop("Hold up! The argument 'contributions' requires a list.")
  }

  # Vector of contributor roles
  contributor_roles <- c(
  "Conceptualization", "Methodology", "Software", "Validation",
  "Formal analysis", "Investigation", "Resources", "Data curation",
  "Writing - original draft preparation", "Writing - review and editing",
  "Visualization", "Supervision", "Project Administration",
  "Funding acquisition"
  )

  # Make sure list includes integer or numeric vectors
  if(is.null(names(contributions))) {
    stop("Hold up! Your list must contain integer or numeric vectors.")
  } else {
    contributors = names(contributions)
  }

  # Build base dataframe
  df_base <- expand_grid(contributors, contributor_roles)

  # Default to 0 for everybody, add ordings columns
  df_built <- mutate(df_base, val = 0) %>%
        group_by(.data$contributor_roles) %>%
        mutate(order_x = seq_along(.data$contributors)) %>%
        group_by(.data$contributors) %>%
        mutate(order_y = seq_along(.data$contributor_roles)) %>%
        ungroup

  # Add contributions from list input and order based on CRedit taxonomy order
  # as well as order in which contributors included in list
  df_out <- enframe(contributions, name = 'contributors') %>%
    right_join(df_built, by = "contributors") %>%
    group_by(.data$contributors) %>%
    mutate(val = +(.data$order_y %in% unlist(.data$value[[1]]))) %>%
    ungroup() %>%
    select(-.data$value) %>%
    mutate(
      contributor_roles = fct_reorder2(.data$contributor_roles,
                                       .data$order_y, .data$order_y),
      contributors = fct_reorder(.data$contributors, .data$order_x, min))

  return(df_out)

}
