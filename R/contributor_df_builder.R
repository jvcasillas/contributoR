#' Build contributor dataframe
#'
#' This function takes a list and generates a dataframe that can be sent to
#' the contributor plotting function
#'
#' @param contributions A list object (defaults to NULL)
#' @param weight (logical) Variable indicating whether or not to include
#' contribution weighting in the dataframe. Defaults to FALSE.
#' @param ... Pass extra parameters
#' @keywords dataframe
#' @import dplyr
#' @import tidyr
#' @import tibble
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
#' contributor_df_builder(ex_1)
#'
#' # A list containing tibbles with 'role' and 'weight' defined
#' ex_2 <- list(
#'  p1 = tibble::tibble(role = 1:3, weight = "low"),
#'  p2 = tibble::tibble(role = 3:8, weight = "high"),
#'  p3 = tibble::tibble(role = 1:3, weight = "high"),
#'  p4 = tibble::tibble(role = 5:12, weight = rep(c("low", "high"), times = 4)))
#'
#' # Example with weights
#' contributor_df_builder(ex_2, weight = TRUE)

contributor_df_builder <- function(contributions = NULL, weight = FALSE, ...) {

  #
  # Fail checks
  #

  # Make sure input is a list
  if(is.list(contributions) == FALSE) {
    stop("Hold up! The argument 'contributions' requires a list.")
  }

  # Make sure weight included in list if weight == TRUE
  # Create enframe from list and unnest (reduces cols to contributors, role
  # weight), then use conditional statement to check in "weight" in column
  # in resulting dataframe
  unnested_hold <- enframe(contributions, name = 'contributors') %>%
    unnest(cols = .data$value)
  if(weight == TRUE & "weight" %in% colnames(unnested_hold) == FALSE) {
    stop("Hold up! You want weights but didn't include any in your list.
          Add a column 'weight' for this to work.")
  }

  # Make sure list includes integer or numeric vectors
  if(is.null(names(contributions))) {
    stop("Hold up! Your list must contain integer or numeric vectors.")
  } else {
    contributors = names(contributions)
  }

  # Create a vector of contributor roles
  contributor_roles <- c(
    "Conceptualization", "Data curation", "Formal Analysis",
    "Funding acquisition", "Investigation", "Methodology",
    "Project administration", "Resources", "Software", "Supervision",
    "Validation", "Visualization", "Writing - original draft",
    "Writing - review & editing"
  )

  # Build base dataframe
  df_base <- expand_grid(contributors, contributor_roles)

  # Default to 0 for everybody, add ordering columns
  df_built <- mutate(df_base, val = 0, weight = NA) %>%
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

  # Add factor weights, if included
  if (weight == TRUE) {
    # Join with df_out by contributors and order_y, replace original object,
    # rename 'role' to 'order_y' in order to left_join,
    # and rename/reorder cols
    df_out <- left_join(df_out, rename(unnested_hold, order_y = .data$role),
      by = c("contributors", "order_y")) %>%
    select(.data$contributors, .data$contributor_roles, .data$val,
      weight = .data$weight.y, .data$order_x, .data$order_y)
  }

  return(df_out)

}
