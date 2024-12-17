#' Print CRediT taxonomy in prose
#'
#' This function takes a contributor dataframe and converts it to prose.
#'
#' @param contributor_df A dataframe built from contributor_df_builder
#' @keywords print
#' @import dplyr
#' @import stringr
#' @import glue
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
#' contributor_df_builder(ex_1) |>
#' contribution_to_text()

contribution_to_text <- function(contributor_df) {
  out <- contributor_df |>
    dplyr::filter(.data = _, .data$val == 1) |>
    group_by(.data = _, contributors) |>
    summarise(
      .data = _,
      contributor_roles = paste(unique(.data$contributor_roles), collapse = ', ')
    ) |>
    transmute(.data = _, out = glue("{contributors}: {contributor_roles}")) |>
    pull(.data = _, out) |>
    as.vector(x = _) |>
    str_flatten(string = _, collapse = "; ") |>
    (\(x) sprintf("%s.", x))()
  return(out)
}
