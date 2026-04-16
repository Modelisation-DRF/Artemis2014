# Ce fichier sert à centraliser les imports du package
#' @importFrom dplyr n_distinct reframe vars join_by filter select mutate summarise
#'              group_by rename inner_join left_join bind_rows bind_cols slice
#'              ungroup full_join arrange case_when row_number summarise_all relocate mutate_at
#'              first last between
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider expand_grid separate_wider_delim nest unnest separate
#' @importFrom purrr map is_empty
#' @importFrom stringr str_to_lower str_sub
#' @importFrom ggplot2 ggplot
#' @importFrom data.table as.data.table setDT setnames copy setorder
#' @import doFuture
#' @import doRNG
#' @import future
#' @import foreach
#' @keywords internal
"_PACKAGE"
