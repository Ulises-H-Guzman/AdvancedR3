usethis::use_git(".Rbuildignore")

# Code chunk

#' Basic statistics
#'
#' @param data lipidomics
#'
#' @return A data.frame/tibble

descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(across(value, list(mean = mean, sd = sd))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 1)))

}
