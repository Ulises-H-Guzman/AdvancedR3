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


#' Title
#'
#' @param data
#'
#' @return plot of distributions

plot_distributions <- function(data){
    data %>% ggplot(aes(value)) + geom_histogram()+
        facet_wrap(vars(metabolite),scales = "free")
}




#' Column values string to snake_case
#'
#' @param data with string columns
#' @param cols the column to convert into snakecase
#'
#' @return a data frame

column_values_to_snake_case <- function(data, cols) {
    data %>%
        dplyr::mutate(dplyr::across({{ cols }}, snakecase::to_snake_case))
}




#' Pivot to wider
#'
#' @param data lipidomics
#'
#' @return wider data_frame

metabolites_to_wider <- function(data) {
    data %>%
        tidyr::pivot_wider(
            names_from = metabolite,
            values_from = value,
            values_fn = mean,
            names_prefix = "metabolite_"
        )
}
