

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

plot_distributions <- function(data) {
  data %>% ggplot(aes(value)) +
    geom_histogram() +
    facet_wrap(vars(metabolite), scales = "free")
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






#' Transformation recipe to pre-process the data
#'
#' @param data  metabolites
#' @param metabolite_variable
#'
#' @return

create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role({{ metabolite_variable }},
      age, gender,
      new_role = "predictor"
    ) %>%
    recipes::update_role(class, new_role = "outcome") %>%
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}



#' workflow object of model and transformations
#'
#' @param model_spec  the model specifications
#' @param recipes_specs recipes specifications
#'
#' @return
create_model_workflow <- function(model_spec, recipes_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(recipes_specs)
}


#' Create tidy output of model results
#'
#' @param workflow_fitted_model  model object fitted
#'
#' @return tidy df

tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy(exponentiate = TRUE)
}

#' convert to a list of data frames
#'
#' @param data lipidomics
#'
#' @returnlist of data frames

split_by_metabolite <- function(data) {
  data %>%
    column_values_to_snake_case(metabolite) %>%
    dplyr::group_split(metabolite) %>%
    purrr::map(metabolites_to_wider)
}


#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
generate_model_results <- function(data) {
    create_model_workflow(
        parsnip::logistic_reg() %>%
            parsnip::set_engine("glm"),
        data %>%
            create_recipe_spec(tidyselect::starts_with("metabolite_"))
    ) %>%
        parsnip::fit(data) %>%
        tidy_model_output()
}




#' Add the original metabolite names (not as snakecase) to the model results.
#'
#' @param model_results The data frame with the model results.
#' @param data The original, unprocessed lipidomics dataset.
#'
#' @return A data frame.
#'
add_original_metabolite_names <- function(model_results, data) {
    data %>%
        dplyr::mutate(term = metabolite) %>%
        column_values_to_snake_case(term) %>%
        dplyr::mutate(term = stringr::str_c("metabolite_", term)) %>%
        dplyr::distinct(term, metabolite) %>%
        dplyr::right_join(model_results, by = "term")
}


#' Calculate the estimates for the model for each metabolite.
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
calculate_estimates <- function(data) {
    data %>%
        column_values_to_snake_case(metabolite) %>%
        dplyr::group_split(metabolite) %>%
        purrr::map(metabolites_to_wider) %>%
        purrr::map(generate_model_results) %>%
        purrr::list_rbind() %>%
        dplyr::filter(stringr::str_detect(term, "metabolite_")) %>%
        add_original_metabolite_names(data)
}



