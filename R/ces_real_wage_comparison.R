#' Plot Regional Real Wage Adjustments
#'
#' This function pulls regional CPI and statewide CES hourly wage data using `BLSloadR`,
#' calculates inflation-adjusted wages, and plots the rolling 3-month average percent
#' change for a target state against the 20th-80th percentile of all states.
#'
#' @param target_state Character. The name of the state to highlight (e.g., "Nevada").
#' @param out_dir Character. The base directory to save the plot. If `NULL` (default), the plot is not saved to disk.
#' @param width Numeric. Width of the saved plot. Default is 12.
#' @param height Numeric. Height of the saved plot. Default is 6.75.
#' @param dpi String or numeric. DPI for the saved plot. Default is "retina".
#'
#' @return A ggplot object displaying the wage trends.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import zoo
#' @import BLSloadR
plot_regional_real_wage <- function(target_state = "Nevada",
                                    out_dir = NULL,
                                    width = 12,
                                    height = 6.75,
                                    dpi = "retina") {

  # 1. Define BLS Regions
  # Source: https://www.bls.gov/cpi/regional-resources.htm
  west_region <- c("Alaska", "Hawaii", "California", "Arizona", "New Mexico", "Nevada", "Utah", "Colorado", "Oregon", "Idaho", "Wyoming", "Washington", "Montana")
  midwest_region <- c("North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota", "Iowa", "Missouri", "Wisconsin", "Illinois", "Indiana", "Michigan", "Ohio")
  ne_region <- c("Pennsylvania", "New Jersey", "New York", "Connecticut", "Rhode Island", "Massachusetts", "New Hampshire", "Vermont", "Maine")
  south_region <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", "Georgia", "West Virginia", "Virginia", "North Carolina", "South Carolina", "Maryland", "Delaware", "District of Columbia", "Puerto Rico", "Virgin Islands")

  # 2. Pull and Process CPI Data
  cpi_regional_dl <- BLSloadR::load_bls_dataset("cu", which_data = "all")$data |>
    dplyr::filter(area_name %in% c("West", "Midwest", "Northeast", "South"))

  cpi_regional <- cpi_regional_dl |>
    dplyr::filter(date >= "1990-01-01",
                  item_name == "All items",
                  period != "M13",
                  stringr::str_detect(period, "M")) |>
    dplyr::select(date, area_name, value) |>
    dplyr::rename("regional_cpi" = "value",
                  "region" = "area_name")

  # 3. Pull and Process CES Data
  ces_dl <- BLSloadR::get_ces()

  ces_data <- ces_dl |>
    dplyr::filter(area_name == "Statewide",
                  stringr::str_detect(data_type_text, "Hourly"),
                  !stringr::str_detect(data_type_text, "Production")) |>
    dplyr::mutate(region = dplyr::case_when(
      state_name %in% west_region ~ "West",
      state_name %in% midwest_region ~ "Midwest",
      state_name %in% ne_region ~ "Northeast",
      state_name %in% south_region ~ "South",
      .default = "Not detected"
    )) |>
    dplyr::left_join(cpi_regional, by = c("date", "region")) |>
    dplyr::group_by(state_name, industry_name) |>
    tidyr::fill(regional_cpi, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::mutate(adjusted_wage = value * (100 / regional_cpi)) |>
    dplyr::select(date, state_name, industry_name, value, adjusted_wage) |>
    dplyr::group_by(state_name, industry_name) |>
    dplyr::mutate(
      average_adjusted_wage_3m = zoo::rollapplyr(data = adjusted_wage, width = 3, FUN = mean, partial = TRUE),
      py_aaw = dplyr::lag(average_adjusted_wage_3m, 12),
      aaw_pct_change = average_adjusted_wage_3m / py_aaw - 1
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(date, industry_name) |>
    dplyr::mutate(
      pct_change_20 = stats::quantile(aaw_pct_change, 0.2, na.rm = TRUE),
      pct_change_80 = stats::quantile(aaw_pct_change, 0.8, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  # 4. Filter for Target State and Create Labels
  target_ces <- ces_data |>
    dplyr::filter(state_name == target_state) |>
    dplyr::mutate(
      label_text = dplyr::if_else(date == max(date),
                                  scales::percent(aaw_pct_change, accuracy = 0.1),
                                  NA_character_)
    )

  if (nrow(target_ces) == 0) {
    stop(paste("No data found for state:", target_state))
  }

  # 5. Build the Plot
  p <- ggplot2::ggplot(target_ces, ggplot2::aes(x = date)) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pct_change_20, ymax = pct_change_80), fill = "grey", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = aaw_pct_change, color = industry_name), linewidth = 1.2) +
    ggplot2::geom_label(ggplot2::aes(y = aaw_pct_change - 0.1, label = label_text, color = industry_name)) +
    ggplot2::facet_wrap(~industry_name) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = paste("Percent Change in Real Average Hourly Wage in", target_state, "vs. All States"),
      subtitle = "Rolling 3-month average, adjusted for CPI by region, grey areas 20-80 percentile",
      caption = "Data pulled from U.S. Bureau of Labor Statistics using BLSloadR",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  # 6. Save Plot (Optional)
  if (!is.null(out_dir)) {
    current_month <- format(max(target_ces$date), "%Y-%m")

    # Construct the file path dynamically
    save_path <- file.path(out_dir, current_month, target_state)
    filename <- file.path(save_path, "Hourly_Wages_Adjusted_for_CPI.png")

    ggplot2::ggsave(filename = filename,
                    plot = p,
                    width = width,
                    height = height,
                    dpi = dpi,
                    create.dir = TRUE)

    message("Plot saved to: ", filename)
  }

  # Return the plot object to the user's environment
  return(p)
}
