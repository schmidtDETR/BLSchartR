#' Generate CES Employment Trend Report
#'
#' This function serves as a CES reporting tool for the \bold{BLSchartR} package.
#' It utilizes \code{BLSloadR::get_ces()} to fetch employment data, calculates
#' growth metrics, determines 5-year peaks, and outputs a formatted \code{gt} table.
#'
#' @param area_code Character. A 5-digit BLS area code. Defaults to "00000" (Statewide).
#' @param seasonal_filter Character. "S" for Seasonally Adjusted, "U" for Not Seasonally Adjusted. Defaults to "S".
#' @param return_type Character. One of "display" (returns table), "save" (saves HTML), or "both".
#' @param sectors_only Logical. If TRUE, filters the report to major high-level industry sectors.
#' @param root_path String. The base directory for report exports. Required if \code{return_type} is 'save' or 'both'.
#' @param bls_state String. Two-letter state abbreviation (e.g., "NV").
#' @param report_date Date/String. Optional. By default, the function will generate a report for the most recent date available.  When report_date is set, it filters the data to be less than or equal to report_date, provided as a string in format 'YYYY-MM-DD'.
#'
#' @return A \code{gt_tbl} object if \code{return_type} is "display" or "both".
#'   Saves an HTML file to \code{root_path} if \code{return_type} is "save" or "both".
#' @export
#'
#' @importFrom BLSloadR get_ces
#' @importFrom dplyr filter mutate select group_by arrange ungroup lag if_else
#' @importFrom lubridate ym years interval time_length
#' @importFrom tidyr fill
#' @importFrom zoo rollapplyr
#' @importFrom stringr str_to_lower str_length
#' @importFrom gt gt fmt_number fmt_percent cols_hide cols_label tab_header tab_spanner tab_style cell_text cells_body cell_fill tab_source_note gtsave
#' @importFrom gtExtras gt_plt_sparkline
#' @examples
#' \donttest{
#' # Basic display for Washington D.C. (Seasonally Adjusted)
#' generate_ces_report(bls_state = "DC", seasonal_filter = "S")
#'
#' # Display Las Vegas, Nevada report for major sectors only
#' generate_ces_report(
#'   bls_state = "NV",
#'   sectors_only = TRUE,
#'   area_code = "29820",
#'   seasonal_filter = "U",
#'   return_type = "display")
#' }
#' \dontrun{
#' # Save a report to a specific directory (root_path required for 'save')
#' generate_ces_report(
#'   bls_state = "NV",
#'   return_type = "save",
#'   root_path = "C:/Users/Reports"
#' )
#' }
generate_ces_report <- function(
    area_code = "00000",
    seasonal_filter = "S",
    return_type = "display",
    sectors_only = FALSE,
    root_path = NULL,
    bls_state = "NV",
    report_date = NULL
) {

  # 1. Argument Code Checks
  if (!is.character(area_code) | stringr::str_length(area_code) != 5) {
    stop("area_code must be a character string with length 5. '", area_code, "' is not valid.")
  }

  if (!(return_type %in% c("display", "save", "both"))) {
    stop("return_type must be 'display' to return the output, 'save' to save to a folder, or 'both'.")
  }

  if (return_type %in% c("save", "both")) {
    if (is.null(root_path)) {
      stop("Please provide a valid folder in which to save the output using the root_path argument.")
    }
  }

  # 2. Data Fetching via BLSloadR
  state_ces <- BLSloadR::get_ces(states = bls_state, simplify_table = FALSE) |>
    dplyr::filter(data_type_code == "01") |>
    dplyr::mutate(area_name = dplyr::if_else(area_code == "00000", state_name, area_name))

  # 3. Date Parsing
  state_ces <- state_ces |>
    dplyr::mutate(
      date = lubridate::ym(paste(year, period, sep = "-"))
    )

  # 4. Date Validation and Filtering
  if (!is.null(report_date)) {
    parsed_date <- as.Date(report_date)
    if (!is.na(parsed_date)) {
      state_ces <- state_ces |>
        dplyr::filter(date <= parsed_date)
    } else {
      stop("report_date is not a valid date format.")
    }
  }

  current_date <- max(state_ces$date, na.rm = TRUE)
  early_date <- current_date - lubridate::years(5)

  # 5. Metric Calculations (using native BLS column names)
  state_ces <- state_ces |>
    dplyr::filter(date >= early_date) |>
    dplyr::select(state_code, area_code, year, period, industry_code,
                  seasonal, benchmark_year, value, area_name, industry_name, date) |>
    dplyr::group_by(seasonal, area_name, industry_code) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      prior_year = dplyr::lag(value, 12),
      yoychange = value - prior_year,
      yoypercent = yoychange / prior_year,
      prior_month = dplyr::lag(value),
      momchange = dplyr::if_else(is.na(prior_month), 0, value - prior_month),
      mompercent = dplyr::if_else(is.na(prior_month), 0, momchange / prior_month),
      m2_change = dplyr::if_else(is.na(dplyr::lag(value, 2)), 0, value - dplyr::lag(value, 2)),
      m3_value = dplyr::lag(value, 3),
      m3_change = dplyr::if_else(is.na(m3_value), 0, value - m3_value),
      m60_value = dplyr::lag(value, 60),
      m60_change = dplyr::if_else(is.na(m60_value), 0, value - m60_value),
      m3_annualized = ((1 + (m3_change / m3_value))^4) - 1,
      m60_annualized = ((1 + (m60_change / m60_value))^0.2) - 1,

      peak_5yr = zoo::rollapplyr(data = value, width = 60, FUN = max, partial = TRUE),
      peak_date = dplyr::if_else(value == peak_5yr, date, as.Date(NA)),
      peak_change = peak_5yr - value,
      percent_below_peak = peak_change / peak_5yr
    ) |>
    tidyr::fill(peak_date, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      months_since_peak = lubridate::time_length(lubridate::interval(peak_date, date), unit = "month")
    )

  # 6. Subset Filtering
  state_ces <- state_ces |>
    dplyr::filter(seasonal == seasonal_filter, area_code == !!area_code)

  max_date <- max(state_ces$date, na.rm = TRUE)
  area_name_val <- unique(state_ces$area_name)[1]
  seasonal_label <- ifelse(seasonal_filter == "S", "Seasonally Adjusted", "Not Seasonally Adjusted")
  month_folder_name <- format(max_date, format = "%B %Y")

  # 7. File System Handling
  if (return_type %in% c("save", "both")) {
    report_folder <- file.path(root_path, "reports", month_folder_name)
    if (!dir.exists(report_folder)) {
      dir.create(report_folder, recursive = TRUE)
      message("Folder created: ", report_folder)
    }
  }

  # 8. Filter for Sparklines (Last 12 Months)
  state_ces_final <- state_ces |>
    dplyr::filter(date >= (max_date - months(12))) |>
    dplyr::group_by(industry_code) |>
    dplyr::mutate(value_12m = list(value)) |>
    dplyr::ungroup() |>
    dplyr::filter(date == max_date) |>
    dplyr::arrange(industry_code)

  if (sectors_only) {
    target_sectors <- c(
      "Total nonfarm", "Mining and logging", "Construction", "Manufacturing",
      "Wholesale Trade", "Retail trade", "Transportation, Warehousing, and Utilities",
      "Information", "Financial activities", "Professional and business services",
      "Educational services", "Health care and social assistance",
      "Leisure and hospitality", "Other services", "Government"
    )
    state_ces_final <- state_ces_final |>
      dplyr::filter(stringr::str_to_lower(industry_name) %in% stringr::str_to_lower(target_sectors))
  }

  # 9. GT Table Generation
  table_title <- paste0(area_name_val, " Employment Trends - ", seasonal_label)

  gt_table <- state_ces_final |>
    dplyr::select(industry_name, industry_code, value, value_12m, momchange, yoypercent,
                  m3_annualized, m60_annualized, peak_5yr, peak_date,
                  peak_change, percent_below_peak, months_since_peak) |>
    dplyr::mutate(peak_date = format(peak_date, format = "%b %y")) |>
    gt::gt() |>
    gt::fmt_number(columns = c(value, momchange, peak_5yr, peak_change, months_since_peak), decimals = 0) |>
    gt::fmt_percent(columns = c(yoypercent, m3_annualized, m60_annualized, percent_below_peak), decimals = 1) |>
    gtExtras::gt_plt_sparkline(value_12m, same_limit = FALSE) |>
    gt::cols_hide(industry_code) |>
    gt::cols_label(
      industry_name = "Industry Name",
      value = "Current Level",
      peak_5yr = "5-Year Peak",
      peak_date = "Peak Date",
      peak_change = "Jobs Below Peak",
      value_12m = "12 Month Trend",
      yoypercent = "12-month",
      momchange = "Monthly Change",
      m3_annualized = "3-month",
      m60_annualized = "5-year",
      months_since_peak = "Months Since Peak",
      percent_below_peak = "Percent Below Peak"
    ) |>
    gt::tab_header(
      title = table_title,
      subtitle = paste0("Data as of ", month_folder_name)
    ) |>
    gt::tab_spanner(label = "Annualized Rate of Change", columns = c(yoypercent, m3_annualized, m60_annualized)) |>
    gt::tab_spanner(label = "5-Year Trends", columns = c(peak_5yr, peak_date, peak_change, percent_below_peak, months_since_peak)) |>
    gt::tab_style(style = gt::cell_text(weight = "bold"), location = gt::cells_body(columns = c(value, industry_name))) |>
    # Conditional Coloring
    gt::tab_style(style = gt::cell_fill(color = "#31a354", alpha = 0.2), location = gt::cells_body(columns = momchange, rows = momchange >= 0)) |>
    gt::tab_style(style = gt::cell_fill(color = "red", alpha = 0.2), location = gt::cells_body(columns = momchange, rows = momchange < 0)) |>
    gt::tab_style(style = gt::cell_fill(color = "#31a354", alpha = 0.2), location = gt::cells_body(columns = yoypercent, rows = yoypercent >= 0)) |>
    gt::tab_style(style = gt::cell_fill(color = "red", alpha = 0.2), location = gt::cells_body(columns = yoypercent, rows = yoypercent < 0)) |>
    gt::tab_style(style = gt::cell_fill(color = "#31a354", alpha = 0.2), location = gt::cells_body(columns = m3_annualized, rows = m3_annualized >= yoypercent)) |>
    gt::tab_style(style = gt::cell_fill(color = "red", alpha = 0.2), location = gt::cells_body(columns = m3_annualized, rows = m3_annualized < yoypercent)) |>
    gt::tab_style(style = gt::cell_fill(color = "#31a354", alpha = 0.2), location = gt::cells_body(columns = m60_annualized, rows = m60_annualized < yoypercent)) |>
    gt::tab_style(style = gt::cell_fill(color = "red", alpha = 0.2), location = gt::cells_body(columns = m60_annualized, rows = m60_annualized >= yoypercent)) |>
    gt::tab_source_note(source_note = "Monthly and 12-month change: Green >= 0, Red < 0.") |>
    gt::tab_source_note(source_note = "3-month annualized: Green if higher than 12-month change.") |>
    gt::tab_source_note(source_note = "5-year annualized: Green if 12-month change is higher than 5-year trend.")

  # 10. Output Logic
  if (return_type %in% c("save", "both")) {
    gt_filename <- file.path(root_path, "reports", month_folder_name, paste0("New ", area_name_val, " CES Trends ", seasonal_label, ".html"))
    gt::gtsave(data = gt_table, filename = gt_filename)
    message("Report saved to: ", gt_filename)
  }

  if (return_type %in% c("display", "both")) {
    return(gt_table)
  }
}
