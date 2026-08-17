#' Generate CES concepts from QCEW data
#'
#' @description Downloads national CES industry data and aggregates QCEW data into CES concepts.
#'
#' @param qcew_area_code A character string representing the QCEW area code.
#' @param year_start A numeric four-digit year indicating the start of the data retrieval.
#'
#' @return A tibble containing aggregated employment data.
#' @export
#'
#' @importFrom dplyr filter mutate group_by summarize select across all_of
#' @importFrom purrr map map_chr map2 compact map_dfr
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_trim str_starts str_length str_extract_all
#' @importFrom rlang exprs !! !!!
generate_ces_from_qcew <- function(qcew_area_code, year_start) {

  if (year_start < 2014) {
    warning("QCEW data is not available from the BLS API prior to 2014. Using 2014 as starting year.")
    year_start <- 2014
  }

  # Setup and Define Filters
  parse_naics <- function(code) {
    if (is.na(code) || code == "") return(NA_character_)

    # Split the string by commas (handling potential whitespace)
    parts <- str_trim(strsplit(code, ",")[[1]])
    base <- parts[1]

    # If there are no extra parts, just return the base code
    if (length(parts) == 1) {
      return(base)
    }

    # For suffixes, calculate how much of the base prefix to keep
    # based on the length of the suffix
    suffixes <- parts[-1]

    extras <- map_chr(suffixes, function(s) {
      prefix_length <- nchar(base) - nchar(s)
      paste0(substr(base, 1, prefix_length), s)
    })

    # Combine the base code with the newly constructed codes
    return(c(base, extras))
  }

  # This will download national CES industry data and extract those industries that exist in state/metro CES data.
  sm_industry <- BLSloadR::fread_bls("https://download.bls.gov/pub/time.series/sm/sm.industry")$data
  ce_industry <- BLSloadR::fread_bls("https://download.bls.gov/pub/time.series/ce/ce.industry")$data

  ce_in_sm <- ce_industry |>
    filter(industry_code %in% sm_industry$industry_code) |>
    mutate(parsed_codes = map(naics_code, parse_naics))

  # Create the rules for detailed industries based on the lookup table
  dynamic_rules <- map2(
    ce_in_sm$parsed_codes,
    ce_in_sm$industry_name,
    function(codes, name) {
      if (is.null(codes) || all(is.na(codes)) || all(codes == "-")) return(NULL)

      # Updated: industry_code and integer own_code
      list(
        filter_expr = exprs(industry_code %in% !!codes, own_code == 5),
        series_name = name
      )
    }
  ) |>
    compact()

  # Manually-generated list of series filters for aggregations where the NAICS is not specified
  manual_filter_rules <- list(
    # --- 1. TOP LEVEL AGGREGATIONS ---
    list(filter_expr = exprs(industry_code == '10', own_code == 0), series_name = "Total nonfarm"),
    list(filter_expr = exprs(industry_code == '10', own_code == 5), series_name = "Total private"),

    # --- 2. GOODS-PRODUCING & SUB-SECTORS ---
    list(filter_expr = exprs(industry_code == '101', own_code == 5), series_name = "Goods-producing"),
    list(filter_expr = exprs(industry_code %in% c('21', '1133'), own_code == 5), series_name = "Mining and logging"),
    list(filter_expr = exprs(industry_code == '1013', own_code == 5), series_name = "Manufacturing"),
    list(filter_expr = exprs(industry_code %in% c('321', '327', '331', '332', '333', '334', '335', '336', '337', '339'), own_code == 5), series_name = "Durable goods"),
    list(filter_expr = exprs(industry_code %in% c('311', '312', '313', '314', '315', '316', '322', '323', '324', '325', '326'), own_code == 5), series_name = "Nondurable goods"),

    # --- 3. SERVICE-PROVIDING & SUB-SECTORS ---
    list(filter_expr = exprs(industry_code == '102', own_code %in% 1:5), series_name = "Service-providing"),
    list(filter_expr = exprs(industry_code == '102', own_code == 5), series_name = "Private service-providing"),
    list(filter_expr = exprs(industry_code == '1021', own_code == 5), series_name = "Trade, transportation, and utilities"),
    list(filter_expr = exprs(industry_code == '1023', own_code == 5), series_name = "Financial activities"),
    list(filter_expr = exprs(industry_code == '1024', own_code == 5), series_name = "Professional and business services"),
    list(filter_expr = exprs(industry_code == '1025', own_code == 5), series_name = "Private education and health services"),
    list(filter_expr = exprs(industry_code == '1026', own_code == 5), series_name = "Leisure and hospitality"),
    list(filter_expr = exprs(industry_code == '44-45', own_code == 5), series_name = "Retail trade"),
    list(filter_expr = exprs(industry_code == '48-49', own_code == 5), series_name = "Transportation and warehousing"),

    # --- 4. GOVERNMENT AGGREGATIONS ---
    list(filter_expr = exprs(industry_code == '10', own_code %in% 1:4), series_name = "Government"),
    list(filter_expr = exprs(industry_code == '10', own_code == 1), series_name = "Federal government"),
    list(filter_expr = exprs(industry_code == '10', own_code == 2), series_name = "State government"),
    list(filter_expr = exprs(industry_code == '10', own_code %in% 3:4), series_name = "Local government"),

    # --- 5. DETAILED GOVERNMENT SUB-SECTORS ---
    list(filter_expr = exprs(industry_code == '622', own_code == 1), series_name = "Federal hospitals"),
    list(filter_expr = exprs(industry_code == '928110', own_code == 1), series_name = "Department of Defense"),
    list(filter_expr = exprs(str_starts(industry_code, '491'), own_code == 1), series_name = "U.S. Postal Service"),
    list(filter_expr = exprs(industry_code == '61', own_code == 2), series_name = "State government education"),
    list(filter_expr = exprs(industry_code == '622', own_code == 2), series_name = "State hospitals"),
    list(filter_expr = exprs(str_length(industry_code) == 2, !industry_code %in% c('10', '61'), own_code == 2), series_name = "State government, excluding education"),
    list(filter_expr = exprs(industry_code == '61', own_code %in% 3:4), series_name = "Local government education"),
    list(filter_expr = exprs(industry_code %in% c('48', '49', '48-49'), own_code %in% 3:4), series_name = "Local government transportation"),
    list(filter_expr = exprs(industry_code == '622', own_code %in% 3:4), series_name = "Local hospitals"),
    list(filter_expr = exprs(str_length(industry_code) == 2, !industry_code %in% c('10', '61'), own_code %in% 3:4), series_name = "Local government, excluding education")
  )

  # Combine the dynamic and manual filters
  full_filters <- c(manual_filter_rules, dynamic_rules)

  # Define a function to aggregate the QCEW into CES concepts
  process_qcew_data <- function(data, filter_expr, series_name, group_vars = c("date", "area_title", "seriesname")) {
    data |>
      filter(!!!filter_expr) |>                   # Use filter expressions
      mutate(seriesname = series_name) |>        # Apply a custom series name
      group_by(across(all_of(group_vars))) |>    # Group by the specified variables
      summarize(employment = sum(employment),
                .groups = "drop")                # Drop grouping after summarization
  }

  # Download the data for a specified area
  area_qcew <- BLSloadR::get_qcew(area_code = qcew_area_code, year_start = year_start)

  area_qcew_employment <- area_qcew |>
    select(industry_code, own_code, year, qtr, month1_emplvl, month2_emplvl, month3_emplvl, area_title) |>
    pivot_longer(cols = month1_emplvl:month3_emplvl, names_to = "month_name", values_to = "employment") |>
    mutate(month = str_extract_all(month_name, "\\d+"),
           month = as.numeric(month),
           month = (qtr - 1) * 3 + month,
           date = lubridate::ym(paste(year, month, sep = "-"))) |>
    select(area_title, date, industry_code, own_code, employment)

  combined_qcew <- map_dfr(
    full_filters,
    ~ process_qcew_data(
      data = area_qcew_employment,
      group_vars = c("date", "seriesname", "area_title"),
      filter_expr = .x$filter_expr,
      series_name = .x$series_name
    )
  )

  result <- combined_qcew |>
    filter(employment > 0)

  return(result)
}
