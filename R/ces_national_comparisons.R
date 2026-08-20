#' Generate CES Employment and Share Comparisons
#'
#' @param out_dir Character. The root directory where image folders will be generated.
#' @param target_fips Character or numeric. The state FIPS code (default is "32" for Nevada).
#' @param set_date Character string representing the baseline date (default "2022-12-01").
#' @param ces_data Optional data frame. Pre-downloaded output from \code{BLSloadR::get_ces()}.
#' @param state_colors Named character vector of colors for specific states.
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by ungroup select rename left_join arrange distinct pull if_else
#' @importFrom lubridate ymd ym
#' @importFrom scales comma percent ordinal
#' @importFrom cowplot plot_grid
#' @export
ces_employment_comparisons <- function(out_dir,
                                       target_fips = "32",
                                       set_date = "2022-12-01",
                                       ces_data = NULL) {

  state_colors <- c(
    "Alabama"        = "#9E1B32",  # Alabama Crimson
    "Alaska"         = "#2F5597",  # State Flag Dark Blue
    "Arizona"        = "#C35817",  # State Flag Copper
    "Arkansas"       = "#ED1C24",  # State Flag Red
    "California"     = "#FBB03B",  # California Bear Gold
    "Colorado"       = "#8A2BE2",  # Purple Mountain Majesties
    "Connecticut"    = "#00205B",  # State Navy Blue
    "Delaware"       = "#72A9BE",  # Colonial Blue
    "Florida"        = "#FA4616",  # Florida Orange
    "Georgia"        = "#BA0C2F",  # Georgia Red
    "Hawaii"         = "#0088CE",  # Pacific Ocean Blue
    "Idaho"          = "#8D5524",  # Russet Brown
    "Illinois"       = "#E84A27",  # Illini Orange
    "Indiana"        = "#D90016",  # Hoosier Crimson
    "Iowa"           = "#FCE100",  # Hawkeye Gold
    "Kansas"         = "#0051BA",  # Jayhawk Blue
    "Kentucky"       = "#0033A0",  # Wildcat Blue
    "Louisiana"      = "#461D7C",  # Mardi Gras Purple
    "Maine"          = "#00693E",  # Pine Tree Green
    "Maryland"       = "#FFD100",  # Flag Yellow
    "Massachusetts"  = "#8B0000",  # Brick Red
    "Michigan"       = "#FFCB05",  # Wolverine Maize
    "Minnesota"      = "#7A0019",  # Gopher Maroon
    "Mississippi"    = "#4A90E2",  # Magnolia Light Blue
    "Missouri"       = "#F1B82D",  # Mizzou Gold
    "Montana"        = "#244C5A",  # Copper Teal
    "Nebraska"       = "#E41C38",  # Cornhusker Red
    "Nevada"         = "#005a9c",  # Nevada Blue
    "New Hampshire"  = "#2E8B57",  # Granite State Green
    "New Jersey"     = "#D2A745",  # State Flag Buff
    "New Mexico"     = "#FFC425",  # Zia Sun Yellow
    "New York"       = "#F2A900",  # Empire State Gold
    "North Carolina" = "#4B9CD3",  # Tar Heel Blue
    "North Dakota"   = "#005643",  # Kelly Green
    "Ohio"           = "#BB0000",  # Buckeye Scarlet
    "Oklahoma"       = "#841617",  # Sooner Crimson
    "Oregon"         = "#154733",  # Oregon Green
    "Pennsylvania"   = "#001E44",  # Penn State Blue
    "Rhode Island"   = "#68ABE8",  # Ocean State Light Blue
    "South Carolina" = "#73000A",  # Gamecock Garnet
    "South Dakota"   = "#003882",  # Jackrabbit Blue
    "Tennessee"      = "#FF8200",  # Volunteer Orange
    "Texas"          = "#BF5700",  # Burnt Orange
    "Utah"           = "#890012",  # Utah Deep Red
    "Vermont"        = "#007155",  # Catamount Green
    "Virginia"       = "#F26522",  # Cavalier Orange
    "Washington"     = "#4B2E83",  # Husky Purple
    "West Virginia"  = "#EAAA00",  # Mountaineer Old Gold
    "Wisconsin"      = "#C5050C",  # Badger Red
    "Wyoming"        = "#492F24"   # Wyoming Brown
  )

  target_fips <- as.character(target_fips)
  set_date_string <- format.Date(lubridate::ymd(set_date), format = "%B %Y")

  if(is.null(ces_data)) {
    ces_data <- BLSloadR::get_ces(cache = TRUE)
  }

  bls_ces_all_state <- ces_data |>
    dplyr::group_by(state_code, area_code, industry_code, seasonal) |>
    dplyr::filter(data_type_code == "01",
                  !(state_name %in% c("Puerto Rico", "Virgin Islands"))) |>
    dplyr::mutate(
      prior_month = dplyr::lag(value, 1),
      prior_year = dplyr::lag(value, 12),
      otm_change = value - prior_month,
      oty_change = value - prior_year,
      otm_pct = otm_change / prior_month,
      oty_pct = oty_change / prior_year
    ) |>
    dplyr::ungroup() |>
    dplyr::select(date, state_code, state_name, area_code, industry_name, seasonal, value, prior_month:oty_pct)

  precovid_peak <- bls_ces_all_state |>
    dplyr::filter(date >= lubridate::ym("2019-03") & date <= lubridate::ym("2020-02")) |>
    dplyr::group_by(state_name, area_code, industry_name, seasonal) |>
    dplyr::filter(value == max(value)) |>
    dplyr::filter(date == max(date)) |>
    dplyr::rename(precovid = value) |>
    dplyr::select(precovid)

  nonfarm_all <- bls_ces_all_state |> dplyr::filter(industry_name == "Total Nonfarm") |> dplyr::select(-c(prior_month:oty_pct, industry_name)) |> dplyr::rename(total_nonfarm = value)
  leisure_all <- bls_ces_all_state |> dplyr::filter(industry_name == "Leisure and Hospitality") |> dplyr::select(-c(prior_month:oty_pct, industry_name)) |> dplyr::rename(leisure = value)
  dec19_all <- bls_ces_all_state |> dplyr::filter(date == lubridate::ymd("2019-12-01")) |> dplyr::select(-c(date, prior_month:oty_pct)) |> dplyr::rename(dec_19 = value)
  set_date_all <- bls_ces_all_state |> dplyr::filter(date == lubridate::ymd(set_date)) |> dplyr::select(-c(date, prior_month:oty_pct)) |> dplyr::rename(set_date_value = value)

  bls_ces_all_state <- bls_ces_all_state |>
    dplyr::left_join(nonfarm_all, by = c("state_code", "area_code", "seasonal", "date", "state_name")) |>
    dplyr::left_join(leisure_all, by = c("state_code", "area_code", "seasonal", "date", "state_name")) |>
    dplyr::left_join(dec19_all, by = c("state_code", "area_code", "seasonal", "industry_name", "state_name")) |>
    dplyr::left_join(set_date_all, by = c("state_code", "area_code", "seasonal", "industry_name", "state_name")) |>
    dplyr::left_join(precovid_peak, by = c("state_name", "area_code", "industry_name", "seasonal")) |>
    dplyr::mutate(ind_share = value / total_nonfarm,
                  total_no_lh = total_nonfarm - leisure,
                  ind_share_no_lh = value / total_no_lh,
                  dec_19_change = value - dec_19,
                  dec_19_pct_change = dec_19_change / dec_19,
                  set_change = value - set_date_value,
                  set_pct_change = set_change / set_date_value,
                  covid_recovery = value - precovid,
                  covid_share_recovered = covid_recovery / precovid + 1) |>
    dplyr::group_by(state_code, area_code, industry_name, seasonal) |>
    dplyr::arrange(date) |>
    dplyr::mutate(decade_ago_share = dplyr::lag(ind_share, 120),
                  decade_share_change = ind_share - decade_ago_share) |>
    dplyr::ungroup()

  ces_states_only <- bls_ces_all_state |>
    dplyr::filter(area_code == "00000") |>
    dplyr::group_by(date, industry_name, seasonal) |>
    dplyr::mutate(
      rank_value = floor(rank(-value)),
      rank_otm_lvl = floor(rank(-otm_change)),
      rank_oty_lvl = floor(rank(-oty_change)),
      rank_otm_pct = floor(rank(-otm_pct)),
      rank_oty_pct = floor(rank(-oty_pct)),
      rank_share = floor(rank(-ind_share)),
      rank_dec_19_change = floor(rank(-dec_19_pct_change)),
      rank_set_date_change = floor(rank(-set_pct_change)),
      rank_decade_share_change = floor(rank(-decade_share_change))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(is_target = dplyr::if_else(state_code == target_fips, "1", "0")) |>
    dplyr::distinct()

  target_state_name <- unique(ces_states_only$state_name[ces_states_only$state_code == target_fips])[1]
  target_color <- unname(state_colors[target_state_name])
  max_date <- max(ces_states_only$date[ces_states_only$state_code == target_fips], na.rm = TRUE)
  current_month <- format(max_date, "%B %Y")

  state_industries_sa <- unique(ces_states_only$industry_name[ces_states_only$state_code == target_fips & ces_states_only$date == max_date & ces_states_only$seasonal == "S"])
  state_industries_nsa <- unique(ces_states_only$industry_name[ces_states_only$state_code == target_fips & ces_states_only$date == max_date & ces_states_only$seasonal == "U"])

  # Ensure Output Directories Exist
  dir_sa <- file.path(out_dir, current_month, "National Comparisons", "Seasonally Adjusted", target_state_name)
  dir_nsa <- file.path(out_dir, current_month, "National Comparisons", "Not Seasonally Adjusted", target_state_name)
  dir_nsa_shares <- file.path(out_dir, current_month, "National Comparisons", "NSA Shares", target_state_name)
  dir.create(dir_sa, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_nsa, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_nsa_shares, recursive = TRUE, showWarnings = FALSE)

  save_plot <- function(filename, plot, width, height, res = 300) {
    grDevices::png(filename, width = width, height = height, units = "px", res = res, pointsize = 1)
    print(plot)
    grDevices::dev.off()
  }

  ### 1. Seasonally Adjusted Plots
  for(ind in state_industries_sa) {
    tmp_data <- ces_states_only |> dplyr::filter(date == max_date, seasonal == "S", industry_name == ind)
    state_data <- tmp_data |> dplyr::filter(state_code == target_fips)
    if(nrow(state_data) == 0) next

    state_employment <- scales::comma(state_data$value)
    state_emp_rank <- scales::ordinal(state_data$rank_value)
    state_oty <- scales::percent(state_data$oty_pct, accuracy = 0.1)
    state_oty_rank <- scales::ordinal(state_data$rank_oty_pct)
    state_share <- scales::percent(state_data$ind_share, accuracy = 0.1)
    state_share_rank <- scales::ordinal(state_data$rank_share)
    state_otm <- scales::percent(state_data$otm_pct, accuracy = 0.1)
    state_otm_rank <- scales::ordinal(state_data$rank_otm_pct)
    state_set <- scales::percent(state_data$set_pct_change, accuracy = 0.1)
    state_set_rank <- scales::ordinal(state_data$rank_set_date_change)
    state_10y <- scales::percent(state_data$decade_share_change, accuracy = 0.1)
    state_10y_rank <- scales::ordinal(state_data$rank_decade_share_change)

    a <- ggplot(tmp_data) + geom_col(aes(x = value, y = reorder(state_name, value), fill = is_target)) + scale_x_continuous(labels = scales::comma) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = paste0(ind, " Employment"), subtitle = paste0(target_state_name, " is ", state_employment, " and ranks ", state_emp_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    b <- ggplot(tmp_data) + geom_col(aes(x = oty_pct, y = reorder(state_name, oty_pct), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "Annual Employment Change", subtitle = paste0(target_state_name, " is ", state_oty, " and ranks ", state_oty_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    c <- ggplot(tmp_data) + geom_col(aes(x = ind_share, y = reorder(state_name, ind_share), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "Share of Total Employment", subtitle = paste0(target_state_name, " is ", state_share, " and ranks ", state_share_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    d <- ggplot(tmp_data) + geom_col(aes(x = otm_pct, y = reorder(state_name, otm_pct), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "Monthly Employment Change", subtitle = paste0(target_state_name, " is ", state_otm, " and ranks ", state_otm_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    e <- ggplot(tmp_data) + geom_col(aes(x = set_pct_change, y = reorder(state_name, set_pct_change), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = paste0("Change Since ", set_date_string), subtitle = paste0(target_state_name, " is ", state_set, " and ranks ", state_set_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    f <- ggplot(tmp_data) + geom_col(aes(x = decade_share_change, y = reorder(state_name, decade_share_change), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "10-Year Industry Share Change", subtitle = paste0(target_state_name, " is ", state_10y, " and ranks ", state_10y_rank), caption = paste0("Seasonally Adjusted Data\n", ind, " Industry"), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))

    grid_all <- cowplot::plot_grid(cowplot::plot_grid(a, b), cowplot::plot_grid(c, d), cowplot::plot_grid(e, f), ncol = 1)
    b_single <- b + labs(title = paste0(ind, " Employment Change")) + theme(axis.text.y = element_text(size = 7))


    save_plot(file.path(dir_sa, paste0("Page-Sized Summary for ", ind, ".png")), grid_all, 2250, 3150, 250)
    save_plot(file.path(dir_sa, paste0("Employment Change for ", ind, ".png")), b_single, 2800, 1540, 300)
  }

  ### 2. Not Seasonally Adjusted Plots
  for(ind in state_industries_nsa) {
    tmp_data <- ces_states_only |> dplyr::filter(date == max_date, seasonal == "U", industry_name == ind)
    state_data <- tmp_data |> dplyr::filter(state_code == target_fips)
    if(nrow(state_data) == 0) next

    safe_ind <- base::strtrim(gsub("[^A-Za-z0-9 _-]", "", ind), 45)

    state_employment <- scales::comma(state_data$value)
    state_emp_rank <- scales::ordinal(state_data$rank_value)
    state_oty <- scales::percent(state_data$oty_pct, accuracy = 0.1)
    state_oty_rank <- scales::ordinal(state_data$rank_oty_pct)
    state_share <- scales::percent(state_data$ind_share, accuracy = 0.1)
    state_share_rank <- scales::ordinal(state_data$rank_share)
    state_otm <- scales::percent(state_data$otm_pct, accuracy = 0.1)
    state_otm_rank <- scales::ordinal(state_data$rank_otm_pct)
    state_dec19 <- scales::percent(state_data$dec_19_pct_change, accuracy = 0.1)
    state_dec19_rank <- scales::ordinal(state_data$rank_dec_19_change)
    state_10y <- scales::percent(state_data$decade_share_change, accuracy = 0.1)
    state_10y_rank <- scales::ordinal(state_data$rank_decade_share_change)

    a <- ggplot(tmp_data) + geom_col(aes(x = value, y = reorder(state_name, value), fill = is_target)) + scale_x_continuous(labels = scales::comma) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = paste0(ind, " Employment"), subtitle = paste0(target_state_name, " is ", state_employment, " and ranks ", state_emp_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    b <- ggplot(tmp_data) + geom_col(aes(x = oty_pct, y = reorder(state_name, oty_pct), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "Annual Employment Change", subtitle = paste0(target_state_name, " is ", state_oty, " and ranks ", state_oty_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    c <- ggplot(tmp_data) + geom_col(aes(x = ind_share, y = reorder(state_name, ind_share), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "Share of Total Employment", subtitle = paste0(target_state_name, " is ", state_share, " and ranks ", state_share_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    d <- ggplot(tmp_data) + geom_col(aes(x = otm_pct, y = reorder(state_name, otm_pct), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "Monthly Employment Change", subtitle = paste0(target_state_name, " is ", state_otm, " and ranks ", state_otm_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    e <- ggplot(tmp_data) + geom_col(aes(x = set_pct_change, y = reorder(state_name, set_pct_change), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = paste0("Change Since ", set_date_string), subtitle = paste0(target_state_name, " is ", state_set, " and ranks ", state_set_rank), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))
    f <- ggplot(tmp_data) + geom_col(aes(x = decade_share_change, y = reorder(state_name, decade_share_change), fill = is_target)) + scale_x_continuous(labels = scales::percent) + scale_fill_manual(values = c("0" = "#aaaaaa", "1" = target_color)) + labs(title = "10-Year Industry Share Change", subtitle = paste0(target_state_name, " is ", state_10y, " and ranks ", state_10y_rank), caption = paste0("Seasonally Adjusted Data\n", ind, " Industry"), x = "", y = "") + theme_bw() + theme(legend.position = "none", axis.text.y = element_text(size = 6))

    grid_all <- cowplot::plot_grid(cowplot::plot_grid(a, b), cowplot::plot_grid(c, d), cowplot::plot_grid(e, f), ncol = 1)
    grid_shares <- cowplot::plot_grid(c, f, nrow = 1)

    save_plot(file.path(dir_nsa, paste0("Summary for ", safe_ind, " Unadjusted.png")), grid_all, 3600, 7200, 300)

    # Custom dimensions for NSA Shares
    grDevices::png(file.path(dir_nsa_shares, paste0("Summary for ", safe_ind, " Unadjusted.png")), width = 900, height = 500, units = "px")
    print(grid_shares)
    grDevices::dev.off()
  }
}

#' Generate CES Wage and Hour Comparisons
#'
#' @param out_dir Character. The root directory where image folders will be generated.
#' @param target_fips Character or numeric. The state FIPS code (default is "32" for Nevada).
#' @param ces_data Optional data frame. Pre-downloaded output from \code{BLSloadR::get_ces()}.
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by ungroup select arrange pull if_else
#' @importFrom zoo rollapplyr
#' @importFrom scales comma percent ordinal dollar
#' @importFrom cowplot plot_grid
#' @export
ces_wage_hour_comparisons <- function(out_dir, target_fips = "32", ces_data = NULL) {

  target_fips <- as.character(target_fips)

  if(is.null(ces_data)) {
    ces_data <- BLSloadR::get_ces(cache = TRUE)
  }

  bls_ces_all_state <- ces_data |>
    dplyr::filter(data_type_code %in% c("02", "03", "07", "08", "11", "30")) |>
    dplyr::group_by(state_code, area_code, industry_code, seasonal, data_type_text) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      value = as.numeric(value),
      prior_month = dplyr::lag(value, 1),
      prior_year = dplyr::lag(value, 12),
      otm_change = value - prior_month,
      oty_change = value - prior_year,
      otm_pct = otm_change / prior_month,
      oty_pct = oty_change / prior_year,
      value_3mo_avg = zoo::rollapplyr(data = value, FUN = mean, width = 3, partial = FALSE, fill = NA),
      otm_pct_3mo_avg = zoo::rollapplyr(data = otm_pct, FUN = mean, width = 3, partial = FALSE, fill = NA),
      oty_pct_3mo_avg = zoo::rollapplyr(data = oty_pct, FUN = mean, width = 3, partial = FALSE, fill = NA)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(date, state_code, state_name, area_code, industry_name, seasonal, value, data_type_text, prior_month:oty_pct_3mo_avg)

  ces_states_only <- bls_ces_all_state |>
    dplyr::filter(area_code == "00000") |>
    dplyr::group_by(date, industry_name, data_type_text, seasonal) |>
    dplyr::mutate(
      rank_value = floor(rank(-value)),
      rank_otm_pct = floor(rank(-otm_pct)),
      rank_oty_pct = floor(rank(-oty_pct)),
      rank_oty_pct_3mo_avg = floor(rank(-oty_pct_3mo_avg)),
      pct_20_oty = stats::quantile(oty_pct_3mo_avg, .2, na.rm = TRUE),
      pct_80_oty = stats::quantile(oty_pct_3mo_avg, .8, na.rm = TRUE),
      pct_20_val = stats::quantile(value, .2, na.rm = TRUE),
      pct_80_val = stats::quantile(value, .8, na.rm = TRUE),
      median_oty = stats::median(oty_pct_3mo_avg, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(is_target = dplyr::if_else(state_code == target_fips, "1", "0"))

  max_date <- max(ces_states_only$date, na.rm = TRUE)
  current_month <- format(max_date, "%B %Y")
  target_state_name <- unique(ces_states_only$state_name[ces_states_only$state_code == target_fips])[1]

  # Ensure Directories
  dir_earn <- file.path(out_dir, current_month, "National Comparisons", "Earnings, Wages, and Hours", target_state_name)
  dir.create(dir_earn, recursive = TRUE, showWarnings = FALSE)

  save_plot <- function(filename, plot, width, height, res = 300) {
    grDevices::png(filename, width = width, height = height, units = "px", res = res, pointsize = 1)
    print(plot)
    grDevices::dev.off()
  }

  # Line Charts: Hourly Wages over Time (All Employees)
  tmp <- ces_states_only |>
    dplyr::filter(state_name == target_state_name)

  p1 <- ggplot(tmp |>
                 dplyr::filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars") |>
                 dplyr::mutate(label_text = dplyr::if_else(date == max(date), scales::dollar(value_3mo_avg), NA_character_)
                               ), aes(x = date, color = industry_name)) +
    geom_ribbon(aes(ymin = pct_20_val, ymax = pct_80_val), fill = "grey", alpha = 0.5, color = NA) +
    geom_line(aes(y = value_3mo_avg), linewidth = 1) +
    geom_label(aes(label = label_text, y = value_3mo_avg), hjust = -0.05, size = 3) +
    facet_wrap(~industry_name) +
    scale_x_date(expand = expansion(add = c(0, 1500))) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title = paste0("3-month Average Hourly Wage in ", target_state_name, " and All States"),
      subtitle = paste0("Grey area represents 20th to 80th percentile for all states, data for ", current_month),
      x = "", y = "") +
    theme_bw() +
    theme(legend.position = "none")

  ggplot2::ggsave(filename = file.path(dir_earn, "Hourly Wages over Time.png"), plot = p1, dpi = 250, height = 1080, width = 1920, units = "px")

  p2 <- ggplot(tmp |>
                 dplyr::filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars") |>
                 dplyr::mutate(label_text = dplyr::if_else(date == max(date), scales::percent(oty_pct_3mo_avg, accuracy = 0.01), NA_character_)),
               aes(x = date, color = industry_name)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_ribbon(aes(ymin = pct_20_oty, ymax = pct_80_oty), fill = "grey", alpha = 0.5, color = NA) +
    geom_line(aes(y = oty_pct_3mo_avg), linewidth = 1) +
    geom_label(aes(label = label_text, y = oty_pct_3mo_avg), hjust = -0.01, show.legend = FALSE, size = 3) +
    facet_wrap(~industry_name) + scale_x_date(expand = expansion(add = c(0, 1500))) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste0("Change in 3-month Average Hourly Wage in ", target_state_name, " and All States"),
      subtitle = paste0("Grey area represents 20th to 80th percentile for all states, data for ", current_month),
      x = "", y = "") +
    theme_bw() +
    theme(legend.position = "none")

  ggplot2::ggsave(filename = file.path(dir_earn, "Hourly Wage Change over Time.png"), plot = p2, dpi = 250, height = 1080, width = 1920, units = "px")
}

#' Generate CES Growth Accelerations Trends
#'
#' @param out_dir Character. The root directory where image folders will be generated.
#' @param target_fips Character or numeric. The state FIPS code (default is "32" for Nevada).
#' @param ces_data Optional data frame. Pre-downloaded output from \code{BLSloadR::get_ces()}.
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by ungroup select rename left_join pull if_else case_when summarize n
#' @importFrom lubridate years
#' @export
ces_growth_trends <- function(out_dir, target_fips = "32", ces_data = NULL) {

  if(is.null(ces_data)) {
    ces_data <- BLSloadR::get_ces(cache = TRUE)
  }

  bls_ces_employment <- ces_data |>
    dplyr::group_by(state_code, area_code, industry_code, seasonal) |>
    dplyr::filter(data_type_code == "01",
                  !(state_name %in% c("Puerto Rico", "Virgin Islands"))) |>
    dplyr::mutate(
      prior_month = dplyr::lag(value, 1),
      prior_year = dplyr::lag(value, 12),
      otm_change = value - prior_month,
      oty_change = value - prior_year,
      otm_pct = otm_change / prior_month,
      oty_pct = oty_change / prior_year
    ) |>
    dplyr::ungroup() |>
    dplyr::select(date, state_code, state_name, area_code, industry_name, seasonal, value, prior_month:oty_pct)

  target_state_name <- unique(bls_ces_employment$state_name[bls_ces_employment$state_code == target_fips])[1]

  state_ind_current <- bls_ces_employment |>
    dplyr::filter(date == max(date), state_name == target_state_name, area_code == "00000", seasonal == "S") |>
    dplyr::pull(industry_name)

  current_date <- max(bls_ces_employment$date)
  current_period <- format.Date(current_date, "%B %Y")

  comp_dates <- c(
    current_date - lubridate::years(1), current_date - lubridate::years(3),
    current_date - lubridate::years(6), current_date - lubridate::years(9),
    current_date - lubridate::years(12), current_date - lubridate::years(15)
  )

  industry_value_comparisons <- bls_ces_employment |>
    dplyr::filter(area_code == "00000", seasonal == "S") |>
    dplyr::select(date, state_name, state_code, industry_name, oty_pct, value)

  current_values <- industry_value_comparisons |>
    dplyr::filter(date == current_date) |>
    dplyr::select(-date) |>
    dplyr::rename(current = oty_pct)

  # Step 1: Create the categorical flags for EVERY state
  categorized_data <- industry_value_comparisons |>
    dplyr::filter(date %in% comp_dates) |>
    dplyr::select(-value) |>
    dplyr::mutate(period_name = format.Date(date, "%B %Y")) |>
    dplyr::select(-date) |>
    dplyr::rename(prior = oty_pct) |>
    dplyr::left_join(current_values, by = c("state_name", "state_code", "industry_name")) |>
    dplyr::mutate(
      growth_category = dplyr::case_when(
        current >= 0 & prior < 0 ~ "Was Shrinking\nIs Growing",
        current >= prior & prior >= 0 ~ "Was Growing\nIs Growing Faster",
        current < prior & current >= 0 ~ "Was Growing\nIs Growing Slower",
        current < 0 & prior >= 0 ~ "Was Growing\nIs Shrinking",
        current < prior & prior < 0 ~ "Was Shrinking\nIs Shrinking Faster",
        current >= prior & current < 0 ~ "Was Shrinking\nIs Shrinking Slower"
      ),
      better_worse = dplyr::if_else(growth_category %in% c("Was Shrinking\nIs Growing", "Was Shrinking\nIs Shrinking Slower", "Was Growing\nIs Growing Faster"), "better", "worse")
    )

  # Step 2: Save a mini-dataframe of exactly where the target state landed
  target_flags <- categorized_data |>
    dplyr::filter(state_code == target_fips) |>
    dplyr::select(industry_name, period_name, growth_category) |>
    dplyr::mutate(is_target = TRUE)

  # Step 3 & 4: Summarize the full data, then join the target flags and build the dynamic label
  joined_values <- categorized_data |>
    dplyr::group_by(industry_name, period_name, growth_category, better_worse) |>
    dplyr::summarize(states = dplyr::n(), employment = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(target_flags, by = c("industry_name", "period_name", "growth_category")) |>
    dplyr::mutate(
      is_target = dplyr::if_else(is.na(is_target), FALSE, is_target),
      label_text = dplyr::if_else(is_target, paste0(states, "*"), as.character(states))
    )

  dir_growth <- file.path(out_dir, current_period, "National Comparisons", "Growth Rates")
  dir.create(dir_growth, recursive = TRUE, showWarnings = FALSE)

  for (ind in state_ind_current) {

    # Safely truncate long industry names for Windows OS
    safe_ind <- base::strtrim(gsub("[^A-Za-z0-9 _-]", "", ind), 45)

    p2 <- joined_values |>
      dplyr::filter(industry_name == ind) |>
      ggplot(aes(x = growth_category, y = employment, fill = better_worse)) +

      # Background shading (updated with alpha = 0.1)
      annotate("rect", xmin = -Inf, xmax = 3.5, ymin = -Inf, ymax = Inf,
               fill = "white", alpha = 0.2) +
      annotate("rect", xmin = 3.5, xmax = Inf, ymin = -Inf, ymax = Inf,
               fill = "grey", alpha = 0.2) +

      # UPDATE: Add the overarching group labels at the top
      annotate("text", x = 2, y = Inf, label = "Was Growing...",
               vjust = 1.5, size = 4.5, fontface = "italic") +
      annotate("text", x = 5, y = Inf, label = "Was Shrinking...",
               vjust = 1.5, size = 4.5, fontface = "italic") +

      geom_col(position = "dodge") +
      geom_text(aes(label = label_text), vjust = -0.5, size = 3.5, fontface = "bold") +
      facet_wrap(~period_name) +
      labs(title = paste0("National Job Growth Trends in ", ind, " in ", current_period),
           subtitle = paste0("Number of states per category, comparison to same month in prior years using annual growth rate (* includes ", target_state_name, ")"),
           y = "Total Employment", x = NULL) +
      scale_fill_manual(values = c("darkgreen", "firebrick3")) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.20))) +

      # UPDATE: Dynamically truncate the x-axis labels
      # This regex removes "Was Growing" or "Was Shrinking" and any following spaces or newlines
      scale_x_discrete(labels = function(x) gsub("^(Was Growing|Was Shrinking)[ \n]*", "...", x)) +

      theme_bw() +
      guides(fill = "none") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

    ggplot2::ggsave(filename = file.path(dir_growth, paste0("Categorical ", safe_ind, ".png")), plot = p2, width = 12, height = 6.75, dpi = "retina")
  }
}
