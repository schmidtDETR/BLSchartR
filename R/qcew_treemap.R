#' Generate QCEW Industry Treemaps
#'
#' @description Downloads Quarterly Census of Employment and Wages (QCEW) data using `BLSloadR`
#' and generates highly detailed treemaps showing employment levels and wage changes across industries.
#' Plots are automatically saved as PNG files in a structured local directory.
#'
#' @param areas Character vector of FIPS area codes. Default is `"32000"` (Nevada).
#' @param ownerships Numeric vector of ownership codes (e.g., 5 for Private). Default is `5`.
#' @param period Character or numeric indicating the quarter/period. Default is `NULL` (determined automatically).
#' @param year Numeric year for the data to be pulled. Required.
#' @param include_suppressed Logical. Should confidential/suppressed data be included in the plot titles and file paths? Default is `FALSE`.
#' @param resolution Character string specifying output plot dimensions. Options are `"regular"` or `"large"`. Default is `"regular"`.
#' @param breakout_digits Numeric indicating the NAICS digit level for detailed breakouts (4, 5, or 6). Default is `5`.
#' @param sectors Character vector specifying the first digit(s) of NAICS sectors to loop through. Default is `c("2", "3", "4", "5", "6", "7", "8")`.
#'
#' @return Invisible `NULL`. The function creates directories and saves `.png` files as a side-effect.
#'
#' @import ggplot2
#' @importFrom dplyr filter pull mutate rename group_by arrange ungroup if_else lag
#' @importFrom treemapify geom_treemap geom_treemap_subgroup_border geom_treemap_text geom_treemap_subgroup_text
#' @importFrom lubridate month
#' @importFrom stringr str_remove str_wrap
#' @importFrom scales dollar comma
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate treemaps for 2025 for Nevada
#' qcew_industry_treemap(year = 2025)
#'
#' # Generate treemaps for Idaho (16), Nevada (32), and California (06)
#' qcew_industry_treemap(year = 2025, areas = c("16000", "32000", "06000"))
#'
#' # Generate high-resolution treemaps for Las Vegas and Reno MSAs
#' qcew_industry_treemap(year = 2023, areas = c("C2982", "C3990"), resolution = "large")
#'
#' # Generate treemaps for California state-owned establishments with 4-digit NAICS detail
#' qcew_industry_treemap(year = 2025, areas = c("06000"), breakout_digits = 4, ownerships = 2)
#' }
qcew_industry_treemap <- function(areas = "32000",
                                  ownerships = 5,
                                  period = NULL,
                                  year,
                                  include_suppressed = FALSE,
                                  resolution = "regular",
                                  breakout_digits = 5,
                                  sectors = c("2", "3", "4", "5", "6", "7", "8")) {

  for(area in areas){
    for(ownership in ownerships){

      qcew_download <- BLSloadR::get_qcew(area_code = area, year_start = year-1, year_end = year, silently = TRUE)

      max_qtr_with_data <- qcew_download |>
        filter(disclosure_code == "",
               industry_code != "10",
               year == max(year)) |>
        pull(date) |>
        lubridate::month() |>
        max()

      qcew_download <- qcew_download |>
        filter(lubridate::month(date) == max_qtr_with_data,
               own_code == ownership) |>
        mutate(
          avgemp = round((month1_emplvl + month2_emplvl + month3_emplvl)/3, 0),
          industry_code = as.numeric(industry_code),
          industry_title = stringr::str_remove(industry_title, "NAICS \\d+ ")
        ) |>
        rename(periodyear = year)

      # Dynamic Plot Dimensions
      if(resolution == "large"){
        height <- 8100
        width <- 14400
        tm_text_size <- 100
        plot_title_size <- 75
        plot_subtitle_size <- 60
        filename_base <- "High Resolution Sector "
      } else {
        height <- 1620
        width <- 2880
        tm_text_size <- 20
        plot_title_size <- 15
        plot_subtitle_size <- 12
        filename_base <- "Sector "
      }

      # Dynamic sub-industry breakouts
      if(breakout_digits == 5){
        indcode_min = 9999
        indcode_max = 99999
      } else if(breakout_digits == 6) {
        indcode_min = 99999
        indcode_max = 999999
      } else {
        indcode_min = 999
        indcode_max = 9999
      }

      own_captions <- c(
        "0" = "Data for All Ownerships",
        "1" = "Data for Federal Government Ownership",
        "2" = "Data for State Government Ownership",
        "3" = "Data for Local Government Ownership",
        "4" = "Data for International Government Ownership",
        "5" = "Data for Private Ownership"
      )

      ## Code Execution ##

      sector_data <- qcew_download |>
        filter(industry_code > indcode_min, industry_code < indcode_max) |>
        mutate(codetitle = stringr::str_wrap(industry_title, 20)) |>
        group_by(industry_code) |>
        arrange(periodyear) |>
        mutate(emp_change = avgemp - dplyr::lag(avgemp),
               wage_change = avg_wkly_wage - dplyr::lag(avg_wkly_wage),
               emp_change_direction = dplyr::if_else(emp_change > 0, "+", ""),
               wage_change_direction = dplyr::if_else(wage_change > 0, "+", "")) |>
        ungroup()

      # Only determine period if user didn't supply one in the function arguments
      if (is.null(period)) {
        period <- as.character(sector_data |>
                                 filter(lubridate::month(date) == max_qtr_with_data) |>
                                 pull(qtr) |>
                                 max())
      }

      area_title <- unique(sector_data$area_title)

      for(i in seq_along(sectors)) {

        if(nrow(sector_data |> filter(periodyear == year,
                                      substr(as.character(naics_2d), 1, 1) == sectors[i],
                                      avgemp > 0)
        ) == 0) { next }

        # Build Plot Title and File Name
        plot_title <- paste0("Detailed Breakout for NAICS ", sectors[i], " in ", unique(sector_data$area_title))
        plot_subtitle <- paste0(year, " Q", period, " average data and change from prior year")
        if(include_suppressed) {plot_title <- paste0("Confidential ", plot_title)}

        if(include_suppressed){
          file_name = paste0("images/", year, "-", period, "/", area, "/BLSloadR/Confidential/Confidential - ", filename_base, sectors[i], " - Ownership ", ownership, " - ", area_title, " - ", breakout_digits, "D Detail.png")
        } else {
          file_name = paste0("images/", year, "-", period, "/", area, "/BLSloadR/", filename_base, sectors[i], " Ownership ", ownership, " - ", area_title, " - ", breakout_digits, "D Detail.png")
        }

        # Execute chart
        ggplot(sector_data |> filter(periodyear == year,
                                     substr(as.character(naics_2d), 1, 1) == sectors[i]),
               aes(area = avgemp,
                   fill = avg_wkly_wage,
                   label = paste0(codetitle, "\n",
                                  scales::dollar(avg_wkly_wage, round = 1), " per week (", wage_change_direction, scales::dollar(wage_change, round = 1), ")\n",
                                  scales::comma(avgemp), " workers (", emp_change_direction, scales::comma(emp_change), ")"),
                   subgroup = naics_2d)) +
          treemapify::geom_treemap() +
          treemapify::geom_treemap_subgroup_border(colour = "white", linewidth = 2) +
          treemapify::geom_treemap_text(size = tm_text_size) +
          treemapify::geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                                                 alpha = 0.25, colour = "black",
                                                 fontface = "italic") +
          labs(title = plot_title,
               subtitle = plot_subtitle,
               caption = own_captions[as.character(ownership)]
          ) +
          scale_fill_gradient(low = "white", high = "orange") +
          theme(legend.position = "none",
                plot.title = element_text(size = plot_title_size),
                plot.subtitle = element_text(size = plot_subtitle_size))

        ggsave(filename = file_name, dpi = 150, height = height, width = width, units = "px", limitsize = FALSE, create.dir = TRUE)
        message("Created Image: ", file_name)

      }
    }
  }
}
