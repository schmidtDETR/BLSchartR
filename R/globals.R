utils::globalVariables(c(
  # From previous functions
  "industry_code", "own_code", "naics_code", "month1_emplvl",
  "month2_emplvl", "month3_emplvl", "year", "qtr", "month_name",
  "employment", "area_title", "month", "date", "seriesname", ".x",
  "disclosure_code", "industry_title", "avgemp", "periodyear",
  "codetitle", "emp_change", "wage_change", "avg_wkly_wage",
  "emp_change_direction", "wage_change_direction", "naics_2d",

  # Added for generate_ces_report
  "data_type_code", "state_name", "area_name", "period", "state_code",
  "seasonal", "benchmark_year", "value", "industry_name", "prior_year",
  "yoychange", "yoypercent", "prior_month", "momchange", "mompercent",
  "m2_change", "m3_value", "m3_change", "m60_value", "m60_change",
  "m3_annualized", "m60_annualized", "peak_5yr", "peak_date",
  "peak_change", "percent_below_peak", "months_since_peak", "value_12m"
))
