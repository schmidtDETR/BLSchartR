utils::globalVariables(c(
  # ces_from_qcew and qcew_treemap functions
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
  "peak_change", "percent_below_peak", "months_since_peak", "value_12m",

  # Added for CES visual comparative scripts
  "is_nv", "precovid", "total_nonfarm", "leisure", "dec_19", "set_date_value",
  "ind_share", "total_no_lh", "ind_share_no_lh", "dec_19_change", "dec_19_pct_change",
  "set_change", "set_pct_change", "covid_recovery", "covid_share_recovered",
  "decade_ago_share", "decade_share_change", "rank_value", "rank_otm_lvl",
  "rank_oty_lvl", "rank_otm_pct", "rank_oty_pct", "rank_share", "rank_dec_19_change",
  "rank_set_date_change", "rank_decade_share_change", "value_3mo_avg",
  "otm_pct_3mo_avg", "oty_pct_3mo_avg", "pct_20_oty", "pct_80_oty", "pct_20_val",
  "pct_80_val", "median_oty", "data_type_text", "prior", "current", "period_name",
  "growth_category", "better_worse", "states", "label_text"
))
