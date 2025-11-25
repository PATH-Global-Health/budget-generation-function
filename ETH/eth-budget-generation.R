# =============================================================================
# File: helpers/10-generate-budget.R
# Purpose: Budget generation: quantify requirements, apply unit costs, build long budget
# Depends on: dplyr, tidyr, purrr, tibble, rlang, tidyselect, stats; internal: `%||%` (from 00-utils-base.R); globals: spatial_planning_unit, local_currency_symbol, target_population
# =============================================================================

#' Generate detailed intervention budget from scenarios & costs
#'
#' Quantifies product/service needs from scenario coverage, applies unit costs,
#' and returns a long-form budget suitable for tables/maps/plots.
#'
#' @param scen_data Data frame of implementation scenarios. Must include columns:
#'   - Spatial keys: `adm1`, `adm2` (and optionally `adm3`) depending on SPU.
#'   - `year`, `scenario_name`, `scenario_description`
#'   - intervention code columns like `code_itn_campaign`, `code_itn_routine`, `code_iptp`,
#'     `code_smc`, `code_pmc`, `code_vacc`, and corresponding `type_*` columns.
#' @param cost_data Data frame of unit/delivery costs containing at least:
#'   `code_intervention`, `type_intervention`, `unit`, `local_currency_cost`, `usd_cost`,
#'   and `cost_year_for_analysis` (may be NA, in which case it is matched to scenario year).
#' @param assumptions Character vector of "Label = value" strings; optional.
#'
#' @return A data frame with columns (subset depending on SPU):\n#'   `adm1`, `adm2`, `adm3`, `year`, `scenario_name`, `scenario_description`,\n#'   `cost_name`, `cost_description`, `code_intervention`, `type_intervention`,\n#'   `target_pop`, `unit`, `quantity`, `cost_class`, `currency`, `unit_cost`,\n#'   `cost_element`, `intervention_nice`, `assumptions_changes`, `assumption_type`, `plan_id`.
#' @details
#' - Expects global `spatial_planning_unit` to be set to "adm1"/"adm2"/"adm3".
#' - Expects global `local_currency_symbol` (used to label non-USD currency rows).
#' - Expects global `target_population` table with the necessary population columns.
#' - Assumption parsing: strings of form `"Label = value"` are evaluated on the right-hand side.
#' - Fixed costs in `cost_data` (where `type_intervention == "Fixed cost"`) are applied nationally.
#' - Output keeps both currencies by pivoting cost columns to `currency` and `unit_cost`.
#'
#' @family helpers-budget
#' @noRd
generate_budget <- function(scen_data, cost_data, assumptions) {
  # --- ENV OPTIONS ----------------------------------------------------------
  # spatial level + currency symbol
  spu <- get("spatial_planning_unit", envir = .GlobalEnv) %||% "adm2"
  local_symbol <- get("local_currency_symbol", envir = .GlobalEnv) %||% "LOCAL"

  # validate spatial_planning_unit
  if (!spu %in% c("adm1", "adm2", "adm3")) {
    warning("Unrecognized spatial_planning_unit = '", spu, "'. Falling back to 'adm2'.")
    spu <- "adm2"
  }

  # Which spatial columns to include in outputs / joins
  spu_cols <- switch(spu,
    "adm1" = c("adm1"),
    "adm2" = c("adm1", "adm2"),
    "adm3" = c("adm1", "adm2", "adm3")
  )

  sel_spu <- function(...) dplyr::all_of(c(spu_cols, ...))

  # --- CONSOLE SUMMARY ------------------------------------------------------
  cat("Costing scenario being generated for the following mix of interventions:\n")

  code_cols <- grep("^code_", names(scen_data), value = TRUE)
  if (length(code_cols) == 0L) {
    cat("No intervention columns starting with 'code_'; skipping summary.\n")
  } else {
    has1 <- "adm1" %in% names(scen_data)
    has2 <- all(c("adm1", "adm2") %in% names(scen_data))
    has3 <- all(c("adm1", "adm2", "adm3") %in% names(scen_data))

    summary_tbl <-
      scen_data %>%
      dplyr::mutate(
        year = suppressWarnings(as.integer(year)),
        adm1_key = if (has1) adm1 else NA_character_,
        adm2_key = if (has2) paste(adm1, adm2, sep = "_") else NA_character_,
        adm3_key = if (has3) paste(adm1, adm2, adm3, sep = "_") else NA_character_
      ) %>%
      dplyr::select(
        dplyr::any_of(c(
          spu_cols, "year", "scenario_name", "scenario_description",
          code_cols, "adm1_key", "adm2_key", "adm3_key"
        ))
      ) %>%
      tidyr::pivot_longer(
        dplyr::all_of(code_cols),
        names_to = "intervention",
        names_prefix = "code_",
        values_to = "included"
      ) %>%
      dplyr::mutate(
        included = dplyr::case_when(
          is.logical(included) ~ as.integer(included),
          is.numeric(included) ~ as.integer(included == 1),
          is.character(included) ~ as.integer(tolower(trimws(included)) %in% c("1", "true", "yes", "y")),
          TRUE ~ 0L
        )
      ) %>%
      dplyr::filter(included == 1L) %>%
      dplyr::group_by(intervention, year) %>%
      dplyr::summarise(
        adm1_targeted = if (has1) dplyr::n_distinct(adm1_key[!is.na(adm1_key)]) else NA_integer_,
        adm2_targeted = if (has2) dplyr::n_distinct(adm2_key[!is.na(adm2_key)]) else NA_integer_,
        adm3_targeted = if (has3) dplyr::n_distinct(adm3_key[!is.na(adm3_key)]) else NA_integer_,
        .groups = "drop"
      )
    print(summary_tbl)
  }

  cat(scen_data$scenario_description[1] %||% "", "\n")

  # --- COST DATA ------------------------------------------------------------
  cost_data <- cost_data |>
    dplyr::filter(!is.na(local_currency_cost)) |>
    dplyr::mutate(cost_year_for_analysis = as.integer(cost_year_for_analysis))

  cost_data_expanded <- scen_data |>
    dplyr::distinct(year) |>
    tidyr::crossing(cost_data) |>
    dplyr::mutate(
      cost_year_for_analysis = dplyr::if_else(is.na(cost_year_for_analysis), year, cost_year_for_analysis)
    ) |>
    dplyr::filter(cost_year_for_analysis == year)

  # --- ASSUMPTIONS ----------------------------------------------------------
  assumptions <- unlist(assumptions)
  target_population <- target_population

  if (!is.null(assumptions) && length(assumptions) > 0) {
    assumption_list <- purrr::map_chr(assumptions, ~.x) |>
      rlang::set_names(purrr::map_chr(strsplit(assumptions, " = "), 1)) |>
      purrr::map(~ eval(parse(text = strsplit(.x, " = ")[[1]][2])))
  } else {
    assumption_list <- list()
  }

  get_assumption <- function(label, default) {
    if (!is.null(assumption_list[[label]])) assumption_list[[label]] else default
  }

  itn_campaign_divisor <- get_assumption("ITN Campaign: people per net", 1.8)
  itn_campaign_bale_size <- get_assumption("ITN Campaign: nets per bale", 50)
  itn_campaign_buffer_mult <- 1 + get_assumption("ITN Campaign: buffer (%)", 0.10)
  itn_campaign_coverage <- get_assumption("ITN Campaign: target population coverage", 1.00)

  itn_routine_coverage <- get_assumption("ITN Routine: target population coverage", 0.30)
  itn_routine_buffer_mult <- 1 + get_assumption("ITN Routine: buffer (%)", 0.10)

  iptp_anc_coverage <- get_assumption("IPTp: ANC attendance", 0.80)
  iptp_doses_per_pw <- get_assumption("IPTp: contact points", 3)
  iptp_buffer_mult <- 1 + get_assumption("IPTp: drug supply buffer", 0.10)

  smc_age_string <- get_assumption("SMC: age targeting", "0.18,0.77")
  smc_split <- as.numeric(strsplit(smc_age_string, ",")[[1]])
  smc_pop_prop_3_11 <- smc_split[1]
  smc_pop_prop_12_59 <- smc_split[2]
  smc_coverage <- get_assumption("SMC: target population coverage", 1.00)
  smc_monthly_rounds <- get_assumption("SMC: cycles", 4)
  smc_buffer_mult <- 1 + get_assumption("SMC: drug supply buffer", 0.10)

  pmc_coverage <- get_assumption("PMC: coverage", 0.85)
  pmc_touchpoints <- get_assumption("PMC: contact points", 4)
  pmc_tablet_factor <- get_assumption("PMC: nutrition scaling factor", 0.75)
  pmc_buffer_mult <- 1 + get_assumption("PMC: drug supply buffer", 0.10)

  vacc_coverage <- get_assumption("Vaccine: coverage", 0.84)
  vacc_doses_per_child <- get_assumption("Vaccine: number of doses", 4)
  vacc_buffer_mult <- 1 + get_assumption("Vaccine: supply buffer", 0.10)

  get_pop_column <- function(label, default_col) {
    pop_assumption <- assumption_list[[label]]
    if (is.null(pop_assumption)) {
      return(default_col)
    }

    mapping <- list(
      "Total population"                           = "pop_total",
      "Children under 5"                           = "pop_0_5",
      "Children under 5 and pregnant women"        = c("pop_0_5", "pop_pw"),
      "Children under 10"                          = c("pop_0_5", "pop_5_10"),
      "Children 3 months to 5 years"               = "pop_0_5",
      "Children 3 months to 10 years"              = c("pop_0_5", "pop_5_10"),
      "Children 0-1"                               = "pop_0_1",
      "Children 1-2"                               = "pop_1_2",
      "Children 5-10"                              = "pop_5_10",
      "Children 5–36 months"                       = "pop_vaccine_5_36_months",
      "Pregnant women"                             = "pop_pw",
      "Urban population"                           = "pop_urban"
    )

    mapped_col <- mapping[[pop_assumption]]
    if (!is.null(mapped_col)) {
      mapped_col
    } else {
      warning(paste("Unrecognized target population assumption:", pop_assumption))
      default_col
    }
  }

  itn_campaign_pop_col <- get_pop_column("ITN Campaign: target population", "pop_total")
  itn_routine_pop_col <- get_pop_column("ITN Routine: target population", c("pop_0_5", "pop_pw"))
  smc_pop_col <- get_pop_column("SMC: target population", "pop_0_5")

  # --- QUANTIFICATION HELPERS -----------------------------------------------
  join_keys <- spu_cols

  safe_quantification <- function(df) {
    if (nrow(df) == 0) {
      tibble::tibble(
        adm1 = character(), adm2 = character(), adm3 = character(), year = integer(),
        scenario_name = character(), scenario_description = character(),
        code_intervention = character(), type_intervention = character(),
        target_pop = numeric(), unit = character(), quantity = numeric()
      )
    } else {
      df
    }
  }

  # --- ITN CAMPAIGN ----------------------------------------------------------
  itn_campaign_quantifications <-
    scen_data |>
    dplyr::select(
      dplyr::all_of(c(spu_cols, "year")), dplyr::contains("itn_campaign"),
      scenario_name, scenario_description
    ) |>
    dplyr::filter(code_itn_campaign == 1) |>
    dplyr::left_join(
      target_population |>
        dplyr::select(dplyr::all_of(c(spu_cols, "year", itn_campaign_pop_col))),
      by = c(stats::setNames(join_keys, join_keys), "year" = "year")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(target_pop = sum(dplyr::c_across(dplyr::all_of(itn_campaign_pop_col)), na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::all_of(itn_campaign_pop_col)) |>
    dplyr::mutate(
      quant_nets = ((target_pop * itn_campaign_coverage) / itn_campaign_divisor) * itn_campaign_buffer_mult,
      quant_bales = quant_nets / itn_campaign_bale_size,
      target_pop = target_pop * itn_campaign_coverage,
      code_intervention = "itn_campaign",
      type_intervention = type_itn_campaign
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("quant"),
      names_to = "unit", values_to = "quantity", names_prefix = "quant_"
    ) |>
    dplyr::mutate(unit = dplyr::case_when(
      unit == "nets" ~ "per ITN",
      unit == "bales" ~ "per bale",
      TRUE ~ unit
    ))

  # --- ITN ROUTINE -----------------------------------------------------------
  itn_routine_quantifications <-
    scen_data |>
    dplyr::select(
      dplyr::all_of(c(spu_cols, "year")), dplyr::contains("itn_routine"),
      scenario_name, scenario_description
    ) |>
    dplyr::filter(code_itn_routine == 1) |>
    dplyr::left_join(
      target_population |>
        dplyr::select(dplyr::all_of(c(spu_cols, "year", itn_routine_pop_col))),
      by = c(stats::setNames(join_keys, join_keys), "year" = "year")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(target_pop = sum(dplyr::c_across(dplyr::all_of(itn_routine_pop_col)), na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::all_of(itn_routine_pop_col)) |>
    dplyr::mutate(
      quant_nets = (target_pop * itn_routine_coverage) * itn_routine_buffer_mult,
      code_intervention = "itn_routine",
      type_intervention = type_itn_routine
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("quant"),
      names_to = "unit", values_to = "quantity", names_prefix = "quant_"
    ) |>
    dplyr::mutate(unit = dplyr::case_when(
      unit == "nets" ~ "per ITN",
      unit == "bales" ~ "per bale",
      TRUE ~ unit
    ))

  # --- IPTp ------------------------------------------------------------------
  iptp_quantifications <-
    scen_data |>
    dplyr::select(
      dplyr::all_of(c(spu_cols, "year")), dplyr::contains("iptp"),
      scenario_name, scenario_description
    ) |>
    dplyr::filter(code_iptp == 1) |>
    dplyr::left_join(
      target_population |>
        dplyr::select(dplyr::all_of(c(spu_cols, "year", "pop_pw"))),
      by = c(stats::setNames(join_keys, join_keys), "year" = "year")
    ) |>
    dplyr::mutate(
      quant_sp_doses = ((pop_pw * iptp_anc_coverage) * iptp_doses_per_pw) * iptp_buffer_mult,
      target_pop = pop_pw,
      code_intervention = "iptp",
      type_intervention = type_iptp
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("quant"),
      names_to = "unit", values_to = "quantity", names_prefix = "quant_"
    ) |>
    dplyr::mutate(unit = dplyr::case_when(unit == "sp_doses" ~ "per SP", TRUE ~ unit))

  # --- SMC -------------------------------------------------------------------
  include_5_10 <- any(grepl("5_10", smc_pop_col))
  smc_quantification <-
    scen_data |>
    dplyr::select(
      dplyr::all_of(c(spu_cols, "year")), dplyr::contains("smc"),
      scenario_name, scenario_description
    ) |>
    dplyr::filter(code_smc == 1) |>
    dplyr::left_join(
      target_population |>
        dplyr::select(dplyr::all_of(c(spu_cols, "year", smc_pop_col))),
      by = c(stats::setNames(join_keys, join_keys), "year" = "year")
    ) |>
    dplyr::mutate(
      quant_smc_spaq_3_11_months = ((pop_0_5 * smc_pop_prop_3_11) * smc_coverage) * smc_monthly_rounds * smc_buffer_mult,
      quant_smc_spaq_12_59_months = ((pop_0_5 * smc_pop_prop_12_59) * smc_coverage) * smc_monthly_rounds * smc_buffer_mult,
      quant_smc_spaq_5_10_years =
        case_when(
          include_5_10 == TRUE ~ (pop_5_10 * smc_coverage) * smc_monthly_rounds * smc_buffer_mult,
          TRUE ~ 0
        ),
      quant_smc_child =
        case_when(
          include_5_10 == TRUE ~ ((pop_0_5 * (smc_pop_prop_3_11 + smc_pop_prop_12_59)) + pop_5_10) * smc_coverage,
          TRUE ~ (pop_0_5 * (smc_pop_prop_3_11 + smc_pop_prop_12_59)) * smc_coverage
        ),
      target_pop = quant_smc_child,
      code_intervention = "smc",
      type_intervention = type_smc
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("quant"),
      names_to = "unit", values_to = "quantity", names_prefix = "quant_smc_"
    ) |>
    dplyr::mutate(
      unit = dplyr::case_when(
        unit == "spaq_3_11_months" ~ "per SPAQ pack 3-11 month olds",
        unit == "spaq_12_59_months" ~ "per SPAQ pack 12-59 month olds",
        unit == "spaq_5_10_years" ~ "per SPAQ pack 5–10 years old",
        unit == "child" ~ "per child",
        TRUE ~ unit
      )
    )

  # --- PMC -------------------------------------------------------------------
  pmc_quantification <-
    scen_data |>
    dplyr::select(
      dplyr::all_of(c(spu_cols, "year")), dplyr::contains("pmc"),
      scenario_name, scenario_description
    ) |>
    dplyr::filter(code_pmc == 1) |>
    dplyr::left_join(
      target_population |>
        dplyr::select(dplyr::all_of(c(spu_cols, "year", "pop_0_1", "pop_1_2"))),
      by = c(stats::setNames(join_keys, join_keys), "year" = "year")
    ) |>
    dplyr::mutate(
      quant_pmc_sp_0_1_years = pop_0_1 * pmc_coverage * pmc_touchpoints * pmc_tablet_factor * pmc_buffer_mult,
      quant_pmc_sp_1_2_years = pop_1_2 * pmc_coverage * pmc_touchpoints * 2 * pmc_tablet_factor * pmc_buffer_mult,
      quant_pmc_sp_total     = quant_pmc_sp_0_1_years + quant_pmc_sp_1_2_years,
      quant_pmc_child        = pop_0_1 * pmc_coverage + pop_1_2 * pmc_coverage,
      target_pop             = quant_pmc_child,
      code_intervention      = "pmc",
      type_intervention      = type_pmc
    ) |>
    dplyr::select(-quant_pmc_sp_0_1_years, -quant_pmc_sp_1_2_years) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("quant"),
      names_to = "unit", values_to = "quantity", names_prefix = "quant_pmc_"
    ) |>
    dplyr::mutate(
      unit = dplyr::case_when(
        unit == "sp_total" ~ "per SP",
        unit == "child" ~ "per child",
        TRUE ~ unit
      )
    )

  # --- VACCINE ---------------------------------------------------------------
  vacc_quantification <-
    scen_data |>
    dplyr::select(
      dplyr::all_of(c(spu_cols, "year")), dplyr::contains("vacc"),
      scenario_name, scenario_description
    ) |>
    dplyr::filter(code_vacc == 1) |>
    dplyr::left_join(
      target_population |>
        dplyr::select(dplyr::all_of(c(spu_cols, "year", "pop_vaccine_5_36_months"))),
      by = c(stats::setNames(join_keys, join_keys), "year" = "year")
    ) |>
    dplyr::mutate(
      quant_vacc_doses = pop_vaccine_5_36_months * vacc_coverage * vacc_buffer_mult * vacc_doses_per_child,
      quant_vacc_child = pop_vaccine_5_36_months * vacc_coverage,
      target_pop = quant_vacc_child,
      code_intervention = "vacc",
      type_intervention = type_vacc
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("quant"),
      names_to = "unit", values_to = "quantity", names_prefix = "quant_vacc_"
    ) |>
    dplyr::mutate(
      unit = dplyr::case_when(
        unit == "doses" ~ "per dose",
        unit == "child" ~ "per child",
        TRUE ~ unit
      )
    )

  # --- COMBINE & COST --------------------------------------------------------
  budget <-
    dplyr::bind_rows(
      safe_quantification(itn_campaign_quantifications),
      safe_quantification(itn_routine_quantifications),
      safe_quantification(iptp_quantifications),
      safe_quantification(smc_quantification),
      safe_quantification(pmc_quantification),
      safe_quantification(vacc_quantification)
    ) |>
    dplyr::left_join(
      cost_data_expanded |> dplyr::select(-year, -original_unit_cost),
      by = c("code_intervention", "type_intervention", "unit", "year" = "cost_year_for_analysis")
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_cost"),
      names_to = "currency",
      values_to = "unit_cost"
    ) |>
    dplyr::mutate(
      cost_element = quantity * unit_cost,
      currency = dplyr::if_else(currency == "usd_cost", "USD", local_symbol),
      intervention_nice = dplyr::case_when(
        code_intervention == "cm_public" ~ "Case Management Public",
        code_intervention == "cm_private" ~ "Case Management Private",
        code_intervention == "iptp" ~ "IPTp",
        code_intervention == "vacc" ~ "Vaccine",
        code_intervention == "itn_routine" ~ "Routine ITN",
        code_intervention == "itn_campaign" ~ "Campaign ITN",
        code_intervention == "smc" ~ "SMC",
        code_intervention == "pmc" ~ "PMC",
        code_intervention == "irs" ~ "IRS",
        code_intervention == "lsm" ~ "LSM",
        TRUE ~ code_intervention
      )
    ) |>
    dplyr::select(
      dplyr::all_of(spu_cols), year,
      scenario_name, scenario_description,
      cost_name, cost_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity,
      cost_class, currency, unit_cost, cost_element,
      intervention_nice
    ) |>
    dplyr::filter(cost_element != 0)

  # Fixed costs (national-level currently)
  fixed_budget <- cost_data_expanded |>
    dplyr::filter(type_intervention == "Fixed cost") |>
    tidyr::pivot_longer(
      cols = tidyselect::ends_with("_cost"),
      names_to = "currency",
      values_to = "unit_cost"
    ) |>
    dplyr::mutate(
      currency = dplyr::if_else(currency == "usd_cost", "USD", local_symbol),
      adm1 = NA_character_, adm2 = NA_character_, adm3 = NA_character_,
      scenario_name = unique(scen_data$scenario_name)[1],
      scenario_description = unique(scen_data$scenario_description)[1],
      cost_name = cost_data$cost_name[1],
      cost_description = cost_data$cost_description[1],
      target_pop = NA_real_,
      quantity = 1,
      cost_element = unit_cost * quantity,
      intervention_nice = dplyr::case_when(
        code_intervention == "cm_public" ~ "Case Management Public",
        code_intervention == "cm_private" ~ "Case Management Private",
        code_intervention == "iptp" ~ "IPTp",
        code_intervention == "vacc" ~ "Vaccine",
        code_intervention == "itn_routine" ~ "Routine ITN",
        code_intervention == "itn_campaign" ~ "Campaign ITN",
        code_intervention == "smc" ~ "SMC",
        code_intervention == "pmc" ~ "PMC",
        code_intervention == "irs" ~ "IRS",
        code_intervention == "lsm" ~ "LSM",
        TRUE ~ code_intervention
      )
    ) |>
    dplyr::select(
      adm1, adm2, adm3, year,
      scenario_name, scenario_description,
      cost_name, cost_description,
      code_intervention, type_intervention,
      target_pop, unit, quantity,
      cost_class, currency, unit_cost, cost_element,
      intervention_nice
    )

  budget_final <- dplyr::bind_rows(budget, fixed_budget) |>
    dplyr::select(dplyr::all_of(c(
      spu_cols, "year",
      "scenario_name", "scenario_description",
      "cost_name", "cost_description",
      "code_intervention", "type_intervention",
      "target_pop", "unit", "quantity",
      "cost_class", "currency", "unit_cost", "cost_element",
      "intervention_nice"
    )))

  # --- ASSUMPTION SUMMARY & PLAN ID -----------------------------------------
  assumption_summary <- if (length(assumption_list) > 0) {
    paste(names(assumption_list), unlist(assumption_list), sep = " = ", collapse = "; ")
  } else {
    "default values"
  }

  budget_final <- budget_final |>
    dplyr::mutate(
      assumptions_changes = assumption_summary,
      assumption_type = dplyr::if_else(assumption_summary == "default values", "baseline assumptions", "adjusted assumptions"),
      plan_id = paste0(scenario_name, " with ", cost_name, " with ", assumption_type),
      plan_id = dplyr::if_else(
        assumption_type == "adjusted assumptions",
        paste0(plan_id, " (", assumptions_changes, ")"),
        plan_id
      )
    )

  return(budget_final)
}

#' Produce a safe HTML/shiny id from a human label
#' @param label character(1)
#' @return character(1) alnum/underscore-only id
#' @family helpers-budget
#' @noRd
safe_id <- function(label) {
  cleaned <- gsub("[^a-zA-Z0-9_]", "_", label)
  cleaned <- gsub("_+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  cleaned
}
