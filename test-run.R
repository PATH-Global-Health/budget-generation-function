# -------------------------------------------------------------------
# Minimal end-to-end test harness for generate_budget()
# - Builds toy scen_data, cost_data, target_population
# - Flips spatial_planning_unit across adm1/adm2
# - Tries default vs adjusted assumptions
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
})

# -- 1) Source the function (adjust path if needed) ------------------
if (!exists("generate_budget")) {
  source("template/budget-generation.R")
}

# -- 2) Globals expected by the function -----------------------------
spatial_planning_unit <- "adm2"
local_currency_symbol <- "NGN"

# Minimal target population table (two adm1, three adm2, two years)
target_population <- tibble::tribble(
  ~adm1, ~adm2, ~year, ~pop_total, ~pop_0_5, ~pop_5_10, ~pop_pw, ~pop_0_1, ~pop_1_2, ~pop_vaccine_5_36_months, ~pop_urban,
  "Alpha", "A-1", 2026, 80000, 12000, 8000, 3200, 3500, 3400, 9000, 30000,
  "Alpha", "A-2", 2026, 60000, 9000, 6000, 2400, 2600, 2500, 7000, 20000,
  "Beta", "B-1", 2026, 70000, 10500, 7000, 2800, 3000, 2900, 8000, 25000,
  "Alpha", "A-1", 2027, 82000, 12300, 8200, 3280, 3600, 3500, 9100, 30500,
  "Alpha", "A-2", 2027, 61500, 9200, 6100, 2460, 2650, 2550, 7100, 20500,
  "Beta", "B-1", 2027, 71000, 10700, 7100, 2840, 3050, 2950, 8050, 25500
)

# -- 3) Scenario data ------------------------------------------------
# All interventions "on" so each quantification branch is exercised.
scen_data <- tibble::tribble(
  ~adm1,   ~adm2, ~year, ~scenario_name, ~scenario_description,
  "Alpha", "A-1", 2026,  "S1",           "Baseline mix across all interventions",
  "Alpha", "A-2", 2026,  "S1",           "Baseline mix across all interventions",
  "Beta",  "B-1", 2026,  "S1",           "Baseline mix across all interventions",
  "Alpha", "A-1", 2027,  "S1",           "Baseline mix across all interventions",
  "Alpha", "A-2", 2027,  "S1",           "Baseline mix across all interventions",
  "Beta",  "B-1", 2027,  "S1",           "Baseline mix across all interventions"
) %>%
  mutate(
    # codes: include all
    code_itn_campaign = 1L, type_itn_campaign = "PBO",
    code_itn_routine  = 1L, type_itn_routine  = "Dual AI",
    code_iptp         = 1L, type_iptp         = "SP",
    code_smc          = 1L, type_smc          = "SP+AQ",
    code_pmc          = 1L, type_pmc          = "SP",
    code_vacc         = 1L, type_vacc         = "RTSS"
  )

# -- 4) Cost data ----------------------------------------------------
# Important: 'unit' values MUST match the labels used by generate_budget()
#            after its internal recoding (e.g., "per ITN", "per SP", etc.)
cost_rows <- tribble(
  ~code_intervention, ~type_intervention, ~unit, ~local_currency_cost, ~usd_cost, ~cost_year_for_analysis, ~cost_name, ~cost_description, ~cost_class,
  "itn_campaign", "PBO", "per ITN", 3500, 3.5, 2026, "COST-V1", "Unit costs v1", "Commodity",
  "itn_campaign", "PBO", "per bale", 150000, 150.0, 2026, "COST-V1", "Unit costs v1", "Packaging",
  "itn_routine", "Dual AI", "per ITN", 3600, 3.6, 2026, "COST-V1", "Unit costs v1", "Commodity",
  "iptp", "SP", "per SP", 400, 0.4, 2026, "COST-V1", "Unit costs v1", "Drug",
  "smc", "SMC", "per SPAQ pack 3-11 month olds", 250, 0.25, 2026, "COST-V1", "Unit costs v1", "Drug",
  "smc", "SP+AQ", "per SPAQ pack 12-59 month olds", 300, 0.30, 2026, "COST-V1", "Unit costs v1", "Drug",
  "smc", "SP+AQ", "per SPAQ pack 5–10 years old", 350, 0.35, 2026, "COST-V1", "Unit costs v1", "Drug",
  "smc", "SP+AQ", "per child", 100, 0.10, 2026, "COST-V1", "Unit costs v1", "Service",
  "pmc", "SP", "per SP", 450, 0.45, 2026, "COST-V1", "Unit costs v1", "Drug",
  "pmc", "SP", "per child", 120, 0.12, 2026, "COST-V1", "Unit costs v1", "Service",
  "vacc", "RTSS", "per dose", 4500, 4.50, 2026, "COST-V1", "Unit costs v1", "Commodity",
  "vacc", "RTSS", "per child", 1000, 1.00, 2026, "COST-V1", "Unit costs v1", "Service",
  # Fixed cost (applied nationally by your function)
  "program_mgmt", "Fixed cost", "annual mgmt", 5e7, 50000, 2026, "COST-V1", "National PMU", "Fixed"
) %>%
  # duplicate to 2027 to match scen_data years (or leave NA and let function align)
  bind_rows(mutate(., cost_year_for_analysis = 2027))

cost_data <- cost_rows

# -- 5) Helper to run a single call and print summaries ----------------------
run_and_summarize <- function(spu, assumptions = NULL, label = "") {
  assign("spatial_planning_unit", spu, envir = .GlobalEnv)

  cat("\n\n===============================\n")
  cat("SPU:", spu, "| Label:", label %||% "baseline", "\n")

  out <- generate_budget(
    scen_data = scen_data,
    cost_data = cost_data,
    assumptions = assumptions
  )

  cat("\nOutput rows:", nrow(out), "\nDistinct currencies:", paste(unique(out$currency), collapse = ", "), "\n")

  print(
    out %>%
      group_by(year, currency, code_intervention) %>%
      summarise(total_cost = sum(cost_element, na.rm = TRUE), .groups = "drop") %>%
      arrange(year, currency, code_intervention)
  )

  # Basic checks (fail loudly if something is off)
  stopifnot(all(c("assumptions_changes", "assumption_type", "plan_id") %in% names(out)))
  stopifnot(any(out$type_intervention == "Fixed cost")) # fixed cost reached
  stopifnot(all(out$cost_element >= 0)) # no negatives
  stopifnot(all(out$quantity >= 0)) # no negatives
  stopifnot(any(out$currency == "USD"), any(out$currency == local_currency_symbol))

  invisible(out)
}

# -- 6) Run: baseline assumptions, SPU=adm2 ---------------------------------
out_adm2_base <- run_and_summarize("adm2", assumptions = NULL, label = "adm2 / default assumptions")

out_adm2_base

# -- 7) Run: adjusted assumptions, SPU=adm2 ---------------------------------
assumptions_adj <- c(
  "ITN Campaign: people per net = 2.0",
  "SMC: cycles = 3",
  "Vaccine: number of doses = 3",
  "Vaccine: coverage = 0.9"
)

out_adm2_adj <- run_and_summarize("adm2", assumptions = assumptions_adj, label = "adm2 / adjusted assumptions")
