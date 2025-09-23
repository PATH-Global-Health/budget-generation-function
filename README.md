# budget-generation-function
Central home for the budget generation scripts used across PATH MACEPA malaria budget apps.

- A single template script defines the universal generate_budget() logic.

- Each country folder contains a minimal copy of the template with country-specific tweaks.

- Apps source the correct script directly from GitHub Raw (country first, then fallback to template).

## Repository Structure
The repository is set up as follows:

``` 
budget-generation-function/
├─ template/
│  └─ budget-generation.R        # canonical template (master copy)
├─ ETH/
│  └─ eth-budget-generation.R    # Ethiopia customization (optional)
├─ DRC/
│  └─ drc-budget-generation.R    # DRC customization (optional)
├─ NGA/
│  └─ nga-budget-generation.R    # Nigeria customization (optional)
└─ README.md
```

### Naming structure

- Template script: `template/budget-generation.R`

- Country scripts: `<ISO3>/<iso3>-budget-generation.R` (lowercase ISO3 prefix in filename, e.g., `nga-budget-generation.R`)

## Quick start (in app)

This code snippet is already in your app’s instance setup script when you downloaded the template (`global/user-defined-country-elements.R`). If your country specific budget generation function exists it will search and load this from the repo but if it does not it will read the template version. 

Ensure you have used the correct `iso3` code to call the correct country function. For example for Ethiopia we would source: 

```
load_budget_function <- function(country_iso3 = "ETH",
                                 repo = "PATH-Org/budget-generation-function",
                                 ref  = "main") {
  iso3 <- toupper(country_iso3)
  iso3_l <- tolower(country_iso3)
  
  # Country-specific path
  country_path <- sprintf("%s/%s-%s", iso3, iso3_l, "budget-generation.R")
  # Template path
  template_path <- "template/budget-generation.R"
  
  raw_base <- sprintf("https://raw.githubusercontent.com/%s/%s/", repo, ref)
  
  # Try country first; if it 404s, source template
  try({
    suppressMessages(source(paste0(raw_base, country_path), local = .GlobalEnv))
  }, silent = TRUE)
  
  if (!exists("generate_budget", mode = "function")) {
    suppressMessages(source(paste0(raw_base, template_path), local = .GlobalEnv))
  }
  
  stopifnot(exists("generate_budget", mode = "function"))
}

# Usage
load_budget_function("ETH") 
``` 

## Modifying or adding a new country

1. Copy the template into a new ISO3 folder:

``` 
mkdir ABC
cp template/budget-generation.R ABC/abc-budget-generation.R
```

2. Edit only what’s necessary (quantification rules, new interventions, support services or fixed costs).

3. Keep the function name and signature identical: `generate_budget(scen_data, cost_data, assumptions)`.

4. If multiple countries need the same tweak, consider upstreaming it into the template behind a small flag (e.g., read from assumptions).

## Changelog

- Template updates: document via Git tags/releases.

- Country scripts: add a header comment block at the top with date, editor, and a one-line change note. Example:
```
# NGA override — 2025-09-23 — J.Doe
# - Adjusted SMC cycles from 3→4; updated dose labels
``` 
## Maintainers

PATH MACEPA | Budget Generation Workstream