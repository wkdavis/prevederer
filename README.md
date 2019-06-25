
# prevederer

R package for accessing the Prevedere API. Access to the Prevedere API
requires an API key.

# Installation

``` r
devtools::install_github("wkdavis/prevederer")
```

# Use

## Endpoints

  - `indicator`: returns basic information about an indicator.
  - `indicator_series`: returns the data for an indicator.
  - `correlation`: calculates Pearson’s r and other statistics at
    different offsets between an endogenous and exogenous indicator.
  - `raw_model`: returns all information about a forecast model.
  - `forecast`: returns historical fit and forecasted values of a
    forecast model.
  - `workbench`: returns the indicators used in a workbench.

## Example

``` r
library(prevederer)

api_key = "xyz123"
provider = "provider_a"
provider_id = "a123"

Prevedere.api.set_key(api_key)
Prevedere.api.indicator_series(provider, provider_id)
```
