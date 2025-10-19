# ImpactFunctions

ImpactFunctions is an R package designed to support data processing workflows within **IMPACT REACH** research cycles. The package provides functions for querying Kobo API data, conducting data quality checks, and managing survey metadata and sampling.

## Maintainer
**Alex Stephenson**  
Email: [alex.stephenson@impact-initiatives.org](mailto:alex.stephenson@impact-initiatives.org)

## Functions
The following functions are currently maintained in the package:

- **`get_kobo_data`** – Retrieves survey data from the Kobo API.
- **`get_kobo_metadata`** – Extracts metadata from Kobo survey forms.
- **`create_xlsx_cleaning_log`** -  this is a fork of the cleaningtools function but there is a drop down for recode_others, speeding up the process massively.
- **`calc_strat_samples`** – Calculates stratified sample sizes.
- **`update_others`** - recode parent values of others.
- **`create_full_loa`** - This creates a loa from your data or from your tool (or both) and you can optionally provide group_vars. Any analysis_vars that are inferred from the data type are printed to the console.
- **`create_full_loa`** - is a wrapper around the whole results table pipeline, so you just input your loa, data, tool and some other info and it makes one results table per disaggregations and one one excel file where each sheet is one of your disaggregations.
- **`get_odds_ratio`** - Performs logistic regression between a binary outcome and multiple categorical grouping variables, returning tidy odds ratios with confidence intervals and p-values.
- **`create_variable_tracker`** - automatically calculates which columns have been added between clean and raw data.

## Installation
You can install **ImpactFunctions** directly from GitHub using **devtools**:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install ImpactFunctions from GitHub
devtools::install_github("alex-stephenson/ImpactFunctions")
```

## Usage
Once installed, you can load the package and use the available functions:

```r
library(ImpactFunctions)

# Example usage
survey_data_and_metadata <- get_kobo_metadata(dataset = data_in_processing, asset_id = "antAdT3siLrnjTdfcdYcFY", un = "alex_stephenson")
```

## Contributing
To contribute, please:
1. **Fork** the repository.
2. **Create a new branch** for your feature or bug fix.
3. **Make your changes** and commit with clear messages.
4. **Submit a pull request** for review.

For bug reports or feature requests, please [open an issue](https://github.com/alex-stephenson/ImpactFunctions/issues).

