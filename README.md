# ImpactFunctions

ImpactFunctions is an R package designed to support data processing workflows within **IMPACT REACH** research cycles. The package provides functions for querying Kobo API data, conducting data quality checks, and managing survey metadata and sampling.

## Maintainer
**Alex Stephenson**  
Email: [alex.stephenson@impact-initiatives.org](mailto:alex.stephenson@impact-initiatives.org)

## Functions
The following functions are currently maintained in the package:

- **`access_kobo_api`** – Retrieves survey data from the Kobo API.
- **`clog_check`** – Performs data quality checks for common issues in clogs.
- **`get_kobo_metadata`** – Extracts metadata from Kobo survey forms.
- **`calc_strat_samples`** – Calculates stratified sample sizes.
- **`update_others`** - recode parent values of others

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
survey_data_and_metadata <- get_kobo_metadata(dataset = data_in_processing, asset_id = "antAdT3siLrnjTdfcdYcFY")
```

## Contributing
To contribute, please:
1. **Fork** the repository.
2. **Create a new branch** for your feature or bug fix.
3. **Make your changes** and commit with clear messages.
4. **Submit a pull request** for review.

For bug reports or feature requests, please [open an issue](https://github.com/alex-stephenson/ImpactFunctions/issues).

