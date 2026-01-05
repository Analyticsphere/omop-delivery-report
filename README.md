# OMOP Delivery Report Generator

Generate interactive HTML reports for OMOP Common Data Model data deliveries.

## Installation

```r
# Installation instructions TBD
```

## Usage

```r
source("R/generate_full_report.R")

generate_full_omop_report(
  delivery_report_path = "delivery_report.csv",
  dqd_results_path = "dqd_results.csv",
  output_path = "report.html"
)
```

## Documentation

See function documentation in R source files for details on parameters and configuration options.
