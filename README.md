# OMOP Delivery Report Generator

Generate interactive HTML reports for OMOP Common Data Model data deliveries.

-- TODO: Add links to ccc-omop-file-processor and describe the input into this package.

## Installation

```r
# TODO: Install from GitHub instructions here
```

## Usage

```r
library(omopDeliveryReport)

generate_full_omop_report(
  delivery_report_path = "delivery_report.csv",
  dqd_results_path = "dqd_results.csv",
  output_path = "report.html"
)
```

## Processing

```
Input CSVs → Parsing → Calculations → Aggregation → Template Rendering → HTML Output 
```

#### Entry Point & Data Loading

**File:** `generate_full_report.R`

`generate_full_omop_report()` serves as the main entry point. It:
- Loads delivery and DQD CSV files via `data_loaders.R`
- Validates data availability
- Orchestrates the processing pipeline
- Writes the final HTML file

#### Parsing

**File:** `data_parsers.R`

Raw CSV data is parsed into structured R data frames:
- `parse_delivery_metrics()` extracts all metrics from the delivery report CSV using regex patterns
- `group_type_concepts()` categorizes OMOP type concepts into broader groups (EHR, Claims, etc.)
- `create_empty_metrics()` provides empty structures when data is unavailable

The parsed metrics contain table counts, vocabulary breakdowns, harmonization flows, quality metrics, and time series data.

#### Calculations

**File:** `calculations.R`

Calculation functions perform mathematical operations to derive various metrics used in the final report:
- `calculate_harmonization()` determines net row changes from vocabulary mapping
- `calculate_quality_issues()` sums data quality problems
- `calculate_percentage()` performs safe division
- `calculate_num_participants()` and `calculate_total_rows_removed()` compute summary statistics

**File:** `table_processors.R`

DQD-specific calculations:
- `calculate_overall_dqd_score()` computes data quality scores
- `create_dqd_grid()` builds the quality dashboard grid
- `get_table_groups()` organizes tables into logical groupings

#### Aggregation

**File:** `aggregators.R`

Business logic aggregates and combines data across tables and groups:
- `prepare_table_data()` aggregates all metrics for a single table into one structure
- `prepare_all_table_data()` applies table preparation across all tables
- `prepare_report_data()` orchestrates group-level aggregations, calculates transition statistics, and prepares the complete report data structure

This is the core business logic layer where data about the delivery is combined and statistics are calculated.

#### Formatting & Template Preparation

**File:** `formatters.R`

Display formatting functions prepare data for human consumption:
- `format_number()` adds thousands separators
- `format_percentage()` formats percentages
- `format_harmonization_display()` and `format_quality_issues_display()` add appropriate signs and CSS classes
- `get_dqd_score_class()` maps scores to color classes

**File:** `template_data.R`

Template-specific data preparation functions format data structures to match HTML template expectations:
- `prepare_overview_data()` formats overview metrics with warning indicators
- `prepare_dqd_grid_rows()` transforms DQD data into table row structures
- `prepare_time_series_data()` calculates year ranges and serializes time series
- `prepare_vocab_harmonization_data()` aggregates vocabularies for display
- `prepare_delivery_report_data()` formats complete delivery report section data

#### HTML Generation

**File:** `report_builder.R`

`build_complete_html_report()` orchestrates HTML assembly:
1. Calls aggregation functions to prepare report data
2. Serializes data to JSON via `build_report_data_json()` for JavaScript consumption
3. Calls template preparation functions for each report section
4. Renders HTML templates with prepared data
5. Returns complete HTML document

**File:** `template_renderer.R`

Template rendering system:
- `load_template()` reads HTML templates from `inst/templates/`
- `substitute_variables()` replaces `{{variable}}` placeholders with values
- `render_template()` combines loading and substitution
- `render_component_list()` renders lists of items (e.g., table rows)
- `get_full_javascript()` and `get_full_css_styles()` load external assets

Templates are stored in `inst/templates/` and use simple `{{variable}}` syntax for variable substitution.

#### File Output

The final HTML string is written to disk. The HTML contains:
- Embedded CSS styles
- All report sections with rendered data
- JavaScript `REPORT_DATA` object with serialized metrics
- Interactive JavaScript for charts and drilldowns

## Data Flow Example

Tracing a single metric through the pipeline:

1. **Input**: `delivery_report.csv` contains row: `"Valid row count: drug_exposure", "", 12690`
2. **Parsing**: `parse_delivery_metrics()` extracts to `metrics$valid_row_counts` data frame
3. **Aggregation**: `prepare_table_data()` combines with other drug_exposure metrics
4. **Calculation**: `calculate_harmonization()` computes net row changes
5. **Formatting**: `format_number(12690)` produces `"12,690"`
6. **Template Prep**: `prepare_delivery_table_row()` creates display structure
7. **Rendering**: `render_template()` substitutes into HTML template
8. **Output**: HTML contains `<td>12,690</td>` in the delivery table

## Configuration

Configuration is centralized in `config.R`:
- Color schemes for charts and tables
- DQD score thresholds
- Table groupings
- Type concept classification rules

## Function Naming Conventions

- `parse_*()` - Parse raw input into structured data
- `calculate_*()` - Perform mathematical calculations
- `format_*()` - Format values for display
- `prepare_*()` - Aggregate and transform data
- `render_*()` - Generate HTML output
- `get_*()` - Simple accessors and getters
- `build_*()` - Construct complex structures