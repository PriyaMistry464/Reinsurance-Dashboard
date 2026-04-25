# Reinsurance Strategy Optimizer

An R Shiny dashboard for modeling Excess of Loss (XoL) reinsurance contracts. Adjust retention and limit parameters interactively to see how risk transfer affects your net loss distribution in real time.

## What it does

The app simulates 5,000 insurance claims from a Gamma distribution and applies an Excess of Loss reinsurance structure based on your inputs. It shows you how much of the gross loss you retain versus how much is recovered from the reinsurer — and what your worst-case net exposure looks like.

## Features

- **Interactive sliders** — set the company retention ($1k–$20k) and reinsurance limit ($10k–$100k)
- **Gross vs. Net loss distribution** — density plot comparing the full claim distribution before and after reinsurance
- **Total expected recovery** — sum of all reinsurance recoveries across the simulated portfolio
- **Net VaR 95%** — 95th percentile of the net loss distribution (worst-case retained exposure)
- **CSV export** — download the full simulation results for audit and reporting

## How the model works

Claims are simulated from a **Gamma(shape=2, scale=5,000)** distribution, which is standard in actuarial severity modeling (right-skewed, always positive).

The Excess of Loss (XoL) waterfall:
- **Net loss** = `min(claim, retention)` — what the company keeps
- **Recovery** = `min(max(claim − retention, 0), limit)` — what the reinsurer pays
- Anything above `retention + limit` falls back to the company

## Setup

**Requirements:** R 4.1+

Install dependencies:

```r
install.packages(c("shiny", "bslib", "bsicons", "ggplot2", "dplyr"))
```

**Run the app:**

```r
shiny::runApp("path/to/reinsurance_dashboard")
```

Or open `app.R` in RStudio and click **Run App**.
