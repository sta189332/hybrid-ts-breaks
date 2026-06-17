download_real_world_data <- function() {
  message('Add official data-download code here after verifying source access conditions.')
  message('Recommended sources: CBN, NBS, FRED, EIA, World Bank, IMF, NGX where accessible.')
}

prepare_real_world_dataset_register <- function() {
  tibble::tibble(
    dataset = c('NGN/USD exchange rate', 'Nigeria CPI/inflation', 'Brent crude oil', 'S&P 500'),
    source = c('CBN/FRED', 'NBS/CBN', 'FRED/EIA/World Bank', 'FRED/Yahoo Finance'),
    frequency = c('Daily/Monthly', 'Monthly', 'Daily/Monthly', 'Daily'),
    relevance = c(
      'Exchange-rate reforms and depreciation regimes',
      'Inflation persistence and policy shocks',
      'Oil-price volatility relevant to Nigeria',
      'External global financial benchmark'
    )
  )
}

