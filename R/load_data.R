## ----read-data-child-libs----------------------------------------------------------

# Load libraries
library(here)
library(readxl)
library(tidyverse)
library(fuzzyjoin)
library(vctrs)
library(lubridate)
library(janitor)
library(tabylextra)
library(kableExtra)
library(scales)
library(knitr)
library(rmarkdown)
library(bookdown)
library(pander)
library(ComplexUpset)
library(ggrepel)
library(sqldf)
library(zoo)
library(hablar)
library(tigris)
library(sf)
library(ggspatial)
library(ggthemes)
library(ggnewscale)
library(classInt)
library(xfun)
library(RcppRoll)
library(DiagrammeR)
library(webshot)
library(RMariaDB)
library(DBI)
library(dbplyr)
library(networkD3)
library(httr)


## ----custom-functions--------------------------------------------------------------

# Set global chunk options using opts_chunk. This allows us to get away from
# adding these commands to each chunk label.
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  results = "asis", 
  fig.pos = "H"
)

#Set Okabe Ito colorblind palette using ggthemes.
okabe_ito_palette <- ggthemes::colorblind_pal()(8)

# Set big mark with Pander, which is a Pandoc writer for R.
panderOptions(
  "big.mark",
  ","
)

# Set Tigris to cache shapefiles. Create "sf" objects by default.
options(
  tigris_use_cache = TRUE,
  tigris_class = "sf"
)
# Define custom function for rounding behavior similar to that found in Excel.
# Will round up at 5.
round.off <- function (
    x,
    digits = 0
) {
  posneg = sign(
    x
  )
  z = trunc(
    abs(
      x
    ) * 10 ^ (
      digits + 1
    )
  ) / 10
  z = floor(
    z * posneg + 0.5
  ) / 10 ^ digits
  return(
    z
  )
}

# Define pct_format object as a function using label_percent() from scales at
# accuracy = 0.1. The purpose is to use this within a mutate when calculating
# percentages. label_percent returns a function, which is a problem if used
# directly within a mutate, because mutate expects a vector. By creating this
# object and then using pct_format() in place of label_percent() in the mutate,
# the mutate works without error.
pct_format <- scales::label_percent(
  accuracy = 0.1
)

# Create pct_format_round.
pct_format_round <- scales::label_percent(
  accuracy = 1
)

# Define custom function to negate %in% for use in determining when a value is
# not in a column or vector.
`%nin%` <- Negate(
  `%in%`
)

# Define custom function to be used in a select(where()) to remove columns where
# all of the values are NA. The reason this is needed is that ETO Results merges
# some columns, which R un-merges, producing unnamed columns with all null
# values.
not_all_na <- function(
    x2
) any(
  !is.na(
    x2
  )
)

# Create a custom function to center text vertically when a row wraps in a
# kable.
centerText <- function(text){
  paste0(
    "\\multirow{2}{*}[0pt]{", text, "}"
  )
}

# Function to add line break to current_fiscal_quarter_year_string.
fiscal_qrt_yr_abbrev <- function(
    target_quarter_year_string
){
  stringr::str_c(
    stringr::str_sub(
      target_quarter_year_string,
      1,
      2
    ),
    "\n",
    "FY",
    stringr::str_sub(
      target_quarter_year_string,
      8
    ),
    collapse = ""
  )
}


## ----additional-parameters---------------------------------------------------------

# Additional parameters

# End date is used to determine which forms are not yet due that should be
# excluded. choose end_date base on the project you are doing

# Set end date as next month of begin date plus 6 days
# end_date_param <- params$start_date %m+% months(1) %m+% days(6)

# Set end date as today 
# end_date_param <- today(
#   )

end_date_param <- params$end_date %m+% 
  days(
    7
  ) 

#end_date_param <- as_date(
#  as.POSIXct(
#    "2020-03-31 00:00:01"
#    )
#  )


# Quarter of the fiscal year of the reporting period.
report_fiscal_quarter_int <- as.integer(
  quarter(
    params$start_date,
    fiscal_start = 7
  )
)

# Fiscal year of the reporting period.
report_fiscal_year <- as.integer(
  quarter(
    params$start_date,
    with_year = TRUE,
    fiscal_start = 7
  )
)

# Prior fiscal year.
prior_fiscal_year <- report_fiscal_year - 1

# The fiscal year before the prior fiscal year.
year_before_prior_fiscal_year <- report_fiscal_year - 2

# Start date used to calculate engagement and admission by quarter
#04/01/2021

start_date_eng_first_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year - 1
    ),
    "-07-01"
  )
) %m-% 
  months(
    3
  )

#07/01/2021
start_date_eng_second_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year - 1
    ),
    "-07-01"
  )
) %m+% 
  months(
    0
  )


#10/01/2021
start_date_eng_third_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year-1
    ),
    "-07-01"
  )
) %m+% 
  months(
    3
  )

#01/01/2022
start_date_eng_fourth_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year-1
    ),
    "-07-01"
  )
) %m+% 
  months(
    6
  )


# End date used to calculate engagement and admission by quarter
#06/30/2021
end_date_eng_first_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year-1
    ),
    "-07-01"
  )
) %m-%
  days(
    1
  )

#09/30/2021
end_date_eng_second_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year-1
    ),
    "-07-01"
  )
) %m+% 
  months(
    3
  ) %m-% 
  days(
    1
  )

# 12/31/2021
end_date_eng_third_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year-1
    ),
    "-07-01"
  )
) %m+% 
  months(
    6
  ) %m-% 
  days(
    1
  )

# 03/31/2021
end_date_eng_fourth_qtr_current_fiscal_year <- lubridate::as_date(
  paste0(
    as.integer(
      report_fiscal_year-1
    ),
    "-07-01"
  )
) %m+% 
  months(
    9
  ) %m-% 
  days(
    1
  )

# Reporting fiscal quarter represented as "Q[QUARTER]".
report_fiscal_quarter_string <- str_c(
  vctrs::vec_c(
    "Q", as.character(
      quarter(
        params$start_date,
        fiscal_start = 7
      )
    )
  ),
  collapse = ""
)

# Reporting fiscal year represented as "FY[YEAR]".
report_fiscal_year_string <- str_c(
  vctrs::vec_c(
    "FY", as.character(
      report_fiscal_year
    )
  ), 
  collapse = ""
)

# Prior fiscal year represented as "FY[YEAR]".
prior_fiscal_year_string <- str_c(
  vctrs::vec_c(
    "FY", as.character(
      prior_fiscal_year
    )
  ),
  collapse = ""
)

# The fiscal year before the prior fiscal year represented as "FY[YEAR]".
year_before_prior_fiscal_year_string <- str_c(
  vctrs::vec_c(
    "FY", as.character(
      year_before_prior_fiscal_year
    )
  ),
  collapse = ""
)

# Reporting fiscal quarter end month and year.
report_fiscal_quarter_end_month_year_string <- str_c(
  as.character(
    lubridate::month(
      ceiling_date(
        params$start_date,
        "quarter"
      ) - 
        months(
          1
        ),
      label = TRUE,
      abbr = FALSE
    )
  ),
  " ",
  as.character(
    as.integer(
      quarter(
        params$start_date,
        with_year = TRUE,
        fiscal_start = 1
      )
    )
  )
)

# Quarter and four digit fiscal year for the current reporting fiscal quarter.
current_fiscal_quarter_year_string <- str_c(
  vctrs::vec_c(
    "Q",
    as.character(
      quarter(
        floor_date(
          params$start_date,
          "quarter"
        ),
        fiscal_start = 7
      )
    ),
    " FY",
    as.character(
      as.integer(
        quarter(
          params$start_date,
          with_year = TRUE,
          fiscal_start = 7
        )
      )
    )
  ),
  collapse = ""
)


# Quarter and two digit fiscal year for the current reporting fiscal quarter.
current_fiscal_quarter_year_string_abbrev <- str_c(
  vctrs::vec_c(
    "Q",
    as.character(
      quarter(
        floor_date(
          params$start_date,
          "quarter"
        ),
        fiscal_start = 7
      )
    ),
    " FY",
    as.character(
      str_sub(
        as.integer(
          quarter(
            params$start_date,
            with_year = TRUE,
            fiscal_start = 7
          )
        ),
        3
      )
    )
  ),
  collapse = ""
)

# Current fiscal year string abbreviated.
current_fiscal_year_string_abbrev <- str_c(
  vctrs::vec_c(
    "FY",
    as.character(
      str_sub(
        as.integer(
          quarter(
            params$start_date,
            with_year = TRUE,
            fiscal_start = 7
          )
        ),
        3
      )
    )
  ),
  collapse = ""
)

# Prior fiscal year string abbreviated.
prior_fiscal_year_string_abbrev <- str_c(
  vctrs::vec_c(
    "FY",
    as.character(
      str_sub(
        as.integer(
          quarter(
            params$start_date,
            with_year = TRUE,
            fiscal_start = 7
          )
        ) - 1,
        3
      )
    )
  ),
  collapse = ""
)

# Quarter and four digit fiscal year for the same reporting period prior fiscal
# year.
same_period_prior_fiscal_year_string <- str_c(
  vctrs::vec_c(
    "Q",
    as.character(
      quarter(
        floor_date(
          params$start_date,
          "quarter"
        ),
        fiscal_start = 7
      )
    ),
    " FY",
    as.character(
      as.integer(
        quarter(
          floor_date(
            params$start_date,
            "quarter"
          ) -
            lubridate::period(
              1,
              "year"
            ),
          with_year = TRUE,
          fiscal_start = 7
        )
      )
    )
  ),
  collapse = ""
)

# First day of the prior fiscal reporting quarter.
prior_fiscal_quarter_start_date <- as_date(
  params$start_date
) - months(
  3
)

# First day of the fiscal reporting quarter before the prior fiscal reporting
# quarter.
fiscal_quarter_start_date_before_prior_fiscal_quarter <- as_date(
  params$start_date
) - months(
  6
)

# Quarter and fiscal year prior to the current reporting fiscal quarter.
prior_fiscal_quarter_year_string <- str_c(
  vctrs::vec_c(
    "Q",
    as.character(
      quarter(
        floor_date(
          params$start_date,
          "quarter"
        ) -
          lubridate::period(
            1,
            "days"
          ),
        fiscal_start = 7
      )
    ),
    " FY",
    as.character(
      as.integer(
        quarter(
          floor_date(
            params$start_date,
            "quarter"
          ) -
            lubridate::period(
              1,
              "days"
            ),
          with_year = TRUE,
          fiscal_start = 7
        )
      )
    )
  ),
  collapse = ""
)

# Quarter and fiscal year of the fiscal period prior to the current reporting
# fiscal quarter.
fiscal_period_before_prior_fiscal_quarter_year_string <- str_c(
  vctrs::vec_c(
    "Q",
    as.character(
      quarter(
        floor_date(
          params$start_date,
          "quarter"
        ) -
          lubridate::period(
            93,
            "days"
          ),
        fiscal_start = 7
      )
    ),
    " FY",
    as.character(
      as.integer(
        quarter(
          floor_date(
            params$start_date,
            "quarter"
          ) -
            lubridate::period(
              93,
              "days"
            ),
          with_year = TRUE,
          fiscal_start = 7
        )
      )
    )
  ),
  collapse = ""
)

# Prior reporting fiscal quarter.
prior_fiscal_quarter <- quarter(
  floor_date(
    params$start_date,
    "quarter"
  ) -
    lubridate::period(
      1,
      "days"
    ),
  fiscal_start = 7
)

# Fiscal quarter before prior reporting fiscal quarter.
fiscal_quarter_before_prior_fiscal_quarter <- quarter(
  floor_date(
    params$start_date,
    "quarter"
  ) -
    lubridate::period(
      93,
      "days"
    ),
  fiscal_start = 7
)

# # Prior reporting fiscal year.
# prior_fiscal_year <- as.integer(
#   quarter(
#     floor_date(
#       params$start_date,
#       "quarter"
#       ) -
#       lubridate::period(
#         1,
#         "days"
#         ),
#     with_year = TRUE,
#     fiscal_start = 7
#     )
#   )

# Quarter and four digit fiscal year for the same prior reporting period of
# current reporting period of prior fiscal quarter. For example, if the current
# reporting period is Q1 FY23, its prior period is Q4 FY22. The desired period
# will be Q4 FY21. This variable is created for those number of engagement, intake
# and admission with 3 months has elapsed. In the automatic periodic report of
# YERE, Some tables and plots will require comparison of one reporting period
# and its same period of prior fiscal year. Because we have the requirement of
# "3 months must have elapsed", For Q1 FY23, we are actually calculating the
# number of how many referrals of Q4 FY22 engaged and admitted in Q1 FY23. This
# why we have this wacky variable. If U have trouble understanding its meaning
# of its name, Just use the variable and call it a day, Bro.
prior_fiscal_period_same_fiscal_period_prior_fiscal_year <- str_c(
  vctrs::vec_c(
    "Q",
    as.character(
      quarter(
        floor_date(
          params$start_date,
          "quarter"
        ) -
          lubridate::period(
            1,
            "days"
          ),
        fiscal_start = 7
      )
    ),
    " FY",
    as.character(
      as.integer(
        quarter(
          floor_date(
            params$start_date,
            "quarter"
          ) -
            lubridate::period(
              13,
              "month"
            ),
          with_year = TRUE,
          fiscal_start = 7
        )
      )
    )
  ),
  collapse = ""
)

# Date for the last day of the last month of the fiscal quarter.
report_fiscal_quarter_end_month_date <- as_date(
  ceiling_date(
    params$start_date, "quarter"
  ) -
    period(
      1,
      "days"
    )
)

# Create string version of last day of the fiscal quarter in mm/dd/yyyy format.
report_fiscal_quarter_end_month_date_string <- format(
  ymd(
    report_fiscal_quarter_end_month_date
  ), 
  "%m/%d/%Y"
)

# Date for the start of the most recent reporting month.
recent_month_start_date <- as_date(
  params$start_date
)

# Date for the end of the most recent reporting month.
recent_month_end_date <- as_date(
  ceiling_date(
    params$start_date,
    "month"
  ) - 
    lubridate::period(
      1,
      "days"
    )
)

# Most recent month and calendar year.
recent_calendar_month_year_string <- str_c(
  as.character(
    month(
      recent_month_start_date,
      label = TRUE,
      abbr = FALSE
    )
  ),
  " ",
  as.character(
    year(
      recent_month_start_date
    )
  )
)

# Start date for the calendar year based on the reporting start date.
calendar_year_start_date <- as_date(
  paste0(
    year(
      params$start_date
    ),
    "-01-01"
  )
)

# End date for reporting based on the calendar year.
calendar_year_end_date <- as_date(
  paste0(
    year(
      params$start_date
    ),
    "-12-31"
  )
)

# Start date for state fiscal year based on the reporting start_date.
fiscal_year_start_date <- as_date(
  paste0(
    report_fiscal_year - 1,
    "-07-01"
  )
)

# April 1 of the prior reporting fiscal year.
april_prior_report_fiscal_year <- add_with_rollback(
  fiscal_year_start_date, 
  months(
    -3
  ), 
  roll_to_first = TRUE
)

# End date for state fiscal year based on the reporting start_date.
fiscal_year_end_date <- as_date(
  paste0(
    report_fiscal_year,
    "-06-30"
  )
)

# Start date for prior state fiscal year based on the reporting start_date.
prior_fiscal_year_start_date <- as_date(
  paste0(
    report_fiscal_year - 2,
    "-07-01"
  )
)

# Create string version of fiscal year start date in mm/dd/yyyy format.
fiscal_year_start_date_string <- format(
  ymd(
    fiscal_year_start_date
  ), 
  "%m/%d/%Y"
)

# Create string version of fiscal year end date in mm/dd/yyyy format.
fiscal_year_end_date_string <- format(
  ymd(
    fiscal_year_end_date
  ), 
  "%m/%d/%Y"
)

# Create string version of fiscal year end date in mm/dd/yyyy format.
end_date_string <- format(
  ymd(
    params$end_date
  ), 
  "%m/%d/%Y"
)

# End date for HCL IP prior fiscal year for the reporting start_date.
prior_fiscal_year_end_date <- as_date(
  paste0(
    report_fiscal_year - 1,
    "-06-30"
  )
)

# Create string version of fiscal year end date in mm/dd/yyyy format.
fiscal_year_end_date_string <- format(
  ymd(
    fiscal_year_end_date
  ), 
  "%m/%d/%Y"
)

# Start date for EPICC fiscal year before the prior fiscal year for the
# reporting start_date.
year_before_prior_fiscal_year_start_date <- as_date(
  paste0(
    report_fiscal_year - 3,
    "-07-01"
  )
)

# End date for EPICC fiscal year before prior fiscal year for the reporting
# start_date.
year_before_prior_fiscal_year_end_date <- as_date(
  paste0(
    report_fiscal_year - 2,
    "-06-30"
  )
)

# Reporting fiscal year start month and year as a string.
report_fiscal_year_start_month_year_string <- str_c(
  as.character(
    lubridate::month(
      fiscal_year_start_date, 
      label = TRUE, 
      abbr = FALSE
    )
  ), 
  " ",
  as.character(
    as.integer(
      quarter(
        params$start_date, 
        with_year = TRUE, 
        fiscal_start = 7
      )
    )-1
  )
)


# Starting month for most recent calendar year.
calendar_start_month_year_string <- str_c(
  as.character(
    month(
      calendar_year_start_date,
      label = TRUE,
      abbr = FALSE
    )
  ),
  " ",
  as.character(
    year(
      calendar_year_start_date
    )
  )
)

# Ending month for most recent calendar year.
calendar_end_month_year_string <- str_c(
  as.character(
    month(
      calendar_year_end_date,
      label = TRUE,
      abbr = FALSE
    )
  ),
  " ",
  as.character(
    year(
      calendar_year_end_date
    )
  )
)

## ----custom-ggplot2-themes---------------------------------------------------------

# theme_min_geomline: Custom ggplot2 theme for plots using geom_line
theme_min_geomline <- function(...) {
  theme_minimal(...) + 
    theme(
      text = element_text(
        size = 14
      ),
      legend.position = "bottom", 
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_line(
        "grey90"
      ),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.background = element_rect(
        color = "#D3D3D3",
        linewidth = 0.5
      )
    )
}

# theme_min_geombar_identity: Custom ggplot2 theme for plots using geom_bar with
# stat = "identity"
theme_min_geombar_identity <- function(...) {
  theme_minimal(...) + 
    theme(
      text = element_text(
        size = 14
      ),
      legend.position = "bottom", 
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(
        color = "#D3D3D3",
        linewidth = 0.5
      )
    )
}

# theme_min_geombar_dodge: Plot using geom_bar or geom_col with or without
# coord_flip, position = position_dodge2(width = 0.6), stat = "identity" and
# reversed legend or when using geom_bar with position = "dodge" and stat =
# "identity"
theme_min_geombar_dodge <- function(...) {
  theme_minimal(...) + 
    theme(
      text = element_text(
        size = 14
      ),
      legend.position = "bottom", 
      legend.direction = "horizontal",
      legend.key = element_rect(
        colour = "transparent", 
        fill = "White"
      ),
      legend.title.align = 0.5,
      legend.text = element_text(
        size = 10
      ), 
      axis.ticks.x = element_blank(), 
      axis.ticks.y = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(),
      plot.background = element_rect(
        color = "#D3D3D3",
        linewidth = 0.5
      )
    )
}

# theme_min_geomsf: Custom ggplot2 theme for maps using geom_sf
theme_min_geomsf <- function(...) {
  theme_minimal(...) + 
    theme(
      plot.title = element_text(
        hjust = 0.5
      ),
      plot.caption = element_text(
        hjust = 0.5
      ),
      legend.title = element_text(
        size = 6
      ), 
      legend.text = element_text(
        size = 6
      ), 
      legend.background = element_blank(),
      legend.position = c(
        0.93, 
        0.19
      ), 
      axis.text.x = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(), 
      panel.grid.major = element_line(
        colour = "transparent"
      ), 
      panel.grid.minor = element_blank()
    )
}

# theme_min_blank: Custom ggplot2 theme for removing grids and legends for
# horizontal bar plots,geom_bar
theme_min_blank <- theme_classic() + 
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(
      color = "gray95"
    ),
    axis.ticks = element_blank(),
    text = element_text(
      family = "sans"
    ),
    axis.text = element_text(
      size = 7, 
      color = "gray30"
    ),
    axis.line.y = element_line(
      linewidth = 1,
      linetype = 'dotted'
    ),
    axis.line.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.key.size = unit(
      0.8,
      "line"
    ),
    legend.text = element_text(
      size = 7,
      color = "gray30"
    )
  )

# theme_min_blank: Custom ggplot2 theme for removing grids and legends for
# vertical bar plots,geom_bar
theme_min_blank_vertical <- theme_classic() + 
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(
      color = "gray95"
    ),
    axis.ticks = element_blank(),
    text = element_text(
      family = "sans"
    ),
    axis.text = element_text(
      size = 7, 
      color = "gray30"
    ),
    axis.line.x = element_line(
      linewidth = 1,
      linetype = 'dotted'
    ),
    axis.line.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.key.size = unit(
      0.8,
      "line"
    ),
    legend.text = element_text(
      size = 7,
      color = "gray30"
    ),
  )

# theme_continue_blank: Custom ggplot2 theme for removing grids and legends for
# line plots,geom_line
theme_continue_blank <-
  theme(
    axis.text.x = element_text(
      size = rel(
        3.5
      )
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.text = element_text(
      size = 25
    ),
    axis.line.x = element_line(
      color = 'black'
    ),
    plot.title = element_text(
      size = 25, 
      hjust = .5, 
      color = "gray30"
    ),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

## ----bcr-read-data-program-participation-------------------------------------------

# Read data
bcr_provider_placement_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare Q_ProviderPlacement_BHN/Q_PROVIDERPLACEMENT_BHN.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_provider_placement_ct <- case_when(
  str_detect(
    bcr_provider_placement_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_provider_placement_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_provider_placement_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_provider_placement_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_provider_placement_nms, "DOCSERNO"
  ) ~ "c",
  str_detect(
    bcr_provider_placement_nms, "DOCREVNO"
  ) ~ "c",
  str_ends(
    bcr_provider_placement_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_provider_placement_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_provider_placement_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_provider_placement_nms, "VISITTM"
  ) ~ "t",
 str_detect(
    bcr_provider_placement_nms, "ZIP"
  ) ~ "c",
  str_detect(
    bcr_provider_placement_nms, "BIRTH_DATE"
  ) ~ "D",
  str_detect(
    bcr_provider_placement_nms, "form_id_"
  ) ~ "n",
  str_detect(
    bcr_provider_placement_nms, "_num_"
  ) ~ "n",
  str_detect(
    bcr_provider_placement_nms, "_num"
  ) ~ "n",
  str_detect(
    bcr_provider_placement_nms, "num_"
  ) ~ "n",
   str_detect(
    bcr_provider_placement_nms, "_CODE"
  ) ~ "c",
  str_detect(
    bcr_provider_placement_nms, "_code_"
  ) ~ "n",
  str_detect(
    bcr_provider_placement_nms, "_code"
  ) ~ "n",
  .default = "c"
) %>% 
  paste0(
    collapse = ""
  )


# Read BCR PATHCLIENT ENROLLMENTS file to get BCR pathway client
# enrollment/pathway event data. Uses clean_names() to transform all the field
# names to snake_case, then renames the necessary columns to conform to
# snake_case.
bcr_provider_placement <- readr::read_csv(
    "P:/DATA/Data Files/FAMCare Q_ProviderPlacement_BHN/Q_PROVIDERPLACEMENT_BHN.csv",
  col_types = bcr_provider_placement_ct,
  na = c(
    "",
    " "
  )
  ) %>% 
  clean_names(
    .,
  ) %>% 
  filter(
    str_detect(
      program_description,
      "BCR"
    )
  )



## ----bcr-read-data-bcr-pathclient-enrollments--------------------------------------

# Create a vector of column names from the BCR PATHCLIENT Enrollments Extract
bcr_pathclient_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_PATHCLIENT_ENROLLMENTS.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_pathclient_ct <- case_when(
  str_detect(
    bcr_pathclient_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_pathclient_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_pathclient_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_pathclient_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_pathclient_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_pathclient_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_pathclient_nms, "DAYS_UNTIL_FORM_DUE"
  ) ~ "n",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_PATHCLIENT_ENROLLMENTS file to get BCR Pathway client
# enrollment/Pathway Event data. Uses clean_names() to transform all the field
# names to snake_case. Most were already renamed in the view to facilitate this.
# Renames client_number.
bcr_pathclient_enrollments <- readr::read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_PATHCLIENT_ENROLLMENTS.csv",
  col_types = bcr_pathclient_ct,
  na = c(
    "",
    " "
  )
  ) %>%
  clean_names(
    .,
  ) %>%
  rename(
    enroll_client_num = client_number,
  )  %>% 
  select(
    enroll_client_num,
    enrollment_starting_date,
    enrollment_ending_date,
    dismissal_reason_description,
    pp_docserno,
    pc_docserno,
    pec_pathclient_docserno,
    agency_description,
    pwy_start_date,
    pwy_end_date,
    pwy_event,
    pwy_forms_docserno
  ) %>% 
  # filter(
  #   enrollment_starting_date >= "2024-10-01"
  # ) %>%
  pivot_wider(
    names_from = pwy_event,
    values_from = pwy_forms_docserno,
    values_fill = NA
  ) %>% 
  clean_names() %>% 
  rename_with(
    ~ paste0(
      str_replace_all(
        .x,
        "(\\d+)",
        function(m) numbers_to_words(as.numeric(m))
      ),
      "_docserno"
    ),
    starts_with(
      "bcr_"
    )
  )

# Duplicate check in bcr_pathclient_enrollments if needed:

# The BCR Duplicate PWY Forms Per Enrollment exists to address duplicate
# forms, but the following may be used to quickly identify the duplicates here.
# Uncomment to run.
# bcr_pathclient_enrollments_dupes <- bcr_pathclient_enrollments %>%
#     summarise(
#     n = n(),
#     .by = c(
#       pp_docserno,
#       pwy_event,
#       enroll_client_num
#     )
#   ) %>%
#   filter(
#     n >1L
#   ) %>% 
#   distinct(
#     enroll_client_num
#   )

# Identify missing DOCSERNOS
missing_docsernos_pec <- bcr_pathclient_enrollments %>% 
  filter(
    is.na(
      bcr_referral_docserno
    )
  )

missing_or_multiple_docsernos <- bcr_pathclient_enrollments %>% 
  filter(
    is.na(bcr_referral_docserno) |
      map_lgl(bcr_referral_docserno, is.list)
  )
  


## ----bcr-read-data-bcr-pathway-docsernos-------------------------------------------

# Create a vector of column names from the BCR PATHWAY FORM DOCSERNOS
bcr_pathway_docsernos_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_PATHWAY_FORM_DOCSERNOS.csv", 
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_pathway_docsernos_ct <- case_when(
  str_detect(
    bcr_pathway_docsernos_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_pathway_docsernos_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_pathway_docsernos_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_pathway_docsernos_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_pathway_docsernos_nms, "_DATE_"
  ) ~ "D",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read bcr CLIENT form file to get DOCSERNO data for the pathway forms. Uses
# clean_names to transform all the field names from FAMCare to snake_case, then
# renames the necessary columns to conform to snake_case.
bcr_pathway_form_docsernos <- readr::read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_PATHWAY_FORM_DOCSERNOS.csv",
  col_types = bcr_pathway_docsernos_ct,
  na = c(
    "",
    " "
  )
) %>% 
  clean_names(
    .,
  ) %>% 
  rename(
    pwy_form_docsernos_pathway_date = pathway_date,
    client_num = client_number,
    bpf_docserno = docserno
  ) %>% 
  select(
    -client_num
  )


## ----bcr-read-data-bcr-client------------------------------------------------------

# Create a vector of column names from the Q_BCR_CLIENT view.
bcr_client_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_CLIENT.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_client_ct <- case_when(
  str_detect(
    bcr_client_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_client_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_client_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_client_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_client_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_client_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_client_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_client_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_client_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_client_nms, "VISITTM"
  ) ~ "t",
 str_detect(
    bcr_client_nms, "ZIP_CODE"
  ) ~ "c",
  str_detect(
    bcr_client_nms, "_CODE"
  ) ~ "c",
  str_detect(
    bcr_client_nms, "FACM"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_CLIENT file to get demographic data. Uses clean_names to
# transform all the field names from FAMCare to snake_case. Rename client_number
# and rename select columns to uniquely identify them for the full join.
bcr_client <- readr::read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_CLIENT.csv",
  col_types = bcr_client_ct,
  na = c(
    "",
    " "
  )
  ) %>%
   clean_names(
    .,
  ) %>%
  rename(
    client_num = client_number,
    c_docserno = docserno
  ) %>%
  select(
    -visittm,
    -client_email,
    -cell_phone,
    -work_phone,
    -visitdt,
    -userid,
    -id
  ) %>%
  relocate(
    client_num,
    .before = c_docserno
  ) %>% 
  mutate(
    reside_in_pz = case_when(
      zip_code %in% c(
          63042,
          63044,
          63101,
          63102,
          63103,
          63106,
          63107,
          63108,
          63112,
          63113,
          63114,
          63115,
          63120,
          63121,
          63130,
          63133,
          63134,
          63135,
          63136,
          63137,
          63138,
          63140,
          63145,
          63147) ~ "1",
          .default = "0"
    )
  )


## ----bcr-read-data-bcr-referral----------------------------------------------------
# Create a vector of column names from the Q_BCR_REFERRAL view.
bcr_ref_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_REFERRAL.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_ref_ct <- case_when(
  str_detect(
    bcr_ref_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_ref_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_ref_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_ref_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_ref_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_ref_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_ref_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_ref_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_ref_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_ref_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_ref_nms, "_CODE"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Creates bcr_pathclient_enrollment_select from bcr_pathclient_enrollments
# to be able to join enrollments later on with the referral form to get the
# enrollment_starting_date on the referral. This will be needed to help
# calculate the clients' ages at referral.
bcr_pathclient_enrollment_select <- bcr_pathclient_enrollments %>%
  select(
    enroll_client_num,
    enrollment_starting_date,
    bcr_referral_docserno,
  ) %>%
  filter(
      !is.na(
        bcr_referral_docserno
      )
  )

# Creates bcr_client_select from bcr_client to be able to join the client
# form with the referral form later on and get the clients birth_date on the
# referral form. This will be used to help calculate the clients' ages at
# referral.
bcr_client_select <- bcr_client %>%
  select(
    client_num,
    birth_date
  )

# Read Q_BCR_REFERRAL file to get data on referrals from any client that was
# active in ETO from 07/01/2024 to present. Uses clean_names to transform all
# the field names from FAMCare to snake_case, then renames select columns to
# uniquely identify them for the full join. Adds state fiscal and federal fiscal
# columns to allow filtering data by various fiscal reporting periods. Mutate
# disposition_recoded to clean up the dispositions.
bcr_ref <- readr::read_csv(
  "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_REFERRAL.csv",
  col_types = bcr_ref_ct,
  na = c(
    "",
    " "
  ),
  ) %>%
  clean_names(
    .,
  ) %>%
  rename(
    ref_pathway_date = pathway_date,
    ref_docserno = docserno,
    ref_client_num = client_number,
    ref_tiedenrollment = tiedenrollment,
    ref_program_participation = bcr_program_participation_description
  ) %>%
  select(
    -visitdt,
    -visittm,
    -userid,
    -client_first,
    -client_last,
    -event_name,
    -id
  ) %>%
  relocate(
    ref_client_num,
    .before = ref_docserno
  ) %>%
  mutate(
  # Extracting referral month and calendar year from ref_pathway_date.Variables
  # for month and state fiscal year and also month and federal fiscal year will
  # follow.
    month_calendar_year_referral = zoo::as.yearmon(
      ref_pathway_date,
      "%B %Y"
    )
  ) %>%
  # State fiscal year variables for referral start here.
  mutate(
  # Determining state fiscal quarter based on July start of fiscal year.
    state_fiscal_quarter_referral = quarter(
      ref_pathway_date,
      fiscal_start = 7
    )
  ) %>%
  mutate(
  # Extracting full state fiscal year (YYYY format).
    state_fiscal_year_referral = as.integer(
      quarter(
        ref_pathway_date,
        with_year = TRUE,
        fiscal_start = 7
      )
    )
  ) %>%
  mutate(
  # Identifying state fiscal year with quarter context (YYYY.Q format).
    state_fiscal_year_qtr_referral = quarter(
      ref_pathway_date,
      with_year = TRUE,
      fiscal_start = 7
      )
    ) %>%
  separate_wider_position(
  # Splitting state fiscal year into century and last two digits.
    state_fiscal_year_referral,
    c(
      century = 2,
      state_fiscal_year_two_digits_referral = 2
    ),
    cols_remove = FALSE
  ) %>%
  select(
  # Removing century column after extracting last two digits from state fiscal
  # year.
    -century
  ) %>%
  mutate(
  # Formatting sate fiscal quarter as "Qx FYxx" format.
    state_fiscal_year_qtr_string_referral = str_c(
      "Q",
      state_fiscal_quarter_referral,
      "\nFY",
      state_fiscal_year_two_digits_referral
      )
    ) %>%
  mutate(
  # Ordering strings for state fiscal year quarters in sequence.
    state_fiscal_year_qtr_string_referral = fct_reorder(
      state_fiscal_year_qtr_string_referral,
      state_fiscal_year_qtr_referral
    )
  ) %>%
  # Creating fiscal month factor for state fiscal year reporting.
  mutate(
    month_state_fiscal_year_referral = as_factor(
      str_c(
        as.character(
          lubridate::month(
            ref_pathway_date,
            label = TRUE
          )
        ),
        "\nFY",
        state_fiscal_year_two_digits_referral
      )
    )
  ) %>%
  arrange(
  # Sorting by referral date for consistency.
    ref_pathway_date
  ) %>%
  mutate(
  # Converting month into state fiscal month number (1-12) based on July fiscal
  # start.
    state_fiscal_month_num_referral = as.character(
      (
        lubridate::month(
          ref_pathway_date,
          label = FALSE
        ) - 7
      ) %% 12 + 1
    )
  ) %>%
  mutate(
  # Combining state fiscal year and month as a factor.
    state_fiscal_year_month_num_referral = as_factor(
      str_c(
        state_fiscal_year_referral,
        ".",
        state_fiscal_month_num_referral
      )
    )
  ) %>%
  # Federal fiscal year variables for referral start here.
  mutate(
  # Extracting federal fiscal quarter based on October-start to the fiscal year.
    federal_fiscal_quarter_referral = quarter(
      ref_pathway_date,
      fiscal_start = 10
    )
  ) %>%
  mutate(
  # Extracting full federal fiscal year (YYYY format).
    federal_fiscal_year_referral = as.integer(
      quarter(
        ref_pathway_date,
        with_year = TRUE,
        fiscal_start = 10
      )
    )
  ) %>%
  # This seems redundant with month_calendar_year_referral (above). Commenting
  # out for the moment because I am sure this is used somewhere in periodic
  # reporting. We'll want to switch, but I figured retaining as commented out
  # will make it easier to troubleshoot. This should be deleted when possible.
  # mutate(
  #   epicc_month_year_referral = zoo::as.yearmon(
  #     ref_pathway_date,
  #     "%B %Y"
  #   )
  # )
  mutate(
  # Identifying federal fiscal year with quarter context (YYYY.Q format).
    federal_fiscal_year_qtr_referral = quarter(
      ref_pathway_date,
      with_year = TRUE,
      fiscal_start = 10
      )
    ) %>%
  separate_wider_position(
  # Splitting federal fiscal year into century and last two digits.
    federal_fiscal_year_referral,
    c(
      century = 2,
      federal_fiscal_year_two_digits_referral = 2
    ),
    cols_remove = FALSE
  ) %>%
  select(
  # Removing century column since the second use of separate_wider_position()
  # added it again.
    -century
  ) %>%
  mutate(
  # Formatting federal fiscal quarter as "Qx FYxx" format.
    federal_fiscal_year_qtr_string_referral = str_c(
      "Q",
      federal_fiscal_quarter_referral,
      "\nFY",
      federal_fiscal_year_two_digits_referral
      )
    ) %>%
  mutate(
  # Ordering federal fiscal year quarter string as a factor using
  # federal_fiscal_year_qtr_referral to ensure that the levels are in sequence.
    federal_fiscal_year_qtr_string_referral = fct_reorder(
      federal_fiscal_year_qtr_string_referral,
      federal_fiscal_year_qtr_referral
    )
  ) %>%
  mutate(
  # Creating federal fiscal month factor for federal fiscal year reporting.
    federal_month_fiscal_year_referral = as_factor(
      str_c(
        as.character(
          lubridate::month(
            ref_pathway_date,
            label = TRUE
          )
        ),
        "\nFY",
        federal_fiscal_year_two_digits_referral
      )
    )
  ) %>%
  arrange(
  # This is probably redundant, but this ensures the sort order of referrals in
  # date order based on ref_pathway_date.
    ref_pathway_date
  ) %>%
  mutate(
  # Converting month into federal fiscal month number (1-12).
    federal_fiscal_month_num_referral = as.character(
      (
        lubridate::month(
          ref_pathway_date,
          label = FALSE
        ) - 10
      ) %% 12 + 1
    )
  ) %>%
  mutate(
  # Factoring federal fiscal year and federal fiscal month number.
    federal_fiscal_year_month_num_referral = as_factor(
      str_c(
        federal_fiscal_year_referral,
        ".",
        federal_fiscal_month_num_referral
      )
    )
  ) %>% 
  # left_joins bcr_pathclient_enrollment_select to get the
  # enrollment_starting_date.
  left_join(
    select(
      bcr_pathclient_enrollment_select,
      enrollment_starting_date,
      bcr_referral_docserno
  ),
  by = join_by(
    "ref_docserno" == "bcr_referral_docserno"
    )
  ) %>%
  # left_joins epicc_client_select to get birth_date.
  left_join(
    select(
      bcr_client_select,
      client_num,
      birth_date
    ),
    by = join_by(
      "ref_client_num" == "client_num"
    )
  ) %>%
  mutate(
    age_enrollment = floor(
      trunc(
        birth_date %--% enrollment_starting_date
      ) / years(
        1
      )
    )
  ) %>%
  mutate(
    age_range_enrollment = as_factor(
      case_when(
        age_enrollment < 18 ~
          "Under 18",
        age_enrollment >= 18 & age_enrollment <= 25 ~
          "18 to 25",
        age_enrollment >= 26 & age_enrollment <= 64 ~
          "26 to 64",
        age_enrollment >= 65 ~
          "65 and Above",
        .default = "Other"
      )
    )
  ) %>% 
  select(
    -birth_date,
    -enrollment_starting_date
  ) 

#quick test
bcr_ref_count <- bcr_ref %>% 
  filter(
    ref_pathway_date >= "2025-10-01"
  )


bcr_ref_source <- bcr_ref %>% 
  filter(
    ref_pathway_date >= "2025-07-01"
  ) %>% 
  select(
    ref_pathway_date,
    ref_source_type,
    ref_source_name,
    ref_through_event
  ) %>% 
  tabyl(
    ref_source_type
  )


## ----bcr-read-ic-------------------------------------------------------------------

# Create a vector of column names from the Q_BCR_IC view.
bcr_ic_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_IC.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_ic_ct <- case_when(
  str_detect(
    bcr_ic_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_ic_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_ic_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_ic_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_ic_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_ic_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_ic_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_ic_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_ic_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_ic_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_ic_nms, "_CODE"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_IC view file to get data on Initial Contact forms for all months
# and years. Uses clean_names() to transform all FAMCare variables into snake_case.
# Renames select columns to uniquely identify them in joins. Removes visitdt,
# visittm, client_number, id, userid, firstname, lastname, and event_name
# because these will not be needed.
bcr_ic <- readr::read_csv(
 "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_IC.csv",
  col_types = bcr_ic_ct,
  na = c(
    "",
    " "
    )
  ) %>%
  clean_names(
    .,
  ) %>%
  rename(
    ic_pathway_date = pathway_date,
    ic_docserno = docserno,
    ic_parent_docserno = parent_docserno,
    ic_tiedenrollment = tiedenrollment
  ) %>%
  select(
    -visitdt,
    -visittm,
    -client_number,
    -id,
    -userid,
    -client_first,
    -client_last,
    -event_name
  )


## ----bcr-read-presenting-concerns--------------------------------------------------
# Create a vector of column names from the Q_BCR_Presenting_concerns view.
bcr_pc_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_PRESENTING_CONCERNS.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_pc_ct <- case_when(
  str_detect(
    bcr_pc_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_pc_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_pc_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_pc_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_pc_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_pc_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_pc_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_pc_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_pc_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_pc_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_pc_nms, "_CODE"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_PRESENTING_CONCERNS view file to get data on Initial Contact forms for all months
# and years. Uses clean_names() to transform all FAMCare variables into snake_case.
# Renames select columns to uniquely identify them in joins. Removes visitdt,
# visittm, client_number, id, userid, firstname, lastname, and event_name
# because these will not be needed.
bcr_pc <- readr::read_csv(
 "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_PRESENTING_CONCERNS.csv",
  col_types = bcr_pc_ct,
  na = c(
    "",
    " "
    )
  ) %>%
  clean_names(
    .,
  ) %>%
  rename(
    pc_pathway_date = pathway_date,
    pres_con_docserno = docserno,
    pc_parent_docserno = parent_docserno
  ) %>%
  select(
    -visitdt,
    -visittm,
    -client_number,
    -id,
    -userid
  )


## ----bcr-read-ref-placed-----------------------------------------------------------

# Create a vector of column names from the Q_BCR_Ref_Placed view.
bcr_rp_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_REF_PLACED.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_rp_ct <- case_when(
  str_detect(
    bcr_rp_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_rp_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_rp_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_rp_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_rp_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_rp_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_rp_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_rp_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_rp_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_rp_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_rp_nms, "_CODE"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_REF_PLACED view file to get data on Initial Contact forms for all months
# and years. Uses clean_names() to transform all FAMCare variables into snake_case.
# Renames select columns to uniquely identify them in joins. Removes visitdt,
# visittm, client_number, id, userid, firstname, lastname, and event_name
# because these will not be needed.
bcr_rp <- readr::read_csv(
 "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_REF_PLACED.csv",
  col_types = bcr_rp_ct,
  na = c(
    "",
    " "
    )
  ) %>%
  clean_names(
    .,
  ) %>%
  rename(
    rp_pathway_date = pathway_date,
    rp_docserno = docserno,
    rp_parent_docserno = parent_docserno,
    rp_tiedenrollment = tiedenrollment
  ) %>%
  select(
    -visitdt,
    -visittm,
    -client_number,
    -id,
    -userid,
    -client_name,
    -event_name
  )


#Creates a table to count of the number of referrals placed by type. This can be
# Used to look at type of referrals by month as well. 
bcr_rp_by_type <- bcr_rp %>%
  select(
    rp_docserno,
    rp_parent_docserno,
    rp_pathway_date,
    bcr_type_ref_placed,
    bh_ref_placed,
    physical_health_ref_placed,
    housing_ref_placed,
    mat_health_ref_placed,
    other_ref_placed,
    soc_services_ref_placed,
    spiritual_care_ref_placed
  ) %>% 
  pivot_longer(
    cols = c(
      bh_ref_placed,
      physical_health_ref_placed,
      housing_ref_placed,
      mat_health_ref_placed,
      other_ref_placed,
      soc_services_ref_placed,
      spiritual_care_ref_placed
    ),
    names_to = "referral_type",
    values_to = "placed"
  ) %>%
  filter(
    placed == 1
  ) %>% 
  tabyl(
    referral_type
  )




## ----bcr-active-housing-status-----------------------------------------------------
# Create a vector of column names from the Q_BCR_ACTIVE_HOUSING_STATUS view.
bcr_active_housing_status_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_ACTIVE_HOUSING_STATUS.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_active_housing_status_ct <- case_when(
  str_detect(
    bcr_active_housing_status_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_active_housing_status_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_active_housing_status_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_active_housing_status_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_active_housing_status_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_active_housing_status_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_active_housing_status_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_active_housing_status_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_active_housing_status_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_active_housing_status_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_active_housing_status_nms, "_CODE"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_ACTIVE_HOUSING_STATUS view file to get data on latest client
# housing status records for all months and years. Uses clean_names() to
# transform the FAMCare fields into snake_case.
bcr_active_housing_status <- readr::read_csv(
  "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_ACTIVE_HOUSING_STATUS.csv",
  col_types = bcr_active_housing_status_ct,
  na = c(
    "",
    " "
  )
  ) %>%
  clean_names(
    .,
  ) %>%
  rename(
    active_housing_parent_docserno = parent_docserno,
    active_housing_docserno = docserno
  ) %>%
  select(
    -client_number,
    -visitdt,
    -visittm,
    -userid,
    -housing_start_date,
    -housing_end_date
  )


## ----bcr-active-payor-source-------------------------------------------------------
# Create a vector of column names from the BCR Active Payor Source Extract.
bcr_active_payor_source_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_ACTIVE_PAYOR_SOURCE.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_active_payor_source_ct <- case_when(
  str_detect(
    bcr_active_payor_source_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_active_payor_source_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_active_payor_source_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_active_payor_source_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_active_payor_source_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_active_payor_source_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_active_payor_source_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_active_payor_source_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_active_payor_source_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_active_payor_source_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_active_payor_source_nms, "_CODE"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_ACTIVE_PAYOR_SOURCE view file to get data on active payor source
# forms for all months and years.Uses clean_names() to transform the FAMCare
# fields into snake_case.
bcr_active_payor_source <- readr::read_csv(
  "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_ACTIVE_PAYOR_SOURCE.csv",
  col_types = bcr_active_payor_source_ct,
  na = c(
    "",
    " "
  )
) %>%
  clean_names(
    .,
  ) %>%
  rename(
    active_payor_source_parent_docserno = parent_docserno,
  ) %>%
  select(
    -client_number,
  )



## ----bcr-client-counseling-sessions------------------------------------------------
# Create a vector of column names from the Q_BCR_CLIENT_COUNSELING_SESSIONS view.
bcr_ccs_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_CLIENT_COUNSELING_SESSIONS.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_ccs_ct <- case_when(
  str_detect(
    bcr_ccs_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_ccs_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_ccs_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_ccs_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_ccs_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_ccs_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_ccs_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_ccs_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_ccs_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_ccs_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_ccs_nms, "_CODE"
  ) ~ "c",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_CLIENT_COUNSELING_SESSIONS view file.
# Uses clean_names() to transform all FAMCare variables into snake_case.
# Renames select columns to uniquely identify them in joins. Removes visitdt,
# visittm, client_number, id, userid, firstname, lastname, and event_name
# because these will not be needed.
bcr_ccs <- readr::read_csv(
 "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_CLIENT_COUNSELING_SESSIONS.csv",
  col_types = bcr_ccs_ct,
  na = c(
    "",
    " "
    )
  ) %>%
  clean_names(
    .,
  ) %>%
  rename(
    ccs_pathway_date = pathway_date,
    ccs_docserno = docserno
  ) %>% 
  mutate(
      month_calendar_year_invoice = zoo::as.yearmon(
      ccs_pathway_date,
      "%B %Y"
    )
  ) %>% 
    mutate(
      month_calendar_year_session = zoo::as.yearmon(
      session_date,
      "%B %Y"
    )
  ) %>% 
  mutate(
   arpa_session_fiscal_quarter = 
   stringr::str_c(
     "Q", as.character(
       lubridate::quarter(
         session_date,
         fiscal_start = 10
         )
       )
     )
   ) %>% 
  mutate(
  dmh_session_fiscal_quarter = 
   stringr::str_c(
     "Q", as.character(
       lubridate::quarter(
         session_date,
         fiscal_start = 7
         )
       )
     )
   ) %>%
  filter(
    !is.na(tiedenrollment)
  )

bcr_ccs_wide <- bcr_ccs %>% 
  arrange(
    client_number,
    session_date
  ) %>% 
  group_by(
    client_number,
    tiedenrollment
  ) %>% 
  mutate(
    session_n = row_number()
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c(client_number,tiedenrollment),
    names_from = session_n,
    values_from = session_date,
    names_glue = "session_{session_n}_{.value}"
  ) %>% 
  left_join(
    select(
    bcr_pathclient_enrollments,
    enroll_client_num,
    pp_docserno,
    pc_docserno,
    enrollment_starting_date
  ),
  by = join_by(
    # "client_number" == "enroll_client_num",
      "tiedenrollment" == "pc_docserno"
  )
  ) %>% 
  mutate(
    total_sessions = rowSums(
      !is.na(
        across(
          starts_with(
            "session_"
            )
          )
        )
    )
  )





## ----bcr-full-data-----------------------------------------------------------------
# Create bcr_full_data using sqldf to join all the tables
# together (bcr_pathclient_enrollments, bcr_client, bcr_ref,
# bcr_ic, bcr_presenting_concerns, bcr_rp, bcr_active_housing_status,
# bcr_active_payor_source)
bcr_full_data_step_one <- sqldf(
  "SELECT *
  FROM bcr_pathclient_enrollments as [BENROLL]
  LEFT JOIN bcr_client AS [BC]
    ON BENROLL.enroll_client_num = BC.client_num
  LEFT JOIN bcr_ref AS [BREF]
    ON BENROLL.bcr_referral_docserno = BREF.ref_docserno
  LEFT JOIN bcr_pc AS [BPC]
    ON BENROLL.bcr_referral_docserno = BPC.pc_parent_docserno
  LEFT JOIN bcr_ic AS [BIC]
    ON BENROLL.bcr_initial_contact_docserno = BIC.ic_docserno
  LEFT JOIN bcr_rp AS [BRP]
    ON BENROLL.bcr_referrals_placed_docserno  = BRP.rp_docserno
  LEFT JOIN bcr_active_housing_status AS [BAHS]
    ON BENROLL.bcr_initial_contact_docserno = BAHS.active_housing_parent_docserno
  LEFT JOIN bcr_active_payor_source AS [BAPS]
    ON BENROLL.bcr_initial_contact_docserno = BAPS.active_payor_source_parent_docserno"
    )

bcr_full_data <- bcr_full_data_step_one %>% 
  mutate(
    planned_event = case_when(
    !is.na(ref_through_event) ~ ref_through_event,
    .default = planned_event
     )
   ) %>%     
  mutate(
     event_type = case_when(
       !is.na(bcr_ref_event) ~ bcr_ref_event,
       .default = event_type
     )
   ) %>% 
  mutate(
     prior_mh_bh_services = case_when(
       !is.na(prev_mh_bh_services) ~ prev_mh_bh_services,
       .default = prior_mh_bh_services
     )
   ) %>% 
mutate(
  reside_stl_city = coalesce(reside_in_stl_city, reside_stl_city),
  reside_stl_city = if_else(
      is.na(reside_stl_city) & !is.na(ic_pathway_date),
      "Unknown",
      reside_stl_city
    )
) %>% 
  select(
    -prev_mh_bh_services,
    -bcr_ref_event,
    -ref_through_event
  ) %>% 
  mutate(
    ic_pwy_date_state_fiscal_quarter = 
  stringr::str_c(
    "Q", as.character(
      lubridate::quarter(
        ic_pathway_date,
        fiscal_start = 7
        )
      )
    )
  ) %>% 
  mutate(
    ic_pwy_date_fed_fiscal_quarter = 
  stringr::str_c(
    "Q", as.character(
      lubridate::quarter(
        ic_pathway_date,
        fiscal_start = 10
        )
      )
    )
  ) 




bcr_key_dates <- bcr_full_data %>% 
  select(
    client_num,
    client_name,
    enrollment_starting_date,
    ref_pathway_date,
    ic_pathway_date,
    bcr_prog_participation_desc,
    rp_pathway_date,
    enrollment_ending_date
  )


missing_ref_ic <- bcr_full_data %>% 
  filter(
    enrollment_starting_date >= "2025-10-01",
    enrollment_starting_date <= "2025-12-31",
    (is.na(ref_pathway_date) | is.na(ic_pathway_date))
  ) %>% 
  select(
    client_num,
    client_name,
    enrollment_starting_date,
    ref_pathway_date,
    ic_pathway_date
  )

# openxlsx::write.xlsx(
#  missing_ref_ic,
#  file = paste0(
#    "P:/DATA/Data Files/Data Audits/BCR/missing_ref_ic_",
#    Sys.Date(),
#    ".xlsx"
#  )
# )



## ----bcr-ref-placed-long-----------------------------------------------------------
bcr_rp_bh_subtype <- bcr_full_data %>%
  select(
    rp_docserno,
    rp_pathway_date,
    bcr_ref_placed_bh_subtype,
    cmhc_ref_placed,
    counseling_ref_placed,
    group_peer_support_ref_placed,     
    individual_peer_support_ref_placed,
    su_ref_placed,
    cmhc_agency,                      
    cmhc_agency_desc,
    date_cmhc_ref_placed,
    counseling_agency,
    counseling_agency_desc,
    date_counseling_ref_placed,
    group_peer_support_agency,         
    group_peer_support_agency_desc,
    date_group_peer_support_ref_placed,
    indiv_peer_support_agency,
    indiv_peer_support_agency_desc,
    date_indiv_peer_support_ref,
    su_agency,
    su_agency_desc,
    date_su_ref,
    notes_rp
    ) %>% 
  pivot_longer(
    cols = c(
    cmhc_ref_placed,
    counseling_ref_placed,
    group_peer_support_ref_placed,     
    individual_peer_support_ref_placed,
    su_ref_placed
    ),
    names_to = "referral_type",
    values_to = "placed"
  ) %>%
  filter(
    placed == 1
  ) %>% 
  tabyl(
    referral_type
  )

bcr_rp_counseling_agencies <- bcr_rp %>% 
  select(
    rp_docserno,
    rp_pathway_date,
    counseling_ref_placed,
    counseling_agency,
    counseling_agency_desc,
    date_counseling_ref_placed
  ) %>% 
  filter(
    counseling_ref_placed == 1
  ) %>% 
  tabyl(
    counseling_agency_desc
  )

#Create long version of the behavioral health subtype referrals with agencies
# and dates. We will repeat this process for each subtype. 
rp_bh_long <- bcr_full_data %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    bh_ref_placed,
    cmhc_ref_placed,
    counseling_ref_placed,
    group_peer_support_ref_placed,
    individual_peer_support_ref_placed,
    su_ref_placed,
    cmhc_agency,
    cmhc_agency_desc,
    date_cmhc_ref_placed,
    counseling_agency,
    counseling_agency_desc,
    date_counseling_ref_placed,
    group_peer_support_agency,
    group_peer_support_agency_desc,
    date_group_peer_support_ref_placed,
    indiv_peer_support_agency,
    indiv_peer_support_agency_desc,
    date_indiv_peer_support_ref,
    su_agency,
    su_agency_desc,
    date_su_ref,
    notes_rp
  ) %>% 
  mutate(
    referral_type = "behavioral_health"
  ) %>% 
  pivot_longer(cols = c(
    cmhc_ref_placed,
    counseling_ref_placed,
    group_peer_support_ref_placed,
    individual_peer_support_ref_placed,
    su_ref_placed
    ),
    names_to = "referral_subtype",
    values_to = "referral_placed"
  ) %>% 
  mutate(
    agency_name = case_when(
      referral_subtype == "cmhc_ref_placed" ~ cmhc_agency_desc,
      referral_subtype == "counseling_ref_placed" ~ counseling_agency_desc,
      referral_subtype == "group_peer_support_ref_placed" ~ group_peer_support_agency_desc,
      referral_subtype == "individual_peer_support_ref_placed" ~ indiv_peer_support_agency_desc,
      referral_subtype == "su_ref_placed" ~ su_agency_desc
    )
  ) %>% 
  mutate(
    agency_code = case_when(
      referral_subtype == "cmhc_ref_placed" ~ cmhc_agency,
      referral_subtype == "counseling_ref_placed" ~ counseling_agency,
      referral_subtype == "group_peer_support_ref_placed" ~ group_peer_support_agency,
      referral_subtype == "individual_peer_support_ref_placed" ~ indiv_peer_support_agency,
      referral_subtype == "su_ref_placed" ~ su_agency
    )
  ) %>% 
  mutate(
    referral_date = case_when(
      referral_subtype == "cmhc_ref_placed" ~ date_cmhc_ref_placed,
      referral_subtype == "counseling_ref_placed" ~ date_counseling_ref_placed,
      referral_subtype == "group_peer_support_ref_placed" ~ date_group_peer_support_ref_placed,
      referral_subtype == "individual_peer_support_ref_placed" ~ date_indiv_peer_support_ref,
      referral_subtype == "su_ref_placed" ~ date_su_ref
    )
  ) %>% 
  filter(
    referral_placed == 1
    ) %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    referral_type,
    referral_subtype,
    agency_code,
    agency_name,
    referral_date,
    notes_rp
  )

# Repeat for Housing Subtype
#Create long version of the behavioral health subtype referrals with agencies
# and dates. We will repeat this process for each subtype. 
rp_housing_long <- bcr_full_data %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    housing_ref_placed,
    furniture_ref_placed,
    home_repair_ref_placed,
    homeless_services_ref_placed,      
    landlord_med_ref_placed,
    rent_assist_ref_placed,
    utility_assist_ref_placed,         
    furniture_agency,
    furniture_agency_desc,
    date_furniture_ref,                
    home_repair_agency,
    home_repair_agency_desc,
    date_home_repairs_ref,             
    homeless_services_agency,
    homeless_services_agency_desc,
    date_homeless_services_ref,        
    landlord_med_agency,
    landlord_med_agency_desc,
    date_landlord_mediation_ref,       
    rent_assist_agency,
    rent_assist_agency_desc,
    date_rent_assist_ref,
    utility_assist_agency,
    utility_assist_agency_desc,
    date_utility_ref,
    notes_rp
  ) %>% 
  mutate(
    referral_type = "housing"
  ) %>% 
  pivot_longer(cols = c(
    furniture_ref_placed,
    home_repair_ref_placed,
    homeless_services_ref_placed,      
    landlord_med_ref_placed,
    rent_assist_ref_placed,
    utility_assist_ref_placed
    ),
    names_to = "referral_subtype",
    values_to = "referral_placed"
  ) %>% 
  mutate(
    agency_name = case_when(
      referral_subtype == "furniture_ref_placed" ~ furniture_agency_desc,
      referral_subtype == "home_repair_ref_placed" ~ home_repair_agency_desc,
      referral_subtype == "homeless_services_ref_placed" ~ homeless_services_agency_desc,
      referral_subtype == "landlord_med_ref_placed" ~ landlord_med_agency_desc,
      referral_subtype == "rent_assist_ref_placed" ~ rent_assist_agency_desc,
      referral_subtype == "utility_assist_ref_placed" ~ utility_assist_agency_desc
    )
  ) %>% 
  mutate(
    agency_code = case_when(
      referral_subtype == "furniture_ref_placed" ~ furniture_agency,
      referral_subtype == "home_repair_ref_placed" ~ home_repair_agency,
      referral_subtype == "homeless_services_ref_placed" ~ homeless_services_agency,
      referral_subtype == "landlord_med_ref_placed" ~ landlord_med_agency,
      referral_subtype == "rent_assist_ref_placed" ~ rent_assist_agency,
      referral_subtype == "utility_assist_ref_placed" ~ utility_assist_agency
    )
  ) %>% 
  mutate(
    referral_date = case_when(
      referral_subtype == "furniture_ref_placed" ~ date_furniture_ref,
      referral_subtype == "home_repair_ref_placed" ~ date_home_repairs_ref,
      referral_subtype == "homeless_services_ref_placed" ~ date_homeless_services_ref,
      referral_subtype == "landlord_med_ref_placed" ~ date_landlord_mediation_ref,
      referral_subtype == "rent_assist_ref_placed" ~ date_rent_assist_ref,
      referral_subtype == "utility_assist_ref_placed" ~ date_utility_ref
    )
  ) %>% 
  filter(
    referral_placed == 1
    ) %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    referral_type,
    referral_subtype,
    agency_code,
    agency_name,
    referral_date,
    notes_rp
  )


# Repeat for Social Service Subtype
#Create long version of the behavioral health subtype referrals with agencies
# and dates. We will repeat this process for each subtype. 
rp_social_services_long <- bcr_full_data %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    child_care_ref_placed,
    clothing_ref_placed,
    dd_ref_placed,
    dv_ref_placed,
    employment_ref_placed,
    food_ref_placed,
    insurance_ref_placed,
    legal_ref_placed,
    lgbtq_ref_placed,
    transport_ref_placed,
    child_care_agency,
    child_care_agency_desc,
    date_child_care_ref_placed,
    clothing_agency,
    clothing_agency_desc,
    date_clothing_ref_placed,
    dd_agency,
    dd_agency_desc,
    date_dd_ref_placed,
    dv_agency,
    dv_agency_desc,
    date_dv_ref_placed,
    food_agency,
    food_agency_desc,
    date_food_ref_placed,
    employment_agency,
    employment_agency_desc,
    date_employment_ref_placed,
    insurance_agency,
    insurance_agency_desc,
    date_insurance_ref_placed,
    legal_agency,
    legal_agency_desc,
    date_legal_ref_placed,
    lgbtq_agency,
    lgbtq_agency_desc,
    date_lgbtq_ref_placed,
    transport_agency,
    transport_agency_desc,
    date_transport_ref_placed,
    notes_rp
  ) %>% 
  mutate(
    referral_type = "social_services"
  ) %>% 
  pivot_longer(cols = c(
    child_care_ref_placed,
    clothing_ref_placed,
    dd_ref_placed,
    dv_ref_placed,
    employment_ref_placed,
    food_ref_placed,
    insurance_ref_placed,
    legal_ref_placed,
    lgbtq_ref_placed,
    transport_ref_placed
    ),
    names_to = "referral_subtype",
    values_to = "referral_placed"
  ) %>% 
  mutate(
    agency_name = case_when(
      referral_subtype == "child_care_ref_placed" ~ child_care_agency_desc,
      referral_subtype == "clothing_ref_placed" ~ clothing_agency_desc,
      referral_subtype == "dd_ref_placed" ~ dd_agency_desc,
      referral_subtype == "dv_ref_placed" ~ dv_agency_desc,
      referral_subtype == "employment_ref_placed" ~ employment_agency_desc,
      referral_subtype == "food_ref_placed" ~ food_agency_desc,
      referral_subtype == "insurance_ref_placed" ~ insurance_agency_desc,
      referral_subtype == "legal_placed" ~ legal_agency_desc,
      referral_subtype == "lgbtq_ref_placed" ~ lgbtq_agency_desc,
      referral_subtype == "transport_ref_placed" ~ transport_agency_desc
    )
  ) %>% 
  mutate(
    agency_code = case_when(
      referral_subtype == "child_care_ref_placed" ~ child_care_agency,
      referral_subtype == "clothing_ref_placed" ~ clothing_agency,
      referral_subtype == "dd_ref_placed" ~ dd_agency,
      referral_subtype == "dv_ref_placed" ~ dv_agency,
      referral_subtype == "employment_ref_placed" ~ employment_agency,
      referral_subtype == "food_ref_placed" ~ food_agency,
      referral_subtype == "insurance_ref_placed" ~ insurance_agency,
      referral_subtype == "legal_placed" ~ legal_agency,
      referral_subtype == "lgbtq_ref_placed" ~ lgbtq_agency,
      referral_subtype == "transport_ref_placed" ~ transport_agency
    )
  ) %>% 
  mutate(
    referral_date = case_when(
      referral_subtype == "child_care_ref_placed" ~ date_child_care_ref_placed,
      referral_subtype == "clothing_ref_placed" ~ date_clothing_ref_placed,
      referral_subtype == "dd_ref_placed" ~ date_dd_ref_placed,
      referral_subtype == "dv_ref_placed" ~ date_dv_ref_placed,
      referral_subtype == "employment_ref_placed" ~ date_employment_ref_placed,
      referral_subtype == "food_ref_placed" ~ date_food_ref_placed,
      referral_subtype == "insurance_ref_placed" ~ date_insurance_ref_placed,
      referral_subtype == "legal_placed" ~ date_legal_ref_placed,
      referral_subtype == "lgbtq_ref_placed" ~ date_lgbtq_ref_placed,
      referral_subtype == "transport_ref_placed" ~ date_transport_ref_placed
    )
  ) %>% 
  filter(
    referral_placed == 1
    ) %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    referral_type,
    referral_subtype,
    agency_code,
    agency_name,
    referral_date,
    notes_rp
  )


# Repeat for Physical Health Subtype
rp_phys_health_long <- bcr_full_data %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    primary_care_ref_placed,
    dental_ref_placed,
    primary_care_agency,
    primary_care_agency_desc,
    date_primary_care_ref_placed,
    dental_agency,
    dental_agency_desc,
    date_dental_ref_placed,
    notes_rp
  ) %>% 
  mutate(
    referral_type = "physical_health"
  ) %>% 
  pivot_longer(cols = c(
    primary_care_ref_placed,
    dental_ref_placed
    ),
    names_to = "referral_subtype",
    values_to = "referral_placed"
  ) %>% 
  mutate(
    agency_name = case_when(
      referral_subtype == "primary_care_ref_placed" ~ primary_care_agency_desc,
      referral_subtype == "dental_ref_placed" ~ dental_agency_desc
    )
  ) %>% 
  mutate(
    agency_code = case_when(
      referral_subtype == "primary_care_ref_placed" ~ primary_care_agency,
      referral_subtype == "dental_ref_placed" ~ dental_agency
    )
  ) %>% 
  mutate(
    referral_date = case_when(
      referral_subtype == "primary_care_ref_placed" ~ date_primary_care_ref_placed,
      referral_subtype == "dental_ref_placed" ~ date_dental_ref_placed
    )
  ) %>% 
  filter(
    referral_placed == 1
    ) %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    referral_type,
    referral_subtype,
    agency_code,
    agency_name,
    referral_date,
    notes_rp
  )

# Maternal Health
rp_mat_health_long <- bcr_full_data %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    mat_health_ref_placed,
    mat_health_agency,
    mat_health_agency_desc,
    date_mat_health_ref_placed,
    notes_rp
  ) %>% 
   mutate(
    referral_type = "maternal_health"
  ) %>% 
  pivot_longer(cols = c(
    mat_health_ref_placed
    ),
    names_to = "referral_subtype",
    values_to = "referral_placed"
  ) %>% 
  mutate(
    agency_name = case_when(
      referral_subtype == "mat_health_ref_placed" ~ mat_health_agency_desc
    )
  ) %>% 
  mutate(
    agency_code = case_when(
      referral_subtype == "mat_health_ref_placed" ~ mat_health_agency
    )
  ) %>% 
  mutate(
    referral_date = case_when(
      referral_subtype == "mat_health_ref_placed" ~ date_mat_health_ref_placed
    )
  ) %>% 
  filter(
    referral_placed == 1
    ) %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    referral_type,
    referral_subtype,
    agency_code,
    agency_name,
    referral_date,
    notes_rp
  )

# Other
rp_other_long <- bcr_full_data %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    other_ref_placed,
    other_agency,
    date_other_ref,
    notes_rp
  ) %>% 
   mutate(
    referral_type = "other"
  ) %>% 
  pivot_longer(cols = c(
    other_ref_placed
    ),
    names_to = "referral_subtype",
    values_to = "referral_placed"
  ) %>% 
  mutate(
    agency_name = case_when(
      referral_subtype == "other_ref_placed" ~ other_agency
    )
  ) %>% 
  mutate(
    referral_date = case_when(
      referral_subtype == "other_ref_placed" ~ date_other_ref
    )
  ) %>% 
  filter(
    referral_placed == 1
    ) %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    referral_type,
    referral_subtype,
    agency_name,
    referral_date,
    notes_rp
  )

# Spiritual Care
rp_spiritual_care_long <- bcr_full_data %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    spiritual_care_ref_placed,
    spiritual_agency,
    spiritual_agency_desc,
    date_spiritual_ref_placed,
    notes_rp
  ) %>% 
   mutate(
    referral_type = "spiritual_care"
  ) %>% 
  pivot_longer(cols = c(
    spiritual_care_ref_placed
    ),
    names_to = "referral_subtype",
    values_to = "referral_placed"
  ) %>% 
  mutate(
    agency_name = case_when(
      referral_subtype == "spiritual_care_ref_placed" ~ spiritual_agency_desc
    )
  ) %>% 
    mutate(
    agency_code = case_when(
      referral_subtype == "spiritual_care_ref_placed" ~ spiritual_agency
    )
  ) %>% 
  mutate(
    referral_date = case_when(
      referral_subtype == "spiritual_care_ref_placed" ~ date_spiritual_ref_placed
    )
  ) %>% 
  filter(
    referral_placed == 1
    ) %>% 
  select(
    client_num,
    enrollment_starting_date,
    rp_docserno,
    referral_type,
    referral_subtype,
    agency_code,
    agency_name,
    referral_date,
    notes_rp
  )

# Bind Rows
rp_all_long <- bind_rows(
  rp_bh_long,
  rp_housing_long,
  rp_social_services_long,
  rp_phys_health_long,
  rp_mat_health_long,
  rp_spiritual_care_long,
  rp_other_long
)



## ----bcr-events--------------------------------------------------------------------
# Create a vector of column names from the BCR events Extract.
bcr_events_nms <- names(
  read_csv(
    "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_EVENTS.csv",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_csv() with a vector of col_types. Without
# this, read_csv() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_csv's column
# guessing. R will arbitrarily change very long integers because it exceeds the
# precision allowed for integers in 32-bit. Ensuring that the long integers,
# such as DOCSERNO, are set to character type and supplying a single string of
# column types to readr's read_csv ensures that the numbers are not changed.
bcr_events_ct <- case_when(
  str_detect(
    bcr_events_nms, "USERID"
  ) ~ "c",
  str_equal(
    bcr_events_nms, "Id"
  ) ~ "n",
  str_equal(
    bcr_events_nms, "ID"
  ) ~ "n",
  str_detect(
    bcr_events_nms, "CLIENT_NUMBER"
  ) ~ "c",
  str_detect(
    bcr_events_nms, "DOCSERNO"
  ) ~ "c",
  str_ends(
    bcr_events_nms, "DATE"
  ) ~ "D",
  str_starts(
    bcr_events_nms, "DATE"
  ) ~ "D",
  str_detect(
    bcr_events_nms, "_DATE_"
  ) ~ "D",
  str_detect(
    bcr_events_nms, "VISITDT"
  ) ~ "D",
  str_detect(
    bcr_events_nms, "VISITTM"
  ) ~ "t",
  str_detect(
    bcr_events_nms, "_CODE"
  ) ~ "c",
    str_detect(
    bcr_events_nms, "_NUM"
  ) ~ "n",
    str_detect(
    bcr_events_nms, "NUM"
  ) ~ "n",
  .default = "c"
  ) %>%
  paste0(
    collapse = ""
  )

# Read Q_BCR_ACTIVE_PAYOR_SOURCE view file to get data on active payor source
# forms for all months and years.Uses clean_names() to transform the FAMCare
# fields into snake_case.
bcr_events <- readr::read_csv(
  "P:/DATA/Data Files/FAMCare BCR Extract/Q_BCR_EVENTS.csv",
  col_types = bcr_events_ct,
  na = c(
    "",
    " "
  )
) %>%
  clean_names(
    .,
  )%>%
  mutate(
  # Extracting referral month and calendar year from ref_pathway_date.Variables
  # for month and state fiscal year and also month and federal fiscal year will
  # follow.
    month_calendar_year_referral = zoo::as.yearmon(
      pathway_date,
      "%B %Y"
    )
  ) %>% 
    # State fiscal year variables for referral start here.
  mutate(
  # Determining state fiscal quarter based on July start of fiscal year.
    state_fiscal_quarter_referral = quarter(
      pathway_date,
      fiscal_start = 7
    )
  ) %>%
  mutate(
  # Extracting full state fiscal year (YYYY format).
    state_fiscal_year_referral = as.integer(
      quarter(
        pathway_date,
        with_year = TRUE,
        fiscal_start = 7
      )
    )
  ) %>%
  mutate(
  # Identifying state fiscal year with quarter context (YYYY.Q format).
    state_fiscal_year_qtr_referral = quarter(
      pathway_date,
      with_year = TRUE,
      fiscal_start = 7
      )
    ) %>%
  separate_wider_position(
  # Splitting state fiscal year into century and last two digits.
    state_fiscal_year_referral,
    c(
      century = 2,
      state_fiscal_year_two_digits_referral = 2
    ),
    cols_remove = FALSE
  ) %>%
  select(
  # Removing century column after extracting last two digits from state fiscal
  # year.
    -century
  ) %>%
  mutate(
  # Formatting sate fiscal quarter as "Qx FYxx" format.
    state_fiscal_year_qtr_string_referral = str_c(
      "Q",
      state_fiscal_quarter_referral,
      "\nFY",
      state_fiscal_year_two_digits_referral
      )
    ) %>%
  mutate(
  # Ordering strings for state fiscal year quarters in sequence.
    state_fiscal_year_qtr_string_referral = fct_reorder(
      state_fiscal_year_qtr_string_referral,
      state_fiscal_year_qtr_referral
    )
  ) %>%
  # Creating fiscal month factor for state fiscal year reporting.
  mutate(
    month_state_fiscal_year_referral = as_factor(
      str_c(
        as.character(
          lubridate::month(
            pathway_date,
            label = TRUE
          )
        ),
        "\nFY",
        state_fiscal_year_two_digits_referral
      )
    )
  ) %>%
  arrange(
  # Sorting by referral date for consistency.
    pathway_date
  ) %>%
  mutate(
  # Converting month into state fiscal month number (1-12) based on July fiscal
  # start.
    state_fiscal_month_num_referral = as.character(
      (
        lubridate::month(
          pathway_date,
          label = FALSE
        ) - 7
      ) %% 12 + 1
    )
  ) %>%
  mutate(
  # Combining state fiscal year and month as a factor.
    state_fiscal_year_month_num_referral = as_factor(
      str_c(
        state_fiscal_year_referral,
        ".",
        state_fiscal_month_num_referral
      )
    )
  ) %>%
  # Federal fiscal year variables for referral start here.
  mutate(
  # Extracting federal fiscal quarter based on October-start to the fiscal year.
    federal_fiscal_quarter = quarter(
      pathway_date,
      fiscal_start = 10
    )
  ) %>%
  mutate(
  # Extracting full federal fiscal year (YYYY format).
    federal_fiscal_year = as.integer(
      quarter(
        pathway_date,
        with_year = TRUE,
        fiscal_start = 10
      )
    )
  ) %>%
  # This seems redundant with month_calendar_year_referral (above). Commenting
  # out for the moment because I am sure this is used somewhere in periodic
  # reporting. We'll want to switch, but I figured retaining as commented out
  # will make it easier to troubleshoot. This should be deleted when possible.
  # mutate(
  #   epicc_month_year_referral = zoo::as.yearmon(
  #     ref_pathway_date,
  #     "%B %Y"
  #   )
  # )
  mutate(
  # Identifying federal fiscal year with quarter context (YYYY.Q format).
    federal_fiscal_year_qtr = quarter(
      pathway_date,
      with_year = TRUE,
      fiscal_start = 10
      )
    ) %>%
  separate_wider_position(
  # Splitting federal fiscal year into century and last two digits.
    federal_fiscal_year,
    c(
      century = 2,
      federal_fiscal_year_two_digits = 2
    ),
    cols_remove = FALSE
  ) %>%
  select(
  # Removing century column since the second use of separate_wider_position()
  # added it again.
    -century
  ) %>%
  mutate(
  # Formatting federal fiscal quarter as "Qx FYxx" format.
    federal_fiscal_year_qtr_string = str_c(
      "Q",
      federal_fiscal_quarter,
      "\nFY",
      federal_fiscal_year_two_digits
      )
    ) %>%
  mutate(
  # Ordering federal fiscal year quarter string as a factor using
  # federal_fiscal_year_qtr_referral to ensure that the levels are in sequence.
    federal_fiscal_year_qtr_string = fct_reorder(
      federal_fiscal_year_qtr_string,
      federal_fiscal_year_qtr
    )
  ) %>%
  mutate(
  # Creating federal fiscal month factor for federal fiscal year reporting.
    federal_month_fiscal_year = as_factor(
      str_c(
        as.character(
          lubridate::month(
            pathway_date,
            label = TRUE
          )
        ),
        "\nFY",
        federal_fiscal_year_two_digits
      )
    )
  ) %>%
  arrange(
  # This is probably redundant, but this ensures the sort order of referrals in
  # date order based on ref_pathway_date.
    pathway_date
  ) %>%
  mutate(
  # Converting month into federal fiscal month number (1-12).
    federal_fiscal_month_num = as.character(
      (
        lubridate::month(
          pathway_date,
          label = FALSE
        ) - 10
      ) %% 12 + 1
    )
  ) %>%
  mutate(
  # Factoring federal fiscal year and federal fiscal month number.
    federal_fiscal_year_month_num = as_factor(
      str_c(
        federal_fiscal_year,
        ".",
        federal_fiscal_month_num
      )
    )
  )




## ----bcr-trauma-assessment---------------------------------------------------------
#Directions
# Export Trauma Awareness Training Assessment from Survey Monkey. Make sure #there are no filters on the data before exporting. Open and
#Save file in Survey Monkey --> SAMHSA folder (as below). Delete columns, F, #G, H, and I. Save and close. 

bcr_trauma_assessment_nms <- names(
  read_excel(
    "P:/DATA/Data Files/Survey Monkey/SAMHSA/Trauma Awareness Training Assessment.xlsx",
    sheet = "Sheet",
    n_max = 0
  )
)  %>%
   append(
      "last"
 )

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_excel() with a vector of col_types. Without
# this, read_excel() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_excel's
# column guessing. Append _ct for "column types".
bcr_trauma_assessment_ct <- case_when(
  str_detect(
    bcr_trauma_assessment_nms, "ID"
  ) ~ "numeric",
  # str_detect(
  #   bcr_trauma_assessment_nms, "Date"
  # ) ~ "date",
  .default = "text"
)

# Rename columns to reflect the questions. Mutate Likert scale questions to
# numeric data. Calculate individual change scores based on prior and now
# responses and then calculate total change score.

bcr_trauma_assessment <- read_excel(
  "P:/DATA/Data Files/Survey Monkey/SAMHSA/Trauma Awareness Training Assessment.xlsx",
  sheet = "Sheet",
  col_types = bcr_trauma_assessment_ct,
  na = c(
    "",
    " "
  )
  ) %>% 
rename(respondent_id = 1,
       collector_id = 2,
       start_date = 3,
       end_date = 4,
      ip_address = 5,
       training_date = 6,
       everyone_reacts_same_tf = 7,
       aces_tf = 8,
       brain_function_tf = 9,
       multiple_events_tf = 10,
       confidentiality_tf = 11,
       identifying_aces_prior = 12,
       identifying_aces_now = 13,
       impact_trauma_brain_prior = 14,
       impact_trauma_brain_now = 15,
       know_trauma_impact_prior = 16,
       know_trauma_impact_now = 17,
       can_explain_prior = 18,
       can_explain_now = 19,
       trauma_impact_interactions_prior = 20,
       trauma_impact_interactions_now = 21,
       comfort_seek_mh_care_prior = 22,
       comfort_seek_mh_care_now = 23
       ) %>% 
  filter(
    !is.na(
      respondent_id
    )
  ) %>%
mutate(
  identifying_aces_prior = case_when (
    identifying_aces_prior == "Extremely knowledgeable" ~ 5,
    identifying_aces_prior == "Knowledgeable" ~ 4,
    identifying_aces_prior == "Somewhat knowledgeable" ~ 3,
    identifying_aces_prior == "Slightly knowledgeable" ~ 2,
    identifying_aces_prior == "Not at all knowledgeable" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  identifying_aces_now = case_when(
    identifying_aces_now == "Extremely knowledgeable" ~ 5,
    identifying_aces_now == "Knowledgeable" ~ 4,
    identifying_aces_now == "Somewhat knowledgeable" ~ 3,
    identifying_aces_now == "Slightly knowledgeable" ~ 2,
    identifying_aces_now == "Not at all knowledgeable" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  impact_trauma_brain_prior = case_when(
    impact_trauma_brain_prior == "Extremely knowledgeable" ~ 5,
    impact_trauma_brain_prior == "Knowledgeable" ~ 4,
    impact_trauma_brain_prior == "Somewhat knowledgeable" ~ 3,
    impact_trauma_brain_prior == "Slightly knowledgeable" ~ 2,
    impact_trauma_brain_prior == "Not at all knowledgeable" ~ 1,
    .default = NA_real_ 
   )
 ) %>% 
mutate(
  impact_trauma_brain_now = case_when(
    impact_trauma_brain_now == "Extremely knowledgeable" ~ 5,
    impact_trauma_brain_now == "Knowledgeable" ~ 4,
    impact_trauma_brain_now == "Somewhat knowledgeable" ~ 3,
    impact_trauma_brain_now == "Slightly knowledgeable" ~ 2,
    impact_trauma_brain_now == "Not at all knowledgeable" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  know_trauma_impact_prior = case_when(
    know_trauma_impact_prior == "Extremely knowledgeable" ~ 5,
    know_trauma_impact_prior == "Knowledgeable" ~ 4,
    know_trauma_impact_prior == "Somewhat knowledgeable" ~ 3,
    know_trauma_impact_prior == "Slightly knowledgeable" ~ 2,
    know_trauma_impact_prior == "Not at all knowledgeable" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  know_trauma_impact_now = case_when(
    know_trauma_impact_now == "Extremely knowledgeable" ~ 5,
    know_trauma_impact_now == "Knowledgeable" ~ 4,
    know_trauma_impact_now == "Somewhat knowledgeable" ~ 3,
    know_trauma_impact_now == "Slightly knowledgeable" ~ 2,
    know_trauma_impact_now == "Not at all knowledgeable" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  identifying_aces_change = identifying_aces_now - identifying_aces_prior
) %>% 
mutate(
  impact_trauma_brain_change = impact_trauma_brain_now - 
    impact_trauma_brain_prior
) %>% 
mutate(
  know_trauma_impact_change = know_trauma_impact_now - know_trauma_impact_prior
) %>% 
mutate(
  total_change_score_trauma = identifying_aces_change + impact_trauma_brain_change + know_trauma_impact_change
) 

#Provides a count of participants who increased knowledge by training 
#session. If NAs are present, adjust as needed. 
num_inc_knowledge_trauma <- bcr_trauma_assessment %>% 
  filter(
    total_change_score_trauma > 0
  ) %>% 
  tabyl(
    training_date
  ) %>%
  adorn_totals(
    "row"
  )

count_inc_knowledge_trauma <- bcr_trauma_assessment %>% 
  filter(
    total_change_score_trauma > 0
  ) %>% 
    summarise(n = dplyr::n()) %>%
    pull(n) %>%
    tidyr::replace_na(0L)



## ----bcr-qpr-assessment------------------------------------------------------------
bcr_qpr_assessment_nms <- names(
  read_excel(
    "P:/DATA/Data Files/Survey Monkey/SAMHSA/QPR Suicide Prevention Assessment.xlsx",
    sheet = "Sheet",
    n_max = 0
  )
)
# %>%
#   append(
#      "last"
# )

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_excel() with a vector of col_types. Without
# this, read_excel() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_excel's
# column guessing. Append _ct for "column types".
bcr_qpr_assessment_ct <- case_when(
  str_detect(
    bcr_qpr_assessment_nms, "ID"
  ) ~ "numeric",
  str_detect(
    bcr_qpr_assessment_nms, "_date_"
  ) ~ "date",
  .default = "text"
)

# Rename columns to reflect the questions. Mutate Likert scale questions to
# numeric data. Calculate individual change scores based on prior and now
# responses and then calculate total change score.

bcr_qpr_assessment <- read_excel(
  "P:/DATA/Data Files/Survey Monkey/SAMHSA/QPR Suicide Prevention Assessment.xlsx",
  sheet = "Sheet",
  col_types = bcr_qpr_assessment_ct,
  na = c(
    "",
    " "
  )
  ) %>% 
rename(respondent_id = 1,
       collector_id = 2,
       start_date = 3,
       end_date = 4,
       ip_address = 5,
       date_of_training = 6,
       know_warning_signs_prior = 7,
       know_warning_signs_now = 8,
       comfortable_asking_prior = 9,
       comfortable_asking_now = 10,
       know_steps_prior = 11,
       know_steps_now = 12,
       know_resources_prior = 13,
       know_resources_now = 14,
       extent_inc_knowledge = 15,
       most_valuable = 16,
       ) %>% 
  filter(
    !is.na(
      respondent_id
    )
  ) %>%
mutate(
  know_warning_signs_prior = case_when (
    know_warning_signs_prior == "Strongly Agree" ~ 5,
    know_warning_signs_prior == "Agree" ~ 4,
    know_warning_signs_prior == "Neutral" ~ 3,
    know_warning_signs_prior == "Disagree" ~ 2,
    know_warning_signs_prior == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  know_warning_signs_now = case_when (
    know_warning_signs_now == "Strongly Agree" ~ 5,
    know_warning_signs_now == "Agree" ~ 4,
    know_warning_signs_now == "Neutral" ~ 3,
    know_warning_signs_now == "Disagree" ~ 2,
    know_warning_signs_now == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  comfortable_asking_prior = case_when (
    comfortable_asking_prior == "Strongly Agree" ~ 5,
    comfortable_asking_prior == "Agree" ~ 4,
    comfortable_asking_prior == "Neutral" ~ 3,
    comfortable_asking_prior == "Disagree" ~ 2,
    comfortable_asking_prior == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  comfortable_asking_now = case_when (
    comfortable_asking_now == "Strongly Agree" ~ 5,
    comfortable_asking_now == "Agree" ~ 4,
    comfortable_asking_now == "Neutral" ~ 3,
    comfortable_asking_now == "Disagree" ~ 2,
    comfortable_asking_now == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  know_steps_prior = case_when (
    know_steps_prior == "Strongly Agree" ~ 5,
    know_steps_prior == "Agree" ~ 4,
    know_steps_prior == "Neutral" ~ 3,
    know_steps_prior == "Disagree" ~ 2,
    know_steps_prior == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  know_steps_now = case_when (
    know_steps_now == "Strongly Agree" ~ 5,
    know_steps_now == "Agree" ~ 4,
    know_steps_now == "Neutral" ~ 3,
    know_steps_now == "Disagree" ~ 2,
    know_steps_now == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>%   
mutate(
  know_resources_prior = case_when (
    know_resources_prior == "Strongly Agree" ~ 5,
    know_resources_prior == "Agree" ~ 4,
    know_resources_prior == "Neutral" ~ 3,
    know_resources_prior == "Disagree" ~ 2,
    know_resources_prior == "Strongly disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  know_resources_now = case_when (
    know_resources_now == "Strongly Agree" ~ 5,
    know_resources_now == "Agree" ~ 4,
    know_resources_now == "Neutral" ~ 3,
    know_resources_now == "Disagree" ~ 2,
    know_resources_now == "Strongly disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  know_warning_signs_change = know_warning_signs_now - know_warning_signs_prior
) %>% 
mutate(
  comfortable_asking_change = comfortable_asking_now - 
    comfortable_asking_prior
) %>% 
mutate(
  know_steps_change = know_steps_now - know_steps_prior
) %>% 
mutate(
  know_resources_change = know_resources_now - know_resources_prior
) %>%   
mutate(
  total_change_score = know_warning_signs_change + comfortable_asking_change + know_steps_change
  + know_resources_change
) 


num_inc_knowledge_qpr_by_date <- bcr_qpr_assessment %>% 
  filter(
    total_change_score > 0
  ) %>% 
  tabyl(
    date_of_training
  ) %>%
  adorn_totals(
    "row"
  )

count_inc_knowledge_qpr <- bcr_qpr_assessment %>% 
  filter(
    total_change_score > 0
  ) %>% 
  summarise(n = dplyr::n()) %>%
  pull(n) %>%
  tidyr::replace_na(0L)


#extent_knowledge_inc = slight, moderate, or significant




## ----bcr-sharing-hope-assessment---------------------------------------------------
bcr_sharing_hope_assessment_nms <- names(
  read_excel(
    "P:/DATA/Data Files/Survey Monkey/SAMHSA/Sharing Hope Survey.xlsx",
    sheet = "Sheet",
    n_max = 0
  )
)
# %>%
#   append(
#      "last"
# )

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_excel() with a vector of col_types. Without
# this, read_excel() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_excel's
# column guessing. Append _ct for "column types".
bcr_sharing_hope_assessment_ct <- case_when(
  str_detect(
    bcr_sharing_hope_assessment_nms, "ID"
  ) ~ "numeric",
  str_detect(
    bcr_sharing_hope_assessment_nms, "Date"
  ) ~ "date",
  .default = "text"
)

# Rename columns to reflect the questions. Mutate Likert scale questions to
# numeric data. Calculate individual change scores based on prior and now
# responses and then calculate total change score.

bcr_sharing_hope_assessment <- read_excel(
  "P:/DATA/Data Files/Survey Monkey/SAMHSA/Sharing Hope Survey.xlsx",
  sheet = "Sheet",
  col_types = bcr_sharing_hope_assessment_ct,
  na = c(
    "",
    " "
  )
  ) %>% 
rename(respondent_id = 1,
       collector_id = 2,
       start_date = 3,
       end_date = 4,
       ip_address = 5,
       extent_inc_knowledge = 6,
       dx_separate_from_indiv_prior = 7,
       dx_separate_from_indiv_now = 8,
       not_anyones_fault_prior = 9,
       not_anyones_fault_now = 10,
       impact_attitudes_beliefs = 11
       ) %>% 
  filter(
    !is.na(
      respondent_id
    )
  ) %>%
mutate(
  extent_inc_knowledge = case_when (
    extent_inc_knowledge == "Significant Increase" ~ 3,
    extent_inc_knowledge == "Moderate Increase" ~ 2,
    extent_inc_knowledge == "Slight Increase" ~ 1,
    extent_inc_knowledge == "No Increase" ~ 0,
    .default = NA_real_
  )
) %>% 
mutate(
  dx_separate_from_indiv_prior = case_when (
    dx_separate_from_indiv_prior == "Strongly Agree" ~ 5,
    dx_separate_from_indiv_prior == "Agree" ~ 4,
    dx_separate_from_indiv_prior == "Neutral" ~ 3,
    dx_separate_from_indiv_prior == "Disagree" ~ 2,
    dx_separate_from_indiv_prior == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  dx_separate_from_indiv_now = case_when (
    dx_separate_from_indiv_now == "Strongly Agree" ~ 5,
    dx_separate_from_indiv_now == "Agree" ~ 4,
    dx_separate_from_indiv_now == "Neutral" ~ 3,
    dx_separate_from_indiv_now == "Disagree" ~ 2,
    dx_separate_from_indiv_now == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  not_anyones_fault_prior = case_when (
    not_anyones_fault_prior == "Strongly Agree" ~ 5,
    not_anyones_fault_prior == "Agree" ~ 4,
    not_anyones_fault_prior == "Neutral" ~ 3,
    not_anyones_fault_prior == "Disagree" ~ 2,
    not_anyones_fault_prior == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  not_anyones_fault_now = case_when (
    not_anyones_fault_now == "Strongly Agree" ~ 5,
    not_anyones_fault_now == "Agree" ~ 4,
    not_anyones_fault_now == "Neutral" ~ 3,
    not_anyones_fault_now == "Disagree" ~ 2,
    not_anyones_fault_now == "Strongly Disagree" ~ 1,
    .default = NA_real_
  )
) %>% 
mutate(
  dx_separate_from_indiv_change = dx_separate_from_indiv_now - dx_separate_from_indiv_prior
) %>% 
mutate(
  not_anyones_fault_change = not_anyones_fault_now - 
    not_anyones_fault_prior
) %>% 
mutate(
  total_change_score_sh =  dx_separate_from_indiv_change + not_anyones_fault_change
) %>% 
mutate(
  inc_knowledge_sh = as.integer(
    coalesce(total_change_score_sh > 0, FALSE) |
      coalesce(extent_inc_knowledge >= 1, FALSE)
  )
)


count_inc_knowledge_sh <- bcr_sharing_hope_assessment %>% 
  filter(
    inc_knowledge_sh == 1
  ) %>% 
  summarise(n = dplyr::n()) %>%
  pull(n) %>%
  tidyr::replace_na(0L)



## ----bcr-pastoral-training-assessment----------------------------------------------
bcr_pastoral_training_assessment_nms <- names(
  read_excel(
    "P:/DATA/Data Files/Survey Monkey/SAMHSA/Advanced Pastoral Training Assessment.xlsx",
    sheet = "Sheet",
    n_max = 0
  )
)

# Use case_when() to set col_types for columns that should be numeric, date, and
# character data types. The purpose for this is to avoid hard-coding column
# types while still providing read_excel() with a vector of col_types. Without
# this, read_excel() can take a long time to read in files when there are many
# columns with lots of null values, since this tends to thwart read_excel's
# column guessing. Append _ct for "column types".
bcr_pastoral_training_assessment_ct <- case_when(
  str_detect(
    bcr_pastoral_training_assessment_nms, "ID"
  ) ~ "numeric",
  str_detect(
    bcr_pastoral_training_assessment_nms, "Date"
  ) ~ "date",
  .default = "text"
)

# Rename columns to reflect the questions. Mutate Likert scale questions to
# numeric data. Calculate individual change scores based on prior and now
# responses and then calculate total change score.

bcr_pastoral_training_assessment <- read_excel(
  "P:/DATA/Data Files/Survey Monkey/SAMHSA/Advanced Pastoral Training Assessment.xlsx",
  sheet = "Sheet",
  col_types = bcr_pastoral_training_assessment_ct,
  na = c(
    "",
    " "
  )
  ) %>% 
rename(respondent_id = 1,
       collector_id = 2,
       start_date = 3,
       end_date = 4,
       ip_address = 5,
       confident_mh_congregation_prior = 6,
       confident_mh_congregation_after = 7,
       familiarity_mh_resources_prior = 8,
       familiarity_mh_resources_after = 9,
       believe_mh_misunderstood_prior = 10,
       believe_mh_misunderstood_after = 11,
       prepared_address_mh_issues_prior = 12,
       prepared_address_mh_issues_after = 13,
       encourage_indiv_seek_mh_care_prior = 14,
       encourage_indiv_seek_mh_care_after = 15,
       access_resources_prior = 16,
       access_resources_after = 17,
       feel_equipped_address_mh_prior = 18,
       feel_equipped_address_mh_after = 19,
       aware_strategies_incorporate_mh_prior = 20,
       aware_strategies_incorporate_mh_after = 21,
       strong_collaborative_relationships_prior = 22,
       strong_collaborative_relationships_after = 23,
       reg_refer_members_mh_services_prior = 24,
       reg_refer_members_mh_services_after = 25,
       oe_value_participating = 26
       ) %>% 
  filter(
    !is.na(
      respondent_id
    )
  )

#Calculate change score
# mutate(
#   extent_inc_knowledge = case_when (
#     extent_inc_knowledge == "Significant Increase" ~ 3,
#     extent_inc_knowledge == "Moderate Increase" ~ 2,
#     extent_inc_knowledge == "Slight Increase" ~ 1,
#     extent_inc_knowledge == "No Increase" ~ 0,
#     .default = NA_real_
#   )
# ) %>% 
# mutate(
#   dx_separate_from_indiv_prior = case_when (
#     dx_separate_from_indiv_prior == "Strongly Agree" ~ 5,
#     dx_separate_from_indiv_prior == "Agree" ~ 4,
#     dx_separate_from_indiv_prior == "Neutral" ~ 3,
#     dx_separate_from_indiv_prior == "Disagree" ~ 2,
#     dx_separate_from_indiv_prior == "Strongly Disagree" ~ 1,
#     .default = NA_real_
#   )
# ) %>% 
# mutate(
#   dx_separate_from_indiv_now = case_when (
#     dx_separate_from_indiv_now == "Strongly Agree" ~ 5,
#     dx_separate_from_indiv_now == "Agree" ~ 4,
#     dx_separate_from_indiv_now == "Neutral" ~ 3,
#     dx_separate_from_indiv_now == "Disagree" ~ 2,
#     dx_separate_from_indiv_now == "Strongly Disagree" ~ 1,
#     .default = NA_real_
#   )
# ) %>% 
# mutate(
#   not_anyones_fault_prior = case_when (
#     not_anyones_fault_prior == "Strongly Agree" ~ 5,
#     not_anyones_fault_prior == "Agree" ~ 4,
#     not_anyones_fault_prior == "Neutral" ~ 3,
#     not_anyones_fault_prior == "Disagree" ~ 2,
#     not_anyones_fault_prior == "Strongly Disagree" ~ 1,
#     .default = NA_real_
#   )
# ) %>% 
# mutate(
#   not_anyones_fault_now = case_when (
#     not_anyones_fault_now == "Strongly Agree" ~ 5,
#     not_anyones_fault_now == "Agree" ~ 4,
#     not_anyones_fault_now == "Neutral" ~ 3,
#     not_anyones_fault_now == "Disagree" ~ 2,
#     not_anyones_fault_now == "Strongly Disagree" ~ 1,
#     .default = NA_real_
#   )
# ) %>% 
# mutate(
#   dx_separate_from_indiv_change = dx_separate_from_indiv_now - dx_separate_from_indiv_prior
# ) %>% 
# mutate(
#   not_anyones_fault_change = not_anyones_fault_now - 
#     not_anyones_fault_prior
# ) %>% 
# mutate(
#   total_change_score_pastoral =  dx_separate_from_indiv_change + not_anyones_fault_change
# ) %>% 
# mutate(
#   inc_knowledge_pastoral = as.integer(
#     coalesce(total_change_score_sh > 0, FALSE) |
#       coalesce(extent_inc_knowledge >= 1, FALSE)
#   )
# )
# 
# 
# count_inc_knowledge_pastoral <- bcr_pastoral_training_assessment %>% 
#   filter(
#     inc_knowledge_pastoral == 1
#   ) %>% 
#   summarise(n = dplyr::n()) %>%
#   pull(n) %>%
#   tidyr::replace_na(0L)



## ----bcr-program-engagement--------------------------------------------------------
bcr_count_all_program_refs_current_fp <- bcr_full_data %>% 
  filter(
    ref_pathway_date >= "2025-07-01" &
    ref_pathway_date <= "2026-03-31"
  ) %>% 
  tabyl(
    month_calendar_year_referral
  ) %>% 
  adorn_totals(
    "row"
  ) 

bcr_count_all_program_enrolls_current_fp <- bcr_full_data %>% 
  filter(
    enrollment_starting_date >= "2025-10-01" &
      enrollment_starting_date <= "2025-12-30"
  ) %>% 
  adorn_totals(
    "row"
  ) 

bcr_count_all_program_refs_current_fp_pz <- bcr_full_data %>% 
  filter(
    ref_pathway_date >= "2024-07-01" &
      ref_pathway_date <= "2025-06-30"
  ) %>% 
  tabyl(
    reside_in_pz
  ) %>% 
  adorn_totals(
    "row", "column"
  ) 



## ----mhb-workbook------------------------------------------------------------------
#MHB Workbook
# bcr_participant_workbook_current_fp <- bcr_full_data %>% 
  # mutate(
  #   mhb_client_status = case_when(
  #     ref_program_participation =="Unable To Contact" ~ "Inactive",
  #     ref_program_participation =="Ineligible" ~ "Inactive",
  #     ref_program_participation =="Declined Services" ~ "Inactive",
  #     ref_program_participation =="Referred To Alternative Services" ~ "Inactive",
  #     ref_program_participation == "Eligible" &
  #     !is.na(rp_pathway_date) ~ "Achieved",
  #     .default = "Active"
  #   )
  # ) %>% 
  # mutate(
  #   mhb_sex = case_when(
  #     gender_description == "Cisgender Man/Boy" ~ "Male",
  #     gender_description == "Cisgender Woman/Girl" ~ "Female",
  #     gender_description == "Transgender" ~ "Self-defined",
  #     gender_description == "Nonbinary/Gender Non-Conforming" ~ "Self-defined",
  #     .default = "Decline to disclose"
  #   )
  # ) %>% 
  # mutate(
  #   mhb_race = case_when(
  #     race_description == "Black or African American" ~ "Black/African American",
  #     race_description == "White" ~ "White/Caucasian",
  #     race_description == "Multiracial" ~ "Bi-Racial/Multi-Racial",
  #     .default = "Decline to disclose"
  #   )
  # ) %>% 
  # select(
  #   enroll_client_num,
  #   birth_date,
  #   zip_code,
  #   homeless_housing_insecure_eto,
  #   zip_of_initial_contact,
  #   mhb_sex,
  #   mhb_race,
  #   ethnicity_description,
  #   ref_pathway_date,
  #   # client_status,
  #   rp_pathway_date,
  #   bcr_grant_desc
  # ) %>% 
  # filter(
  #   bcr_grant_desc == "MHB"
  # )

#As of 12/12/2025 there were changes made to BCR forms and workflow necessitating a change in the MHB Workbook syntax. All clients will now have an IC. If program participation at IC is anything other than eligible, the client will be dismissed/Inactive.

bcr_participant_workbook_july_dec_2025 <- bcr_full_data %>% 
  filter(
    enrollment_starting_date >= "2025-07-01",
    bcr_grant_desc == "MHB"
  ) %>% 
  mutate(
    homeless_housing_insecure = case_when(
      housing_status_unhoused == 1 ~ "X",
      housing_status_precariously_housed == 1 ~ "X",
      housing_status_stably_housed == 1 ~ "No",
      housing_status_institutionally_housed ==1 ~ "No"
  )
  ) %>% 
  mutate(
    client_status = case_when(
      bcr_prog_participation_desc == "Eligible" & !is.na(rp_pathway_date) ~ "Achieved",
      bcr_prog_participation_desc == "Eligible" & is.na(rp_pathway_date) ~ "Active",
      bcr_prog_participation_desc == "Eligible" ~ "Inactive"
    )
  ) %>% 
    mutate(
    mhb_sex = case_when(
      gender_description == "Cisgender Man/Boy" ~ "Male",
      gender_description == "Cisgender Woman/Girl" ~ "Female",
      gender_description == "Transgender" ~ "Self-defined",
      gender_description == "Nonbinary/Gender Non-Conforming" ~ "Self-defined",
      .default = "Decline to disclose"
    )
  )%>% 
  mutate(
    mhb_race = case_when(
      race_description == "Black Or African American" ~ "Black/African American",
      race_description == "White" ~ "White/Caucasian",
      race_description == "Multiracial" ~ "Bi-Racial/Multi-Racial",
      .default = "Decline to disclose"
    )
  ) %>%   
  select(
    enroll_client_num,
    birth_date,
    zip_code,
    homeless_housing_insecure,
    zip_of_initial_contact,
    mhb_sex,
    mhb_race,
    ethnicity_description,
    enrollment_starting_date,
    client_status,
    rp_pathway_date,
    bcr_grant_desc
  )

#Check Zip codes to ensure they are STL City
#Check DOB to ensure only 18+ years old

# openxlsx::write.xlsx(
#   bcr_participant_workbook_july_dec_2025,
#     file = paste0(
#      "P:/DATA/Data Files/Data Audits/BCR/v2_mhb_participant_workbook_",
#     Sys.Date(),
#         ".xlsx"
#     ))


#Demographics for FY25


## ----bcr-samhsa-metrics------------------------------------------------------------
bcr_samhsa_r1 <- bcr_full_data %>% 
  filter(
    enrollment_starting_date >= "2025-10-01" &
      enrollment_starting_date <= "2026-3-31" &
      !is.na(
        rp_pathway_date
      )
  ) %>% 
  tabyl(
    month_calendar_year_referral
  ) %>% 
  adorn_totals(
    "row"
  ) 

bcr_samhsa_tr1 <- bcr_events %>% 
  filter(
    pathway_date >= "2026-01-01" &
    pathway_date <= "2026-3-31"
  ) %>% 
  filter(
    event_grant_description == "SAMHSA"
  ) %>% 
  select(
    pathway_date,
    event_name,
    event_topic,
    event_grant_description,
    total_num_attendees
  ) %>% 
  adorn_totals(
    "row"
  )  


#NAB1 will come from FAMCare if the training is MHFA, otherwise it will 
#come from it's respective Survey Monkey Total
# bcr_samhsa_nab1 = case_when(
#   training
# )


## ----arpa-metrics------------------------------------------------------------------
count_arpa_clients_by_month <- bcr_full_data %>%
  filter(
    bcr_grant_desc == "ARPA",
  ) %>% 
  tabyl(
    month_calendar_year_referral
  ) %>% 
  adorn_totals(
    "row"
  )

count_arpa_referred_to_counseling_clients_by_month <- bcr_full_data %>%
  filter(
    bcr_grant_desc == "ARPA",
    counseling_ref_placed == 1
  ) %>% 
  select(
    client_num,
    client_name,
    enrollment_starting_date,
    bcr_type_ref_placed,
    counseling_ref_placed,
    month_calendar_year_referral
  )%>% 
  tabyl(
    month_calendar_year_referral
  ) %>% 
  adorn_totals(
    "row"
  )

arpa_paid_counseling_sessions_by_month <- bcr_ccs %>% 
  filter(
    session_grant == "ARPA"
  ) %>% 
  tabyl(
    month_calendar_year_session
  )%>% 
  adorn_totals(
    "row"
  )
  


## ----bcr-reporting-subset, eval=FALSE----------------------------------------------
## 
## # Subset full_data tibble to various reporting data sets.
## # Filter full_data to enrollments that were made in the most recent month.
## # bcr_full_data_recent_month <- bcr_full_data %>%
## #   filter(
## #     enrollment_starting_date >= recent_month_start_date &
## #       enrollment_starting_date <= recent_month_end_date
## #   )
## #
## # # Filter full_data to referrals that were made in the current reporting fiscal
## # # quarter.
## # # bcr_events_samhsa_report_quarter <- bcr_events %>%
## #   #filter(
## # #
## # # Filter full_data to referrals that were made in the prior reporting fiscal
## # # quarter.
## # bcr_full_data_report_prior_quarter <- bcr_full_data %>%
## #   filter(
## #     federal_fiscal_quarter_referral == prior_fiscal_quarter &
## #       federal_fiscal_quarter_referral == prior_fiscal_year
## #   )
## #
## # # Filter full_data to referrals that were made in the current fiscal reporting
## # # year.
## # bcr_full_data_fiscal_year <- bcr_full_data %>%
## #   filter(
## #     enrollment_starting_date >= bcr_fiscal_year_start_date &
## #       enrollment_starting_date <= bcr_fiscal_year_end_date
## #   )
## #
## # # Filter full_data to referrals that were made in the prior fiscal reporting
## # # year.
## # bcr_full_data_prior_fiscal_year <- bcr_full_data %>%
## #   filter(
## #     enrollment_starting_date >= bcr_prior_fiscal_year_start_date &
## #       enrollment_starting_date <= bcr_prior_fiscal_year_end_date
## #   )
## #
## # # Filter full_data to referrals that were made in the year before the prior
## # # fiscal reporting year.
## # bcr_full_data_year_before_prior_fiscal_year <- bcr_full_data %>%
## #   filter(
## #     enrollment_starting_date >= bcr_year_before_prior_fiscal_year_start_date &
## #       enrollment_starting_date <= bcr_year_before_prior_fiscal_year_end_date
## #   )
## #
## # # Filter full_data to referrals that were made between program inception and the
## # # end of the most recent month.
## # bcr_full_data_inception <- bcr_full_data %>%
## #   filter(
## #     enrollment_starting_date >= bcr_program_inception_date &
## #       enrollment_starting_date <= params$end_date
## #   )
## 
## 


## ----test-bcr-data, eval=FALSE-----------------------------------------------------
## missing_referrals_ic <- bcr_full_data %>%
##   filter(
##     is.na(bcr_referral_docserno)|
##       is.na(bcr_initial_contact_docserno)
##   ) %>%
##   select(
##     client_num,
##     client_name,
##     enrollment_starting_date,
##     enrollment_ending_date,
##     dismissal_reason_description,
##     bcr_referral_docserno,
##     bcr_initial_contact_docserno,
##     bcr_referrals_placed_docserno
##   )
## 
## # openxlsx::write.xlsx(
## #  missing_referrals,
## #  file = paste0(
## #    "P:/DATA/Data Files/Data Audits/BCR/missing_referral_",
## #    Sys.Date(),
## #    ".xlsx"
## #  )
## # )
## 
## 
## # bcr_count_program_refs_by_county <- bcr_full_data %>%
## #   filter(
## #     enrollment_starting_date >= "2025-10-01" &
## #     enrollment_starting_date <= "2025-12-30"
## #   )%>%
## #   tabyl(
## #     month_calendar_year_referral,
## #     reside_stl_city
## #   ) %>%
## #   adorn_totals(
## #     "row"
## #   )
## 
## fy25_zips <- bcr_full_data %>%
##   filter(
##     enrollment_starting_date >= "2024-07-01",
##     enrollment_starting_date <= "2025-06-30",
##     reside_stl_city == "Yes",
##     !is.na(
##       zip_code
##     )
##   ) %>%
##   select(
##     client_num,
##     enrollment_starting_date,
##     zip_code,
##     reside_in_stl_city,
##     reside_stl_city
##   )
## 
## 
## #Comfort Seeking MH Care
## 
## likert_levels <- c(
##  "Strongly disagree",
##  "Disagree",
##  "Neutral",
##  "Agree",
##  "Strongly Agree"
## )
## 
## comfort_seeking_care <- bcr_trauma_assessment %>%
##  select(
## respondent_id,
##   comfort_seek_mh_care_prior,
##  comfort_seek_mh_care_now ) %>%
##  mutate(
##  prior_score = as.numeric(
## factor(comfort_seek_mh_care_prior, levels = likert_levels)
##  ),
##  now_score = as.numeric(
## factor(comfort_seek_mh_care_now, levels = likert_levels)
##  )
##  ) %>%
## mutate(
## improved = now_score > prior_score
## #|
## #(prior_score == 5 & now_score == 5)
## )
## 
## percent_improved <- comfort_seeking_care %>%
## summarise(
## n_total = sum(!is.na(prior_score) & !is.na(now_score)),
##  n_improved = sum(improved, na.rm = TRUE),
## pct_improved = n_improved / n_total
##  )
## 
## percent_inc_knowledge <- bcr_qpr_assessment %>%
##  select(
## respondent_id,
## extent_inc_knowledge
##  ) %>%
##  tabyl(
## extent_inc_knowledge
##  )
## 
## 
## pastoral_increases <- bcr_pastoral_training_assessment %>%
##  select(
## respondent_id,
##  prepared_address_mh_issues_prior,
##  prepared_address_mh_issues_after,
##  strong_collaborative_relationships_prior,
##  strong_collaborative_relationships_after
##  ) %>%
##  mutate(
##  stigma_prior_score = as.numeric(
##  factor(prepared_address_mh_issues_prior, levels = likert_levels)
##  ),
##  stigma_now_score = as.numeric(
##  factor(prepared_address_mh_issues_after, levels = likert_levels)
##  )
##  ) %>%
## mutate(
## improved_stigma = stigma_now_score > stigma_prior_score |
## (stigma_prior_score == 5 & stigma_now_score == 5)
## ) %>%
##  mutate(
##  collab_prior_score = as.numeric(
##  factor(strong_collaborative_relationships_prior, levels = likert_levels)
##  ),
##  collab_now_score = as.numeric(
##  factor(strong_collaborative_relationships_after, levels = likert_levels)
##  )
##  ) %>%
## mutate(
## improved_collab = collab_now_score > collab_prior_score |
## (collab_prior_score == 5 & collab_now_score == 5)
## )
## 
## stigma_percent_improved <- pastoral_increases %>%
##  summarise(
##  n_total_stigma = sum(!is.na(stigma_prior_score) & !is.na(stigma_now_score)),
##  n_improved_stigma = sum(improved_stigma, na.rm = TRUE),
##  pct_improved_stigma = n_improved_stigma / n_total_stigma
##  )
## 
## collab_percent_improved <- pastoral_increases %>%
##  summarise(
##  n_total_collab = sum(!is.na(collab_prior_score) & !is.na(collab_now_score)),
##  n_improved_collab = sum(improved_collab, na.rm = TRUE),
##  pct_improved_collab = n_improved_collab / n_total_collab
##  )
## 
## 
## 
## #Clients Fiscal Year to Date by Grant, including Ref. Placed Status
## fy26_ytd_clients <- bcr_full_data %>%
##   filter(
##     enrollment_starting_date >= "2025-07-01",
##     enroll_client_num != "000033295"
##   ) %>%
##   select(
##     enroll_client_num,
##     client_name,
##     enrollment_starting_date,
##     ref_pathway_date,
##     ic_pathway_date,
##     reside_stl_city,
##     bcr_grant_desc,
##     rp_pathway_date,
##     enrollment_ending_date,
##     dismissal_reason_description
##   )
## 
## 
## fy26_ytd_grant_summary <- fy26_ytd_clients %>%
##   mutate(
##     enrollment_id = paste(enroll_client_num, enrollment_starting_date, sep = " | "),
##     has_rp = !is.na(rp_pathway_date)
##   ) %>%
##   group_by(bcr_grant_desc) %>%
##   summarise(
##     total_enrollments = n_distinct(enrollment_id),
##     enrollments_with_rp = n_distinct(enrollment_id[has_rp]),
##     pct_with_rp = enrollments_with_rp / total_enrollments
##   ) %>%
##   ungroup() %>%
##   arrange(desc(total_enrollments)) %>%
##   mutate(
##     pct_with_rp = scales::percent(pct_with_rp, accuracy = 0.1)
##   )
## 
## 
## # openxlsx::write.xlsx(
## #   list(
## #     "Full Data" = fy26_ytd_clients,
## #     "Summary by Grant" = fy26_ytd_grant_summary
## #   ),
## #     file = paste0(
## #      "P:/DATA/Data Files/_BCR_data/fy26_ytd_clients__",
## #     Sys.Date(),
## #         ".xlsx"
## #     ),
## #   overwrite = TRUE
## #   )
## 
## 
## #Attempt one at Presenting Concerns vs. Referrals Placed
## pc_and_rp <- bcr_full_data %>%
##    filter(
##     !is.na(pres_con_docserno),
##     !is.na(bcr_referrals_placed_docserno),
##     enrollment_starting_date >= "2025-07-01"
##   ) %>%
##   select(
##     client_num,
##     client_name,
##     enrollment_starting_date,
##     bcr_presenting_concerns,
##     bcr_type_ref_placed,
##     unmet_needs,
##     unmet_needs_exp
##   ) %>%
##   filter(
##     bcr_presenting_concerns != bcr_type_ref_placed
##   )
## 
## #Attempt 2
## presenting_concerns_and_ref_placed <- bcr_full_data %>%
##    filter(
##     !is.na(pres_con_docserno),
##     !is.na(bcr_referrals_placed_docserno),
##    enrollment_starting_date >= "2025-07-01"
##   ) %>%
##   select(
##     client_num,
##     client_name,
##     enrollment_starting_date,
##     bcr_presenting_concerns,
##     behavioral_health_need,
##     housing_need,
##     maternal_health_need,
##     other_need,
##     physical_health_need,
##     social_services_need,
##     spiritual_care_need,
##     other_need_ref,
##     bcr_type_ref_placed,
##     bh_ref_placed,
##     physical_health_ref_placed,
##     housing_ref_placed,
##     mat_health_ref_placed,
##     other_ref_placed,
##     soc_services_ref_placed,
##     spiritual_care_ref_placed,
##     unmet_needs,
##     unmet_needs_exp
##   ) %>%
##   rename(
##     behavioral_health_ref_placed = bh_ref_placed,
##     maternal_health_ref_placed = mat_health_ref_placed,
##     social_services_ref_placed = soc_services_ref_placed,
##     rp_unmet_needs = unmet_needs,
##     rp_unmet_needs_exp = unmet_needs_exp
##   )
## 
## #pivot longer
## pc_rp_longer <- presenting_concerns_and_ref_placed %>%
##   pivot_longer(
##     cols = c(
##       behavioral_health_need, behavioral_health_ref_placed,
##       housing_need, housing_ref_placed,
##       maternal_health_need, maternal_health_ref_placed,
##       other_need, other_ref_placed,
##       physical_health_need, physical_health_ref_placed,
##       social_services_need, social_services_ref_placed,
##       spiritual_care_need, spiritual_care_ref_placed
##     ),
##     names_to = c("need_type", ".value"),
##     names_pattern = "(.*)_(need|ref_placed)"
##   ) %>%
##   mutate(
##     need_matched = case_when(
##       need == 1 & ref_placed == 1 ~ 1,
##       need == 1 & ref_placed == 0 ~ 0,
##       TRUE ~ NA_real_
##     )
##   )
## 
## needs_match_summary <- pc_rp_longer %>%
##   filter(
##     need == 1
##   ) %>%
##   group_by(
##     need_type
##   ) %>%
##   summarise(
##     clients_w_need = n(),
##     matched_needs = sum(ref_placed ==1, na.rm = TRUE),
##     unmatched_needs = sum(ref_placed==0, na.rm = TRUE)
##   ) %>%
##   ungroup()
## 
## unmatched_detail_list <- pc_rp_longer %>%
##   filter(
##     need ==1,
##     ref_placed == 0
##   ) %>%
##   select(
##     -other_need_ref
##   )
## 
## 
## bcr_community_mem_ref <- bcr_full_data %>%
##   filter(
##     ref_source_type == "Community Member",
##     enrollment_starting_date >= "2026-01-01"
##   )
## 
## 
