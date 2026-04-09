# Goal: How does mixed-layer depth (MLD) vary across ecoregions in the US?

# 1. Load packages needed for running the analysis

library("readr") # para cargar datos en R (de csv o excel)
library("dplyr") # para manipular datos en data.frame (tabelas)
library("ggplot2") # para crear graficos

# 2. Load the data

p7 <- readr::read_csv("2007/nla2007_profile_20091008.csv")

# 2.1. Basic information about a data frame

nrow(p7)
ncol(p7)
dim(p7)
summary(p7)
dplyr::glimpse(p7)

# 2.2 How many lakes do we have?

# TODO ABAJO hace lo mismo

length(unique(p7$SITE_ID))

p7 |>
    dplyr::distinct(SITE_ID) |>
    nrow()


p7 |>
    dplyr::pull(SITE_ID) |>
    unique() |>
    length()

p7$SITE_ID |>
    unique() |>
    length()

# Which is the deepest lake?

p7 |>
    dplyr::group_by(SITE_ID) |>
    dplyr::add_tally() |>
    dplyr::select(SITE_ID, n) |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::desc(n))


p7 |>
    dplyr::group_by(SITE_ID, YEAR, VISIT_NO) |>
    dplyr::summarize(MAX_DEPTH = max(DEPTH, na.rm = TRUE))

# 3. Define a function to calculate the MLD

lago <- "NLA06608-0006"

p7 |>
    dplyr::filter(SITE_ID == lago) |>
    dplyr::select(SITE_ID, YEAR, VISIT_NO, DEPTH, TEMP_FIELD) |>
    dplyr::group_by(VISIT_NO) |>
    dplyr::mutate(TEMP_DIFF = c(NA, diff(TEMP_FIELD))) |>
    print(n = 100)

data1 <- p7 |>
    dplyr::filter(SITE_ID == lago, VISIT_NO == 1) |>
    dplyr::select(SITE_ID, YEAR, VISIT_NO, DEPTH, TEMP_FIELD)

calc_thermocline_depth <- function(data) {
    data |>
        dplyr::mutate(TEMP_DIFF = c(NA, diff(TEMP_FIELD))) |>
        dplyr::select(SITE_ID, DEPTH, TEMP_DIFF) |>
        dplyr::slice_min(order_by = TEMP_DIFF)
}

calc_thermocline_depth(data1)

# 4. Apply this function to calculate MLD for all lakes

# 5. Summarize the MLD by ecoregion in a table

# 6. Create a chart showing the distribution of MLD by ecoregion
