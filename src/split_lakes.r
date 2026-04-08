box::use(dplyr[...])
box::use(ggplot2[...])
box::use(readr[read_csv])

i7 <- read_csv(
  "2007/nla2007_sampledlakeinformation_20091113.csv",
  show_col_types = FALSE
)
p7 <- read_csv("2007/nla2007_profile_20091008.csv", show_col_types = FALSE)
s7 <- read_csv("2007/nla2007_secchi_20091008.csv", show_col_types = FALSE)

names(p7)
names(s7)

d7 <- left_join(
  p7,
  s7,
  by = c("SITE_ID", "YEAR"),
  relationship = "many-to-many"
) |>
  left_join(
    i7,
    by = c("SITE_ID"),
    relationship = "many-to-many"
  ) |>
  select(
    site_id = SITE_ID,
    year = YEAR,
    depth = DEPTH,
    metalimnion = METALIMNION,
    temp = TEMP_FIELD,
    do = DO_FIELD,
    ph = PH_FIELD,
    cond = COND_FIELD,
    secchi = SECMEAN,
    clear_to_bottom = CLEAR_TO_BOTTOM,
    # ecoregion / geography
    st = ST,
    epa_reg = EPA_REG,
    wsa_eco3 = WSA_ECO3,
    wsa_eco9 = WSA_ECO9,
    eco_lev_3 = ECO_LEV_3,
    eco_l3_nam = ECO_L3_NAM,
    nut_reg = NUT_REG,
    nutreg_name = NUTREG_NAME,
    eco_nuta = ECO_NUTA,
    huc_2 = HUC_2,
    huc_8 = HUC_8,
    # lake size & morphology
    area_ha = AREA_HA,
    lakearea = LAKEAREA,
    lakeperim = LAKEPERIM,
    area_cat7 = AREA_CAT7,
    size_class = SIZE_CLASS,
    sld = SLD,
    depth_x = DEPTH_X,
    depthmax = DEPTHMAX,
    elev_pt = ELEV_PT,
    # lake type / condition
    lake_origin = LAKE_ORIGIN,
    urban = URBAN,
    eco3_x_origin = ECO3_X_ORIGIN,
    rt_nla = RT_NLA,
    ref_cluster = REF_CLUSTER,
    ref_nutr = REF_NUTR,
    site_type = SITE_TYPE
  )


# Identify the thermocline depth as the depth of maximum temperature gradient
thermo <- d7 %>%
  group_by(site_id, year) %>%
  # add number of observations per group (depth profile)
  add_tally() %>%
  # filter out profiles with fewer than 5 observations (not enough data to identify thermocline)
  filter(n > 5) %>%
  # filter out profiles that have missing temperature data
  filter(!is.na(temp)) %>%
  arrange(site_id, year, depth) %>%
  mutate(temp_gradient = c(NA, diff(temp))) %>%
  # filter(site_id == "NLA06608-3320") %>%
  mutate(thermocline_depth = depth[which.min(temp_gradient)]) %>%
  ungroup() |>
  distinct(site_id, year, thermocline_depth, .keep_all = TRUE)


# OMERNIK Ecoregions:

# Eastern Highlands

#     Northern Appalachians (NA): Includes rugged mountains and forests spanning from New England into the Central Appalachians.
#     Southern Appalachians (SA): Covers the diverse forest and plateau regions of the southeastern United States.

# Plains and Lowlands

#     Coastal Plain (CP): Includes the flat, often marshy areas along the Atlantic and Gulf coasts.
#     Northern Plains (NP): Comprises the vast grassland and agricultural areas of the upper Midwest and Dakotas.
#     Southern Plains (SP): Encompasses the drier grasslands and prairies of the South Central U.S..
#     Temperate Plains (TP): Covers the highly productive agricultural "corn belt" of the central Midwest.
#     Upper Midwest (UM): Characterized by glaciated terrain and numerous lakes and wetlands.

# West

#     Western Mountains (WM): Includes the Rockies, Cascades, and Sierra Nevada ranges.
#     Xeric West (XR): Encompasses the arid and semi-arid deserts and plateaus of the West

# Summarise thermocline depth by wsa_eco9
thermo_summary <- thermo %>%
  group_by(wsa_eco9) %>%
  summarise(
    mean_thermocline_depth = mean(thermocline_depth, na.rm = TRUE),
    median_thermocline_depth = median(thermocline_depth, na.rm = TRUE),
    n_lakes = n()
  )

# histogram of thermocline depth by wsa_eco9
ggplot(thermo, aes(x = thermocline_depth)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~wsa_eco9) +
  labs(
    title = "Distribution of Thermocline Depth by Ecoregion",
    x = "Thermocline Depth (m)",
    y = "Count of Lakes"
  ) +
  theme_minimal()
