box::use(dplyr[...])
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
d7 %>%
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
  select(site_id, year, depth, temp, temp_gradient, thermocline_depth, n)
