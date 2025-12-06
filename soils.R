# Load libraries
library(soilDB)
library(here)
library(tidyverse)
library(sf)

# Read in SSURGO attribute table exported from ArcGIS Pro to get mukey values
ssurgo_tbl <- read_csv(here('data/soils/SSURGO_Clip_Short_Table.csv'))

# Create vector of mukey values
mukey_list <- paste(unique(ssurgo_tbl$mukey), collapse = ",")

# Create query (pulling some unncessary data)
qry <- paste0("
SELECT 
  l.areasymbol, l.areaname, 
  mu.mukey, mu.musym, mu.muname,
  c.cokey, c.comppct_r, ch.hzname,
  ch.chkey, ch.hzdept_r, ch.hzdepb_r,
  ch.claytotal_r, ch.sandtotal_r, ch.sandvf_r, 
  ch.silttotal_r, ch.om_r,
  ch.kffact, ch.kwfact,
  c.hydgrp, c.slope_r,
  ch.ksat_r,
  cs.structgrade, cs.structsize, cs.structtype,
  csg.structgrpname
FROM mapunit mu
JOIN legend l       ON mu.lkey = l.lkey
JOIN component c    ON mu.mukey = c.mukey
JOIN chorizon ch    ON c.cokey = ch.cokey
LEFT JOIN chstructgrp csg ON ch.chkey = csg.chkey
LEFT JOIN chstruct cs     ON csg.chstructgrpkey = cs.chstructgrpkey
WHERE mu.mukey IN (", mukey_list, ")
ORDER BY mu.mukey, c.cokey, ch.chkey
")

# Retrieve tabular data
soil_df <- SDA_query(qry) %>%
  filter(hzdepb_r <= 20, 
         !grepl("^O", hzname)) %>%
  group_by(mukey, cokey) %>%
  slice_min(order_by = hzdepb_r, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    kffact = as.numeric(kffact),
    kwfact = as.numeric(kwfact)
  )

# Save CSV
write.csv(soil_df, 
          here('data/soils/soil_data_updated.csv'), 
          row.names = FALSE)

# Fetch spatial data
soil_polys <- fetchSDA_spatial(unique(soil_df$mukey), 
                               by.col = 'mukey',
                               method = 'feature',
                               geom.src = 'mupolygon')

# Join tabular data to spatial data via mukey
soil_polys_tab <- soil_polys %>%
  left_join(soil_mukey_agg, by = join_by(mukey)) %>%
  rename(natlmusym = nationalmusym)

# Save spatial data
st_write(soil_polys_tab, here('data/soils/SSURGO_K.gpkg'), delete_dsn = TRUE)
