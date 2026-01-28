library(sf)
library(rmapshaper)

sa3_sf <- st_read("public_data/sa3_cases.shp")

# Simplify geometry (keep 10% of points - adjust as needed)
sa3_sf_simple <- ms_simplify(sa3_sf, keep = 0.1, keep_shapes = TRUE)

# Save simplified version
st_write(sa3_sf_simple, "public_data/sa3_cases_simple.shp", delete_dsn = TRUE)


# Read Ixodes holocyclus distribution GeoJSON and extract the polyline
holocyclus_line <- geojson_sf(
  "public_data/holocyclus_distribution_polyline.geojson"
) %>%
  filter(st_geometry_type(.) == "LINESTRING") %>%
  smooth(method = "spline")

saveRDS(holocyclus_line, "public_data/holocyclus_polyline_smooth.rds")
