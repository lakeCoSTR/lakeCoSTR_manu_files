GEE files used to create the file temp_zonal_stats_sunapee_1985_2020_06_15.csv

# getSkinTemp.js
Imports list:
var imgCol = ee.ImageCollection("users/christinaherrickunh/ids-lutz/sunapee"),
    ag100 = ee.Image("NASA/ASTER_GED/AG100_003"),
    zones = ee.FeatureCollection("users/steeleb/sun_hf_sites");

# _main.js
Imports list:
var vapor = ee.ImageCollection("NCEP_RE/surface_wv"),
    l4t1 = ee.ImageCollection("LANDSAT/LT04/C01/T1"),
    l4t2 = ee.ImageCollection("LANDSAT/LT04/C01/T2"),
    l5t1 = ee.ImageCollection("LANDSAT/LT05/C01/T1"),
    l5t2 = ee.ImageCollection("LANDSAT/LT05/C01/T2"),
    l7t1 = ee.ImageCollection("LANDSAT/LE07/C01/T1"),
    l7t2 = ee.ImageCollection("LANDSAT/LE07/C01/T2"),
    l8t1 = ee.ImageCollection("LANDSAT/LC08/C01/T1"),
    l8t2 = ee.ImageCollection("LANDSAT/LC08/C01/T2");

# bandPrep.js
Imports list:
var geo = /* color: #98ff00 */ee.Geometry.Point([-69.54750396069761, 44.431822517098155]);

# aoi.js
Imports list:
var sw = ee.Image("JRC/GSW1_2/GlobalSurfaceWater"),
    jrc_meta = ee.Image("JRC/GSW1_2/Metadata"),
    sunapee = 
    /* color: #be3bb3 */
    /* shown: false */
    ee.Geometry.Polygon(
        [[[-72.09056854248053, 43.316935124838935],
          [-72.02533721923834, 43.31643554010213],
          [-72.022933959961, 43.432229129757175],
          [-72.09194183349615, 43.432727761476514]]]),
    china = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-69.60895873364683, 44.482793782477906],
          [-69.60895873364683, 44.3901302067578],
          [-69.50252867993589, 44.3901302067578],
          [-69.50252867993589, 44.482793782477906]]], null, false),
    auburn = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-70.28859882717776, 44.176301724335815],
          [-70.28859882717776, 44.12432415852625],
          [-70.22336750393558, 44.12432415852625],
          [-70.22336750393558, 44.176301724335815]]], null, false),
    stordalen = 
    /* color: #d63000 */
    /* shown: false */
    ee.Geometry.Polygon(
        [[[19.055061423583993, 68.35725395878866],
          [19.050340735717782, 68.35677911513442],
          [19.04810913781739, 68.35592437156379],
          [19.051799857421884, 68.35244175078549],
          [19.05591973046876, 68.35139686053382],
          [19.058065497680673, 68.35215678547826],
          [19.058322989746102, 68.35294834695895],
          [19.05939587335206, 68.35324913309712],
          [19.061434199525305, 68.35398916009333],
          [19.061970761693402, 68.35556811679125],
          [19.057378891282774, 68.35692149191452]]]),
    wrs2 = ee.FeatureCollection("users/christinaherrickunh/WRS2_descending_2018"),
    utmbounds = ee.FeatureCollection("users/christinaherrickunh/UTM_Zone_Boundaries");
