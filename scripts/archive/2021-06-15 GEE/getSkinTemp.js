// set user variables
print('zones',zones);
print('images',imgCol);

print('the first one', imgCol.first());

var getAOI = require('users/christinaherrickunh/IDS-Lutz:skin_temps/aoi.js');
var aoi = getAOI.aoi;
var geo = aoi.geometry();
var pctTime = getAOI.pctTime;
var lakesurface = ee.Number(getAOI.lakesurface);

//print(aoi,pctTime);
Map.addLayer(aoi,{},'lake bounds');

var zone_buffers = zones.map(function(ft) {
  var b = ft.buffer({distance: 180, maxError: 10});
  return b.intersection({right: aoi, maxError: 5})});
Map.addLayer(zone_buffers, {}, 'zone buffers');

//////////////////////////////////////////////////
// Statistics to return

var reducers = ee.Reducer.mean().combine({
  reducer2: ee.Reducer.stdDev(), sharedInputs:true})
  .combine({reducer2: ee.Reducer.minMax(), sharedInputs:true})
  .combine({reducer2: ee.Reducer.median({maxBuckets:500, minBucketWidth: 0.125}), sharedInputs:true})
  .combine({reducer2: ee.Reducer.skew(), sharedInputs:true})
  .combine({reducer2: ee.Reducer.percentile({percentiles: [25,75], maxBuckets: 500, minBucketWidth: 0.125}), sharedInputs:true})
  .combine({reducer2: ee.Reducer.count(), sharedInputs:true});

var zreducers = ee.Reducer.mean().combine({ //for z-score bands
  reducer2: ee.Reducer.minMax(), sharedInputs:true}).combine({
    reducer2: ee.Reducer.percentile({percentiles: [25,75], maxBuckets: 500, minBucketWidth: 0.125}), sharedInputs:true});
    
//////////////////////////////////////////////////
// Atmospheric Correction & skin temp calculation

var aster13 = ag100.select(['emissivity_band13']).multiply(0.001).reduceRegion({reducer: ee.Reducer.mean(), geometry: geo, scale:100}).get('emissivity_band13');
var aster14 = ag100.select(['emissivity_band14']).multiply(0.001).reduceRegion({reducer: ee.Reducer.mean(), geometry: geo, scale:100}).get('emissivity_band14');
var emissivity = ee.Number.expression('(e13 + e14) / 2',{
  'e13': aster13,
  'e14': aster14
});

var e = ee.Number(1).divide(emissivity);

exports.atmosCorr = function(img) {
  img = ee.Image(img);
  var crs_out = img.select(0).projection();
  var systime = img.get('system:time_start');
  
  var radi = img.select('radi');
  var bt = img.select('bt');
  var Bg = img.select('Bg');
  
  var gamma_top = bt.multiply(bt);
  var gamma_bot = radi.multiply(Bg);
  var gamma = gamma_top.divide(gamma_bot).select([0],['gamma']);
  
  var delta_right = gamma_top.divide(Bg);
  var delta = bt.subtract(delta_right).select([0],['delta']);
  
  var v = ee.Image(img.get('vapor')) //THIS GETS THE CORRESPONDING VAPOR IMAGE
          .multiply(0.1) // kg/m2 to g/cm2
          .select([0],['vapor']);
  //Since the literature says that the algorithm doesn't work well with
  //water vapor columns over 2.5g/cm, I masked those pixels
  v = v.mask(v.lte(2.5));
  
  img = img.addBands(v);
  
  var psi_1 = img.expression(
    '(c1*v*v)+(c2*v)+c3',{
      c1: img.select('c11'),
      c2: img.select('c12'),
      c3: img.select('c13'),
      v: img.select('vapor')
    }).select([0],['psi_1']);
    
  var psi_2 = img.expression(
    '(c1*v*v)+(c2*v)+c3',{
      c1: img.select('c21'),
      c2: img.select('c22'),
      c3: img.select('c23'),
      v: img.select('vapor')
    }).select([0],['psi_2']);
  
  var psi_3 = img.expression(
    '(c1*v*v)+(c2*v)+c3',{
      c1: img.select('c31'),
      c2: img.select('c32'),
      c3: img.select('c33'),
      v: img.select('vapor')
    }).select([0],['psi_3']);
 
  var surface_temp = psi_1.multiply(radi).add(psi_2).multiply(e).add(psi_3).multiply(gamma).add(delta);
  
  surface_temp = ee.Image(surface_temp).setDefaultProjection(crs_out,null,30);
  surface_temp = ee.Image(surface_temp).select([0],['surface_temp']).copyProperties(img);
  surface_temp = surface_temp.set('system:time_start',systime);

  return surface_temp;
};

///////////////////////////////////////////
// export statistics for entire image
exports.exportStatistics = function(img){ 
// var exportStatistics = function(img){
  img = ee.Image(img).clip(geo);
  
  // existing properties from the img
  var landsattime = img.get('system:time_start');
  var cloudcover = img.get('CLOUD_COVER');
  var vaporImg = ee.Image(img.get('vapor'));
  var vaportime = vaporImg.get('system:time_start');
  var esd = img.get("EARTH_SUN_DISTANCE");
  var elev = img.get('SUN_ELEVATION');
  var azi = img.get('SUN_AZIMUTH');
  var sza = img.get('sza');
  var count = img.get('pixel_count');
  var pctAvail = ee.Number(count).divide(lakesurface);
  
  // get water vapor value; if there's more than one value, get the max
  var vaporMath = ee.Algorithms.If(ee.Algorithms.IsEqual(vaporImg,null),-9999,vaporImg);
  var vaporMathg = ee.Image(vaporMath).multiply(0.1);
  
  var getWaterCol = vaporMathg.reduceRegion({
    reducer: ee.Reducer.max(),
    geometry: geo,
    bestEffort: true,
    scale: 30
  });
  var waterCol = ee.Dictionary(getWaterCol).get('pr_wtr');
  
  // collect stats about the whole image
  var stats = img.reduceRegion({
    reducer: reducers,
    geometry: geo,
    //bestEffort: true,
    scale: 30,
    maxPixels:5e9
  });
  
  // get zscores over the whole image
  var lake_mean = ee.Number(stats.get('surface_temp_mean'));
  var lake_stdev = ee.Number(stats.get('surface_temp_stdDev'));
  var zscore = img.subtract(lake_mean).divide(lake_stdev).select([0],['zscore']);
  
  var zscore_stats = zscore.reduceRegion({
    reducer: zreducers,
    geometry: geo,
    scale: 30,
    maxPixels:5e9
  });
  
  var more_stats = ee.Dictionary({
      'pixel_count':count,
      'vapor_time':vaportime,
      'landsat_time':landsattime,
      'cloud_cover':cloudcover,
      'water_column':waterCol,
      'emiss':emissivity,
      'elev':elev,
      'azimuth':azi,
      'esd':esd,
      'sza':sza,
      'pct_lake':pctAvail,
      'l_exceltime': ee.Number(landsattime).divide(1000.0).divide(86400).add(25569),
      'v_exceltime': ee.Number(vaportime).divide(1000.0).divide(86400).add(25569)
  });
  
  stats = stats.combine(more_stats).combine(zscore_stats);

  return ee.Feature(null,stats);
};

///////////////////////////////////////////
// export zonal statistics using insitu data locations
  
exports.exportZonalStatistics = function(img){
  // existing properties from the img
  var uid = img.get('uid')
  var landsattime = img.get('system:time_start');
  var cloudcover = img.get('CLOUD_COVER');
  var vaporImg = ee.Image(img.get('vapor'));
  var vaportime = vaporImg.get('system:time_start');
  var esd = img.get("EARTH_SUN_DISTANCE");
  var elev = img.get('SUN_ELEVATION');
  var azi = img.get('SUN_AZIMUTH');
  var sza = img.get('sza');
  var total_count = img.get('pixel_count');
  var pctAvail = ee.Number(total_count).divide(lakesurface);
  
  // get water vapor value; if there's more than one value, get the max
  var vaporMath = ee.Algorithms.If(ee.Algorithms.IsEqual(vaporImg,null),-9999,vaporImg);
  var vaporMathg = ee.Image(vaporMath).multiply(0.1);
  
  var getWaterCol = vaporMathg.reduceRegion({
    reducer: ee.Reducer.max(),
    geometry: geo,
    bestEffort: true,
    scale: 30
  });
  var waterCol = ee.Dictionary(getWaterCol).get('pr_wtr');
  
  var zone_stats = ee.Image(img).reduceRegions({
    collection: zone_buffers,
    reducer: reducers,
    // geometry: geo,
    //bestEffort: true,
    scale: 30,
  });
  
  var more_stats = ee.Dictionary({
    'pixel_count':total_count,
    'uid':uid,
    'vapor_time':vaportime,
    'landsat_time':landsattime,
    'cloud_cover':cloudcover,
    'water_column':waterCol,
    'emiss':emissivity,
    'elev':elev,
    'azimuth':azi,
    'esd':esd,
    'sza':sza,
    'pct_lake':pctAvail,
    'l_exceltime': ee.Number(landsattime).divide(1000.0).divide(86400).add(25569),
    'v_exceltime': ee.Number(vaportime).divide(1000.0).divide(86400).add(25569)
  });
  
  return zone_stats.map(function(ft){
     return ft.setMulti(more_stats);
  })};


exports.attachStatistics = function(img) {
  img = ee.Image(img).clip(geo);
  
  var landsattime = img.get('system:time_start');
  var cloudcover = img.get('CLOUD_COVER');
  var vaporImg = ee.Image(img.get('vapor'));
  var vaportime = vaporImg.get('system:time_start');
  var esd = img.get("EARTH_SUN_DISTANCE");
  var elev = img.get('SUN_ELEVATION');
  var azi = img.get('SUN_AZIMUTH');
  var sza = img.get('sza');
  var count = img.get('pixel_count');
  var total_count = img.get('pixel_count');
  var pctAvail = ee.Number(total_count).divide(lakesurface);
  
  var vaporMath = ee.Algorithms.If(ee.Algorithms.IsEqual(vaporImg,null),-9999,vaporImg);
  var vaporMathg = ee.Image(vaporMath).multiply(0.1);
  
  var getWaterCol = vaporMathg.reduceRegion({
    reducer: ee.Reducer.max(),
    geometry: geo,
    bestEffort: true,
    scale: 30
  });
  var waterCol = ee.Dictionary(getWaterCol).get('pr_wtr');
  
  // collect stats about the whole image
  var stats = img.reduceRegion({
    reducer: reducers,
    geometry: geo,
    //bestEffort: true,
    scale: 30,
    maxPixels:5e9
  });
  
    var more_stats = ee.Dictionary({
      'pixel_count':count,
      'vapor_time':vaportime,
      'landsat_time':landsattime,
      'cloud_cover':cloudcover,
      'water_column':waterCol,
      'emiss':emissivity,
      'elev':elev,
      'azimuth':azi,
      'esd':esd,
      'sza':sza,
      'pct_lake':pctAvail,
      'l_exceltime': ee.Number(landsattime).divide(1000.0).divide(86400).add(25569),
      'v_exceltime': ee.Number(vaportime).divide(1000.0).divide(86400).add(25569)
  });
  
  var lake_mean = ee.Number(stats.get('surface_temp_mean'));
  var lake_stdev = ee.Number(stats.get('surface_temp_stdDev'));
  var zscore = img.subtract(lake_mean).divide(lake_stdev).select([0],['zscore']);
  img = img.addBands(zscore);


  return ee.Image(img).set(stats);
};