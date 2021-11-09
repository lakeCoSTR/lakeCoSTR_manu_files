/*
This script uses the JRC Global Surface Water dataset to find surface water.
It's currently set up to return an outline of Lake Sunapee
*/

// var box = /* color: #be3bb3 */ee.Geometry.Polygon(
//         [[[-72.09056854248053, 43.316935124838935],
//           [-72.02533721923834, 43.31643554010213],
//           [-72.022933959961, 43.432229129757175],
//           [-72.09194183349615, 43.432727761476514]]]);

//Define area of interest
var geo = sunapee;
var pctTime = 55; //what percent of the time does a pixel need to classify as water?

var pathrow = wrs2.filterBounds(geo);//
var printPR = pathrow.limit(5).toList(5).map(function(x) {
  return ee.Feature(x).get('PR');
});
print('path row',printPR);

var utm = utmbounds.filterBounds(geo);
var printUTM = utm.limit(2).toList(2).map(function(x) {return ee.Feature(x).get('ZONE')});
print('utm zone',printUTM);

Map.addLayer(utm,{color:'gray'}, 'utm zone', false);
Map.addLayer(pathrow,{color: 'red'},'path row', false);

exports.pctTime = pctTime;

Map.centerObject(geo);

//////////////////////////////////////////////////////////////////////////////////
/*FIND BIG WATER BODIES*/
//////////////////////////////////////////////////////////////////////////////////
/*
Since a pixel isn't the same from scene to scene, this ensures that the same pixels are
being compared from scene to scene. Using the Global Surface Water data layer, I 
delineated the lake water bodies. I could have done it by hand, but for future extrapolation,
I wanted an "automated" way to do it.
*/
var wateroccurrence = sw.select(0);

var water = wateroccurrence.gte(pctTime);
water = water.updateMask(water.neq(0));//.int8();

var regions = water.addBands(wateroccurrence).reduceToVectors({
  reducer: ee.Reducer.min(),
  geometry: geo,
  scale: 30,
  labelProperty: 'surfaceWater'
}).map(function(ft) {
  var area = ft.area(10);
  return ft.set('area',area);
}).sort('area',false);

var lake_outline = ee.Feature(regions.first());

var watercount = water.reduceRegion({
  reducer: ee.Reducer.count(), 
  geometry: lake_outline.geometry(),
  scale:30
});
exports.lakesurface = watercount.get('occurrence'); //total pixels over lake
print("# of lake pixels: ", watercount.get('occurrence'));

//Sunapee
var observations = jrc_meta.select(["valid_obs"]).clip(lake_outline);
var obs_mean = observations.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: lake_outline.geometry(),
  scale: 30
});
print('mean lake observations:', obs_mean.get('valid_obs'));
Map.addLayer(observations,{},'valid observations',false);
exports.aoi = lake_outline;

//China
//exports.aoi = ee.Feature(regions.filterMetadata('system:index','equals',featureid).first());

//var forExport = regions.filterMetadata('system:index','equals',featureid);

Export.table.toDrive({
  collection: ee.FeatureCollection(lake_outline),
  description: 'lake_bounds_shp',
  folder: 'GEE_IDS_Lutz',
  fileFormat: 'SHP'
});

Map.addLayer(regions,{'color':'green'},'all water');
Map.addLayer(lake_outline,{'color':'blue'},'single lake')
