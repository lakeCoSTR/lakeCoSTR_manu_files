var getAOI = require('users/christinaherrickunh/IDS-Lutz:skin_temps/aoi.js');
var bandPrep = require('users/christinaherrickunh/IDS-Lutz:skin_temps/bandPrep.js');
var skintemp = require('users/steeleb/nasa-herrick:getSkinTemp.js');
//var stats = require('users/christinaherrickunh/IDS-Lutz:skin_temps/statistics.js');

var pixel_min = ee.Number(getAOI.lakesurface).multiply(0.25);
print('pixel min here', pixel_min);
// var pixel_min = 5910; // sunapee
//var pixel_min = 3400; // auburn
//var pixel_min = 75; // stordalen
var rmse = 24;
var y1 = 2018, y2 = 2018; //year range
var m1 = 5, m2 = 11; //month range
var p1 = 13, p2 = 13; // not gt p1, not lt p2
var r1 = 30, r2 = 30; // not less than r1, not gt r2
//var d1 = '2009-06-01', d2 = '2014-06-30';
//var path = 13, row = 30; //landsat

var aoi = getAOI.aoi;
var geo = aoi.geometry();


//////////////////////////////////////////////////////////////////////////////////
/* FILTER AND STACK LANDSAT */
//////////////////////////////////////////////////////////////////////////////////

/*
For each landsat stack, filter by total cloud cover, date, site location, and
make sure all landsat scenes are in descending orbit (wrs<234), then create radiance band, 
toa brightness temp band in Celcius, & band coefficients and constants.
Stack them together, and make sure the metadata and system time carries over. For landsat 8, 
make sure scenes are nadir and the TIRS algorithm isn't preliminary version

Coefficients derive from modeling the TIGR2311 atmospheric sounding database, except landsat 8,
which come from GAPRI4714.  They're the 'coeff' variable below

See this paper from 2003 for the gist - https://unh.box.com/v/jimenez-2003
this paper, table 2 for landsat 4-7 coefficients - https://unh.box.com/v/jimenez-2009
this paper, eq 6 for landsat 8 coefficients - https://unh.box.com/v/jimenez-2014
*/

var l4 = l4t1.merge(l4t2)
        //.filterMetadata('CLOUD_COVER','less_than',cc)
        .filterMetadata('DATA_TYPE','equals','L1TP')
        .filterMetadata('GEOMETRIC_RMSE_MODEL','not_greater_than',rmse)
        .filterBounds(geo)
        //.filterDate(d1,d2)
        //.filterMetadata('WRS_PATH','equals',path)
        //.filterMetadata('WRS_ROW','equals',row)
        .filter(ee.Filter.calendarRange(y1,y2,'year'))
        .filter(ee.Filter.calendarRange(m1,m2,'month'))
        .filterMetadata('WRS_ROW','not_less_than',r1).filterMetadata('WRS_ROW','not_greater_than',r2)
        .filterMetadata('WRS_PATH','not_greater_than',p1).filterMetadata('WRS_PATH','not_less_than',p2)
        //.filterMetadata('WRS_ROW','less_than',234)
        .map(bandPrep.prep4bands);

var l5 = l5t1.merge(l5t2)
        //.filterMetadata('CLOUD_COVER','less_than',cc)
        .filterMetadata('DATA_TYPE','equals','L1TP')
        .filterMetadata('GEOMETRIC_RMSE_MODEL','not_greater_than',rmse)
        .filterBounds(geo)
        //.filterDate(d1,d2)
        //.filterMetadata('WRS_PATH','equals',path)
        //.filterMetadata('WRS_ROW','equals',row)
        .filter(ee.Filter.calendarRange(y1,y2,'year'))
        .filter(ee.Filter.calendarRange(m1,m2,'month'))
        .filterMetadata('WRS_ROW','not_less_than',r1).filterMetadata('WRS_ROW','not_greater_than',r2)
        .filterMetadata('WRS_PATH','not_greater_than',p1).filterMetadata('WRS_PATH','not_less_than',p2)
        //.filterMetadata('WRS_ROW','less_than',234)
        .map(bandPrep.prep5bands);

var l7 = l7t1.merge(l7t2)
        //.filterMetadata('CLOUD_COVER','less_than',cc)
        .filterMetadata('DATA_TYPE','equals','L1TP')
        .filterMetadata('GEOMETRIC_RMSE_MODEL','not_greater_than',rmse)
        .filterBounds(geo)
        //.filterDate(d1,d2)
        //.filterMetadata('WRS_PATH','equals',path)
        //.filterMetadata('WRS_ROW','equals',row)
        .filter(ee.Filter.calendarRange(y1,y2,'year'))
        .filter(ee.Filter.calendarRange(m1,m2,'month'))
        .filterMetadata('WRS_ROW','not_less_than',r1).filterMetadata('WRS_ROW','not_greater_than',r2)
        .filterMetadata('WRS_PATH','not_greater_than',p1).filterMetadata('WRS_PATH','not_less_than',p2)
        //.filterMetadata('WRS_ROW','less_than',234)
        .map(bandPrep.prep7bands);

var l8 = l8t1.merge(l8t2)
        .filterMetadata('NADIR_OFFNADIR','equals','NADIR')
        .filterMetadata('TIRS_SSM_MODEL','not_equals','PRELIMINARY')
        //.filterMetadata('CLOUD_COVER','less_than',cc)
        .filterMetadata('DATA_TYPE','equals','L1TP')
        .filterMetadata('GEOMETRIC_RMSE_MODEL','not_greater_than',rmse)
        .filterBounds(geo)
        //.filterDate(d1,d2)
        //.filterMetadata('WRS_PATH','equals',path)
        //.filterMetadata('WRS_ROW','equals',row)
        .filter(ee.Filter.calendarRange(y1,y2,'year'))
        .filter(ee.Filter.calendarRange(m1,m2,'month'))
        .filterMetadata('WRS_ROW','not_less_than',r1).filterMetadata('WRS_ROW','not_greater_than',r2)
        .filterMetadata('WRS_PATH','not_greater_than',p1).filterMetadata('WRS_PATH','not_less_than',p2)
        //.filterMetadata('WRS_ROW','less_than',234)
        .map(bandPrep.prep8bands);

////////////////////////////////////
//Make one large stacks of landsat data
/////////////////////////////////////
var landsat = ee.ImageCollection((l4).merge(l5).merge(l7).merge(l8)).filterMetadata('sza','less_than',77).sort('system:time_start');
var first = ee.Image(landsat.first());
print("landsat images analyzed:",landsat.size());
//print("first landsat image",landsat.first());


//////////////////////////////////////////////////////////////////////////////////
/*ATMOSPHERIC CORRECTION & WATER SKIN TEMP OF LANDSAT*/
//////////////////////////////////////////////////////////////////////////////////

// This connects the vapor image to the landsat image so it can be pulled during the 'atmosCorr' function
// It uses time to find the closest, and it also uses geometry to find one that intersects
var joinedVapor = ee.Join.saveBest({
  matchKey: 'vapor',
  // ordering: 'system:time_start',
  measureKey: 'difference'
}).apply({
  primary: landsat,
  secondary: vapor,
  condition: ee.Filter.and(
    ee.Filter.maxDifference({
      difference: 1000 * 60 * 60 * 6, //ms -> sec -> min -> hrs
      leftField: 'system:time_start',
      rightField: 'system:time_start',
    }),
    ee.Filter.intersects({
      leftField: '.geo',
      rightField: '.geo',
    })
  )
});

// Apply atmospheric correction
var temps = joinedVapor.map(skintemp.atmosCorr).filterMetadata('sza','less_than',77); // check solar zenith angle

var countedPixels = temps.map(function(img){
  img = ee.Image(img);
  var getCount = img.reduceRegion({
    reducer: ee.Reducer.count(),
    geometry: geo,
    scale: 30,
    maxPixels:5e9
  });
  var count = ee.Dictionary(getCount).get('surface_temp');
  return img.set('pixel_count',count);
}).filterMetadata('pixel_count','not_less_than',pixel_min);
print('counted pixels count',countedPixels.first());


var fhisto = ee.Image(countedPixels.first())//.filterMetadata('pixel_count','not_less_than',pixel_min)
    .reduceRegion({
        reducer: ee.Reducer.fixedHistogram(10,20,20),
        geometry: geo,
        scale: 30,
        maxPixels: 5e9
      });
      



//////////////////////////////////////////////////////////////////////////////////
// GET STATS //
//////////////////////////////////////////////////////////////////////////////////

var temp_stats = countedPixels.map(skintemp.exportStatistics);
var zonal_stats = countedPixels.map(skintemp.exportZonalStatistics).flatten();
var updateimgs = countedPixels.map(skintemp.attachStatistics);
print('remaining landsat:',updateimgs.size());
// var printdates = updateimgs.toList(100).map(function(x) {return ee.Image(x).date()});

print('zonal stats', zonal_stats.limit(100));

exports.imgCol = updateimgs;

//////////////////////////////////////////////////////////////////////////////////
// EXPORT STUFF //
//////////////////////////////////////////////////////////////////////////////////

Export.table.toDrive({
  collection: temp_stats,
  description: "temp_stats_sunapee_1985_2020_05-11",
  fileFormat: "CSV",
  folder: "GEE_IDS_Lutz"
});

Export.table.toDrive({
  collection: zonal_stats,
  description: "temp_zonal_stats_sunapee_2018_06_16",
  fileFormat: "CSV",
  folder: "herrick_etal_temp"
});

// var utmzone = ee.Algorithms.String(updateimgs.first().get("UTM_ZONE"));
// var crs_string = ee.Algorithms.String("EPSG:326").cat(utmzone).replace(".0","");
// print(crs_string);


var utm18n = "EPSG:32618";

// function appendBand(current, previous){
//   current = ee.Image(current);

//   // Build a name for the band
//   var bandName1 = ee.Algorithms.String(current.bandNames().get(0)).cat('_').cat(current.get('DATE_ACQUIRED'));
//   var bandName2 = ee.Algorithms.String(current.bandNames().get(1)).cat('_').cat(current.get('DATE_ACQUIRED'));
//   // Rename the band
//   current = current.select([0,1],[bandName1,bandName2]);
//   // Append it to the result (Note: only return current item on first element/iteration)
//   var accum = ee.Algorithms.If(ee.Algorithms.IsEqual(previous,null), current, current.addBands(ee.Image(previous)));
//   // Return the accumulation
//   return accum;
// }
// var renamed = ee.Image(updateimgs.iterate(appendBand));
// print(renamed);

// Export.image.toDrive({
//   image: renamed,
//   description: 'temp_zscore_alldates',
//   folder: 'GEE_IDS_Lutz',
//   region: geo,
//   maxPixels: 5e9,
//   crs: crs_string,
//   scale: 30
// });


// Export.image.toAsset({
//   image: updateimgs, 
//   //description: "asset: temp_zscore_alldates", 
//   region: geo, 
//   scale:30, 
//   crs:utm18n, 
//   maxPixels: 5e9
// });

// Export.image.toDrive({
//   image:jun18, 
//   description: 'temps_sunapee_2016jun18', 
//   folder: "GEE_IDS_Lutz", 
//   region: geo,
//   maxPixels: 5e9,
//   crs: utm18n,
//   scale: 30
// });

// Export.image.toDrive({
//   image:jul12, 
//   description: 'temps_sunapee_2016jul12', 
//   folder: "GEE_IDS_Lutz", 
//   region: geo,
//   maxPixels: 5e9,
//   crs: utm18n,
//   scale: 30
// });

// Export.image.toDrive({
//   image:sep22, 
//   description: 'temps_sunapee_2016sep22', 
//   folder: "GEE_IDS_Lutz", 
//   region: geo,
//   maxPixels: 5e9,
//   crs: utm18n,
//   scale: 30
// });


