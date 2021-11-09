/////////////////////////////////////////
//required to prep Landsat for surface temp

var k = ee.Kernel.square({
    radius: 60,
    units: 'meters'
  });
  
  var k8 = ee.Kernel.square({
    radius: 200,
    units: 'meters'
  });
  
  //https://unh.box.com/v/landsat4-7-qavalues
  var qa_values = ee.List([672,676,680,684,704,708,712,716,928,932,936,940,960,964,968,972]); //KEEP these pixel values
                          //1696,1700,1704,1708,1728,1732,1736,1740]); <-- these are snow/ice
  
  //https://unh.box.com/v/landsat8-qavalues
  var qa_val_8 = ee.List([2,2720,2722,2724,2728,2732,2752,2756,2760,2764,2976,2980,2984,2988,3008,3012,3016,3020]); //KEEP these pixel values
                          //,3744,3748,3752,3756,3776,3780,3784,3788]); <-- these are snow/ice
  
  // var coeff4 = [0.06240,0.00373,1.02425,-0.52383,-1.19361,0.12908,-0.00960,1.33393,-0.25891]; //TIGR1761 
  // var coeff5 = [0.07518,-0.00492,1.03189,-0.59600,-1.22554,0.08104,-0.02767,1.43740,-0.25844]; //TIGR1761
  // var coeff7 = [0.06518,0.00683,1.02717,-0.53003,-1.25866,0.10490,-0.01965,1.36947,-0.24310]; //TIGR1761
  
  var coeff4 = [0.06674,-0.03447,1.04483,-0.50095,-1.15652,0.09812,-0.04732,1.50453,-0.34405]; //TIGR2311
  var coeff5 = [0.08158,-0.05707,1.05991,-0.58853,-1.08536,-0.00448,-0.06201,1.59086,-0.33513]; //TIGR2311
  var coeff7 = [0.06982,-0.03366,1.04896,-0.51041,-1.20026,0.10490,-0.05457,1.52631,-0.32136]; //TIGR2311
  var coeff8 = [0.04019,0.02916,1.01523,-0.38333,-1.50294,0.20324,0.00918,1.36072,-0.27514]; //GAPRI4838
  
  exports.prep4bands = function(img) {
    var systime = img.get('system:time_start');
    var elev = img.get('SUN_ELEVATION');
    var sza = ee.Number(90).subtract(elev);
    var uid = img.get('system:index');
    // var id = img.id().getInfo();//.split('1_');
    // var uid = id;
    
    var radiance = ee.Algorithms.Landsat.calibratedRadiance(img).select(['B6'],['radi']);
    var toa = ee.Algorithms.Landsat.TOA(img);
    toa = toa.select(['B[1-6]'],['blue','green','red','nir','swir','temp']);
    
    var red = toa.select(['B3'],['red']);
    var nir = toa.select(['B4'],['nir']);
    var ndvi = toa.normalizedDifference(['nir','red']).select(['nd'],['ndvi']);
    var ndsi = toa.normalizedDifference(['green','swir']).select(['nd'],['ndsi']);
    
    var bt = toa.select(['temp'],['bt']).subtract(273.15);
    
    var coeff = ee.Image(coeff4).select([0,1,2,3,4,5,6,7,8],['c11','c12','c13','c21','c22','c23','c31','c32','c33']);
    var Bg = ee.Image.constant(1290).select([0],['Bg']);
            
    //var cs = ee.Algorithms.Landsat.simpleCloudScore(toa).select(['cloud']).lt(cloudthresh);
  
    // use the QA band to mask cloudy pixels  
    var p_qa = img.select(["BQA"]);
    var p_mask = p_qa.remap(qa_values,qa_values).mask().int8();//.updateMask(ndsi.lt(0.05)).int8();
    // use entropy on the mask to erode the masked areas by 60m/100m (1 more pixel)
    var ent = p_mask.entropy(k).multiply(10).uint8();
    var ent2 = ent.remap([0],[0]).mask();
    var entMask = p_mask.updateMask(ent2);
    
    var both = radiance.addBands(coeff).addBands(Bg).addBands(bt).addBands(ndvi).addBands(ndsi)
                //.mask(cs)
                .updateMask(p_mask)
                .copyProperties(img)
                .set('sza',sza)
                .set('uid',uid)
                .set('system:time_start',systime);
    
    return ee.Image(both);
  };
  
  exports.prep5bands = function(img){
    var systime = img.get('system:time_start');
    var elev = img.get('SUN_ELEVATION');
    var sza = ee.Number(90).subtract(elev);
    var uid = img.get('system:index');
    // var id = img.id().getInfo();//.split('1_');
    // var uid = id;
    
    var radiance = ee.Algorithms.Landsat.calibratedRadiance(img).select(['B6'],['radi']);
    var toa = ee.Algorithms.Landsat.TOA(img);
    toa = toa.select(['B[1-6]'],['blue','green','red','nir','swir','temp']);
    
    var red = toa.select(['B3'],['red']);
    var nir = toa.select(['B4'],['nir']);
    var ndvi = toa.normalizedDifference(['nir','red']).select(['nd'],['ndvi']);
    var ndsi = toa.normalizedDifference(['green','swir']).select(['nd'],['ndsi']);
    
    var bt = toa.select(['temp'],['bt']).subtract(273.15);
    
    var coeff = ee.Image(coeff5).select([0,1,2,3,4,5,6,7,8],['c11','c12','c13','c21','c22','c23','c31','c32','c33']);
    var Bg = ee.Image.constant(1256).select([0],['Bg']);
            
    //var cs = ee.Algorithms.Landsat.simpleCloudScore(toa).select(['cloud']).lt(cloudthresh);
  
    // use the QA band to mask cloudy pixels  
    var p_qa = img.select(["BQA"]);
    var p_mask = p_qa.remap(qa_values,qa_values).mask().int8();//.updateMask(ndsi.lt(0.05)).int8();
    // use entropy on the mask to erode the masked areas by 60m/100m (1 more pixel)
    var ent = p_mask.entropy(k).multiply(10).uint8();
    var ent2 = ent.remap([0],[0]).mask();
    var entMask = p_mask.updateMask(ent2);
    
    var both = radiance.addBands(coeff).addBands(Bg).addBands(bt).addBands(ndvi).addBands(ndsi)
                //.mask(cs)
                .updateMask(p_mask)
                .copyProperties(img)
                .set('sza',sza)
                .set('uid',uid)
                .set('system:time_start',systime);
    
    return ee.Image(both);
  };
  
  exports.prep7bands = function(img){
    var systime = img.get('system:time_start');
    var elev = img.get('SUN_ELEVATION');
    var sza = ee.Number(90).subtract(elev);
    var uid = img.get('system:index');
    // var id = img.id().getInfo();//.split('1_');
    // var uid = id;
    
    var radiance = ee.Algorithms.Landsat.calibratedRadiance(img).select(['B6_VCID_1'],['radi']);
    var toa = ee.Algorithms.Landsat.TOA(img);
    toa = toa.select(['B[1-5]','B6_VCID_1'],['blue','green','red','nir','swir','temp']);
    
    var red = toa.select(['B3'],['red']);
    var nir = toa.select(['B4'],['nir']);
    var ndvi = toa.normalizedDifference(['nir','red']).select(['nd'],['ndvi']);
    var ndsi = toa.normalizedDifference(['green','swir']).select(['nd'],['ndsi']);
    
    var bt = toa.select(['temp'],['bt']).subtract(273.15);
    
    var coeff = ee.Image(coeff7)
                  .select([0,1,2,3,4,5,6,7,8],['c11','c12','c13','c21','c22','c23','c31','c32','c33']);
    var Bg = ee.Image.constant(1277).select([0],['Bg']);
    
    //var cs = ee.Algorithms.Landsat.simpleCloudScore(toa).select(['cloud']).lt(cloudthresh);
    
    // use the QA band to mask cloudy pixels  
    var p_qa = img.select(["BQA"]);
    var p_mask = p_qa.remap(qa_values,qa_values).mask().int8();//.updateMask(ndsi.lt(0.05)).int8();
    // use entropy on the mask to erode the masked areas by 60m/100m (1 more pixel)
    var ent = p_mask.entropy(k).multiply(10).uint8();
    var ent2 = ent.remap([0],[0]).mask();
    var entMask = p_mask.updateMask(ent2);
    
    var both = radiance.addBands(coeff).addBands(Bg).addBands(bt).addBands(ndvi).addBands(ndsi)
                //.mask(cs)
                .updateMask(p_mask)
                .copyProperties(img)
                .set('sza',sza)
                .set('uid',uid)
                .set('system:time_start',systime);
    
    return ee.Image(both);
  };
  
  exports.prep8bands = function(img){
    var systime = img.get('system:time_start');
    var elev = img.get('SUN_ELEVATION');
    var sza = ee.Number(90).subtract(elev);
    var uid = img.get('system:index');
    // var id = img.id().getInfo();//.split('1_');
    // var uid = id;
    
    var radiance = ee.Algorithms.Landsat.calibratedRadiance(img).select(['B10'],['radi']);
    var toa = ee.Algorithms.Landsat.TOA(img);
    toa = toa.select(['B[2-6]','B10'],['blue','green','red','nir','swir','temp']);
    
    var red = toa.select(['B4'],['red']);
    var nir = toa.select(['B5'],['nir']);
    var ndvi = toa.normalizedDifference(['nir','red']).select(['nd'],['ndvi']);
    var ndsi = toa.normalizedDifference(['green','swir']).select(['nd'],['ndsi']);
    
    var bt = toa.select(['temp'],['bt']).subtract(273.15);
    
    var coeff = ee.Image(coeff8).select([0,1,2,3,4,5,6,7,8],['c11','c12','c13','c21','c22','c23','c31','c32','c33']);
    var Bg = ee.Image.constant(1324).select([0],['Bg']);
    
    //var cs = ee.Algorithms.Landsat.simpleCloudScore(toa).select(['cloud']).lt(cloudthresh);
    
    // use the QA band to mask cloudy pixels  
    var p_qa = img.select(["BQA"]);
    var p_mask = p_qa.remap(qa_val_8,qa_val_8).mask().int16();//.updateMask(ndsi.lt(0.05)).int16();
    // use entropy on the mask to erode the masked areas by 60m/100m (1 more pixel)
    var ent = p_mask.entropy(k8).multiply(10).uint16();
    var ent2 = ent.remap([0],[0]).mask();
    var entMask = p_mask.updateMask(ent2);
    
    var both = radiance.addBands(coeff).addBands(Bg).addBands(bt).addBands(ndvi).addBands(ndsi)
                //.mask(cs)
                .updateMask(p_mask)
                .copyProperties(img)
                .set('sza',sza)
                .set('uid',uid)
                .set('system:time_start',systime);
    
    return ee.Image(both);
  };