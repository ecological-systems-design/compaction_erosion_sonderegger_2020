from osgeo import gdal, osr, gdal_array
import numpy as np
from scipy import mgrid
import matplotlib.pyplot as plt


class MyRaster():
    
    """
    The MyRaster object has the raster properties and some functions attached to it.
    """
    
    def __init__(self, raster_file):
        
        """
        The init function attaches raster properties to the MyRaster object.
        It is based on the gdal-package.
        """
        
        self.raster = gdal.Open(raster_file)
        raster = self.raster

        self.driver = raster.GetDriver().ShortName
        self.projection = raster.GetProjection()
        self.bands = raster.RasterCount
        
        self.xSize = raster.RasterXSize
        self.ySize = raster.RasterYSize

        geotransform = raster.GetGeoTransform()
        self.geotransform = geotransform

        if not geotransform is None:
            self.xMin = geotransform[0]
            self.xMax = geotransform[0] + raster.RasterXSize*geotransform[1]
            if geotransform[5] < 0:
                self.yMin = geotransform[3] + raster.RasterYSize*geotransform[5]
                self.yMax = geotransform[3]
            else:
                self.yMin = geotransform[3]
                self.yMax = geotransform[3] + raster.RasterYSize*geotransform[5]

            self.xOrigin = geotransform[0]
            self.yOrigin = geotransform[3]
            self.pixelWidth = geotransform[1]
            self.pixelHeight = geotransform[5]
        self.NoDataValue = raster.GetRasterBand(1).GetNoDataValue()

        
    def show_raster_properties(self):
    
        """
        This function shows the properties of a raster data set.
        It is based on the gdal-package.
        """

        print('Driver: ', self.driver)
        print('Projection: ', self.projection)
        print(100*'-')
        print('Size: ', self.xSize, 'x', self.ySize, 'x', self.bands)
        print('X min = ', self.xMin)
        print('X max = ', self.xMax)
        print('Y min = ', self.yMin)
        print('Y max = ', self.yMax)
        print('Origin = (', self.xOrigin, ',', self.yOrigin,')')
        print('Pixel Size = (', self.pixelWidth, ',', self.pixelHeight,')')
        print('No Data Value = ', self.NoDataValue)
        
        
    def get_array(self):
        
        """
        This function reads the raster into a numpy array.
        It makes sure the top left of the map corresponds to the top left of the array.
        """
        
        array = self.raster.ReadAsArray()
        if self.pixelHeight > 0:
            array = np.flipud(array)
        return array
    
    
    def get_bounded_array(self, bbox_xMinDeg, bbox_yMinDeg, bbox_xMaxDeg, bbox_yMaxDeg):

        """
        This function reads the raster into a numpy array according to boundaries set by the user.
        It makes sure the top left of the map corresponds to the top left of the array.
        """
        
        xDegrees = self.xMax - self.xMin
        yDegrees = self.yMax - self.yMin

        x_pix_p_deg = self.xSize / xDegrees
        y_pix_p_deg = self.ySize / yDegrees

        bbox_xSizePix = int((bbox_xMaxDeg - bbox_xMinDeg) * x_pix_p_deg)
        bbox_ySizePix = int((bbox_yMaxDeg - bbox_yMinDeg) * y_pix_p_deg)
        bbox_xMinPix = int((bbox_xMinDeg - self.xMin) * x_pix_p_deg)
        if self.pixelHeight > 0:
            bbox_yMinPix = int((bbox_yMinDeg - self.yMin) * y_pix_p_deg)
        else:
            bbox_yMinPix = self.ySize - bbox_ySizePix - int((bbox_yMinDeg - self.yMin) * y_pix_p_deg) 

        array = self.raster.ReadAsArray(bbox_xMinPix, bbox_yMinPix, bbox_xSizePix, bbox_ySizePix)
        if self.pixelHeight > 0:
            array = np.flipud(array)

        return array
    
    
    def get_masked_array(self):
        
        """
        This function reads the raster into a masked numpy array.
        It makes sure the top left of the map corresponds to the top left of the array.
        """
        
        array = self.get_array()
        
        mask=np.zeros(np.shape(array))
        mask[array == self.NoDataValue] = 1
        masked_array = np.ma.masked_array(array, mask)
        
        return masked_array
            
            
    def plot_raster(self):
        
        """
        This function plots the masked array of the raster using matplotlib.
        """
        
        array = self.get_masked_array()
        
        fig, ax = plt.subplots()

        ax.imshow(array, extent=[self.xMin, self.xMax, self.yMin, self.yMax])

        
def mask_array(array, NoDataValue):
    
    """
    This function masks a numpy array.
    """
        
    mask=np.zeros(np.shape(array))
    mask[array == NoDataValue] = 1
    masked_array = np.ma.masked_array(array, mask)

    return masked_array
            

        
def raster_average(raster_list):
    
    """
    This function calculates average values for several rasters in a list.
    Returns an array.
    """

    array_list = []
    mask=np.zeros(np.shape(raster_list[0].raster.ReadAsArray()))
    no_data_value = raster_list[0].NoDataValue
        
    for i in range(0, len(raster_list)):
        array = raster_list[i].raster.ReadAsArray()
        if raster_list[i].pixelHeight > 0:
            array = np.flipud(array)
            
        array_list.append(array)

        mask[array == raster_list[i].NoDataValue] = 1
        masked_array = np.ma.masked_array(array, mask)
        
    mean = np.mean(array_list, axis=0)
    mean[mask == 1] = no_data_value

    return mean
    
    
def raster_average_no_mask(raster_list):
    
    """
    This function calculates average values for several rasters in a list.
    Returns an array
    """
        
    array_list = []
        
    for i in range(0, len(raster_list)):
        array = raster_list[i].raster.ReadAsArray()
        if raster_list[i].pixelHeight > 0:
            array = np.flipud(array)
                
        array_list.append(array)
                
    mean = np.mean(array_list, axis=0)

    return mean
      

def array2geotiff(array, outfile_name, no_data_value, xsize, ysize, 
                  originX, originY, pixelWidth, pixelHeight,
                  compression='LZW'):
    
    """
    This function writes a numpy array into a GeoTIFF-file.
    If compression is not set to anything else, the file is compressed with LZW.
    It is based on the gdal-package.
    """
    
    # the array has to be flipped upside down if originY is at the bottom-left (equal to pixelHeight > 0)
    if pixelHeight > 0:
        array = np.flipud(array)
    
    # create raster
    DataType = gdal_array.NumericTypeCodeToGDALTypeCode(array.dtype)
    driver = gdal.GetDriverByName('GTiff')
    
    if compression == None:
        compression = str(compression)
    compression = ['COMPRESS=' + compression]
    out_raster = driver.Create(outfile_name + '.tif', xsize, ysize, 1, DataType, options=compression)
    out_raster.SetGeoTransform((originX, pixelWidth, 0, originY, 0, pixelHeight))
    
    out_raster_SRS = osr.SpatialReference()
    out_raster_SRS.ImportFromEPSG(4326)
    out_raster.SetProjection(out_raster_SRS.ExportToWkt())
    
    out_raster.GetRasterBand(1).WriteArray(array)
    out_raster.GetRasterBand(1).SetNoDataValue(no_data_value)
    
    
def array2geotiff_rastercopy(array, outfile_name, raster,
                             compression='LZW'):
    
    """
    This function writes a numpy array into a GeoTIFF-file.
    Properties are copied from the blueprint raster provided.
    If compression is not set to anything else, the file is compressed with LZW.
    It is based on the gdal-package.
    """
    
    # create raster
    DataType = gdal_array.NumericTypeCodeToGDALTypeCode(array.dtype)
    driver = gdal.GetDriverByName('GTiff')
    
    if compression == None:
        compression = str(compression)
    compression = ['COMPRESS=' + compression]
    out_raster = driver.Create(outfile_name + '.tif', raster.RasterXSize, raster.RasterYSize, 1, DataType, options=compression)
    out_raster.SetGeoTransform(raster.GetGeoTransform())

    out_raster_SRS = osr.SpatialReference()
    out_raster_SRS.ImportFromEPSG(4326)
    out_raster.SetProjection(out_raster_SRS.ExportToWkt())
    
    out_raster.GetRasterBand(1).WriteArray(array)
    out_raster.GetRasterBand(1).SetNoDataValue(raster.GetRasterBand(1).GetNoDataValue())
     

def cut_array_yboundaries(array, y_min_old, y_max_old, y_min_new, y_max_new, pixelHeight):

    # calculate rows and columns of array to cut:
    cut_top = round(abs(round(y_max_old - y_max_new)/pixelHeight))
    cut_bottom = round(abs(round(y_min_old - y_min_new)/pixelHeight))
    
    array_cut = array[cut_top:array.shape[0]-cut_bottom]
    
    return array_cut


def cut_raster_yboundaries(raster, outfile_name, y_min_new, y_max_new):
    
    """
    This function cuts a raster-file to y_min_new and y_max_new values and writes it into GeoTIFF-file.
    It is based on the gdal-package.
    For (in)rasters with the origin at y_min, the raster is flipped and the origin set to y_max.
    """
    
    xSize_old = raster.RasterXSize
    ySize_old = raster.RasterYSize
    geotransform = raster.GetGeoTransform()
    x_min_old = geotransform[0]
    pixelWidth = geotransform[1]
    pixelHeight = geotransform[5]
    
    #x_max_old = x_min_old + xSize_old*pixelWidth
    if pixelHeight < 0:
        y_min_old = geotransform[3] + ySize_old*pixelHeight
        y_max_old = geotransform[3]
    else:
        y_max_old = geotransform[3] + ySize_old*pixelHeight
        y_min_old = geotransform[3]

    # calculate rows and columns of raster array to cut:
    #cut_left = abs(x_max_old - x_max)/geotransform[3]
    #cut_right = abs(x_min_old - x_min)/geotransform[3]
    cut_top = round(abs(round(y_max_old - y_max_new)/pixelHeight))
    cut_bottom = round(abs(round(y_min_old - y_min_new)/pixelHeight))
 
    array = raster.ReadAsArray()
    # the array has to be flipped upside down if originY is at the bottom-left (equal to pixelHeight > 0)
    if pixelHeight > 0:
        array = np.flipud(array)
    array_cut = array[cut_top:array.shape[0]-cut_bottom].copy()
    array = None
    
    ySize_new = ySize_old-cut_top-cut_bottom
    if pixelHeight > 0:
        pixelHeight = -pixelHeight
    
    no_data_value = raster.GetRasterBand(1).GetNoDataValue()
    
    DataType = gdal_array.NumericTypeCodeToGDALTypeCode(array_cut.dtype)
    driver = gdal.GetDriverByName('GTiff')
    out_raster_SRS = osr.SpatialReference()
    out_raster_SRS.ImportFromEPSG(4326)
    
    out_raster = driver.Create(outfile_name + '.tif', xSize_old, ySize_new, 1, DataType)
    out_raster.SetGeoTransform((x_min_old, pixelWidth, 0, y_max_new, 0, pixelHeight))
    out_raster.SetProjection(out_raster_SRS.ExportToWkt())
    out_raster.GetRasterBand(1).WriteArray(array_cut)
    out_raster.GetRasterBand(1).SetNoDataValue(no_data_value)
    
    return out_raster

    array_cut = None

    
def resample_array_to_higher_resolution(array, resample_factor):
    
    """
    This function resamples the array to a higher resolution.
    """
    
    x_tiles = int(array.shape[1] * resample_factor)
    y_tiles = int(array.shape[0] * resample_factor)
    newshape = (y_tiles, x_tiles)
    
    assert len(array.shape) == len(newshape)
    
    # create two arrays with the size of the new array, one filled with
    # the y, the other with the x coordinates of the original array
    slices = [slice(0,old, float(old)/new) for old,new in zip(array.shape,newshape)]
    coordinates = mgrid[slices]
    indices = coordinates.astype('i')   #choose the biggest smaller integer index
    
    # create new array that takes the values from the old 
    # array according to the coordinates given in tuple(indices)
    new_array = array[tuple(indices)]
    
    return new_array


def resample_array_to_lower_resolution(array, resample_factor, NoDataValue, NoData_average=True):
    
    """
    This function resamples the array to a lower resolution.
    For tiles with NoDataValues, either the average is calculated
    (NoData_average=True which is the default option) or the whole
    tile is set to NoDataValue (set NoData_average=False).
    """
    
    # number of tiles to cut old array into = shape of new array
    x_tiles = int(array.shape[1] * resample_factor)
    y_tiles = int(array.shape[0] * resample_factor)
    newshape = (y_tiles, x_tiles)
    
    # tile size in old array
    x_size = int(array.shape[1] / x_tiles)
    y_size = int(array.shape[0] / y_tiles)

    # offset for each tile in old array
    x_offs = [0]
    y_offs = [0]
    for i in range(1, x_tiles):
        x_offs.append(int(i*x_size))
    for i in range(1, y_tiles):
        y_offs.append(int(i*y_size))
    
    # create empty new array
    new_array = np.full(newshape, NoDataValue)
    
    # create help tile to check whether tile does have data values
    #no_data = np.full((y_size, x_size), NoDataValue)
    
    # calculate average of old grid cells for new grid cell
    for j in range(0, y_tiles):
        for i in range(0, x_tiles):
            
            # get tile of old grid
            tile = array[y_offs[j]:(y_offs[j] + y_size), x_offs[i]:(x_offs[i] + x_size)]
            
            # calculate average
            if np.all(tile == NoDataValue):
                value = NoDataValue
            elif np.any(tile == NoDataValue):
                if NoData_average == True:
                    mask = tile == NoDataValue
                    tile_ma = np.ma.masked_array(tile, mask)
                    value = tile_ma.mean()
                else:
                    value = NoDataValue
            else:
                mask = tile == NoDataValue
                tile_ma = np.ma.masked_array(tile, mask)
                value = tile_ma.mean()
                    

            new_array[j][i] = value
            
    new_array[np.isnan(new_array) == True] = NoDataValue
    
    return new_array

