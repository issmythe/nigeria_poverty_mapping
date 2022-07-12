import numpy as np
from shapely.geometry import Polygon


TILE_LEVEL = 14
MinLatitude = -85.05112878
MaxLatitude = 85.05112878  
MinLongitude = -180
MaxLongitude = 180  


def __bing_y_to_latitude(bing_y, zoom_factor):
    k = np.exp((((bing_y - 0.5 / 256) / zoom_factor) - 0.5) * 4 * np.pi)
    latitude = np.arcsin((k - 1) / (k + 1))
    latitude_degrees = latitude * 180. / np.pi
    return -latitude_degrees


def bing_tile_to_bounding_box(bing_x, bing_y, level=TILE_LEVEL):
    zoom_factor = 2 ** level
    x_width = 360.0 / zoom_factor
    y1 = __bing_y_to_latitude(bing_y + 1, zoom_factor)
    x1 = x_width * bing_x - 180
    y2 = __bing_y_to_latitude(bing_y, zoom_factor)
    x2 = x_width * (bing_x + 1) - 180
    return (y1, x1, y2, x2)


def __bing_tile_to_polygon(bing_x, bing_y, level=TILE_LEVEL):
    bb = bing_tile_to_bounding_box(bing_x, bing_y, level=TILE_LEVEL)
    corners = [[bb[1], bb[0]], 
               [bb[3], bb[0]], 
               [bb[3], bb[2]],
               [bb[1], bb[2]]]
    return Polygon(corners)


def __clip(val, min_val, max_val):
    return np.maximum(min_val, np.minimum(val, max_val))


def __lon_to_x(longitude, level=TILE_LEVEL):
    zoom_factor = 2 ** level
    longitude = __clip(longitude, MinLongitude, MaxLongitude)
    x = ((longitude + 180.) / 360.) * zoom_factor
    return int(__clip(np.floor(x), 0, zoom_factor - 1))


def __lat_to_y(latitude, level=TILE_LEVEL):
    zoom_factor = 2 ** level
    latitude = __clip(latitude, MinLatitude, MaxLatitude)
    sin_latitude = np.sin(latitude * np.pi / 180.)
    y = (0.5 - np.log((1 + sin_latitude) / (1 - sin_latitude))\
        / (4 * np.pi)) * zoom_factor
    return int(__clip(np.floor(y), 0, zoom_factor - 1))


def bing_tile_to_polygon(df):
    return __bing_tile_to_polygon(df['x'], df['y'])
