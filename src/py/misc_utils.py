import geopandas as gpd
import pandas as pd

pd.options.mode.chained_assignment = None

"""
Used to project whatever data is in "tiles" (probably rwi or consumption
predictions) onto the spatial units in "region". The contribution of the
tile-level estimate to the region-level average is weighted by the
population of the tile and the fraction of the tile that overlaps with
the region.
"""
def tile_weighted_avg(region, tiles, region_col, col_to_weight):
    def get_intersection_area(df):
            return df['geometry'].intersection(df['geometry_geom']).area

    overlaps = gpd.sjoin(region, tiles, how='right', op='intersects')\
                  .reset_index()\
                  .rename({'index': 'tile_index'}, axis=1)
    overlaps = overlaps.merge(region[['geometry', region_col]]\
                              .rename({'geometry': 'geometry_' + region_col}, axis=1),
                              on=region_col)
    edge_tiles = overlaps[overlaps.duplicated(subset=['tile_index'], keep=False)]
    center_tiles = overlaps.drop_duplicates(subset=['tile_index'], keep=False)

    edge_tiles['overlap_frac'] = edge_tiles.apply(lambda x: (x['geometry'].intersection(
                                                  x['geometry_' + region_col]).area /
                                                  x['geometry'].area),
                                                  axis=1)
    center_tiles['overlap_frac'] = 1
    overlaps = pd.concat([edge_tiles, center_tiles])

    overlaps['weighted_col'] = overlaps[col_to_weight] * overlaps['overlap_frac'] \
                               * overlaps['pop']
    overlaps['weighted_pop'] = overlaps['overlap_frac'] * overlaps['pop']

    agg_df = overlaps.groupby(region_col)[['weighted_col', 'weighted_pop']]\
                     .sum()\
                     .reset_index()
    agg_df[col_to_weight] = agg_df['weighted_col'] / agg_df['weighted_pop']

    return agg_df.merge(region[['geometry', region_col]], on=region_col)

"""
Aggregate survey predictions over administrative boundaries given in
geom_df.
"""
def survey_agg(survey_df, geom_df, region_col, rwi_col, weighted=True):
    subset_cols = [region_col, rwi_col]
    if weighted:
        subset_cols += ['weight']
    survey_df = survey_df[subset_cols]
    survey_df['count'] = 1
    survey_agg = survey_df.groupby(region_col).agg('sum').reset_index()
    survey_agg['survey_rwi'] = survey_agg[rwi_col]
    if weighted:
        survey_agg['survey_rwi'] /= survey_agg['weight']
    else:
        survey_agg['survey_rwi'] /= survey_agg['count']
    return geom_df.merge(survey_agg, on=region_col)


def standardize(col):
    return (col - col.mean()) / col.std()


def unpack_state(x):
    for state in subset_states:
        if state in x:
            return state.title()
