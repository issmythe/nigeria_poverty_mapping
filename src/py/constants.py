RWI_UNWEIGHTED = ['survey_rwi_subset', 'survey_rwi_full']
RWI_WEIGHTED = ['survey_rwi_weighted_subset', 'survey_rwi_weighted_full']
RWI_COLS = RWI_UNWEIGHTED + RWI_WEIGHTED

DEFAULT_CRS = 'EPSG:4326'
POVERTY_LINE = 247679

PCA_DROP_COLS = ['hhid', 'interview__key', 'sector', 'zone', 'state',
                 'lga', 'ward', 'ea', 'cluster', 'month_of_interview',
                 'year_of_interview', 'hh_gps_latitude', 'hh_gps_longitude',
                 'hh_gps_accuracy', 'hh_gps_altitude', 'popw', 'weight',
                 'totcons_pc', 'totcons_adj', 'pl', 'geometry']
PCA_FACTOR_COLS = ['educat7_hhh', 'educat5_hhh', 'educat4_hhh',
                   'male_hhh', 'primaryjobstatus_hhh',
                   'primaryjobsector14_hhh', 'pjobsinglesector14_hhh',
                   'primaryjobsector3_hhh', 'pjobsinglesector3_hhh',
                   'foodsec_skipped', 'foodsec_hungry', 'sanitation_type',
                   'main_water', 'electricity', 'improved_water',
                   'improved_sanitation', 'rooftype', 'wallstype',
                   'floortype', 'slum_ea', 'slum_hh', 'refusetype',
                   'cookstovetype']
SUBSET_PCA_COLS = ['electricity', 'numasset_smartphone',
                   'numasset_regmobilephone', 'numasset_car',
                   'numasset_motorbike', 'numasset_fridge', 'numasset_tv',
                   'numasset_radio', 'main_water', 'improved_water',
                   'cookstovetype', 'refusetype', 'sanitation_type',
                   'floortype', 'wallstype', 'rooftype', 'numsleepingrooms']

SUBSET_PCA_COLS_NO_TRASH = ['electricity', 'numasset_smartphone',
                            'numasset_regmobilephone', 'numasset_car',
                            'numasset_motorbike', 'numasset_fridge', 'numasset_tv',
                            'numasset_radio', 'main_water', 'improved_water',
                            'cookstovetype', 'refusetype',  'floortype', 'wallstype',
                            'rooftype', 'numsleepingrooms']

PALETTE = ['#443983', '#31688e', '#21918c', '#35b779', '#90d743']