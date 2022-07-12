import random
import pandas as pd

N_PARTITIONS = 4

def get_assignments(n_row, n_partitions):
    assignments = [x for i in range(int(n_row / n_partitions))
                  for x in range(n_partitions)]
    extra = [i for i in range(n_partitions)]
    random.shuffle(extra)
    assignments += extra[: n_row % n_partitions]
    random.shuffle(assignments)
    return assignments


def get_assignments_df(df, n_partitions=N_PARTITIONS):
    df.loc[:, 'cv_group'] = get_assignments(len(df), n_partitions)
    return df


def get_cluster_assignments(df, n_partitions=N_PARTITIONS):
    clusters = df['cluster'].unique()
    assignments = [x for i in range(int(len(clusters) / n_partitions))
                  for x in range(n_partitions)]
    extra = [i for i in range(n_partitions)]
    random.shuffle(extra)
    assignments += extra[: len(clusters) % n_partitions]
    random.shuffle(assignments)
    assignment_df = pd.DataFrame({'cluster': clusters,
                                  'cv_group': assignments})
    return df.merge(assignment_df, how='left', on='cluster')['cv_group'].to_list()


def get_cluster_assignments_df(df):
    df.loc[:, 'cv_group'] = get_cluster_assignments(df)
    return df
