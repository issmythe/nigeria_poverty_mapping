import pandas as pd
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler


def fill_missing(df, factor_cols, continuous_cols):
    if factor_cols is not None:
        for col in factor_cols:
            df[col] = df[col].astype('category').cat.add_categories('MISSING')
        # Give missing factor variables their own category
        df[factor_cols] = df[factor_cols].fillna('MISSING')
    if continuous_cols is not None:
        # For continuous cols, fill na with column mean
        df[continuous_cols] = df[continuous_cols].fillna(df[continuous_cols].mean())
    return df


def __factorize(df, factor_cols):
    to_factorize = set(df.columns).intersection(set(factor_cols))
    for col in to_factorize:
        dummies = pd.get_dummies(df[col], prefix=col)
        df = pd.concat([df, dummies], axis=1)
    return df.drop(to_factorize, axis=1)


def __get_first_component(df, verbose=False):
    transformed_data = StandardScaler().fit_transform(df)
    pca = PCA(n_components=1)
    pca.fit_transform(transformed_data)
    if verbose:
        print(pca.explained_variance_ratio_)
    loadings = pd.DataFrame(pca.components_.tolist()[0], columns=['loadings'])\
                 .set_index(df.columns)
    return loadings, [pca.components_.dot(row)[0]
                      for row in transformed_data]


def run_pca(df, factor_cols):
    df = __factorize(df, factor_cols)
    return __get_first_component(df)
