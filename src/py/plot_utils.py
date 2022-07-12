import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

from scipy.stats import pearsonr
from sklearn.linear_model import LinearRegression
from skmisc import loess
from statsmodels.stats.weightstats import DescrStatsW

from constants import PALETTE as pal

sns.set_style("dark", {"axes.facecolor": "0.95"})


def __standardize(col):
    return (col - col.mean()) / col.std()


def display_correlations(compare_df, survey_cols, estimate_col):
    for col in survey_cols:
        weighted_corr = DescrStatsW(compare_df[[col, estimate_col]],
                                    weights=compare_df['pop']).corrcoef
        print('Weighted correlation with', col, '=', round(weighted_corr[1][0], 3))
        unweighted_corr = pearsonr(compare_df[col], compare_df[estimate_col])
        print('Unweighted correlation with', col, '=', round(unweighted_corr[0], 3))

    return compare_df[survey_cols + [estimate_col]].corr()


def __plot_loess_helper(x, y, ax):
    l = loess.loess(x=x, y=y)
    l.fit()
    pred = l.predict(x, stderror=True)
    ci = loess.loess_confidence_intervals(pred, 0.95)
    sns.lineplot(x, pred.values, color=pal[2], ax=ax)
    sns.lineplot(x, ci.lower, color='None', ax=ax)
    sns.lineplot(x, ci.upper, color='None', ax=ax)
    plt.fill_between(ax.lines[1].get_xydata()[:,0], 
                     ax.lines[1].get_xydata()[:,1],
                     ax.lines[2].get_xydata()[:,1],
                     color=pal[2],
                     alpha=0.3)
    
    
def __plot_linreg_helper(x, y, ax):
    sns.regplot(x=x,
                y=y,
                ax=ax,
                ci=95,
                scatter=False,
                color=pal[2])

def scatterplot(x, y, pop, rho, xlabel, ylabel, title, plot_loess,
                plot_linreg=True, kwargs=None):
    if kwargs is None:
        kwargs={}
    if 'facecolors' not in kwargs:
        kwargs['facecolors'] = 'none'
    if 'edgecolor' not in kwargs:
        kwargs['edgecolor'] = 'black'
        
    if 'ax' not in kwargs:
        plt.rcParams['figure.figsize'] = (10, 10)
        fig, ax = plt.subplots()
        sns.set(font_scale=1.5)
    else:
        ax = kwargs['ax']
    hue_order = None if 'hue' not in kwargs else sorted(hue.unique())
    
    sns.scatterplot(x=x,
                    y=y,
                    size=pop,
                    sizes=(40, 1000),
                    legend='brief',
                    hue_order=hue_order,
                    **kwargs)

    if plot_loess:
        __plot_loess_helper(x, y, ax)
    elif plot_linreg:
        __plot_linreg_helper(x, y, ax)

    plt.legend(loc='lower right')
    corr_label =  r'$\rho=%.3f$' % rho
    ax.text(0.02, 0.96, corr_label, transform=ax.transAxes, fontsize=24,
            verticalalignment='top', bbox={'facecolor': 'white'})
    ax.set(ylabel=ylabel, xlabel=xlabel)
    if title:
        ax.set_title(title, fontsize=28)


# def scatterplot(x, y, pop, rho, xlabel, ylabel, title, plot_loess,
#                 plot_linreg=True, hue=None, edgecolor='black',
#                 ax=None):
#     if not ax:
#         plt.rcParams['figure.figsize'] = (10, 10)
#         fig, ax = plt.subplots()
#         sns.set(font_scale=1.5)

#     if hue is None:
#         hue_order = None
#     else:
#         hue_order = sorted(hue.unique())

#     sns.scatterplot(x=x,
#                     y=y,
#                     facecolors='None',
#                     edgecolor=edgecolor,
#                     size=pop,
#                     sizes=(40, 1000),
#                     legend='brief',
#                     ax=ax,
#                     hue=hue,
#                     hue_order=hue_order)

#     if plot_loess:
#         __plot_loess_helper(x, y, ax)
#     elif plot_linreg:
#         __plot_linreg_helper(x, y, ax)

#     plt.legend(loc='lower right')
#     corr_label =  r'$\rho=%.3f$' % rho
#     ax.text(0.02, 0.96, corr_label, transform=ax.transAxes, fontsize=24,
#             verticalalignment='top', bbox={'facecolor': 'white'})
#     ax.set(ylabel=ylabel, xlabel=xlabel)
#     if title:
#         ax.set_title(title, fontsize=28)


"""
Project estimates in plot_df onto estimates in train_df (i.e., satellite
estimates onto survey data). Use loess if transform=True, else a linear
regression. Since the units of RWI are essentially arbitrary, this makes
the distribution of estimates and survey data consistent so it's easier
to visually compare maps of the two.
"""    
def get_mapping_df(plot_df, transform, train_df=None, est_col='estimated_rwi_plot',
                   survey_col='survey_rwi_plot', out_col='estimated_rwi_map',
                   norm=True):
    # If there's not survey data for every geographical unit
    train_df = train_df if train_df is not None else plot_df

    map_df = gpd.GeoDataFrame(plot_df, crs='EPSG:4326')
    if transform:
        l = loess.loess(x=train_df[est_col],
                        y=train_df[survey_col],
                        surface='direct')
        l.fit()
        map_df[out_col] = l.predict(map_df[est_col]).values
    else:
        l = LinearRegression()
        l.fit(np.array(train_df[est_col]).reshape(-1, 1),
              np.array(train_df[survey_col]).reshape(-1, 1))
        preds = l.predict(np.array(map_df[est_col]).reshape(-1, 1))
        map_df[out_col] = [x[0] for x in preds]
    if norm:
        	map_df[out_col] = __standardize(map_df[out_col])
        
    return map_df




