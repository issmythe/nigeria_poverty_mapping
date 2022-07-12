#################
##### Setup #####
#################

library(cowplot)
library(dplyr)
library(foreign)
library(ggplot2)
library(grid)
library(gridExtra)
library(Metrics)
library(png)
library(RColorBrewer)
library(showtext)
library(stringr)
library(tidyr)

#################
##### Setup #####
#################
setwd('/Users/Home/Desktop/poverty_mapping/');
font_add("LM Roman 10", "~/Documents/fonts/Latin-Modern-Roman/lmroman10-regular.otf")
font_add("LM Roman 10 Bold", "~/Documents/fonts/Latin-Modern-Roman/lmroman10-bold.otf")
showtext.auto()

target_type_pal <- c('black', 'palegreen4', 'lightblue')
gray <- 'gray60'
pal <- brewer.pal(n=4, name='RdYlBu')
gray <- 'gray60'

scatterplot_ptsz <- 1
scatterplot_lnwt <- 0.5
scatterplot_lncol <- pal[4]
w <- 4

scatterplot_theme <- theme_classic() +
  theme(text=element_text(size=24, family="LM Roman 10"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size=24),
        legend.position ="none",
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        legend.margin = margin(c(0, 0, 0, 0)))

###################
##### Helpers #####
###################

get_plot <- function(xcol, ycol, xstr, ystr, method, df, ylab) {
  xlab <- paste (xstr, 'Wealth Estimate', sep=' ')
  ylab <- paste ('Ground Truth Wealth')
  minmax <- get_min_max(df)
  plt <- ggplot(aes_string(x=xcol, y=ycol), data=df) + 
    geom_point(color=gray, size=scatterplot_ptsz) +
    geom_smooth(method=method, formula=y~x, color=scatterplot_lncol, fill=scatterplot_lncol,
                fullrange=TRUE, size=scatterplot_lnwt) +
    labs(x=xlab, y=ylab) +
    annotate("text", x=-Inf, y=Inf, hjust=-2, vjust=1.5,
             label='rho', parse=TRUE, size=10) + 
    annotate("text", x=-Inf, y=Inf, hjust=-0.4, vjust=1.5,
             label=get_corr_label(xcol, ycol, df),
             size=10, family="LM Roman 10") + 
    scatterplot_theme
  
  return(plt)
}

get_min_max <- function(df) {
  vmin <- min(df$dhs_plot, df$nlss_plot, df$satellite_plot, na.rm=TRUE) - 0.1
  vmax <- max(df$dhs_plot, df$nlss_plot, df$satellite_plot, na.rm=TRUE) + 0.1
  return(c(vmin, vmax))
}

get_corr_label <- function(x, y, df) {
  cor <- round(cor(df[,x], df[,y], use='pairwise.complete.obs'), digits=3)
  cor <- formatC(cor, format='f', digits=3)
  label <- paste(' = ', cor, sep='')
  return(label)
}

############################
##### Get scatterplots #####
############################

state_ests <- read.csv('data/figure_data/state_wealth_estimates.csv')
lga_ests <- read.csv('data/figure_data/lga_wealth_estimates.csv')
ward_ests <- read.csv('data/figure_data/ward_wealth_estimates.csv')

state_min <- min(state_ests$nlss_plot[!is.na(state_ests$nlss_plot)])
lga_min <- min(lga_ests$nlss_plot[!is.na(lga_ests$nlss_plot)])
ward_min <- min(ward_ests$nlss_plot[!is.na(ward_ests$nlss_plot)])

state_max <- max(state_ests$nlss_plot[!is.na(state_ests$nlss_plot)])
lga_max <- max(lga_ests$nlss_plot[!is.na(lga_ests$nlss_plot)])
ward_max <- max(ward_ests$nlss_plot[!is.na(ward_ests$nlss_plot)])

dhs_state <- get_plot('dhs_plot', 'nlss_plot', 'DHS', 'NLSS', 'lm',
                      state_ests, '')
sat_state <- get_plot('satellite_plot', 'nlss_plot', 'ML-Based', 'NLSS', 'lm',
                      state_ests, 'Ground Truth Wealth')

dhs_lga <- get_plot('dhs_plot', 'nlss_plot', 'DHS', 'NLSS', 'loess',
                    lga_ests, '')
sat_lga <- get_plot('satellite_plot', 'nlss_plot', 'ML-Based', 'NLSS', 'loess',
                    lga_ests, 'Ground Truth Wealth')

dhs_ward <- get_plot('dhs_plot', 'nlss_plot', 'DHS', 'NLSS', 'loess',
                     ward_ests, '')
sat_ward <- get_plot('satellite_plot', 'nlss_plot', 'ML-Based', 'NLSS', 'loess',
                     ward_ests, 'Ground Truth Wealth')

##########################################################
##### Make figure S7: Get scatterplots, imputed data #####
##########################################################
imputed_lgas <- read.csv('data/figure_data/20220117/dhs_imputed_rwi_lga.csv')
imputed_wards <- read.csv('data/figure_data/20220117/dhs_imputed_rwi_ward.csv')

imputed_lgas <- imputed_lgas %>% 
  mutate(dhs_plot=imputed_rwi, nlss_plot=nlss_rwi, satellite_plot=0)
imputed_lgas_plt <- get_plot('imputed_rwi', 'nlss_rwi', 'DHS', 'NLSS', 'loess',
                             imputed_lgas, '') + labs(title='LGAs')

imputed_wards <- imputed_wards %>% 
  mutate(dhs_plot=imputed_rwi, nlss_plot=nlss_rwi, satellite_plot=0)
imputed_wards_plt <- get_plot('imputed_rwi', 'nlss_rwi', 'DHS', 'NLSS', 'loess',
                              imputed_wards, '') + labs(title='Wards')
imputed_title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                          textGrob('Correlation of Imputed DHS Poverty Maps with Ground Truth',
                                   gp=gpar(fontsize=18, fontfamily='LM Roman 10 Bold'),
                                   hjust=0.5))

imputed_plt <- plot_grid(imputed_lgas_plt, imputed_wards_plt, nrow=1) #, rel_widths=c(0.525, 0.475))
#ggsave('paper_figs/2022_pnas_revisions/imputed_dhs_scatterplot.png',
#       units='in', width=2.5*w, height=1.25*w, dpi=300)
imputed_plt


###################################
##### Make tidied up figure 2 #####
###################################
maps <- readPNG('paper_figs/0206/coverage_maps_short.png')

titleA <- textGrob('A. Comparison of Poverty Map Coverage',
                   gp=gpar(fontsize=18, fontfamily='LM Roman 10 Bold'),
                   hjust=0.64)
titleB <- textGrob('B. Correlation of Poverty Maps with \nGround Truth',
                   gp=gpar(fontsize=18, fontfamily='LM Roman 10 Bold'),
                   hjust=0.61)

state <- plot_grid(dhs_state, sat_state, nrow=1) #, rel_widths=c(0.525, 0.475))
ggsave('paper_figs/20220106/states_w25.png', units='in', width=2.5*w, height= w, dpi=300)

lga <- plot_grid(dhs_lga, sat_lga, nrow=1)
ggsave('paper_figs/20220106/lgas_w25.png', units='in', width=2.5*w, height=w, dpi=300)

ward <- plot_grid(dhs_ward, sat_ward, nrow=1)
ggsave('paper_figs/20220106/wards_w25.png', units='in', width=2.5*w, height=w, dpi=300)

map_fig <- plot_grid(titleA, rasterGrob(maps), rel_heights=c(1, 30), nrow=2)
#ggsave('paper_figs/20220106/map.png', units='in', width=3*w, height=3 * w, dpi=300)


##############################################
##### Get fraction of DIDL wards in NLSS #####
##############################################
state_ests <- read.csv('data/figure_data/20220117/ml_wards_in_nlss.csv')
#state_ests <- filter(state_ests, n_wards_targeted <= 1000)
head(state_ests)

scatterplot_theme <- theme_classic() +
  theme(text=element_text(size=16, family="LM Roman 10"),
        axis.text = element_text(size = 16),
        #plot.title = element_text(hjust = 0.5, size=20),
        #legend.position = c(0.65, 0.2),
        #legend.position = c(0.5, 1),
        legend.position ="none",
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        legend.margin = margin(c(0, 0, 0, 0)))

plt <- ggplot(aes_string(x='n_wards_targeted', y='frac_in_nlss'), data=state_ests) + 
  geom_line(color=gray, size=0.25) +
  labs(x='X Poorest Wards', y='Fraction Covered by NLSS') +
  scatterplot_theme
title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                   textGrob('Fraction of ML-Ranked Poorest\nWards Covered by NLSS Data',
                            gp=gpar(fontsize=16, fontfamily='LM Roman 10 Bold'),
                            hjust=0.5))

plt_t <- plot_grid(title, plt, nrow=2, rel_heights=c(1, 5))
# plot(acc_t)
ggsave('paper_figs/2022_pnas_revisions/ml_wards_in_nlss_all.png', units='in', width=1.2*w, height=1.1*w, dpi=300)


#####################
##### Figure S4 #####
#####################
con <- read.csv('data/figure_data/20220117/consumption_hist2.csv') %>%
  mutate(consumption = if_else(consumption / 1000 < 1000, consumption / 1000, 1000))

plt <- ggplot(con, aes(consumption)) + geom_density(aes(y=..count.. * 30)) + geom_histogram(binwidth=30, alpha=0.3) +
  scatterplot_theme +
  scale_x_continuous(breaks=c(0, 200, 400, 600, 800, 1000),
                     labels=c('0', '200', '400', '600', '800', '1000+')) +
  labs(x='Annual Consumption (1,000s of Naira)', y='1,000s of Individuals') +
  geom_vline(aes(xintercept=137429.52 / 1000), color=gray, linetype='dashed') +
  geom_vline(aes(xintercept=137429.52 / 2000), color=gray, linetype='dashed') +
  annotate(geom="text", x=c(137429.52 / 1000 + 70, 137429.52 / 2000 - 90),  y = c(30050, 30000),
           label=c('Poor', 'Extreme\nPoor'), color=c(gray, gray), family='LM Roman 10') +
  annotate("segment", x=137429.52 / 1000 + 40, xend=137429.52 / 1000 + 10, y=30000, yend=30000,
           lwd=0.3, colour=gray, arrow=arrow(length=unit(2, "mm"))) +
  annotate("segment", x=137429.52 / 2000 - 40, xend=137429.52 / 2000 - 10, y=30000, yend=30000,
           lwd=0.3, colour=gray, arrow=arrow(length=unit(2, "mm")))

ggsave('paper_figs/2022_pnas_revisions/consumption_hist_updated.png', units='in', width=2.2*w, height=1.1*w, dpi=300)
