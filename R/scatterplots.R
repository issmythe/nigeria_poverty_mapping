#################
##### Setup #####
#################

library(cowplot)
library(RColorBrewer)
library(dplyr)
library(fixest)
library(foreign)
library(ggplot2)
library(grid)
library(gridExtra)
library(Metrics)
library(png)
library(showtext)
library(stringr)
library(tidyr)


setwd('/Users/Home/Desktop/nigeria/');

font_add("LM Roman 10", "~/Documents/fonts/Latin-Modern-Roman/lmroman10-regular.otf")
font_add("LM Roman 10 Bold", "~/Documents/fonts/Latin-Modern-Roman/lmroman10-bold.otf")
showtext.auto()

display.brewer.pal(n=4, name='RdYlBu')
pal <- brewer.pal(n=4, name='RdYlBu')
gray <- 'gray60'

scatterplot_ptsz <- 1
scatterplot_lnwt <- 0.5
scatterplot_lncol <- pal[4]
fontsz <- 16

scatterplot_theme <- theme_classic() +
  theme(text=element_text(size=14, family="LM Roman 10"),
        plot.title = element_text(hjust = 0, size=16, family="LM Roman 10 Bold"))

state_ests <- read.csv('data/figure_data/state_wealth_estimates.csv')
lga_ests <- read.csv('data/figure_data/lga_wealth_estimates.csv')
ward_ests <- read.csv('data/figure_data/ward_wealth_estimates.csv')

get_plot <- function(xcol, ycol, xstr, ystr, method, df) {
  xlab <- paste (xstr, 'Wealth Estimate', sep=' ')
  #xlab <- paste ('Estimated Wealth')
  ylab <- paste ('Ground Truth Wealth')
  minmax <- get_min_max(df)
  plt <- ggplot(aes_string(x=xcol, y=ycol), data=df) + 
    geom_point(color=gray, size=scatterplot_ptsz) +
    geom_smooth(method=method, formula=y~x, color=scatterplot_lncol, fill=scatterplot_lncol,
                fullrange=TRUE, size=scatterplot_lnwt) +
    labs(x=xlab, y=ylab) +
    annotate("text", x=-Inf, y=Inf, hjust=-2, vjust=1.5,
             label='rho', parse=TRUE, size=5) + 
    annotate("text", x=-Inf, y=Inf, hjust=-0.4, vjust=1.5,
             label=get_corr_label(xcol, ycol, df),
             size=5, family="LM Roman 10") + 
    #xlim(minmax[1], minmax[2]) +
    #ylim(minmax[1], minmax[2]) +
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


############
# All data #
############
gc()

dhs_state <- get_plot('dhs_plot', 'nlss_plot', 'DHS', 'NLSS', 'lm', state_ests) #+ labs(title='States')
sat_state <- get_plot('satellite_plot', 'nlss_plot', 'ML-Based', 'NLSS', 'lm', state_ests)
#sat_state <- sat_state + labs(title='States')

dhs_lga <- get_plot('dhs_plot', 'nlss_plot', 'DHS', 'NLSS', 'loess', lga_ests) #+ labs(title='LGAs')
sat_lga <- get_plot('satellite_plot', 'nlss_plot', 'ML-Based', 'NLSS', 'loess', lga_ests)
#sat_lga <- sat_lga + labs(title='LGAs')

dhs_ward <- get_plot('dhs_plot', 'nlss_plot', 'DHS', 'NLSS', 'loess', ward_ests) #+ labs(title='Wards')
sat_ward <- get_plot('satellite_plot', 'nlss_plot', 'ML-Based', 'NLSS', 'loess', ward_ests)
#sat_ward <- sat_ward + labs(title='Wards')

##############
# Orig plots #
##############
w=4
png("paper_figs/0930/scatterplots_ml.png", units="in", width=w*2, height=3, res=300)
grid.arrange(sat_state, sat_lga, sat_ward, nrow=1,
             top=textGrob("A. ML-Based Poverty Maps",
                          gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                          hjust=1.13))
dev.off()

w=4
png("paper_figs/1026/scatterplots_dhs_lab.png", units="in", width=w*2, height=3, res=300)
grid.arrange(dhs_state, dhs_lga, dhs_ward, nrow=1,
             top=textGrob("B. Survey-Based (Benchmark) Poverty Maps", 
                          gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                          hjust=0.7))
dev.off()

#w=4
#png("paper_figs/0930/scatterplots.png", units="in", width=w*2, height=w * 4/3, res=300)
#grid.arrange(sat_state, sat_lga, sat_ward, dhs_state, dhs_lga, dhs_ward, nrow = 2)
#dev.off()
##############
w <- 4

lga <- grid.arrange(sat_lga, dhs_lga, nrow=1)
ward <- grid.arrange(sat_ward, dhs_ward, nrow=1)
maps <- readPNG('paper_figs/0930/coverage_maps_short_leg.png')

plot(state)

titleA <- textGrob('A. Coverage Comparison, Survey- and ML-Based Poverty Maps',
                   gp=gpar(fontsize=26, fontfamily='LM Roman 10 Bold'),
                   hjust=0.61)
titleB <- textGrob('B. Correlation of Poverty Maps with Ground Truth',
                   gp=gpar(fontsize=18, fontfamily='LM Roman 10 Bold'),
                   hjust=0.61)
png('paper_figs/1026/states.png', units="in", width=w*2, height=w, res=300)
state <- grid.arrange(sat_state, dhs_state, nrow=1, top=titleB)
plot(state)
dev.off()
#ggsave('paper_figs/0930/states.png', units='in', width=2*w, height=w, dpi=300)




title <- grid.arrange(titleA)
plot(title)
ggsave('paper_figs/1026/titleA.png', units='in', width=2*w, dpi=300)
plot(p)

###############
# Urban wards #
###############
urban_ward <- read.csv('data/figure_data/0105/urban_scatterplot_df.csv')

sat_urban_ward <- get_plot('satellite_plot', 'nlss_plot', 'ML-Based',
                           'NLSS', 'loess', urban_ward) +
  labs(title='A. ML-Based')

dhs_urban_ward <- get_plot('dhs_plot', 'nlss_plot', 'Survey Benchmark',
                           'NLSS', 'loess', urban_ward) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title='B. Survey-Based Benchmark')


w=4
png("paper_figs/0930/urban_scatterplot.png", units="in", width=w * 2, height=w, res=300)
#png("paper_figs/0930/urban_scatterplots_h2.png", units="in", width=w*2, height=w, res=300)
#grid.arrange(sat_urban_ward, dhs_urban_ward, nrow=1)
plot_grid(sat_urban_ward, dhs_urban_ward, align = "h", nrow = 1,
          rel_widths = c(0.525, 0.475))
dev.off()
###############










