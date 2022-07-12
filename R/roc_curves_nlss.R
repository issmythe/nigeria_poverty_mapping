library(cowplot)
library(dplyr)
library(fixest)
library(foreign)
library(ggplot2)
library(grid)
library(gridExtra)
library(Metrics)
library(RColorBrewer)
library(showtext)
library(stringr)
library(tidyr)

#########
# Setup #
#########
setwd('/Users/Home/Desktop/poverty_mapping/');

font_add("LM Roman 10", "~/Documents/fonts/Latin-Modern-Roman/lmroman10-regular.otf")
font_add("LM Roman 10 Bold", "~/Documents/fonts/Latin-Modern-Roman/lmroman10-bold.otf")
showtext.auto()

display.brewer.pal(n=4, name='RdYlBu')
pal <- brewer.pal(n=4, name='RdYlBu')
pal3 <- pal[c(1, 2, 4)]
target_type_pal <- c('black', 'palegreen4', 'lightblue')
gray <- 'gray60'

scatterplot_ptsz <- 1
scatterplot_lnwt <- 0.5
scatterplot_lncol <- pal[4]

scatterplot_theme <- theme_classic() +
  theme(text=element_text(size=14, family="LM Roman 10"),
        legend.position = c(0.65, 0.2),
        plot.title = element_text(hjust = 0.5, size=16),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.margin = margin(c(0, 0, 0, 0)))

get_roc_plot <- function(df, x, y, labels, title, palette) {
  plt<- ggplot(data=df) +
    geom_line(aes_string(x=x, y=y, color='type'), size=scatterplot_lnwt) +
    geom_abline(intercept=0, slope=1, linetype='dashed', color=gray, size=scatterplot_lnwt) +
    scale_color_manual(values=palette, labels=labels) +
    #labs(x='False Positives', y='True Positives', title=title) +
    labs(x='False Positive Rate', title=title) +
    scatterplot_theme
  return(plt)
}

#################
# NLSS ROC plot #
#################
nlss_plot_data <- read.csv('data/figure_data/0112/nlss_roc_plot_data.csv')
nlss_plot_data$type <- factor(nlss_plot_data$type,
                              levels=c('Ward', 'LGA', 'State'))
auc_poor <- c(0.932, 0.881, 0.783)
auc_ultrapoor <- c(0.940, 0.844, 0.744)

poor_labels <- c(paste('Wards, AUC=', auc_poor[1], sep=''),
                 paste('LGAs, AUC=', auc_poor[2], sep=''),
                 paste('States, AUC=', auc_poor[3], sep=''))
ultrapoor_labels <- c(paste('Wards, AUC=', auc_ultrapoor[1], sep=''),
                      paste('LGAs, AUC=', auc_ultrapoor[2], sep=''),
                      paste('States, AUC=', auc_ultrapoor[3], sep=''))

nlss_poor_plot <- get_roc_plot(nlss_plot_data, 'asset_nonpoor_inc', 'asset_poor_inc',
                               poor_labels, 'Targeting the Poor', palette=pal3) +
                  labs(y='True Positive Rate')
nlss_ultrapoor_plot <- get_roc_plot(nlss_plot_data, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                                    ultrapoor_labels, 'Targeting the Extreme Poor', palette=pal3) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
w = 4
#png("paper_figs/0206/nlss_roc.png", units="in", width=w*2, height=w, res=300)
#grid.arrange(nlss_poor_plot, nlss_ultrapoor_plot, nrow=1)
plot_grid(nlss_poor_plot, nlss_ultrapoor_plot, align = "h", nrow = 1,
          rel_widths = c(0.525, 0.475))
#dev.off()
# ggsave("paper_figs/0122/nlss_roc.png", g, units="in", width=w*2, height=w, dpi=300)
ggsave('paper_figs/2022_pnas_revisions/nlss_roc.png', units='in', width=2*w, height=w, dpi=300)


#################
# Ward ROC plot #
#################
#ward_plot_data_p <- read.csv('data/figure_data/0112/ward_roc_poor_renamed.csv')
#ward_plot_data_up <- read.csv('data/figure_data/0112/ward_roc_ultrapoor_renamed.csv')

#ward_plot_data_p <- read.csv('data/figure_data/0707/ward_roc_poor_frac.csv')
#ward_plot_data_up <- read.csv('data/figure_data/0707/ward_roc_ultrapoor_frac.csv')

ward_plot_data_p <- read.csv('data/figure_data/0707/ward_roc_poor_median.csv')
ward_plot_data_up <- read.csv('data/figure_data/0707/ward_roc_ultrapoor_median.csv')

ward_plot_data_p <- filter(ward_plot_data_p, type != 'DHS Upper Bound')
ward_plot_data_up <- filter(ward_plot_data_up, type != 'DHS Upper Bound')

ward_plot_data_p$type <- ordered(ward_plot_data_p$type,
                              levels=c('Optimal', 'Satellite', 'DHS'))
ward_plot_data_up$type <- ordered(ward_plot_data_up$type,
                              levels=c('Optimal', 'Satellite', 'DHS'))

#ward_auc_poor <- c(0.932, 0.869, 0.811, 0.872)
#ward_auc_ultrapoor <- c(0.940, 0.859, 0.803, 0.856)
#ward_auc_poor <- c(0.95, 0.861, 0.802)
#ward_auc_ultrapoor <- c(0.964, 0.835, 0.774)
ward_auc_poor <- c(0.931, 0.863, 0.808)
ward_auc_ultrapoor <- c(0.947, 0.854, 0.803)

ward_poor_labels <- c(paste('Optimal, AUC=', ward_auc_poor[1], sep=''),
                      paste('ML-Based, AUC=', ward_auc_poor[2], sep=''),
                      paste('Survey Benchmark, \nAUC=', ward_auc_poor[3], sep=''))
ward_ultrapoor_labels <- c(paste('Optimal, AUC=', ward_auc_ultrapoor[1], sep=''),
                           paste('ML-Based, AUC=', ward_auc_ultrapoor[2], sep=''),
                           paste('Survey Benchmark, \nAUC=', ward_auc_ultrapoor[3], sep=''))

ward_poor_plot <- get_roc_plot(ward_plot_data_p, 'asset_nonpoor_inc', 'asset_poor_inc',
                               ward_poor_labels, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positives') + 
  theme(legend.margin = margin(c(0, 0, 0, 0)))
ward_ultrapoor_plot <- get_roc_plot(ward_plot_data_up, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                                    ward_ultrapoor_labels, 'Targeting the Extreme Poor',
                                    target_type_pal) +
  scatterplot_theme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.margin = margin(c(0, 0, 0, 0)))


w = 4
plots <- plot_grid(ward_poor_plot, ward_ultrapoor_plot, align = "h", nrow = 1,
          rel_widths = c(0.525, 0.475))
title <- textGrob("A. Targeting by Median Wealth",
                 gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                 hjust=0.96)
plot_grid(title, plots, nrow=2, rel_heights = c(1, 10))
ggsave("paper_figs/0720/ward_roc_median.png", units="in", width=w*2, height=1.1*w, dpi=300)

dev.off()


#######################
# Urban ward ROC plot #
#######################
ward_plot_data_p <- read.csv('data/figure_data/0111/urban_ward_roc_combo_rwi_poor_renamed.csv')
ward_plot_data_up <- read.csv('data/figure_data/0111/urban_ward_roc_combo_rwi_ultrapoor_renamed.csv')
ward_plot_data_p <- filter(ward_plot_data_p, type != 'DHS Upper Bound')
ward_plot_data_up <- filter(ward_plot_data_up, type != 'DHS Upper Bound')

ward_plot_data_p$type <- factor(ward_plot_data_p$type,
                                levels=c('Optimal', 'Satellite', 'DHS'))
ward_plot_data_up$type <- factor(ward_plot_data_up$type,
                                 levels=c('Optimal', 'Satellite', 'DHS'))

# Combo RWI
ward_auc_poor <- c(0.908, 0.792, 0.776, 0.835)
ward_auc_ultrapoor <- c(0.964, 0.877, 0.806, 0.824)

ward_poor_labels <- c(paste('Optimal, AUC=', ward_auc_poor[1], sep=''),
                      paste('ML-Based, AUC=', ward_auc_poor[2], sep=''),
                      paste('Survey Benchmark, \nAUC=', ward_auc_poor[3], sep=''))
ward_ultrapoor_labels <- c(paste('Optimal, AUC=', ward_auc_ultrapoor[1], sep=''),
                           paste('ML-Based, AUC=', ward_auc_ultrapoor[2], sep=''),
                           paste('Survey Benchmark, \nAUC=', ward_auc_ultrapoor[3], sep=''))

ward_poor_plot <- get_roc_plot(ward_plot_data_p, 'asset_nonpoor_inc', 'asset_poor_inc',
                               ward_poor_labels, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positives') + theme(legend.margin=margin(c(0, 0, 0, 0)))

ward_ultrapoor_plot <- get_roc_plot(ward_plot_data_up, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                                    ward_ultrapoor_labels, 'Targeting the Extreme Poor', target_type_pal) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.margin=margin(c(0, 0, 0, 0)))

plots <- plot_grid(ward_poor_plot, ward_ultrapoor_plot, align = "h", nrow = 1,
          rel_widths = c(0.525, 0.475))
title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                       textGrob("A. Targeting Urban Wards (TPR vs FPR)",
                                gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                                hjust=0.71))
urban_roc <- plot_grid(title, plots, nrow=2, rel_heights = c(1, 10))
#ggsave("paper_figs/1026/urban_ward_roc_titled.png", units="in", width=w*2, height=1.1*w, dpi=300)

plt <-plot_grid(urban_roc, acc_plt, nrow=2)

ggsave("paper_figs/1026/urban_roc_acc.png", units="in", width=w*2, height=2.2*w, dpi=300)


#dev.off()













