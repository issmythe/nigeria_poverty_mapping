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
  theme(text=element_text(size=16, family="LM Roman 10"),
        #axis.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size=16),
        legend.position ="none",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        legend.margin = margin(c(0, 0, 0, 0)))

#####################################
#####  Make mean accuracy panel #####
#####################################

# toggle for urban vs all wards
#acc_data <- read.csv('data/figure_data/0122/accuracy_plot_data.csv')
#acc_data <- read.csv('data/figure_data/20220117/acc_plot_data_all_wards.csv')
acc_data <- read.csv('data/figure_data/0122/accuracy_plot_data_urban.csv')

acc_data <- acc_data %>%
  filter(targeting_approach != 'DHS Upper Bound') %>%
  mutate(targeting_approach=factor(targeting_approach,
                                   levels=c('Optimal', 'Satellite', 'DHS')))

poor_acc <- ggplot(data=acc_data) +
  geom_line(aes(x=thresh, y=poor, color=targeting_approach), size=scatterplot_lnwt) +
  scale_color_manual(values=target_type_pal) +
  ylim(0, 1) +
  labs(x='Program Size as Fraction of Poor',
       y='Target Sample Accuracy',
       title='Targeting the Poor') +
  scatterplot_theme +
  theme(legend.position="none")

ex_poor_acc <- ggplot(data=acc_data) +
  geom_line(aes(x=thresh, y=verypoor, color=targeting_approach), size=scatterplot_lnwt) +
  scale_color_manual(values=target_type_pal, labels=c('Optimal', 'ML-Based', 'Survey Benchmark')) +
  ylim(0, 1) +
  labs(x='Program Size as Fraction of Poor',
       title='Targeting the Extreme Poor',
       y='') +
  scatterplot_theme +
  theme(legend.position = c(0.7, 0.85), legend.title = element_blank(), # orig: c(0.7, 0.8)
        axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.margin=margin(c(0, 0, 0, 0)))

titleB <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                   textGrob('B. Targeting by Mean Wealth (Accuracy vs Program Size)',
                            gp=gpar(fontsize=16, fontfamily='LM Roman 10 Bold'),
                            hjust=0.51))
titleB <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                   textGrob('B. Targeting All Wards (Accuracy vs Program Size)',
                            gp=gpar(fontsize=16, fontfamily='LM Roman 10 Bold'),
                            hjust=0.58))
titleB <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                   textGrob('B. Targeting Urban Wards (Accuracy vs Program Size)',
                            gp=gpar(fontsize=16, fontfamily='LM Roman 10 Bold'),
                            hjust=0.54))

acc <- plot_grid(poor_acc, ex_poor_acc, nrow=1, rel_widths=c(0.525, 0.475))
acc_t <- plot_grid(titleB, acc, nrow=2, rel_heights=c(2, 10))
# plot(acc_t)
ggsave('paper_figs/2022_pnas_revisions/temp_accuracy.png', units='in', width=2*w, height=1.1*w, dpi=300)


############################
##### ROC plot helpers #####
############################

roc_theme <- theme_classic() +
  theme(text=element_text(size=16, family="LM Roman 10"),
        legend.position = c(0.65, 0.2),
        plot.title = element_text(hjust = 0.5, size=16),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.margin = margin(c(0, 0, 0, 0)))

poor_theme <- labs(y='True Positive Rate') + 
  theme(legend.margin = margin(c(0, 0, 0, 0)))
up_theme <- theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.margin = margin(c(0, 0, 0, 0)))


get_roc_plot <- function(df, x, y, auc, title, palette) {
  labels <- c(paste('Optimal, AUC=', auc[1], sep=''),
              paste('ML-Based, AUC=', auc[2], sep=''),
              paste('Survey Benchmark, \nAUC=', auc[3], sep=''))

  plt<- ggplot(data=df) +
    geom_line(aes_string(x=x, y=y, color='type'), size=scatterplot_lnwt) +
    geom_abline(intercept=0, slope=1, linetype='dashed', color=gray, size=scatterplot_lnwt) +
    scale_color_manual(values=palette, labels=labels) +
    #labs(x='False Positives', y='True Positives', title=title) +
    labs(x='False Positive Rate', title=title) +
    roc_theme
  return(plt)
}

get_roc_plot_dhsgt <- function(df, x, y, auc, title, palette) {
  labels <- c(paste('Optimal (DHS),\nAUC=', auc[1], sep=''),
              paste('ML-Based, AUC=', auc[2], sep=''),
              paste('Survey Benchmark\n(NLSS), AUC=', auc[3], sep=''))
  
  plt<- ggplot(data=df) +
    geom_line(aes_string(x=x, y=y, color='type'), size=scatterplot_lnwt) +
    geom_abline(intercept=0, slope=1, linetype='dashed', color=gray, size=scatterplot_lnwt) +
    scale_color_manual(values=palette, labels=labels) +
    #labs(x='False Positives', y='True Positives', title=title) +
    labs(x='False Positive Rate', title=title) +
    roc_theme
  return(plt)
}

read_roc_data <- function(path) {
  roc <- read.csv(path) %>%
    filter(type != 'DHS Upper Bound') %>%
    mutate(type = ordered(type, levels=c('Optimal', 'Satellite', 'DHS')))
  return(roc)
}

############################
##### ROC plot by mean #####
############################
mean_roc_p <- read_roc_data('data/figure_data/0112/ward_roc_poor_renamed.csv')
mean_roc_up <- read_roc_data('data/figure_data/0112/ward_roc_ultrapoor_renamed.csv')

mean_auc_p <- c(0.932, 0.869, 0.811)
mean_auc_up <- c(0.940, 0.859, 0.803)

mean_plot_p <- get_roc_plot(mean_roc_p, 'asset_nonpoor_inc', 'asset_poor_inc',
                            mean_auc_p, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positive Rate') + 
  theme(legend.margin = margin(c(0, 0, 0, 0)))
mean_plot_up <- get_roc_plot(mean_roc_up, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                                    mean_auc_up, 'Targeting the Extreme Poor',
                                    target_type_pal) + up_theme

mean <- plot_grid(mean_plot_p, mean_plot_up, align = "h", nrow = 1, rel_widths = c(0.525, 0.475))
mean_title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                       textGrob("A. Targeting by Mean Wealth (ROC Curve)",
                                gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                                hjust=0.68))

mean_plt <- plot_grid(mean_title, mean, nrow=2, rel_heights = c(2, 10))
ggsave("paper_figs/2022_pnas_revisions/ward_roc_mean.png", units="in", width=w*2, height=1.1*w, dpi=300)

##############################
##### ROC plot by median #####
##############################
med_roc_p <- read_roc_data('data/figure_data/0707/ward_roc_poor_median.csv')
med_roc_up <- read_roc_data('data/figure_data/0707/ward_roc_ultrapoor_median.csv')

med_auc_p <- c(0.931, 0.863, 0.808)
med_auc_up <- c(0.947, 0.854, 0.803)

med_plot_p <- get_roc_plot(med_roc_p, 'asset_nonpoor_inc', 'asset_poor_inc',
                           med_auc_p, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positive Rate') + 
  theme(legend.margin = margin(c(0, 0, 0, 0)))
med_plot_up <- get_roc_plot(med_roc_up, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                            med_auc_up, 'Targeting the Extreme Poor',
                            target_type_pal) + up_theme

med <- plot_grid(med_plot_p, med_plot_up, align = "h", nrow = 1, rel_widths = c(0.525, 0.475))
med_title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                      textGrob("A. Targeting by Median Wealth",
                      gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                      hjust=0.92))
med_plt <- plot_grid(med_title, med, nrow=2, rel_heights = c(2, 10))
ggsave("paper_figs/2022_pnas_revisions/ward_roc_med.png", units="in", width=w*2, height=1.1*w, dpi=300)

################################
##### ROC plot by fraction #####
################################
frac_roc_p <- read_roc_data('data/figure_data/0707/ward_roc_poor_frac.csv')
frac_roc_up <- read_roc_data('data/figure_data/0707/ward_roc_ultrapoor_frac.csv')

frac_auc_p <- c(0.95, 0.861, 0.802)
frac_auc_up <- c(0.964, 0.835, 0.774)

frac_plot_p <- get_roc_plot(frac_roc_p, 'asset_nonpoor_inc', 'asset_poor_inc',
                            frac_auc_p, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positive Rate') + 
  theme(legend.margin = margin(c(0, 0, 0, 0)))
frac_plot_up <- get_roc_plot(frac_roc_up, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                             frac_auc_up, 'Targeting the Extreme Poor',
                             target_type_pal) + up_theme

frac <- plot_grid(frac_plot_p, frac_plot_up, align = "h", nrow = 1, rel_widths = c(0.525, 0.475))
frac_title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                       textGrob("B. Targeting by Fraction Poor",
                       gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                       hjust=0.97))
frac_plt <- plot_grid(frac_title, frac, nrow=2, rel_heights = c(2, 10))
ggsave("paper_figs/2022_pnas_revisions/ward_roc_frac.png", units="in", width=w*2, height=1.1*w, dpi=300)

############################################
##### ROC plot by mean, all NLSS wards #####
############################################
all_roc <- read_roc_data('data/figure_data/20220117/roc_plot_data_all_wards.csv')

all_auc_p <- c(0.934, 0.867, 0.842)
all_auc_up <- c(0.924, 0.822, 0.807)

all_plot_p <- get_roc_plot(all_roc, 'asset_nonpoor_inc', 'asset_poor_inc',
                           all_auc_p, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positive Rate') + 
  theme(legend.margin = margin(c(0, 0, 0, 0)))
all_plot_up <- get_roc_plot(all_roc, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                            all_auc_up, 'Targeting the Extreme Poor',
                            target_type_pal) + up_theme

all_fig <- plot_grid(all_plot_p, all_plot_up, align = "h", nrow = 1, rel_widths = c(0.525, 0.475))
all_title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                      textGrob("A. Targeting All Wards (ROC Curve)",
                               gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                               hjust=0.80))
all_plt <- plot_grid(all_title, all_fig, nrow=2, rel_heights = c(2, 10))
ggsave("paper_figs/2022_pnas_revisions/temp_roc.png", units="in", width=w*2, height=1.1*w, dpi=300)

#################################################
##### ROC plot by mean, DHS as ground truth #####
#################################################
dhsgt_roc_p <- read.csv('data/figure_data/20220117/ward_roc_poor_dhsgt.csv') %>%
  mutate(type = ordered(type, levels=c('Optimal - DHS', 'Satellite', 'Survey Benchmark - NLSS')))
dhsgt_roc_up <- read.csv('data/figure_data/20220117/ward_roc_ultrapoor_dhsgt.csv') %>%
  mutate(type = ordered(type, levels=c('Optimal - DHS', 'Satellite', 'Survey Benchmark - NLSS')))

dhsgt_auc_p <- c(0.955, 0.904, 0.808)
dhsgt_auc_up <- c(0.960, 0.876, 0.817)

dhsgt_plot_p <- get_roc_plot_dhsgt(dhsgt_roc_p, 'asset_nonpoor_inc', 'asset_poor_inc',
                                   dhsgt_auc_p, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positive Rate') + theme(legend.margin = margin(c(-10, -10, -10, -10)))

dhsgt_plot_up <- get_roc_plot_dhsgt(dhsgt_roc_up, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                                    dhsgt_auc_up, 'Targeting the Extreme Poor', target_type_pal) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.margin = margin(c(-10, -10, -10, -10)))

dhsgt <- plot_grid(dhsgt_plot_p, dhsgt_plot_up, align = "h", nrow = 1, rel_widths = c(0.525, 0.475))
dhsgt_title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                        textGrob("Targeting with DHS as Ground Truth",
                                gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                                hjust=0.80))
dhsgt_plt <- plot_grid(dhsgt_title, dhsgt, nrow=2, rel_heights = c(2, 10))
ggsave("paper_figs/2022_pnas_revisions/dhs_gt_roc.png", units="in", width=w*2, height=1.1*w, dpi=300)



#################################
##### ROC plot, urban wards #####
#################################
ward_plot_data_p <- read.csv('data/figure_data/0111/urban_ward_roc_combo_rwi_poor_renamed.csv')
ward_plot_data_up <- read.csv('data/figure_data/0111/urban_ward_roc_combo_rwi_ultrapoor_renamed.csv')
ward_plot_data_p <- filter(ward_plot_data_p, type != 'DHS Upper Bound')
ward_plot_data_up <- filter(ward_plot_data_up, type != 'DHS Upper Bound')

ward_plot_data_p$type <- factor(ward_plot_data_p$type,
                                levels=c('Optimal', 'Satellite', 'DHS'))
ward_plot_data_up$type <- factor(ward_plot_data_up$type,
                                 levels=c('Optimal', 'Satellite', 'DHS'))

# Combo RWI
ward_auc_poor <- c(0.908, 0.792, 0.776)
ward_auc_ultrapoor <- c(0.964, 0.877, 0.806)

urban_plot_p <- get_roc_plot(ward_plot_data_p, 'asset_nonpoor_inc', 'asset_poor_inc',
                             ward_auc_poor, 'Targeting the Poor', target_type_pal) +
  labs(y='True Positive Rate') + 
  theme(legend.margin = margin(c(0, 0, 0, 0)))
urban_plot_up <- get_roc_plot(ward_plot_data_up, 'asset_nonultrapoor_inc', 'asset_ultrapoor_inc',
                              ward_auc_ultrapoor, 'Targeting the Extreme Poor',
                              target_type_pal) + up_theme

mean <- plot_grid(urban_plot_p, urban_plot_up, align = "h", nrow = 1, rel_widths = c(0.525, 0.475))
mean_title <- grobTree(rectGrob(gp=gpar(fill='white', col='white')),
                       textGrob("A. Targeting Urban Wards (ROC Curve)",
                                gp=gpar(fontsize=16, fontfamily="LM Roman 10 Bold"),
                                hjust=0.74))

mean_plt <- plot_grid(mean_title, mean, nrow=2, rel_heights = c(2, 10))
#########################
##### Combined plot #####
#########################

urban_plots <- plot_grid(mean_plt, acc_t, nrow=2)
ggsave('paper_figs/2022_pnas_revisions/urban_roc_acc.png', units='in', width=2*w, height=2.2*w, dpi=300)

mean_plots <- plot_grid(mean_plt, acc_t, nrow=2)
ggsave('paper_figs/2022_pnas_revisions/target_by_mean.png', units='in', width=2*w, height=2.2*w, dpi=300)

sensitivity_plots <- plot_grid(med_plt, frac_plt, nrow=2)
ggsave('paper_figs/2022_pnas_revisions/roc_sensitivity.png', units='in', width=2*w, height=2.2*w, dpi=300)

all_wards_plots <- plot_grid(all_plt, acc_t, nrow=2)
ggsave('paper_figs/2022_pnas_revisions/targeting_all_wards.png', units='in', width=2*w, height=2.2*w, dpi=300)













