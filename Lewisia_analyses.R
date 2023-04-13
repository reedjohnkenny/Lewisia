setwd("~/Desktop/Lewisia/Lewisia_github/Lewisia_github/")

#install.packages("rgl")
#install.packages("ggfortify")

library(dplyr)
library(tidyverse)
library(rgl)
library(ggfortify)
library(MASS)

#Analyze data extracted from inaturalist photos and san juan ridge photos

Inat_data <- read_csv("data/Inat_photos_data.csv")

Inat_data <- Inat_data %>% mutate(RedBlue = Red_Mean/Blue_Mean, 
                                  RedGreen = Red_Mean/Green_Mean, 
                                  GreenBlue = Green_Mean/Blue_Mean)


Inat_stats <- dplyr::select(Inat_data, RedGreen, RedBlue, GreenBlue, Petal_Count)

pcr_inat_dat <- prcomp(Inat_stats)

autoplot(pcr_inat_dat, data = Inat_data, colour = 'Taxon', loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.size = 4, loadings.label.colour = "black") +
  geom_point(aes(fill = Taxon, shape = Taxon), colour = "black", size = 2) +
  stat_ellipse(aes(color = Taxon)) +
  stat_ellipse(geom = "polygon", aes(fill = Taxon), alpha = 0.1) +
  scale_fill_manual(values = c("L. nevadensis" = "#a6a6a6", "San Juan Ridge population" = "#a962f7")) +
  scale_color_manual(values = c("L. nevadensis" = "#a6a6a6", "San Juan Ridge population" = "#a962f7")) +
  scale_shape_manual(values = c(21, 24)) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(title = "PCA of Lewisia Floral Traits") + 
  theme(text = element_text(size = 16, family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) 
ggsave("PCAofLewisiaFloralTraits.png", dpi=300, height=6, width=9, units="in")


#Analyse data from herbarium and field measurements

Lewisia_morpho <- read_csv("data/LewisiaMorphology.csv")

# have to drop 2 rows to maintain same number of rows as Lewisia_morpho_comp

Lewisia_morpho <- Lewisia_morpho[!is.na(Lewisia_morpho$basal_leaves_length_mm_min),]

Lewisia_morpho <- Lewisia_morpho[!is.na(Lewisia_morpho$`inflorescence stalk count`),]

# select only quantitative traits measured on all specimens 

Lewisia_morpho_comp <- Lewisia_morpho %>% dplyr::select(`corolla length (mm.)`, `corrolla width (mm.)`, `calyx length (mm.)`, `calyx width (mm.)`, `calyx dist. to widest width`, `inflorescence stalk count`, `inflorescence stalk length`, `peduncle length`, `stem leaves length`, basal_leaves_length_mm_min, basal_leaf_length_max, basal_leaves_width_max)

# keep only rows that have no missing data

Lewisia_morpho_comp <- Lewisia_morpho_comp[complete.cases(Lewisia_morpho_comp),]

# add a couple variables 

Lewisia_morpho_comp <- Lewisia_morpho_comp %>% mutate(leaf_ln_wd_ratio = basal_leaf_length_max/basal_leaves_width_max, ped_ln_lf_ln_ratio = `peduncle length`/basal_leaf_length_max)

pcr_morpho_dat <- prcomp(Lewisia_morpho_comp)

autoplot(pcr_morpho_dat, data = Lewisia_morpho, colour = 'Taxon', loadings = TRUE, frame = FALSE, frame.type = 'norm', frame.colour = "Taxon", loadings.label = TRUE, loadings.colour = "black", loadings.label.size = 4, loadings.label.colour = "black") +
  geom_point(aes(fill = Taxon, shape = Taxon), colour = "black", size = 2) +
  scale_fill_manual(values = c("L. nevadensis" = "#a6a6a6", "San Juan Ridge population" = "#a962f7")) +
  stat_ellipse(aes(color = Taxon)) +
  stat_ellipse(geom = "polygon", aes(fill = Taxon), alpha = 0.1) +
  scale_color_manual(values = c("L. nevadensis" = "#a6a6a6", "San Juan Ridge population" = "#a962f7")) +
  scale_shape_manual(values = c(21, 24)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(title = "PCA of Lewisia Morphological Traits") +
  theme(text = element_text(size = 16, family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))
ggsave("PCAofLewisiaMorphoTraits.png", dpi=300, height=6, width=9, units="in")

ggplot(pcr_morpho_dat, aes(fill = Taxon, shape = Taxon, colour = "black"))

#Original colors
autoplot(pcr_morpho_dat, data = Lewisia_morpho, colour = 'Taxon', loadings = TRUE, frame = FALSE, frame.type = 'norm', frame.colour = "Taxon", loadings.label = TRUE, loadings.colour = "black", loadings.label.size = 4, loadings.label.colour = "black") +
  geom_point(aes(fill = Taxon, shape = Taxon), colour = "black", size = 2) +
  scale_fill_manual(values = c("L. nevadensis" = "honeydew", "San Juan Ridge population" = "violet")) +
  stat_ellipse(aes(color = Taxon)) +
  stat_ellipse(geom = "polygon", aes(fill = Taxon), alpha = 0.25) +
  scale_color_manual(values = c("L. nevadensis" = "honeydew", "San Juan Ridge population" = "violet")) +
  scale_shape_manual(values = c(21, 24)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(title = "PCA of Lewisia Morphological Traits") +
  theme(text = element_text(size = 16, family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))
ggsave("PCAofLewisiaMorphoTraits.png", dpi=300, height=6, width=9, units="in")

