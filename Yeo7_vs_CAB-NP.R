# Create Sankey plot from Yeo 7 to CABNP to show how vertex assignment change

# /* 
# ----------------------------- Libraries & paths ---------------------------
# */
# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)
library(assortedRFunctions) 
library(ggimage)
library(cowplot)
# for load_ciftiTools() via remotes::install_github("JAQuent/assortedRFunctions")

# Load ciftiTools and set workbench paths
possible_wb_paths <- c("/usr/bin/wb_command", "/home1/Jaquent/Toolboxes/workbench/bin_rh_linux64/")
load_ciftiTools(possible_wb_paths) # Using ciftiTools_0.12.2

# /* 
# ----------------------------- Prepare data ---------------------------
# */
# Load Yeo 7 and CABNP parcellation 
Yeo7_xii  <- read_cifti("Yeo7.dlabel.nii")
CABNP_xii <- read_cifti("CortexSubcortex_ColeAnticevic_NetPartition_wSubcorGSR_netassignments_LR.dlabel.nii")

# Create data frame
df <- data.frame(Yeo7 = c(as.matrix(Yeo7_xii)),  
                 CAB_NP = c(as.matrix(CABNP_xii)))

# Remove rows where Yeo 7 is zero 
df <- df[df$Yeo7 != 0, ]

# Prepare levels and labels
Yeo7_levels <- sort(unique(df$Yeo7))
Yeo7_labels <- paste0("Yeo 7 - ", row.names(Yeo7_xii$meta$cifti$labels$parcels)[-1])
CAB_NP_levels <- sort(unique(df$CAB_NP))
CAB_NP_labels <- paste0("CAB NP - ", row.names(CABNP_xii$meta$cifti$labels$`#1`)[-1])

# Make the labels factors
df$Yeo7   <- factor(df$Yeo7, levels = Yeo7_levels, labels = Yeo7_labels) 
df$CAB_NP <- factor(df$CAB_NP, levels = CAB_NP_levels, labels = CAB_NP_labels) 

# Use make_long convert a 'wide' data frame into a format that 'geom_sankey' 
df_long <- make_long(df, Yeo7, CAB_NP)

# /* 
# ----------------------------- Create brain images ---------------------------
# */
# CIFTI plotting parameters
CIFTI_width <- 1300
view_cifti_surface(Yeo7_xii, fname = "support_images/Yeo6.png", 
                   width = CIFTI_width, view = "medial", hemisphere = "left",
                   legend_fname = "support_images/Yeo6_legend.png",
                   legend_embed = FALSE)
view_cifti_surface(CABNP_xii, fname = "support_images/CAB_NP.png", 
                   width = CIFTI_width, view = "medial", hemisphere = "left",
                   legend_fname = "support_images/CAB_NP_legend.png",
                   legend_embed = FALSE)

# /* 
# ----------------------------- Construct figure  ---------------------------
# */
# Format references
refs <- c("[1] Yeo, B. T. T. et al. The organization of the human cerebral cortex estimated by intrinsic functional connectivity. Journal of Neurophysiology 106, 1125–1165 (2011).",
          "[2] Ji, J. L. et al. Mapping the human brain’s cortical-subcortical functional network organization. NeuroImage 185, 35–57 (2019).")
refs <- paste(refs, collapse = "\n")

# Prepare colours
Yeo7_colours <- c("#E79523", "#CD3E4E", "#00760F", "#DCF8A4", "#C43BFA", "#4682B4", "#781286")
CAB_NP_colours = c("#0000FF", "#6400FF", "#00FFFF", "#990099", "#00FF00", 
                   "#009B9B", "#FFFF00", "#FA3EFB", "#FF0000", "#B15928",
                   "#FF9D00", "#417D00")
CAB_NP_colours <- CAB_NP_colours[order(CAB_NP_labels)] # Order alphabetically


# Create main figure sankey figure
p_main <- ggplot(df_long, aes(x = x, 
                    next_x = next_x, 
                    node = node, 
                    next_node = next_node,
                    fill = node)) +
  geom_sankey(flow.alpha = 0.75, node.color = 1) +
  scale_fill_manual(values = c(CAB_NP_colours, Yeo7_colours)) +
  scale_x_discrete(labels = c("Yeo 7", "CAB-NP")) +
  theme_sankey(base_size = 16) +
  theme(plot.subtitle = element_text(hjust = 0, size = 9),
        plot.title = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0, size = 7)) + 
  labs(x = "", fill = "Network", title = "Network mapping differences between\nYeo 7 [1] & Cole-Anticevic Brain Network Parcellation [2]",
       caption = refs)

# Create brain figure
p_brain <- ggplot() + 
  geom_image(aes(x = 0.5, y = 0.5, image = "support_images/Yeo6.png"), size = 0.2, by = "width") + 
  geom_image(aes(x = 0.5, y = 1, image = "support_images/CAB_NP.png"), size = 0.2, by = "width") + 
  # geom_text(aes(x = 0.5, y = 0.35, label = "Yeo 7")) +
  geom_text(aes(x = 0.5, y = 0.85, label = "CAB NP")) +
  coord_cartesian(ylim = c(0, 1.5)) +
  theme_nothing() + 
  theme(plot.background = element_rect(fill = "white", colour = "white"))

# Combine
p_combined <- plot_grid(p_main, p_brain, rel_widths = c(6, 1), nrow = 1)
p_combined

# Save
ggsave("Yeo7_vs_CAB-NP.png", p_combined, width = 3840, height = 2160, units = "px")

