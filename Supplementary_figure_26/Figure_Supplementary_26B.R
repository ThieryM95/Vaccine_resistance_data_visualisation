###############################################################################
# Code to visualize Figure Supplement 21                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load the data
Scenario_liste <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_26B.csv", header = T)

# Define the factor level
Scenario_liste$Degree_resistance <- as.factor(Scenario_liste$Degree_resistance)
Scenario_liste$Blood_coverage <- as.factor(Scenario_liste$Blood_coverage)
Scenario_liste$Seasonality <- as.factor(Scenario_liste$seasonality)
Scenario_liste$Age <- factor(Scenario_liste$Age, levels = c("Children", "18", "0.75"))
Scenario_liste$Blood_coverage <- as.factor(Scenario_liste$Blood_coverage)
Scenario_liste$Vaccine_type <- factor(Scenario_liste$Vaccine_type, levels = c("PEV", "BSV"))


# Define the label
S.labs <- c("Perenial setting", "Seasonal setting")
names(S.labs) <- c("sesonality1", "sesonality2")

V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("PEV", "BSV")

A.labs <- c("Low treatment acess (25%)", "High treatment access (60%)")
names(A.labs) <- c(0.1, 0.3)

H.labs <- c("Half-life = 1 year", "Half-life = 1.5 years")
names(H.labs) <- c(1, 1.5)

V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("PEV", "BSV")

A.labs <- c("Children", "Whole population", "Adults")
names(A.labs) <- c("Children", "0.75", "18")


# Panel B1
###########

# Select the data
Scenario_liste_2 <- Scenario_liste[Scenario_liste$Age == "Children", ]

# Define the constant
constant <- 2.5
pd <- position_dodge(0.5)

# Plot
PLOT_A <- 
  ggplot(Scenario_liste_2,
         aes(x = Degree_resistance,
             y = IR, 
             color = seasonality, 
             shape = Blood_coverage),
         size = 5 / constant,
         stroke = 2,
         position = pd) +
  stat_summary(fun = mean,
               position = pd,
               size = 1.3 / constant,
               stroke = 2.5 / constant) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 5.25 / constant,
               position = pd) +
  labs(title = "", 
       x = "Degree of resistance", 
       y = "Relative risk reduction of clinical malaria in children\n under 5 years (%) assuming resistant frequency of 100%") +
  ylim(0, 100) +
  facet_nested(. ~ Age + Vaccine_type, labeller = labeller(Age = A.labs, Vaccine_type = V.labs)) +
  scale_color_manual(name = "Deployment",
                     values = c("#CC6677", "#88CCEE"),
                     breaks = c("sesonality2", "sesonality1"),
                     labels = c("Seasonal", "Perennial")) +
  scale_shape_manual(values = c(22, 24),
                     name = "Combined with drug:",
                     breaks = c(0, 1),
                     labels = c("No", "Yes")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5,
                                  face = "bold"),
        legend.text = element_text(size = 18 / constant),
        legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(legend.position = "none")


# Panel B2
##########

# Select the data
Scenario_liste_2 <- Scenario_liste[Scenario_liste$Age != "Children", ]

# Plot
PLOT_B <- 
  ggplot(Scenario_liste_2,
         aes(x = Degree_resistance ,
             y = IR,
             color = seasonality,
             shape = Blood_coverage),
         size = 5 / constant,
         stroke = 2,
         position = pd) +
  stat_summary(fun = mean,
               position = pd,
               size = 1.3 / constant,
               stroke =  2.5 / constant) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 5 / constant,
               position = pd) +
  labs(title = "", x = "Degree of resistance", y = "Relative risk reduction of clinical malaria in the whole\n population (%) assuming resistant frequency of 100%") +
  ylim(0, 100) +
  facet_nested(. ~ Age + Vaccine_type, labeller = labeller(Age = A.labs, Vaccine_type = V.labs)) +
  scale_color_manual(name = "Deployment:",
                     values = c("#CC6677", "#88CCEE"),
                     breaks = c("sesonality2", "sesonality1"),
                     labels = c("Seasonal", "Perennial")) +
  scale_shape_manual(values = c(22, 24),
                     name = "Combined with drug:",
                     breaks = c(0, 1),
                     labels = c("No", "Yes")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant,
                                  hjust = 0.5,
                                  face = "bold"),
    legend.text = element_text(size = 18 / constant),
    legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(legend.position = "none")

# Get the legend
LEG <- 
  ggplot(Scenario_liste_2,
         aes(x = Degree_resistance, 
             y = IR,
             color = seasonality,
             shape = Blood_coverage),
         size = 25 / constant,
         stroke = 2,
         position = pd) +
  stat_summary(fun = mean,
               position = pd,
               size = 1.3 / constant,
               stroke =  2.5 / constant) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 5 / constant,
               position = pd) +
  labs(title = "", x = "Degree of resistance", y = "Relative risk reduction of clinical\nmalaria in the whole population (%)\nassuming resistant frequency of 100%") +
  ylim(0, 100) +
  facet_nested(. ~ Age + Vaccine_type, labeller = labeller(Age = A.labs, Vaccine_type = V.labs)) +
  scale_color_manual(name = "Deployment:",
                     values = c("#CC6677", "#88CCEE"),
                     breaks = c("sesonality2", "sesonality1"),
                     labels = c("Seasonal", "Perennial")) +
  scale_shape_manual(values = c(22, 24),
                     name = "Combined with drug:",
                     breaks = c(0, 1),
                     labels = c("No", "Yes")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 18 / constant),
        legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
  strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(legend.position = "top")

# Legend
LEG <- get_legend(LEG)

# Merge
PLOT_4 <- plot_grid(PLOT_A, PLOT_B, ncol = 2, nrow = 1, labels = c("", "", ""), label_size = 18 / constant, label_fontface = 2, scale = 1, rel_heights  = c(1, 1))
PLOT_4 <-plot_grid(PLOT_26A, PLOT_4, LEG, ncol = 1, nrow = 3, labels = c("A", "B", ""), label_size = 18 / constant, label_fontface = 2, scale = 0.99, rel_heights  = c(10, 10, 1))
PLOT_4

ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_26.pdf", plot = PLOT_4, width = 20, height = 15, device = "pdf", units = "cm", dpi = 300)
