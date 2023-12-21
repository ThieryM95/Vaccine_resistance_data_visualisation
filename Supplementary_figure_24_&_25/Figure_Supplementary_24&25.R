###############################################################################
# Code to visualize Figure Supplement 24 & 25                                 #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load the data
Scenario_liste <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_24&25.csv", header = T)

# Define factor level
Scenario_liste$Degree_resistance <- as.factor(Scenario_liste$Degree_resistance)
Scenario_liste$Blood_coverage <- as.factor(Scenario_liste$Blood_coverage)

# Define label
S.labs <- c("Perenial setting", "Seasonal setting")
names(S.labs) <- c("sesonality1", "sesonality2")

V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("PEV", "BSV")

Ac.labs <- c("Low treatment acess (25%)", "High treatment access (60%)")
names(Ac.labs) <- c(0.1, 0.3)

H.labs <- c("Half-life = 1 year", "Half-life = 1.5 years")
names(H.labs) <- c(1, 1.5)

V.labs <- c("Anti-infective vaccine", "Blood-stage vaccine")
names(V.labs) <- c("PEV", "BSV")

A.labs <- c("Children", "Whole population", "Adults")
names(A.labs) <- c("Children", "0.75", "18")

E.labs <- c("Low transmisison intensity (EIR = 5)", "Medium transmisison intensity (EIR = 50)")
names(E.labs) <- c("5", "50")


# Plot 24
##########

# Select the data
Scenario_liste_2 <- Scenario_liste[Scenario_liste$Age == "Children", ]
Scenario_liste_2 <- Scenario_liste_2[Scenario_liste_2$eir != 150, ]

# Define the constant
constant <- 2.5
pd <- position_dodge(0.5)

# Plot
PLOT_A <-
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
               size = 1.5 / constant,
               stroke = 2.5 / constant) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 2.5,
               position = pd) +  
  labs(title = "", x = "Degree of resistance", y = "Relative risk reduction of clinical malaria in children\n under 5 years (%) assuming resistant frequency of 50%") +
  ylim(0, 100) +
  facet_nested(eir + Access ~ Age + Vaccine_type + half_life,
               labeller = labeller(Vaccine_type = V.labs,
                                   Age = A.labs, 
                                   half_life = H.labs, 
                                   Access = Ac.labs, 
                                   eir = E.labs)) +
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
  theme(legend.position = "top")

# save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_24.pdf", plot = PLOT_A, width = 17, height = 19, device = "pdf", units = "cm", dpi = 300)

# Plot 25
#########

# Select the data
Scenario_liste_2 <- Scenario_liste[Scenario_liste$Age != "Children", ]
Scenario_liste_2 <- Scenario_liste_2[Scenario_liste_2$eir != 150, ]

# define factor level
Scenario_liste_2$Age <- factor(Scenario_liste_2$Age, levels = c("Children", "18", "0.75"))

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
               size = 1.5 / constant,
               stroke = 2.5 / constant) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 2.5,
               position = pd) +
  labs(title = "", 
       x = "Degree of resistance", 
       y = "Relative risk reduction of clinical malaria in in the whole\n population (%) assuming resistant frequency of 50%") +
  ylim(0, 100) +
  facet_nested(eir + Access ~ Age + Vaccine_type + half_life,
               labeller = labeller(Vaccine_type = V.labs,
                                   Age = A.labs,
                                   half_life = H.labs,
                                   Access = Ac.labs,
                                   eir = E.labs)) +
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
  theme(legend.position = "top")

# save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_25.pdf", plot = PLOT_B, width = 17, height = 19, device = "pdf", units = "cm",  dpi = 300)
