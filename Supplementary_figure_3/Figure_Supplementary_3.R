###############################################################################
# Code to visualsise Figure Supplement 3                                     #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# load package
library("rlang")
library("ggh4x")
library("ggplot2")
library('plyr')

# load Data
Precision_final <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_Supplement_3.csv", header = T)

# Define constrained variable as a factor
Precision_final$Coverage_reduced <- factor(Precision_final$Coverage_reduced, levels = c("0", "0.2"))
Precision_final$seasonality <- factor(Precision_final$seasonality, levels = c("sesonality1", "sesonality2"))
Precision_final$Vaccine_type <- factor(Precision_final$Vaccine_type, levels = c("AIV", "BSV"))
Precision_final$Drug <- factor(Precision_final$Drug, levels = c("No", "Yes"))

# Creat labels for each constrain factor
C.labs <- c("No coverage reduction", "Coverage reduction = 20%")
names(C.labs) <- c("0", "0.2")

S.labs <- c("Perennial deployment", "Seasonal deployment")
names(S.labs) <- c("sesonality1", "sesonality2")

V.labs <- c("AIV", "BSV")
names(V.labs) <- c("AIV", "BSV")

D.labs <- c("No drug with booster", "Drug with booster")
names(D.labs) <- c("No", "Yes")

# Estimate the COR and RMSEE (see function bellow)
cors <- ddply(Precision_final, c("Coverage_reduced", "seasonality", "Vaccine_type", "Drug"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final, c("Coverage_reduced", "seasonality", "Vaccine_type", "Drug"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

# visualize
constant <- 2.5
PLOT <-
  ggplot(data = Precision_final, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(
    Drug + Coverage_reduced ~ Vaccine_type + seasonality,
    labeller = labeller(seasonality = S.labs,
                        Coverage_reduced = C.labs,
                        Vaccine_type = V.labs,
                        Drug = D.labs)) +
  geom_point(size = 2.5 / constant) +
  theme_bw() +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = FALSE,
              size = 2 / constant) +
  geom_text(data = cors,
            aes(label = paste("Cor=", cor, sep = "")),
            x = 0.1,
            y = 0.2,
            size = 5.5 / constant) +
  geom_text(data = RMSEE,
            aes(label = paste("RMSE=", cor, sep = "")),
            x = 0.1,
            y = 0.23,
            size = 5.5 / constant) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  ylab("Predicted rate of spread") +
  xlab("Observed rate of spread") +
  ggtitle("") +
  theme(strip.text.x = element_text(
        size = 18 / constant,
        color = "black",
        face = "bold"),
    strip.text.y = element_text(
      size = 18 / constant,
      color = "black",
      face = "bold"))

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_Supplement_3.pdf", plot = PLOT, width = 16, height = 13, device = "pdf", units = "cm", dpi = 300)


#--------------------------------------------------
# Function to estimate the Root mean squared error.
# --------------------------------------------------
RMSE <- function(x, y) {
  # do a liner regression
  model_regression <- lm(y ~ x)
  
  # estimate the residuals
  model_summary <- summary(model_regression)
  
  # estimate the Root mean squared error
  RMSE <- sqrt(mean((model_summary$residuals) ^ 2))
  
  # return RMSE
  return(RMSE)
}
