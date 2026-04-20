library(tidyverse)

# 1. Patiënt Stamdata
pk_patients <- data.frame(
  `Patiënt ID` = c("PAT_01", "PAT_02", "PAT_03", "PAT_04", "PAT_05"),
  Leverfunctie = c("Normaal", "Licht_Falen", "Ernstig_Falen", "Normaal", "Licht_Falen"),
  Leeftijd = c(45, 62, 58, 39, 71)
) %>%
  filter(Patiënt.ID != "PAT_05")

# 2. PK Concentratie metingen (breed format)
pk_metingen <- data.frame(
  pid = c("PAT_01", "PAT_02", "PAT_03", "PAT_04", "PAT_06"), # PAT_06 is onbekend in stamdata
  T0_5_uur = c(2.1, 4.5, 8.2, 1.9, 3.3),
  T1_0_uur = c(5.4, 10.2, 15.1, 4.8, 6.1),
  T4_0_uur = c(3.2, 12.5, 22.4, 2.9, 5.2),
  T12_0_uur = c(1.1, 6.2, 18.5, 0.9, 2.1),
  T24_0_uur = c(0.2, 2.1, 12.4, 0.1, 0.5),
  Opmerking = c("Geen", "Lichte misselijkheid", "NA", "Slaapzak", "NA")
) %>%
  filter(pid != "PAT_06") %>%
  rename("Patiënt.ID" = "pid") %>%
  left_join(pk_patients, by="Patiënt.ID") %>%
  pivot_longer(cols = 2:6,
               names_to = "Tijd",
               values_to = "Waarde") %>%
  mutate(Tijd=parse_number(Tijd))%>%
  mutate(Waarde=round(log10(Waarde), digits = 2)) %>%
  group_by(Leverfunctie, Tijd) %>%
  summarise(Mean_waarde=mean(Waarde), SD=sd(Waarde)) %>%
  ggplot(mapping=aes(x=Tijd,
                     y=Mean_waarde,
                     colour = Leverfunctie,
                     group=Leverfunctie)) +
  scale_colour_discrete(labels=c("Ernstig_Falen" = "Ernstig Falen",
                                 "Licht_Falen" = "Licht Falen")) +
  geom_line() +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5,
                                   face="bold",
                                   family="sans",
                                  size=14),
        axis.title.x = element_text(hjust=0.5,
                                    face="bold",
                                    family="sans",
                                    size=14),
        axis.title.y = element_text(hjust=0.5,
                                    face="bold",
                                    family="sans",
                                    size=14),
        axis.text=element_text(hjust=0.5,
                                face="bold",
                                family="sans",
                                size=12)) +
  labs(title = "Leverfunctie per categorie",
       x="Tijd (uren)",
       y="Waarde")
  
  
  
  


# Theoretische curve data (om mee te vergelijken)
theoretisch_model <- data.frame(
  Tijd_u = c(0.5, 1, 4, 12, 24),
  Target_Conc = c(2.5, 6.0, 4.0, 1.5, 0.5)
)
