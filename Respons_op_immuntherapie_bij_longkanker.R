library(tidyverse)

set.seed(42)
n_patients <- 150

# Dataset 1: Patiënt basisinformatie
patients <- data.frame(
  pat_id = 1:n_patients,
  age_gender = paste0(sample(45:85, n_patients, replace = TRUE), "_", sample(c("M", "V", "X"), n_patients, replace = TRUE)),
  mutation_status = sample(c("EGFR+", "KRAS+", "ALK+", "WT", "Unknown"), n_patients, replace = TRUE, prob = c(0.2, 0.2, 0.1, 0.4, 0.1)),
  location_code = sample(c("LOC-A", "LOC-B", "LOC-C"), n_patients, replace = TRUE)
)

# Dataset 2: Biomarkers over tijd (Weken na start therapie)
biomarkers <- expand.grid(pat_id = 1:n_patients, week = c("week_0", "week_2", "week_4", "week_8")) %>%
  mutate(protein_level = rnorm(n(), mean = 50, sd = 15)) %>%
  mutate(protein_level = ifelse(runif(n()) < 0.05, NA, protein_level)) %>% 
  mutate(protein_level = paste0("val_", round(protein_level, 2)))

# Dataset 3: Behandeluitkomst, verder gewerkt tot groeipercentage
outcomes <- data.frame(
  id = 1:n_patients,
  response = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), n_patients, replace = TRUE),
  hosp_name = "LUMC-Oncology-Center"
) %>%
  rename("pat_id" ="id") %>%
  left_join(patients, by="pat_id") %>%
  left_join(biomarkers, by="pat_id") %>%
  separate(col = (age_gender),
           sep = "_",
           into = c("Age", "Gender"),
           convert = T) %>%
  separate(col=(protein_level),
           sep="_",
           into=c("val","protein_level"),
           convert = T) %>%
  select(-(val)) %>%
  separate(col=(week),
           sep="_",
           into=c("week","weeknummer"),
           convert = T) %>%
  select(-(week)) %>%
  mutate(location_code=case_when(location_code == "LOC-A" ~ "A",
                                 location_code == "LOC-B" ~ "B",
                                 location_code == "LOC-C" ~ "C")) %>%
  drop_na() %>%
  group_by(hosp_name,mutation_status,weeknummer) %>%
  filter(weeknummer %in% c(8,0)) %>%
  summarise(mean_Protein_level = round(mean(protein_level), digits = 2),
            SD_Protein_level = round(sd(protein_level), digits = 2)) %>%
  pivot_wider(names_from = weeknummer,
              values_from = c(mean_Protein_level,SD_Protein_level)) %>%
  group_by(mutation_status) %>%
  mutate(groeipercentage=round((mean_Protein_level_8-mean_Protein_level_0)/mean_Protein_level_0*100, digits = 1)) %>%
  arrange(groeipercentage)

# data om verder te werken voor data visualisatie
# Biomarker verloop trend
linegrafiek <- data.frame(
  id = 1:n_patients,
  response = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), n_patients, replace = TRUE),
  hosp_name = "LUMC-Oncology-Center"
) %>%
  rename("pat_id" ="id") %>%
  left_join(patients, by="pat_id") %>%
  left_join(biomarkers, by="pat_id") %>%
  separate(col = (age_gender),
           sep = "_",
           into = c("Age", "Gender"),
           convert = T) %>%
  separate(col=(protein_level),
           sep="_",
           into=c("val","protein_level"),
           convert = T) %>%
  select(-(val)) %>%
  separate(col=(week),
           sep="_",
           into=c("week","weeknummer"),
           convert = T) %>%
  select(-(week)) %>%
  mutate(location_code=case_when(location_code == "LOC-A" ~ "A",
                                 location_code == "LOC-B" ~ "B",
                                 location_code == "LOC-C" ~ "C")) %>%
  drop_na() %>%
  group_by(hosp_name,mutation_status,weeknummer) %>%
  summarise(mean_Protein_level = round(mean(protein_level), digits = 2),
            SD_Protein_level = round(sd(protein_level), digits = 2)) %>%
  ggplot(mapping=aes(x=weeknummer,
                     y=mean_Protein_level,
                     color=mutation_status)) +
  geom_line(linewidth=0.5) +
  geom_point(alpha=0.8, size=1, color="black") +
  theme_classic() +
  scale_color_brewer(palette="Set2") +
  theme(plot.title = element_text(hjust=0.5,
                                   face="bold",
                                   family="sans",
                                  size=16),
        plot.subtitle=element_text(hjust=0.5,
                                    face="bold",
                                    family="sans",
                                    size=14),
        axis.text=element_text(face="bold",
                                family="sans",
                                size=14),
        axis.title=element_text(face="bold",
                               family="sans",
                               size=14),
        legend.title=element_text(size=12,
                                   face="bold",
                                   family="sans"),
        legend.text=element_text(size=12,
                                  face="bold",
                                  family="sans")) +
  labs(title="Protein level over time",
       subtitle = "Different types of lungcancer mutations",
       x="weeknummer",
       y="Protein level",
       color="Mutation status")

# Respons verdeling
bargrafiek <- data.frame(
  id = 1:n_patients,
  response = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), n_patients, replace = TRUE),
  hosp_name = "LUMC-Oncology-Center"
) %>%
  rename("pat_id" ="id") %>%
  left_join(patients, by="pat_id") %>%
  left_join(biomarkers, by="pat_id") %>%
  separate(col = (age_gender),
           sep = "_",
           into = c("Age", "Gender"),
           convert = T) %>%
  separate(col=(protein_level),
           sep="_",
           into=c("val","protein_level"),
           convert = T) %>%
  select(-(val)) %>%
  separate(col=(week),
           sep="_",
           into=c("week","weeknummer"),
           convert = T) %>%
  select(-(week)) %>%
  mutate(location_code=case_when(location_code == "LOC-A" ~ "A",
                                 location_code == "LOC-B" ~ "B",
                                 location_code == "LOC-C" ~ "C")) %>%
  drop_na() %>%
  group_by(hosp_name,mutation_status,weeknummer, response) %>%
  summarise(mean_Protein_level = round(mean(protein_level), digits = 2),
            SD_Protein_level = round(sd(protein_level), digits = 2)) %>%
  ggplot(mapping=aes(x=as.factor(weeknummer),
                     y=mean_Protein_level,
                     fill=response)) +
  geom_col(width=0.5) +
  theme_classic() +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(hjust=0.5,
                                  face="bold",
                                  family="sans",
                                  size=16),
        plot.subtitle=element_text(hjust=0.5,
                                   face="bold",
                                   family="sans",
                                   size=14),
        axis.text=element_text(face="bold",
                               family="sans",
                               size=14),
        axis.title=element_text(face="bold",
                                family="sans",
                                size=14),
        legend.title=element_text(size=12,
                                  face="bold",
                                  family="sans"),
        legend.text=element_text(size=12,
                                 face="bold",
                                 family="sans")) +
  labs(title="Protein level over time",
       subtitle = "Different types of lungcancer mutations",
       x="weeknummer",
       y="Protein level",
       fill="Respons status")

# Correlatie check
boxplot <- data.frame(
  id = 1:n_patients,
  response = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), n_patients, replace = TRUE),
  hosp_name = "LUMC-Oncology-Center"
) %>%
  rename("pat_id" ="id") %>%
  left_join(patients, by="pat_id") %>%
  left_join(biomarkers, by="pat_id") %>%
  separate(col = (age_gender),
           sep = "_",
           into = c("Age", "Gender"),
           convert = T) %>%
  separate(col=(protein_level),
           sep="_",
           into=c("val","protein_level"),
           convert = T) %>%
  select(-(val)) %>%
  separate(col=(week),
           sep="_",
           into=c("week","weeknummer"),
           convert = T) %>%
  select(-(week)) %>%
  mutate(location_code=case_when(location_code == "LOC-A" ~ "A",
                                 location_code == "LOC-B" ~ "B",
                                 location_code == "LOC-C" ~ "C")) %>%
  drop_na() %>%
  group_by(hosp_name,mutation_status,weeknummer, response) %>%
  summarise(mean_Protein_level = round(mean(protein_level), digits = 2),
            SD_Protein_level = round(sd(protein_level), digits = 2)) %>% 
  pivot_wider(names_from = weeknummer,
              values_from = c(mean_Protein_level,SD_Protein_level)) %>%
  group_by(mutation_status) %>%
  mutate(groeipercentage=round((mean_Protein_level_8-mean_Protein_level_0)/mean_Protein_level_0*100, digits = 1)) %>%
  arrange(groeipercentage) %>%
  ggplot(mapping=aes(x=as.factor(mutation_status),
                     y=groeipercentage,
                     fill=mutation_status)) +
  geom_col(width=0.5) +
  geom_jitter(alpha=1.0, color="darkred") +
  theme_classic() +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(hjust=0.5,
                                  face="bold",
                                  family="sans",
                                  size=16),
        plot.subtitle=element_text(hjust=0.5,
                                   face="bold",
                                   family="sans",
                                   size=14),
        axis.text=element_text(face="bold",
                               family="sans",
                               size=14),
        axis.title=element_text(face="bold",
                                family="sans",
                                size=14),
        legend.title=element_text(size=12,
                                  face="bold",
                                  family="sans"),
        legend.text=element_text(size=12,
                                 face="bold",
                                 family="sans")) +
  labs(title="Protein level over time",
       subtitle ="Different types of lungcancer mutations",
       x="Mutatie status",
       y="Groeipercentage",
       fill="Mutatie status")

