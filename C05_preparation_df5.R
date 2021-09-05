#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

## variabile TYP_CAMP ##

## compute distribution
df_5_camp_cat_type_camp <- df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_5_camp_cat_type_camp

plot_df5_TYP_CAMP_distribution=ggplot(df5_TYP_CAMP_distribution, aes(TYP_CAMP,TOT_TYP_CAMPS))+
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(y="Numero di campagne attivate", x="Tipologia di Campagna")+
  ggtitle("Tipologia di campagna" )


#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)
