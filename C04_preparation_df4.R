#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

##### consideriamo solo la categoria persone #####

id_persone <- as.data.frame(df_1_persone$ID_CLI)
colnames(id_persone) <- "ID_CLI"

df4_persone <- merge(id_persone, df_4_cli_privacy_clean, by="ID_CLI")

#### EXPLORE COLUMNS of df_4 ####

# EXPLORE the df_4_cli_privacy_clean relevant variables

### variabile Privacy 1 ###

## compute distribution
df_4_p_flag1_distrib <- df4_persone %>%
  group_by(FLAG_PRIVACY_1) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_p_flag1_distrib

## plot distribution
plot_df_4_p_flag1_distrib <- (
  ggplot(data=df_4_p_flag1_distrib
         , aes(x=FLAG_PRIVACY_1, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_p_flag1_distrib

### Variabile Privacy 2 ###

## compute distribution
df_4_cli_p_flag2_distrib <- df4_persone %>%
  group_by(FLAG_PRIVACY_2) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_cli_p_flag2_distrib

## plot distribution
plot_df_4_p_flag2_distrib <- (
  ggplot(data=df_4_p_flag2_distrib
         , aes(x=FLAG_PRIVACY_2, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_p_flag2_distrib

### variabile direct_marketing ###

## compute distribution
df_4_p_flag_mkt_distrib <- df4_persone %>%
  group_by(FLAG_DIRECT_MKT) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_p_flag_mkt_distrib

## plot distribution
plot_df_4_p_flag_mkt_distrib <- (
  ggplot(data=df_4_p_flag_mkt_distrib
         , aes(x=FLAG_DIRECT_MKT, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_p_flag_mkt_distrib


#### FINAL REVIEW df_4_clean ####

str(df4_persone)
summary(df4_persone)