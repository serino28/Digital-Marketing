#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE, week_start=1)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 6) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO AND average SCONTO by COD_REPARTO

## compute aggregate
df7_dist_importosconto2 <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, COD_REPARTO) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto2 <- df7_dist_importosconto2 %>%
  group_by(COD_REPARTO) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto2

## plot aggregate
plot_df7_dist_importo2 <- (
  ggplot(data=df7_dist_importosconto2 %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=COD_REPARTO, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo2

## plot aggregate
plot_df7_dist_sconto2 <- (
  ggplot(data=df7_dist_importosconto2 %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=COD_REPARTO, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto2

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

table(df_7_tic_clean_final$ID_ARTICOLO)
barplot(table(df_7_tic_clean_final$ID_ARTICOLO),  main = "ID_ARTICOLO distribution")


# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

## compute aggregate
df7_dist_importosconto3 <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, ID_CLI) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto3 <- df7_dist_importosconto3 %>%
  group_by(ID_CLI) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto3

## plot aggregate
plot_df7_dist_importo3 <- (
  ggplot(data = df7_dist_importosconto3 %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=ID_CLI, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="black", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo3

## plot aggregate
plot_df7_dist_sconto3 <- (
  ggplot(data=df7_dist_importosconto3 %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=ID_CLI, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="black", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto3

# compute the distribution of customers by number of purchases (as described in the slides)
#drop refund tickets
df7_purchase <- df_7_tic_clean_final[df_7_tic_clean_final$DIREZIONE == 1,]
df7_purchase

#compute aggregate
df7_Number_Of_Purchase <- df7_purchase %>%
  group_by(ID_CLI) %>%
  summarize(NUMBER_OF_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame()
df7_Number_Of_Purchase 



one_or_more <- c(sum(df7_Number_Of_Purchase$NUMBER_OF_PURCHASE >= 1, na.rm=TRUE))
two_or_more <- c(sum(df7_Number_Of_Purchase$NUMBER_OF_PURCHASE >= 2, na.rm=TRUE))
three_or_more <- c(sum(df7_Number_Of_Purchase$NUMBER_OF_PURCHASE >= 3, na.rm=TRUE))
four_or_more <- c(sum(df7_Number_Of_Purchase$NUMBER_OF_PURCHASE >= 4, na.rm=TRUE))
five_or_more <- c(sum(df7_Number_Of_Purchase$NUMBER_OF_PURCHASE >= 5, na.rm=TRUE))
six_or_more <- c(sum(df7_Number_Of_Purchase$NUMBER_OF_PURCHASE >= 6, na.rm=TRUE))
Count <- c(one_or_more, two_or_more, three_or_more, four_or_more, five_or_more, six_or_more )


Number_of_Purchase<- c("one_or_more", "two_or_more", "three_or_more", "four_or_more", "five_or_more", "six_or_more" )

df7_NOP_Final <- data.frame(Number_of_Purchase, Count)

df7_NOP_Final

#plot aggregate

plot_df7_NOP_Final <- (
  ggplot(data=df7_NOP_Final
         , aes(x=Number_of_Purchase, y=Count)) +
    geom_bar(stat="identity", fill="#FF6E77") +
    theme_minimal()
)

plot_df7_NOP_Final

# compute the days for next purchase curve (as described in the slides)

df_7_tic_Purchase_curve <- df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%
  summarize(AVG_Repurchase = floor(sum(difftime(TIC_DATE, 
                                          lag(TIC_DATE), units = "days"), 
                                 na.rm=TRUE)/(n()-1)))%>%
  ungroup() %>%
  group_by(AVG_Repurchase) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df_7_tic_Purchase_curve

plot_df_7_tic_Purchase_curve <- (
  ggplot(data=df_7_tic_Purchase_curve %>%
           filter(AVG_Repurchase < 14)
         , aes(x=AVG_Repurchase, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df_7_tic_Purchase_curve

### DAYS_TO_OPEN vs CUMULATE PERCENT ###

## compute aggregate
df_7_tic_Purchase_curve_vs_cumulate <- df_7_tic_Purchase_curve %>%
  arrange(AVG_Repurchase) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df_7_tic_Purchase_curve_vs_cumulate <- (
  ggplot(data=df_7_tic_Purchase_curve_vs_cumulate %>%
           filter(AVG_Repurchase < 14)
         , aes(x=AVG_Repurchase, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    theme_minimal()
)

plot_df_7_tic_Purchase_curve_vs_cumulate

#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)

