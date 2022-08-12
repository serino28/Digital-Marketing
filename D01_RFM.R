install.packages("rfm")
install.packages("descriptr")
library(rfm)
library(magrittr)
library(dplyr)
library(lubridate)
### Create DataFrame for RFM Analysis ###

df_RFM <- df_7_tic_clean_final[df_7_tic_clean_final$DIREZIONE == 1, ]
df_RFM <- df_RFM[c(1,2,7,11)]

df_RFM_final <- df_RFM %>%
  group_by(ID_CLI, TIC_DATE) %>%
  summarize(REVENUE = sum(IMPORTO_LORDO)) %>%
  ungroup() %>%
  as.data.frame()

### RFM TABLE ##

analysis_date <- lubridate::as_date("2019-04-30", tz = "UTC")
rfm_result <- rfm_table_order(df_RFM_final, ID_CLI, TIC_DATE, REVENUE, analysis_date)
rfm_result

### PLOT THE RESULTS ###

rfm_heatmap(rfm_result)
rfm_bar_chart(rfm_result)
rfm_histograms(rfm_result)
rfm_order_dist(rfm_result)

## SCATTER PLOTS ##
rfm_rm_plot(rfm_result)
rfm_fm_plot(rfm_result)
rfm_rf_plot(rfm_result)

### SEGMENTS ###

segment_names <- c(  "Champions"
                   , "Loyal Customers"
                   , "Potential Loyalist"
                   , "New Customers"
                   , "Promising"
                   , "Need Attention"
                   , "About To Sleep"
                   , "At Risk"
                   , "Can't Lose Them"
                   , "hibernating"
                   , "Lost")

# We set the upper and lower bounds for recency, frequency, and monetary for the above segments
recency_lower   <- c(4, 2, 3, 4, 3, 3, 2, 1, 1, 2, 1)
recency_upper   <- c(5, 4, 5, 5, 4, 4, 3, 2, 1, 3, 1)
frequency_lower <- c(4, 3, 1, 1, 1, 3, 1, 2, 4, 2, 1)
frequency_upper <- c(5, 4, 3, 1, 1, 4, 2, 5, 5, 3, 1)
monetary_lower  <- c(4, 4, 1, 1, 1, 3, 1, 2, 4, 2, 1)
monetary_upper  <- c(5, 5, 3, 1, 1, 4, 2, 5, 5, 3, 1)

# We use the segments and the bounds we previously established to group our users into different segments
segments <- rfm_segment(rfm_result,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)


### TABULATE SEGMENTS ###


segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

## average recency ##
rfm_plot_median_recency(segments)

## average frequency ##
rfm_plot_median_frequency(segments)

## average monetary value ##
rfm_plot_median_monetary(segments)

### Calcolo della spesa media  per categoria ###

calcolo_prezzo_medio <- segments %>%
  group_by(segment) %>%
  summarize(REVENUE = mean(amount)) %>%
  ungroup() %>%
  as.data.frame()

plot_spesa_media <- (
  ggplot(data=calcolo_prezzo_medio
         ,aes(x=REVENUE, y = segment)) +
    geom_bar(stat="identity", fill="red3") +
    theme_minimal() + ggtitle("Spesa Media per Tipologia di Cliente")
)

plot_spesa_media  

### Calcolo del numeri di acquisti medi per tipologia di cliente ###


calcolo_numeri_acquisti_medio <- segments %>%
  group_by(segment) %>%
  summarize(Numero_Acquisti_medio = mean(transaction_count)) %>%
  ungroup() %>%
  as.data.frame()

plot_numero_acquisti_media <- (
  ggplot(data=calcolo_numeri_acquisti_medio
         ,aes(x=Numero_Acquisti_medio , y = segment)) +
    geom_bar(stat="identity", fill="red3") +
    theme_minimal() + ggtitle("Numero di Acquisti medio per Tipologia di Cliente")
)

plot_numero_acquisti_media

### Calcolo dello scontrino medio ### 


calcolo_scontrino_medio <- segments %>%
  group_by(segment) %>%
  summarize(Scontrino_Medio = sum(amount)/sum(transaction_count)) %>%
  ungroup() %>%
  as.data.frame()

plot_scontrino_medio <- (
  ggplot(data=calcolo_scontrino_medio
         ,aes(x=Scontrino_Medio, y = segment)) +
    geom_bar(stat="identity", fill="red3") +
    theme_minimal() + ggtitle("Scontrino medio per categoria")
)

plot_scontrino_medio 




