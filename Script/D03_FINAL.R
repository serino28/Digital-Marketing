Final_DF <- churn_pred

table(Final_DF$LAST_STATUS_FID_PRED)


segments <- segments %>% 
  rename(
    ID_CLI = customer_id
  )

segments_cut <- segments[,c(1,2)]

### Unione dataset Churn propensity e RFM ###

df_final_analysis <- merge(Final_DF, segments_cut, by = "ID_CLI")

### Creazione dataset per ogni segmento ###

df_need_attention <- df_final_analysis[df_final_analysis$segment == "Need Attention", ]

table(df_need_attention$LAST_STATUS_FID_PRED)

df_Champions <- df_final_analysis[df_final_analysis$segment == "Champions", ]

table(df_Champions$LAST_STATUS_FID_PRED)

df_loyal <- df_final_analysis[df_final_analysis$segment == "Loyal Customers", ]

table(df_loyal$LAST_STATUS_FID_PRED)

df_potential_loyal <- df_final_analysis[df_final_analysis$segment == "Potential Loyalist", ]

df_at_risk <- df_final_analysis[df_final_analysis$segment == "At Risk", ]

table(df_at_risk$LAST_STATUS_FID_PRED)

df_sleep <- df_final_analysis[df_final_analysis$segment == "About To Sleep", ]

df_others <- df_final_analysis[df_final_analysis$segment == "Others", ]

df_lost <- df_final_analysis[df_final_analysis$segment == "Lost", ]


### Unione dataset churn propensity e cltv ### 


results_final <- results_final %>% 
  rename(
    ID_CLI = Id
  )

df_final_analysis_CLTV <- merge(Final_DF, results_final, by = "ID_CLI")


### Unione dataset rfm e cltv ###

df_final_analysis_RFM_CLTV <- merge(segments, results_final, by = "ID_CLI")


