
process_data <- function(data) {
  #Fill in missing values in training data
  data_mv <- COPY<-data.frame(data)
  data_mv = data_mv %>% replace_na(list(STARS = 0))

  for(i in 1:ncol(data_mv)){
    data_mv[is.na(data_mv[,i]), i] <- mean(data_mv[,i], na.rm = TRUE)
  }

  #Fill in missing values in evaluation data
  eval_data_mv <- COPY<-data.frame(eval_data)

  eval_data_mv = eval_data_mv %>% replace_na(list(STARS = 0))

  for(i in 1:ncol(eval_data_mv)){
    eval_data_mv[is.na(eval_data_mv[,i]), i] <- mean(eval_data_mv[,i], na.rm = TRUE)
  }

  #Apply scalarization to training and evaluation data
  data_mv_scaled = scale(data_mv)

  eval_data_mv_scaled = scale(eval_data_mv, center=attr(data_mv_scaled, "scaled:center"),
                            scale=attr(data_mv_scaled, "scaled:scale"))

  write.csv(data_mv,
            here("analysis","data","derived_date","wine-training-data-mv.csv")
  )
  write.csv(data_mv_scaled,
            here("analysis","data","derived_date","wine-training-data-mv-scaled.csv")
  )

  #Evaluation
  write.csv(eval_data_mv,
            here("analysis","data","derived_date","wine-evaluation-data-mv.csv")
  )
  write.csv(eval_data_mv_scaled,
            here("analysis","data","derived_date","wine-evaluation-data-mv-scaled.csv")
  )
}
