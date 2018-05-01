library(sparklyr)
library(dplyr)
library(SparkR)
library(caret)
sc <- spark_connect(master = "local")

data <- data.table::fread("/home/evgeny/clust_info")
test <- data.frame(data$dist,data$speed,data$classes_speed)
colnames(test) <- c("dist", "speed", "classes_speed")
test_c <- copy_to(sc, test, name = "test_clust", overwrite = T)
clust <- copy_to(sc, data_new, name = "clust",overwrite = TRUE)
spark_read_csv(sc, name = "clust_info", path = "clust_info")
clust_tbl <- tbl(sc, "test_c")

fit_rf <- ml_random_forest(test_c, response = "classes_speed", features = colnames(test_c)[1:2], type = "classification")
ml_tree_feature_importance(sc = sc, model = fit_rf)
ml_formula <- formula(test_c$classes_speed ~ test_c$dist + test_c$speed)

test_rf <- sdf_predict( test_c,fit_rf)
test_rf_f1 <- test_rf %>%
  ml_classification_eval(label = "classes_speed",
                         predicted_lbl = "prediction",
                         metric = "f1")
rf_test_df <- collect(test_rf)

confusionMatrix(data = rf_test_df$predicted_label, reference = rf_test_df$classes_speed)

kmeans_model <- test_c %>% select(dist, speed) %>% ml_kmeans(centers = 3) %>% collect

ml_log <- ml_logistic_regression(test_c, response = "classes_speed", features = c("dist", "speed"))
log_pred <- sdf_predict(test_c, ml_nb) 
log_pred_df <- collect(nb_pred)
confusionMatrix(data = nb_pred_df$predicted_label, reference = nb_pred_df$classes_speed)


