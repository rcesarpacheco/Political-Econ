library(here)
library(data.table)
library(fastDummies)
library(stargazer)
library(clipr)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(hrbrthemes)
data_images <- fread(here('Results','images_results.csv'))
data_text <- fread(here('Results','text_results.csv'))
data <- merge(data_images,data_text,by.x = 'file',by.y='id',all.x=T)
data <- dummy_cols(data,select_columns = 'label_sentiment')
data[,face_detected_treated:='No Face']
data[face_detected=='detected_face',face_detected_treated:='Face']
data[,race:=str_to_title(race)]

summary_gender <- prop.table(table(data[ gender!='',gender],data[ gender!='',source]),2)*100
summary_race <- prop.table(table(data[ race!='',race],data[ race!='',source]),2)*100
summary_sentiment <- prop.table(table(data[ label_sentiment!='',label_sentiment],data[ label_sentiment!='',source]),2)*100
summary_face <- prop.table(table(data[ face_detected_treated!='',face_detected_treated],data[ face_detected_treated!='',source]),2)*100

summary <- rbind(summary_face,summary_gender,summary_race,summary_sentiment)

clipr::write_clip(summary)       


# Gender x Sentiment
gender_table_nyt <- prop.table(table(data[source=='nyt' & gender!='',gender],data[source=='nyt'& gender!='',label_sentiment]),1)
gender_table_fn <- prop.table(table(data[source=='fn' & gender!='',gender],data[source=='fn'& gender!='',label_sentiment]),1)
gender_table <- cbind(gender_table_fn,gender_table_nyt)*100

# race x Sentiment
race_table_nyt <- prop.table(table(data[source=='nyt' & race!='',race],data[source=='nyt'& race!='',label_sentiment]),1)
race_table_fn <- prop.table(table(data[source=='fn' & race!='',race],data[source=='fn'& race!='',label_sentiment]),1)
race_table <- cbind(race_table_fn,race_table_nyt)*100

# faces x Sentiment
face_table_nyt <- prop.table(table(data[source=='nyt' & face_detected_treated!='',face_detected_treated],data[source=='nyt'& face_detected_treated!='',label_sentiment]),1)
face_table_fn <- prop.table(table(data[source=='fn' & face_detected_treated!='',face_detected_treated],data[source=='fn'& face_detected_treated!='',label_sentiment]),1)
face_table <- cbind(face_table_fn,face_table_nyt)*100
tabs <- rbind(gender_table,race_table,face_table)
clipr::write_clip(tabs)       


# some beautiful plots
data_plots <- data[,.(source,gender,race,label_sentiment,`Reply Count`,`Quote Count`,`Like Count`)]
data_plots <- melt(data_plots,measure.vars = c('gender','race','label_sentiment'),variable.name = 'Type',value.name = 'Category')
data_plots[,Category:=str_to_title(Category)]
data_plots <- data_plots[Category!='',]
data_plots[,Category:=factor(Category,c('Man','Woman','White','Black','Asian','Indian','Latino Hispanic','Middle Eastern','Negative','Neutral','Positive'))]
extract_quantiles <- function(category,column_name,data_set,media) {
  quantiles <- quantile(data_set[Category==category & source==media,eval(column_name)],c(0.25,0.75))
  quantile_max <- quantiles[2]+(quantiles[2]-quantiles[1])*1.5
  return(quantile_max)
}
list_categories <- unique(data_plots$Category)
upper_limit_fn <-max(sapply(list_categories,FUN = extract_quantiles,quote(`Reply Count`),data_plots,'fn'))
upper_limit_nyt <-max(sapply(list_categories,FUN = extract_quantiles,quote(`Reply Count`),data_plots,'nyt'))


plt1 <- ggplot(data_plots[source=='fn',], aes(x = Category, y = `Reply Count`, fill = Category)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black") +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  coord_cartesian( ylim = c(0,upper_limit_fn))+
  xlab("")
ggsave(plot=plt1,here('engagement_fn.png'),width = 14,height = 7)


plt2 <- ggplot(data_plots[source=='nyt',], aes(x = Category, y = `Reply Count`, fill = Category)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black") +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  coord_cartesian(ylim = c(0,upper_limit_nyt))+
  xlab("")
ggsave(plot=plt2,here('engagement_nyt.png'),width = 14,height = 7)


#  descriptive statistics of age

age_desc <- data %>%
  filter(!is.na(age)) %>%
  group_by(source) %>% 
  summarize(Min = min(age),
            '1st Quantile' = quantile(age, 0.25),
            Median = round(median(age),2),
            Mean = round(mean(age),2),
            '3rd Quantile' = quantile(age, 0.75),
            Max = max(age))

clipr::write_clip(age_desc)     

#  descriptive statistics of face detection

data[,face_detected_aux:=0]
data[face_detected_treated=='Face',face_detected_aux:=1]
data[,Number_faces:=sum(face_detected_aux),by=file]

faces_desc <- data %>%
  group_by(source) %>%
  summarize(Min = min(Number_faces),
            '1st Quantile' = quantile(Number_faces, 0.25),
            Median = round(median(Number_faces),2),
            Mean = round(mean(Number_faces),2),
            '3rd Quantile' = quantile(Number_faces, 0.75),
            Max = max(Number_faces))

clipr::write_clip(faces_desc)     
