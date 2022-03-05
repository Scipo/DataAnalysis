#install.packages("tidyverse")
#install.packages("broom")
#install.packages("janitor")
#install.packages("scales")

library("janitor")
library("tidyverse")
library("scales")


#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy data sets (csv files) here
df1<-read.csv("D:/GoogleProject/csv/202101-divvy-tripdata.csv")
df2<-read.csv("D:/GoogleProject/csv/202102-divvy-tripdata.csv")
df3<-read.csv("D:/GoogleProject/csv/202103-divvy-tripdata.csv")
df4<-read.csv("D:/GoogleProject/csv/202104-divvy-tripdata.csv")
df5<-read.csv("D:/GoogleProject/csv/202105-divvy-tripdata.csv")
df6<-read.csv("D:/GoogleProject/csv/202106-divvy-tripdata.csv")
df7<-read.csv("D:/GoogleProject/csv/202107-divvy-tripdata.csv")
df8<-read.csv("D:/GoogleProject/csv/202108-divvy-tripdata.csv")
df9<-read.csv("D:/GoogleProject/csv/202109-divvy-tripdata.csv")
df10<-read.csv("D:/GoogleProject/csv/202110-divvy-tripdata.csv")
df11<-read.csv("D:/GoogleProject/csv/202111-divvy-tripdata.csv")
df12<-read.csv("D:/GoogleProject/csv/202112-divvy-tripdata.csv")
#nrow(df1)

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
one_df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
#View(one_df)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
#colnames(all_trips)  #List of column names
#nrow(all_trips)  #How many rows are in data frame?
#dim(all_trips)  #Dimensions of the data frame?
#head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
#str(all_trips)  #See list of columns and data types (numeric, character, etc)
#summary(all_trips)  #Statistical summary of data. Mainly for numerics

#Removing any empty rows or columns 
one_df <- remove_empty(one_df, which=c("cols"))
one_df <- remove_empty(one_df, which=c("rows"))

# Data frame dimensions
dim(one_df)

# Data frame summary
glimpse(one_df)
summary(one_df)

# 3.1 count() returns unique values of the variable passed
one_df %>% 
  count(start_station_name)

# 3.2 omitting NA values in the entire data frame
new_one_df <- na.omit(one_df)

# dimensions and summary of new data frame
dim(new_one_df)
dim(one_df)
glimpse(new_one_df)
summary(new_one_df)

# 3.3 Checking for missing values
count(filter(new_one_df, start_station_name==''),start_station_name, member_casual,sort=TRUE)

new_one_df$started_at <- as.POSIXct(new_one_df$started_at, "%Y-%m-%d %H:%M:%S")
new_one_df$ended_at <- as.POSIXct(new_one_df$ended_at, "%Y-%m-%d %H:%M:%S")

glimpse(new_one_df)

# 3.4 Removing duplicates
print(nrow(new_one_df))
new_one_df_no_dups <- new_one_df[!duplicated(new_one_df$ride_id), ]
print(nrow(new_one_df_no_dups))

#3.5 Data manipulation(new column added)
#3.5.1 calculating riding length 
riding_len <- new_one_df_no_dups
riding_len <- riding_len %>% mutate(riding_length = as.numeric(ended_at - started_at)/60)
#3.5.2 year and month
riding_len <- riding_len %>% 
  mutate(year_month=paste(strftime(riding_len$started_at, "%Y"), "-",
                                                   strftime(riding_len$started_at, "%m"),
                                                   paste("(", strftime(riding_len$started_at, "%b"),")",sep="")))

#3.5.3 Weekday
riding_len <- riding_len %>% 
  mutate(weekday=strftime(riding_len$ended_at, "%a"))
#3.5.4 start_hour
riding_len <- riding_len %>% 
  mutate(start_hour=strftime(riding_len$ended_at, format = "%H",tz = "UTC"))

unique(riding_len$start_hour)

# Summary of resultant clean data frame
glimpse(riding_len)
View(riding_len)


#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# 4.1 comparing the number of members and casual riders

df <- riding_len
df %>% 
  group_by(member_casual) %>% 
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100)

ggplot(df, aes(member_casual, fill=member_casual))+
  geom_bar()+
  labs(title="Annual Members vs Casual members")+
  scale_y_continuous(labels=comma)

# 4.2 Months with annual and casual member destribution
df %>% 
  group_by(year_month) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
  arrange(year_month)

View(df %>% group_by(year_month) %>%
        summarize(count=length(ride_id),
                  percentage_of_total=(length(ride_id)/nrow(df))*100,
                  members_count=sum(member_casual=="member"),
                  members_percent=(sum(member_casual=="member")/length(ride_id))*100,
                  casual_count=sum(member_casual=="casual"),
                  casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
        arrange(year_month))

ggplot(df, aes(year_month, fill=member_casual))+
  geom_bar()+
  coord_flip()+
  labs(title="Months with annual and casual member destribution")+
  scale_y_continuous(labels=comma)

# 4.3 Starting hours of the ride 
df %>% 
  group_by(start_hour) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
  arrange(start_hour)

View(df %>% 
       group_by(start_hour) %>%
       summarize(count=length(ride_id),
                 percentage_of_total=(length(ride_id)/nrow(df))*100,
                 members_count=sum(member_casual=="member"),
                 members_percent=(sum(member_casual=="member")/length(ride_id))*100,
                 casual_count=sum(member_casual=="casual"),
                 casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
       arrange(start_hour))

ggplot(df, aes(start_hour, fill=member_casual))+
  geom_bar()+
  facet_wrap(~weekday)+
  labs(title="Starting hours of the ride")+
  scale_y_continuous(labels=comma)

# 4.4 Days of the week
df %>% 
  group_by(weekday) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100)

View(df %>% 
       group_by(weekday) %>%
       summarize(count=length(ride_id),
                 percentage_of_total=(length(ride_id)/nrow(df))*100,
                 members_count=sum(member_casual=="member"),
                 members_percent=(sum(member_casual=="member")/length(ride_id))*100,
                 casual_count=sum(member_casual=="casual"),
                 casual_percent=(sum(member_casual=="casual")/length(ride_id))*100)
)
 
ggplot(df, aes(weekday, fill=member_casual))+
  geom_bar()+
  labs(title="Days of the week")+
  scale_y_continuous(labels=comma)
