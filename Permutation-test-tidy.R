#Function that returns p-values from randomization/permuation test

#Read the dataframe
df <- read.csv("Data_wide_example.csv")

#Make it tidy
df_tidy <- df %>% gather(Condition, Value)

#Set number of randomizations
nsteps <- 1000

#Mean values as reference
df_obs_stats <- df_tidy %>% group_by(Condition) %>% summarise(mean=mean(Value, na.rm=TRUE), median=median(Value, na.rm=TRUE))


#Get all Control values from shiny interface
#  control_condition <- as.character(input$zero)

#Get a list with control values
Controls <- df_tidy %>% filter(Condition=="Control", !is.na(Value)) %>% select("Value") %>% unlist(use.names = FALSE)

#generate a df with differences
df_obs_stats$mean <- df_obs_stats$mean - mean(Controls)
df_obs_stats$median <- df_obs_stats$median - median(Controls)

#Determine number of observations in Control sample
number_controls <- length(Controls)

#Make a new dataframe with control values for each of the conditions
df_controls <- data.frame(Condition=rep(levels(factor(df_tidy$Condition)), each=number_controls), Value=Controls)

#Add the original data, generating (per condition) Control&Sample values in the column "Value".
df_combi <- bind_rows(df_controls, df_tidy) %>% filter(!is.na(Value))

df_new_stats <- data.frame()

#Perform the randomization nsteps number of times (typically 1,000x)
for (i in 1:nsteps) {
  
      #Randomize the dataframe
      df_permutated <- df_combi %>% group_by(Condition) %>% sample_frac()
      
      #Determine the (new) control mean and (new) sample mean
      df_control <- df_permutated %>% slice(1:51) %>% summarise(new_control_mean=mean(Value), new_control_median=median(Value))
      df_sample <- df_permutated %>% slice(52:length(Value))%>% summarise(new_sample_mean=mean(Value), new_sample_median=median(Value))
      df_diff <- full_join(df_control, df_sample,by="Condition")
      
      df_new_stats <- bind_rows(df_new_stats, df_diff)

}

#Calculate the difference in mean and median (sample-control) for all the calculated new stats
df_all_diffs <- df_new_stats %>% mutate(new_diff_mean=new_sample_mean-new_control_mean,
                                        new_diff_median=new_sample_median-new_control_median)


#Add the observed stats to stats from permutated df
df_all_diffs <- full_join(df_all_diffs, df_obs_stats, by="Condition")

#Determine the occurences where the permutated difference is more extreme than the observed difference
df_p_mean <- df_all_diffs %>% group_by(Condition) %>% mutate(count = if_else(abs(new_diff_mean) >= mean, 1, 0)) %>% summarise(p=mean(count))
df_p_median <- df_all_diffs %>% group_by(Condition) %>% mutate(count1 = if_else(abs(new_diff_median) >= median, 1, 0)) %>% summarise(p=mean(count1))
