#Function that returns p-values from randomization/permutation test
require(dplyr)
require(tidyr)

#Set number of randomizations
nsteps <- 1000

#Read the dataframe - the example data used below is available at: https://github.com/JoachimGoedhart/PlotsOfData
df <- read.csv("Data_wide_example.csv")

#Make it tidy
df_tidy <- df %>% gather(Condition, Value)

#Median and mean values as reference/observed values
df_obs_stats <- df_tidy %>% group_by(Condition) %>% summarise(mean=mean(Value, na.rm=TRUE), median=median(Value, na.rm=TRUE))


#Get the 'Control' condition from the shiny interface (disabled when running as independent script)
#  control_condition <- as.character(input$zero)

#Get a list with control values, assumes that there is a condition named "Control"
Controls <- df_tidy %>% filter(Condition=="Control", !is.na(Value)) %>% select("Value") %>% unlist(use.names = FALSE)

#generate a df with differences from the observations
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
      df_control <- df_permutated %>% slice(1: number_controls) %>% summarise(new_control_mean=mean(Value), new_control_median=median(Value))
      df_sample <- df_permutated %>% slice((number_controls+1):length(Value))%>% summarise(new_sample_mean=mean(Value), new_sample_median=median(Value))
      df_diff <- full_join(df_control, df_sample,by="Condition")
      
      df_new_stats <- bind_rows(df_new_stats, df_diff)

}

#Calculate the difference in mean and median (sample-control) for all the calculated new stats
df_all_diffs <- df_new_stats %>% mutate(new_diff_mean=new_sample_mean-new_control_mean,
                                        new_diff_median=new_sample_median-new_control_median)


#Add the observed stats to stats from permutated df
df_all_diffs <- full_join(df_all_diffs, df_obs_stats, by="Condition")

#Determine the occurences where the permutated difference is more extreme than the observed difference (use absolute values for both for two-tailed test)
df_p_mean <- df_all_diffs %>% group_by(Condition) %>% mutate(count = if_else(abs(new_diff_mean) >= abs(mean), 1, 0)) %>% summarise(p_mean=mean(count))
df_p_median <- df_all_diffs %>% group_by(Condition) %>% mutate(count1 = if_else(abs(new_diff_median) >= abs(median), 1, 0)) %>% summarise(p_median=mean(count1))

def_p <- as.data.frame(full_join(df_p_mean, df_p_median,by="Condition"))

#Replace p-values of zero with <0.001 (0 is theoretically not possible, but an upper bound can estimated, which is 1/nsteps = 1/1000)
def_p[def_p==0]<-"<0.001"

def_p
