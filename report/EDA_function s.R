




# Importing preprocessed data

# Make new data for EDA
eda_dat <- FIHI_sub3 %>%
  dplyr::select(-ends_with("_changed"), -respondent_id)



# Exploratory Data Analysis

## Summary Statistics


# Tracking follow-up questions to fix incorrect missing data instances:
# Problem: Respondents who gave a response not in favor of the follow-up 
# question will incorrectly count as missing data for the follow-up questions
# - employment_type (Q3): filter out NO for employed
# - weekly_work_hrs (Q4): filter out NO for employed
# - transport_reliability (Q13): filter out Not applicable for mode_transport
# Problem with Q13 though, the same number carried over from Q12 instead of 
# falling short of Not applicable respondents
# - 
# - spent_night_elsewhere (Q22): filter out 'Yes' for permanent address
# - 

summ_stats <- data.frame() # initialize summary statistics container
nc <- NCOL(eda_dat)
var_name <- variable.names(eda_dat)
nr_vec <- vector(length = nc) # a vector of number of rows for the summary df of 
# each variable to aid in creating a striped formatted table.

# Key-value pairs matching follow-up question variables to their main questions
main_var <- c("employment_type"="employment", "weekly_work_hrs"="employment", "transport_reliability"="mode_transport", "spent_night_elsewhere"="permanent_address")
# Key-value pairs matching follow-up question variables to tokens for filtering the true sample sizes
filter_token <- c("employment_type"="No", "weekly_work_hrs"="No", "transport_reliability"="Not applicable", "spent_night_elsewhere"="Yes")

for (i in 1:nc) {

  # Computing the summary statistics for the ith variable
  if(var_name[i] %in% names(main_var)) {
    # Get summary stats for variables from follow-up survey questions.
    # This part is necessary to account for the true sample sizes and the true
    # missing values for such variables.
    summ <- eda_dat %>% 
              filter(!!as.name(main_var[var_name[i]]) != filter_token[var_name[i]]) %>%
              select(!!as.name(var_name[i]))%>%
              group_by(!!as.name(var_name[i])) %>%
              summarise(freq = n()) %>%
              mutate(pct = freq/sum(freq),
                     pct = percent(pct, 0.01))
  } else {
    # Get summary stats for variables from main survey questions
    summ <- eda_dat %>% select(!!as.name(var_name[i]))%>%
              group_by(!!as.name(var_name[i])) %>%
              summarise(freq = n()) %>%
              mutate(pct = freq/sum(freq),
                     pct = percent(pct, 0.01))
  }
  
  # Add extra column indicating the particular variable for which the summary was generated
  var_col <- c(var_name[i], rep("", (NROW(summ)-1))) 
  summ <- cbind(var_col, summ) 
  
  # Renaming the columns
  names(summ) <- c("Variable", "Levels", "Obervations", "Percentage")
  
  # Storing summary statistics for each variable
  summ_stats <- rbind(summ_stats, summ)
  nr_vec[i] <-  length(var_col) # store the number of rows for each variable in the df.
}

# To help highlight each variable information as a block
get_stripe_index <- function(indices)
{
  index_vec <- NULL
 
  for(i in seq_along(indices)) {
    
    if(i%%2 == 0) next # skip the even values
    
    end_index <- sum(indices[1:i])
    
    start_index <- end_index - (indices[i]-1)
    
    index_vec <- c(index_vec, start_index:end_index)
    
  }
  return(index_vec)
}

kable(summ_stats, align = "llll", longtable=T, booktabs=T, linesep="",
      caption = " A summary of variables of interest") %>%
  kable_paper("hover", full_width = F)%>%
  kable_styling(font_size = 9, 
                latex_options = c("HOLD_position", "striped", "repeat_header"),
                stripe_index = get_stripe_index(nr_vec)
                )
 


# # Make data for plotting
# na_to_missing <- function(x) 
# {
#   return(ifelse(is.na(x), 'Missing', x ))
# }
# 
# FIHI_sub4 <- FIHI_sub3[, -1] %>% 
#   mutate(across(everything(), na_to_missing))
#   

# For creating bar graphs of the response variables
mybarplot <- function(var, palette="Set2",
                      xlab="", ylab="",title="",data='') 
{
  # palette=c("Set2","Set3","Accent","Set1","Paired","Dark2")
  ylab = ifelse(ylab=="", "Number of Respondents", ylab)
  
  if(!(class(data) %in% c('data.frame','tbl','tbl_df'))) data <- FIHI_sub3

   if(ncol(data) == 4)
   {
     # A plotting data was provided
     plotdf <- data
   } else {
     # prepare data frame for plotting
      plotdf <- data %>%
      group_by({{ var }}) %>%
      summarise(n=n()) %>%
      mutate(pct = n/sum(n),
             lbl = percent(pct)) 
   }
    
  # create the plot
  return(
    ggplot(plotdf, aes({{ var }},n, fill={{ var }})) +
    geom_bar(stat = "identity",position = "dodge")  +
    geom_text(aes(label = n), size=3, position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette=palette) +
    labs(x=xlab, y=ylab, title=title) +
    theme_classic() +
    theme(legend.position = "none")
  )
}
# "Ever hungry but didn't eat?"
# mybarplot(FI_q31)
# mybarplot(spent_night_elsewhere, data = FIHI_sub3)

