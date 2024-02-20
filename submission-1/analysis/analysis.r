#analysis
# Problem 1
#   How many hospitals filed more than one report in the same year? Show your answer as a line graph of the number of hospitals over time.
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, TTR, scales, Matching, here, vars, cobalt)

source('sumbission-1/data-code/H1_HCRISv1996.R')
source('sumbission-1/data-code/H2_HCRISv2010.R')

final.hcris.v1996=read_rds('data/output/HCRIS_Data_v1996.rds')
final.hcris.v2010=read_rds('data/output/HCRIS_Data_v2010.rds')

final.hcris.v1996 = final.hcris.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)


## combine v1996 and v2010 hcris forms, and sort by provider_number/year
final.hcris=rbind(final.hcris.v1996,final.hcris.v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear)
final.hcris %>% group_by(fyear) %>% count()

# Clean data --------------------------------------------------------------

## create count of reports by hospital fiscal year
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

## create running total of reports
final.hcris =
  final.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(report_number=row_number())
## count of hospitals/provider_number by year
final.hcris %>% group_by(fyear) %>% count()

unique.hcris1 =
  final.hcris %>%
  filter(total_reports==1) %>%
  mutate(source='unique reports')

## identify hospitals with multiple reports per fiscal year
duplicate.hcris = 
  final.hcris %>%
  filter(total_reports>1) %>%
  mutate(time_diff=fy_end-fy_start)

# Count reports by year
report_counts <- (duplicate.hcris %>%
  group_by(fyear = lubridate::year(fy_start)) %>%
summarise (num_hospitals = n_distinct(provider_number)))

# Create a line graph
fig.dup.yr <- duplicate.hcris %>% ungroup() %>% group_by(fyear) %>%
  ggplot(aes(x=as.factor(fyear),y=n_distinct(provider_number))) +
  labs(
    x="Year",
    y="Hospitals With Duplicate Reports",
    title="Duplicate Reports Per Year"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()
plot(fig.dup.yr)

# Problem 2

# After removing/combining multiple reports, how many unique hospital IDs (Medicare provider numbers) exist in the data?

unique_counts <- (unique.hcris1 %>%
  group_by(fyear = lubridate::year(fy_start)) %>%
summarise (num_hospitals = n_distinct(provider_number)))

fig.unq.yr <- unique.hcris1 %>% ungroup() %>% group_by(fyear) %>%
  ggplot(aes(x=as.factor(fyear),y=n_distinct(provider_number))) +
  labs(
    x="Year",
    y="Hospitals With Unique Reports",
    title="Uniqye Reports Per Year"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()
plot(fig.unq.yr)

## calculate elapsed time between fy start and fy end for hospitals with multiple reports
duplicate.hcris = 
  duplicate.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(total_days=sum(time_diff))

## if the elapsed time within a fy sums to around 365, then just take the total of the two
## this will be the second set of hospitals in the final dataset
unique.hcris2 = 
  duplicate.hcris %>%
  filter(total_days<370) %>%
  group_by(provider_number, fyear) %>%
  mutate(hrrp_payment=if_else(is.na(hrrp_payment),0,hrrp_payment),
         hvbp_payment=if_else(is.na(hvbp_payment),0,hvbp_payment)) %>%
  summarize(beds=max(beds), tot_charges=sum(tot_charges), tot_discounts=sum(tot_discounts),
            tot_operating_exp=sum(tot_operating_exp), ip_charges=sum(ip_charges),
            icu_charges=sum(icu_charges), ancillary_charges=sum(ancillary_charges),
            tot_discharges=sum(tot_discharges), mcare_discharges=sum(mcare_discharges),
            mcaid_discharges=sum(mcaid_discharges), tot_mcare_payment=sum(tot_mcare_payment),
            secondary_mcare_payment=sum(secondary_mcare_payment), hvbp_payment=sum(hvbp_payment),
            hrrp_payment=sum(hrrp_payment), fy_start=min(fy_start), fy_end=max(fy_end),
            date_processed=max(date_processed), date_created=min(date_created), 
            street=first(street), city=first(city), state=first(state),
            zip=first(zip), county=first(county)) %>%
  mutate(source='total for year')

## identify hospitals with more than one report and with elapsed time exceeding 370 days
duplicate.hcris2 =
  duplicate.hcris %>%
  filter(total_days>=370) %>%
  mutate(max_days=max(time_diff), max_date=max(fy_end))

## identify hospitals with one report (out of multiple) that appears to cover the full year
## this will be the third set of hospitals in the final dataset
unique.hcris3 = 
  duplicate.hcris2 %>%
  filter(max_days==time_diff, time_diff>360, max_date==fy_end) %>%
  mutate(source='primary report')

## identify remaining hospitals (those with more than one report that cover more than one full year and that do
##   not appear to have one report that takes up the full year)
## these hospitals appear to have changed their fiscal years
duplicate.hcris3=anti_join(duplicate.hcris2, unique.hcris3, by=c("provider_number", "fyear"))
duplicate.hcris3 =
  duplicate.hcris3 %>%
  mutate(total_days=as.integer(total_days), time_diff=as.integer(time_diff)) %>%
  mutate_at(c("tot_charges","tot_discounts", "tot_operating_exp", "ip_charges",
              "icu_charges", "ancillary_charges", "tot_discharges", "mcare_discharges",
              "mcaid_discharges", "tot_mcare_payment", "secondary_mcare_payment",
              "hvbp_payment", "hrrp_payment"),list(~ .*(time_diff/total_days)))

## form weighted average of values for each fiscal year
unique.hcris4 = 
  duplicate.hcris3 %>%
  group_by(provider_number, fyear) %>%
  mutate(hrrp_payment=if_else(is.na(hrrp_payment),0,hrrp_payment),
         hvbp_payment=if_else(is.na(hvbp_payment),0,hvbp_payment)) %>%
  summarize(beds=max(beds), tot_charges=sum(tot_charges), tot_discounts=sum(tot_discounts),
            tot_operating_exp=sum(tot_operating_exp), ip_charges=sum(ip_charges),
            icu_charges=sum(icu_charges), ancillary_charges=sum(ancillary_charges),
            tot_discharges=sum(tot_discharges), mcare_discharges=sum(mcare_discharges),
            mcaid_discharges=sum(mcaid_discharges), tot_mcare_payment=sum(tot_mcare_payment),
            secondary_mcare_payment=sum(secondary_mcare_payment), hvbp_payment=sum(hvbp_payment),
            hrrp_payment=sum(hrrp_payment), fy_start=min(fy_start), fy_end=max(fy_end),
            date_processed=max(date_processed), date_created=min(date_created), 
            street=first(street), city=first(city), state=first(state),
            zip=first(zip), county=first(county)) %>%
  mutate(source='weighted_average')
  


# Save final data ---------------------------------------------------------

final.hcris.data=rbind(unique.hcris1, unique.hcris2, unique.hcris3, unique.hcris4)
final.hcris.data =
  final.hcris.data %>%
  rename(year=fyear) %>%
  arrange(provider_number, year)

write_rds(final.hcris.data,'data/output/HCRIS_Data.rds')


#Problem 3 What is the distribution of total charges (tot_charges in the data) in each year? Show your results with a “violin” plot, with charges on the y-axis and years on the x-axis. For a nice tutorial on violin plots, look at Violin Plots with ggplot2.
totcharges <- ggplot(final.hcris.data, aes(x = as.factor(fyear), y = tot_charges)) +
  geom_violin() +
  labs(title = "Distribution of Total Charges by Year",
       x = "Year",
       y = "Total Charges")
plot(totcharges)
#Problem 4
# What is the distribution of estimated prices in each year? Again present your results with a violin plot, and recall our formula for estimating prices from class. Be sure to do something about outliers and/or negative prices in the data.

# Problem 5
hcris.data <- read_rds(here("data/output/HCRIS_Data.rds"))

hcris.data <- hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

final.hcris <- hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<

mean.pen <- round(mean(final.hcris$price[which(final.hcris$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris$price[which(final.hcris$penalty==0)]),2)
print(mean.pen)
print(mean.nopen)
# Problem 6 
# Split hospitals into quartiles based on bed size. To do this, create 4 new indicator variables, where each variable is set to 1 if the hospital’s bed size falls into the relevant quartile. Provide a table of the average price among treated/control groups for each quartile.
final.hcris.data <- final.hcris.data %>% 
  mutate(quartile = ntile(beds, 4))

# Creating indicator variables for each quartile
final.hcris.data <- final.hcris.data %>% 
  mutate(quartile_1 = ifelse(quartile == 1, 1, 0),
         quartile_2 = ifelse(quartile == 2, 1, 0),
         quartile_3 = ifelse(quartile == 3, 1, 0),
         quartile_4 = ifelse(quartile == 4, 1, 0))

summary_table <- final.hcris.data %>%
  group_by(quartile) %>%
  summarise(avg_price_treated = mean(final.hcris$price[which(final.hcris$penalty==1)], na.rm = TRUE),
            avg_price_control = (mean(final.hcris$price[which(final.hcris$penalty==0)], na.rm = TRUE)))
print(summary_table)
# Print the summary table
print(summary_table)

# Problem 7

# inverse variance
lp.vars <- final.hcris %>% 
  dplyr::select(beds, mcaid_discharges, penalty, ip_charges, 
         mcare_discharges, tot_mcare_payment, price) %>%
  filter(complete.cases(.))
lp.covs <- lp.vars %>%  dplyr::select(-c("penalty","price"))

love.plot(bal.tab(lp.covs,treat=lp.vars$penalty), colors="black", shapes="circle", threshold=0.1) + 
  theme_bw() + theme(legend.position="none")

m.nn.var <- Matching::Match(Y=lp.vars$price,
                            Tr=lp.vars$penalty,
                            X=lp.covs,
                            M=4,  #<<
                            Weight=1,
                            estimand="ATE")

v.name=data.frame(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments"))

# Nearest neighbor matching (1-to-1) with Mahalanobis distance based on quartiles of bed size
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")     

#Inverse propensity weighting, where the propensity scores are based on quartiles of bed size
logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
            tot_mcare_payment, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")

# Simple linear regression, adjusting for quartiles of bed size using dummy variables and appropriate interactions as discussed in class
 #is this using weighting


# With these different treatment effect estimators, are the results similar, identical, very different?

# Do you think you’ve estimated a causal effect of the penalty? Why or why not? (just a couple of sentences)

# Briefly describe your experience working with these data (just a few sentences). Tell me one thing you learned and one thing that really aggravated or surprised you.# 


save.image("submission-1/Hwk2_workspace.Rdata")
