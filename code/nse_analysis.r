# weekly Settings
sw_forecast <- 29   # forecast through week
year_forecast <- 2019   # forecast year
year.subfolder <- "2019_inseason" #subfolder for forecast through week
sw.subfolder <- "SW29" #subfolder for forecast through week
data.directory <-file.path('data', year.subfolder, sw.subfolder)


# create file structure to save inseason estimates
if(!dir.exists(file.path("output", year.subfolder))){
  dir.create(file.path("output",year.subfolder))}
if(!dir.exists(file.path("output", year.subfolder, sw.subfolder))){
  dir.create(file.path("output",year.subfolder, sw.subfolder))}

if(!dir.exists(file.path("figs", year.subfolder))){
  dir.create(file.path("figs",year.subfolder))}
if(!dir.exists(file.path("figs", year.subfolder, sw.subfolder))){
  dir.create(file.path("figs",year.subfolder, sw.subfolder))}

# load ----
source("code/functions.r")

# data ----
nse<- read.csv(file.path(data.directory,'nse_1984_2018.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) 

# create sex deviations
sex_dev(nse) -> full_data
full_data <- as.data.frame(full_data)

full_data %>% 
  filter(week <= sw_forecast) -> data

full_data %>% 
  filter(year < year_forecast) -> data_year

# cum catch (10 year average)
data_year %>% 
  dplyr::select(year, week, catch) %>%
  filter(year > year_forecast-11) %>%
  group_by(week) %>%
  summarise(catch = mean(catch)) %>%
  mutate(year = '10_year_average') -> avg

full_data %>% 
  dplyr::select(year, week, catch) %>%
  filter(year == year_forecast & week <= sw_forecast) -> x

fig_data<-rbind(x, avg)
chart(fig_data) + ggtitle("Cumulative Purse Seine Pink Salmon Harvest \n Northern Southeast Alaska (Districts 9-14)")
ggsave(file.path("figs",year.subfolder, sw.subfolder,"/",paste("NSE_chart.png")),
       dpi=600, height=6, width=8, units="in")

# likelihood functions ----
f_est(data) -> fits

# parameters ----
f_params(fits) -> params  
f_params(fits, sex = TRUE) -> params_sex  

# predictions ----
f_pred(data, params) -> preds
f_pred(data, params_sex, sex = TRUE) -> preds_sex

# model selection ----
model_select(preds, preds_sex) -> results

write.csv(results,file.path("output",year.subfolder, sw.subfolder,"/",paste("NSE.csv")), row.names=TRUE)

# figures
model_fig(data_year) + ggtitle("NSE")
ggsave(file.path("figs",year.subfolder, sw.subfolder,"/",paste("NSE.png")),
       dpi=600, height=6, width=9, units="in")


