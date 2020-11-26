########################################################################################################################
###                           Subsidence/Thermokarst Analysis from NEON Data                                         ###
###                                         Code by HGR 5/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(sf)
library(rgdal)
library(readxl)
library(lme4)
# library(heavy)
library(MuMIn)
library(emmeans)
library(pbkrtest)
library(viridis)
library(lwgeom)
library(tidyverse)
########################################################################################################################

### Load Data ##########################################################################################################
filenames <- list.files('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/DTM',
                        full.names = TRUE,
                        pattern = '.tif$')
crop_extent <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))
elev <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), crop_extent),
             crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), crop_extent),
             crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), crop_extent))
sub <- brick(stack("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/subsidence/subsidence_2017_2018.tif",
                   "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/subsidence/subsidence_2017_2019.tif"))

karst_1 <- brick(stack("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_1_filter_9km_1.tif",
                       "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_1_filter_9km_2.tif",
                       "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_1_filter_9km_3.tif"))
karst_3 <- brick(stack("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_3_filter_9km_1.tif",
                       "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_3_filter_9km_2.tif",
                       "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_3_filter_9km_3.tif"))
crs(karst_1) <- CRS('+init=epsg:32606')
crs(karst_3) <- CRS('+init=epsg:32606')

eml_wtrshd <- st_read("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/eml_bnd/boundry_poly3.shp")

filenames <- list.files("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output",
                        full.names = TRUE,
                        pattern = 'shp$')

karst_1_poly <- map(filenames[which(str_detect(filenames, pattern = 'karst_combined_1'))],
                    ~ st_read(.x))
names(karst_1_poly) <- c(2017, 2018, 2019)

karst_3_poly <- map(filenames[which(str_detect(filenames, pattern = 'karst_combined_3'))],
                    ~ st_read(.x))
names(karst_3_poly) <- c(2017, 2018, 2019)

filenames <- list.files("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output",
                        full.names = TRUE,
                        pattern = 'mtopo.+9km')

mtopo <- list(stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_1\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo25.+_1\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo35.+_1\\.tif$'))]),
              stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_2\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo25.+_2\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo35.+_2\\.tif$'))]),
              stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_3\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo25.+_3\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo35.+_3\\.tif$'))]))
rm(filenames)

ec_alt_2017 <- read_excel('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/ALT_Measurements_201708.xlsx')
ec_alt_2019 <- read_excel('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/ec_tower_alt_20190810.xlsx')
points_2017 <- st_read('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/All_Points_2017_SPCSAK4.shp')
points_2019 <- st_read('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/All_Points_2019_Aug_SPCSAK4.shp')
########################################################################################################################


### Image-Wide Analysis
### Calculate Thermokarst Coverage #####################################################################################
# calculate area of entire image (each cell is 1 m^2, so just getting the number of cells is the area in m^2)
karst_area <- karst_1 %>%
  as.data.frame() %>%
  rename(class.2017 = 1, class.2018 = 2, class.2019 = 3) %>%
  gather(key = 'year', value = 'thermokarst', 1:3) %>%
  mutate(year = as.numeric(str_sub(year, 7)),
         thermokarst = ifelse(thermokarst == 0,
                              'undisturbed',
                              ifelse(thermokarst == 1,
                                     'thermokarst',
                                     NA))) %>%
  group_by(year, thermokarst) %>%
  summarise(n = n())

# summarize
karst_cover <- karst_area %>%
  filter(!is.na(thermokarst)) %>%
  spread(key = 'thermokarst', value = 'n') %>%
  mutate(percent.thermokarst = thermokarst/(thermokarst + undisturbed))
########################################################################################################################

### Summary Statistics From Polygons ###################################################################################
n_features <- map_df(karst_1_poly, ~ nrow(.x)) %>%
  pivot_longer(`2017`:`2019`, names_to = 'year', values_to = 'n_karst_1') %>%
  full_join(map_df(karst_3_poly, ~ nrow(.x)) %>%
              pivot_longer(`2017`:`2019`, names_to = 'year', values_to = 'n_karst_3'),
            vy = c('year'))

karst_1_depth <- map2_df(mtopo,
                         karst_1_poly,
                         ~ st_as_sf(raster::extract(.x,
                                                    as(.y, 'Spatial'),
                                                    layer = 1,
                                                    nl = 3,
                                                    sp = TRUE)) %>%
                           rename(depth.15 = 4,
                                  depth.25 = 5,
                                  depth.35 = 6) %>%
                           mutate(year = i + 2016))

write.csv(karst_1_depth, '/scratch/hgr7/output/karst_1_depth.csv', row.names = FALSE)

karst_3_depth <- map2_df(mtopo,
                         karst_3_poly,
                         ~ st_as_sf(raster::extract(.x,
                                                    as(.y, 'Spatial'),
                                                    layer = 2,
                                                    nl = 3,
                                                    sp = TRUE)) %>%
                           rename(depth.25 = 4,
                                  depth.35 = 5) %>%
                           mutate(year = i + 2016))

write.csv(karst_3_depth, '/scratch/hgr7/output/karst_3_depth.csv', row.names = FALSE)
########################################################################################################################

### Identify Thermokarst Edges #########################################################################################
karst_na <- karst_1
for (i in 1:nlayers(karst_na)) {
  karst_na[[i]][karst_na[[i]] == 0] <- NA
}

edges <- brick(boundaries(karst_na[[1]]),
               boundaries(karst_na[[2]]),
               boundaries(karst_na[[3]]))

edges_0 <- edges
for (i in 1:nlayers(edges_0)) {
  edges_0[[i]][is.na(edges_0[[i]])] <- 0
}

# combine edges with thermokarst classification (1 = thermokarst, 2 = thermokarst edge)
# and then extract thermokarst values from that
karst_edges <- brick(karst_1[[1]] + edges_0[[1]],
                     karst_1[[2]] + edges_0[[2]],
                     karst_1[[3]] + edges_0[[3]])

plot(karst_edges[[1]])
plot(karst_edges[[2]])
plot(karst_edges[[3]])
########################################################################################################################


### Subsidence Mixed-Effects Model
######################## DEFINE FUNCTIONS TO EXTRACT AND GRAPH CI #########################
#Extract the coefficients for the fixed effects from your model, make a dataframe with them called model
extract_ci <- function(x) {coefs<-fixef(x) 
modeldf<-as.data.frame(coefs)
#calculate confidence intervals; merge fixed effects and ci into one dataframe
ci <- confint(x,method="boot",boot.type="norm",level=0.95,nsim=1000)
modelci<-merge(ci,modeldf,by="row.names",all.x=F)
#rename colnames so that they make sense and remove symbols
colnames(modelci)<-c("term","min","max","coefs")
return (modelci)}

# graph CI
graph_ci <- function(ci,figtitle,model) {ggplot(ci,aes(x=names,y=coefs))+
    geom_errorbar(aes(ymin=min,ymax=max),width=0,size=1)+
    geom_point(aes(size=2))+
    labs (title = paste(figtitle, ", AIC:", round(AIC(model),2), sep =" ") , x = "Fixed effect", y = "Effect size and 95% CI") +
    guides(size=F,shape=F)+
    theme_bw()+
    theme(axis.text.x=element_text(size=18),
          axis.title.x=element_text(size=26),
          axis.title.y=element_text(size=26,vjust=1),
          axis.text.y=element_text(size=22),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank())+
    geom_hline(yintercept=0)+
    coord_flip() } 
########################################################################################################################

### Mixed Effects Model of Subsidence by Thermokarst Class Over Time ###################################################
# # take stratified random sample of cells (currently set up for non-thermokarst, thermokarst center and thermokarst edges)
# set.seed(333)
# samples <- st_as_sf(sampleStratified(karst_edges[[1]], size = 2000, xy = TRUE, sp = TRUE)) %>%
#   select(-4) %>%
#   mutate(ID = seq(1, 1500))
# ggplot(samples, aes(x = x, y = y)) +
#   geom_point() +
#   coord_fixed()
# 
# st_write(samples, '/scratch/hgr7/analysis/subsidence_rate_samples_500.shp')
# st_write(samples, '/scratch/hgr7/analysis/subsidence_rate_samples_1000.shp')
# st_write(samples, '/scratch/hgr7/analysis/subsidence_rate_samples_2000.shp')
samples <- st_read('/scratch/hgr7/analysis/subsidence_rate_samples_1000.shp')

# # extract values from subsidence brick
# # use a buffer to average out erroneous very high/low reads
# # extract all cells from sub (do not average them) join with the karst extract with all cells in
# # a 1.5 m buffer and then filter out non-matching thermokarst classes and then average
# sub_extract <- raster::extract(sub, as(samples, 'Spatial'), buffer = 1.5, layer = 1, nl = 2, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame() %>%
#   rename(cell = cells,
#          sub.2018 = 3,
#          sub.2019 = 4) %>%
#   mutate(sub.2017 = 0) %>%
#   gather(key = year, value = sub, sub.2018:sub.2017) %>%
#   mutate(year = as.numeric(str_sub(year, 5))) %>%
#   arrange(year, ID)
# 
# elev_extract <- raster::extract(elev, as(samples, 'Spatial'), buffer = 1.5, layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame() %>%
#   rename(cell = cells,
#          elev.2017 = 3,
#          elev.2018 = 4,
#          elev.2019 = 5) %>%
#   gather(key = year, value = elev, elev.2017:elev.2019) %>%
#   mutate(year = as.numeric(str_sub(year, 6))) %>%
#   arrange(year, ID)
# 
# 
# karst_extract_buffer <- raster::extract(karst_1, as(samples, 'Spatial'), buffer = 1.5, layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame() %>%
#   rename(cell = cells,
#          karst.2017 = 3,
#          karst.2018 = 4,
#          karst.2019 = 5) %>%
#   mutate(karst.2017 = ifelse(karst.2017 == 1 | karst.2018 == 1 | karst.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(karst.2017 == 2 | karst.2018 == 2  | karst.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0)),
#          karst.2018 = ifelse(karst.2017 == 1 | karst.2018 == 1 | karst.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(karst.2017 == 2 | karst.2018 == 2  | karst.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0)),
#          karst.2019 = ifelse(karst.2017 == 1 | karst.2018 == 1 | karst.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(karst.2017 == 2 | karst.2018 == 2  | karst.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0))) %>%
#   gather(key = year, value = karst, karst.2017:karst.2019) %>%
#   mutate(year = as.numeric(str_sub(year, 7))) %>%
#   arrange(year, ID)
# 
# # extract values from thermokarst classification brick
# # no buffer so that this dataset can be used to identify the cells which are samples from
# # the buffers when joined with the buffered extracted sub and karst
# karst_extract <- st_as_sf(raster::extract(karst_edges, as(samples, 'Spatial'), layer = 1, nl = 3, sp = TRUE)) %>%
#   rename(tk.2017 = 5,
#          tk.2018 = 6,
#          tk.2019 = 7) %>%
#   mutate(tk.2017 = ifelse(tk.2017 == 1 | tk.2018 == 1 | tk.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(tk.2017 == 2 | tk.2018 == 2  | tk.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0)),
#          tk.2018 = ifelse(tk.2017 == 1 | tk.2018 == 1 | tk.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(tk.2017 == 2 | tk.2018 == 2  | tk.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0)),
#          tk.2019 = ifelse(tk.2017 == 1 | tk.2018 == 1 | tk.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(tk.2017 == 2 | tk.2018 == 2  | tk.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0))) %>%
#   st_drop_geometry() %>%
#   gather(key = year, value = thermokarst, tk.2017:tk.2019) %>%
#   mutate(year = as.numeric(str_sub(year, 4))) %>%
#   arrange(year)
# 
# # join subsidence and thermokarst extract dataframes
# sub_karst <- karst_extract_buffer %>%
#   full_join(sub_extract, by = c('ID', 'cell', 'year')) %>%
#   full_join(elev_extract, by = c('ID', 'cell', 'year')) %>%
#   full_join(karst_extract, by = c('ID', 'cell', 'year')) %>%
#   group_by(year, ID) %>%
#   mutate(sample_cell = ifelse(mean(thermokarst, na.rm = TRUE) == 0,
#                                      0,
#                                      ifelse(mean(thermokarst, na.rm = TRUE) >= 1,
#                                             1)))
# sub_karst_summary <- sub_karst %>%
#   filter(karst == sample_cell) %>%
#   summarise(x = mean(x, na.rm = TRUE),
#             y = mean(y, na.rm = TRUE),
#             thermokarst = mean(thermokarst, na.rm = TRUE),
#             mean.elev = mean(elev),
#             se.elev = sd(elev)/sqrt(n()),
#             mean.sub = mean(sub),
#             se.sub = sd(sub)/sqrt(n())) %>%
#   mutate(time = year - 2017,
#          id.factor = as.factor(ID),
#          thermokarst.presence = as.factor(ifelse(thermokarst == 0,
#                                                  0,
#                                                  ifelse(thermokarst >= 1,
#                                                         1,
#                                                         NA))),
#          thermokarst.class = as.factor(thermokarst),
#          elev.transform = mean.elev^-2) %>%
#   ungroup()#%>%
#   # group_by(ID) %>%
#   # filter((max(se.elev) - min(se.elev)) < 0.1)
# write.csv(sub_karst_summary, '/scratch/hgr7/output/sub_karst_summary_1000.csv', row.names = FALSE)
sub_karst_summary <- read.csv('/scratch/hgr7/output/sub_karst_summary_1000.csv')

mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 0 & sub_karst_summary$year == 2018)])
mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 1 & sub_karst_summary$year == 2018)])
mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 0 & sub_karst_summary$year == 2019)])
mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 1 & sub_karst_summary$year == 2019)])

hist(sub_karst_summary$mean.elev)
hist(sub_karst_summary$elev.transform)

# I don't know if it makes sense to filter out mean subsidence values with high standard deviations
# if I'm trying to find a pattern despite noise in the data by getting enough points

# model.full <- lmer(mean.elev ~ time*thermokarst.presence +
#                 (1+time|id.factor), # random intercept and slope for each sample point
#               data = sub_karst_summary,
#               REML = FALSE,
#               control=lmerControl(check.conv.singular="warning"))
# summary(model.full)
# 
# model.intercept <- lmer(mean.elev ~ time + thermokarst.presence +
#                      (1+time|id.factor), # random intercept and slope for each sample point
#                    data = sub_karst_summary,
#                    REML = FALSE,
#                    control=lmerControl(check.conv.singular="warning"))
# summary(model.intercept)
# 
# model.simple <- lmer(mean.elev ~ time +
#                      (1+time|id.factor), # random intercept and slope for each sample point
#                    data = sub_karst_summary,
#                    REML = FALSE,
#                    control=lmerControl(check.conv.singular="warning"))
# summary(model.simple)
# 
# AIC(model.full, model.intercept, model.simple)
# 
# # look at residuals
# model.resid <- resid(model.full)
# model.fitted <- fitted(model.full)
# model.sqrt <- sqrt(abs(resid(model.full)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model.fitted, model.resid, main='resid, model')
# plot(model.fitted, model.sqrt, main='sqrt resid, model')
# qqnorm(model.resid, main = 'model')
# qqline(model.resid)
# hist(model.resid)
# par(mfrow=c(1,1))
# 
# # The errors are REALLY not normal...
# shapiro.test(sample(model.resid, size = 5000))
# 
# # However, that should not impact the model itself, only the determination of statistical significance
# # of parameters via p-values. Can use bootstrapping instead (which is what we normally do anyway).
# # See: https://data.library.virginia.edu/normality-assumption/
# 
# 
# 
# # re-run with reml = TRUE
# model.full <- lmer(mean.elev ~ time+thermokarst.presence +
#                      (1+time|id.factor), # random intercept and slope for each sample point
#                    data = sub_karst_summary,
#                    control=lmerControl(check.conv.singular="warning"))
# saveRDS(model.full, '/scratch/hgr7/output/sub_karst_model_1000.rds')
model.full <- readRDS('/scratch/hgr7/output/sub_karst_model_1000.rds')
summary(model.full)
r2 <- r.squaredGLMM(model.full)
# # this is crashing R - need fewer samples
# # This doesn't make sense for a model with only different intercepts
# LetterResults <- emmeans(model.full, ~ thermokarst.presence) 
# LetterResults2 <- LetterResults %>% cld(Letters=letters)
# LetterResults

# model_ci <- extract_ci(model.full)
# write.csv(model_ci, '/scratch/hgr7/analysis/model_ci_1000.csv', row.names = FALSE)
model_ci <- read.csv('/scratch/hgr7/analysis/model_ci.csv')

# # make confidence interval data frame for graphing
# ConfData <- expand.grid(time = 0:2,
#                         thermokarst.presence = as.factor(0:1))
# 
# myStats <- function(model){
#   out <- predict( model, newdata=ConfData, re.form=~0 )
#   return(out)
# }
# 
# bootObj <- bootMer(model.full, FUN=myStats, nsim = 1000)
# ConfData <- cbind(ConfData, predict(model.full, newdata=ConfData, re.form=~0 )) %>%
#   cbind(confint( bootObj,  level=0.95 ))
# colnames(ConfData) <- c('time', 'thermokarst.presence', 'fit', 'lwr', 'upr')
# # write.csv(ConfData, '/scratch/hgr7/analysis/subsidence_thermokarst_fit.csv', row.names = FALSE)
ConfData <- read.csv('/scratch/hgr7/analysis/subsidence_thermokarst_fit.csv')

subsidence_model_table <- data.frame(Response = c('Elevation', '', '', ''),
                                     `Full Model` = c('Year*Thermokarst', '', '', ''),
                                     `Final Variables` = c('Intercept (Non-Thermokarst)', 'Intercept (Thermokarst)', 'Year*Non-thermokarst', 'Year*Thermokarst'),
                                     Coeficient = c(model_ci$coefs[1], model_ci$coefs[1] + model_ci$coefs[2], model_ci$coefs[3], model_ci$coefs[3] + model_ci$coefs[4]),
                                     `Min CI` = c(model_ci$coefs[1], model_ci$coefs[1] + model_ci$coefs[2], model_ci$coefs[3], model_ci$coefs[3] + model_ci$coefs[4]),
                                     `Max CI` = c(model_ci$coefs[1], model_ci$coefs[1] + model_ci$coefs[2], model_ci$coefs[3], model_ci$coefs[3] + model_ci$coefs[4]),
                                     `R2 Marginal` = c(r2[1], rep('', 3)),
                                     `R2 Conditional` = c(r2[2], rep('', 3)),
                                     AIC = c(AIC(subsidence_model), rep('', 3)))
write.csv(subsidence_model_table, '/scratch/hgr7/analysis/subsidence_model_table.csv', row.names = FALSE)

# ### Look at different slopes - not too different
# sub_karst_summary_2 <- sub_karst_summary %>%
#   mutate(yhat = predict(model.full, re.form = ~(1+time|id.factor)))
# fixed.effects <- fixef(model.full)
# random.effects <- ranef(model.full)[[1]]
# random.effects <- random.effects %>%
#   arrange(time) %>%
#   mutate(order = seq(1:3000),
#          slope = time + fixed.effects[[2]])
# ggplot(random.effects, aes(x = order, y = slope)) +
#   geom_point()

ggplot(sub_karst_summary_2, aes(x = year, y = yhat, color = thermokarst.presence, group = id.factor)) +
  geom_line()
# determine boxcox transformation for elevation
# lambda = -2
# lmodel <- lm(mean.elev ~ time*thermokarst.presence,
#              data = sub_karst_summary)
# MASS::boxcox(lmodel, lambda = seq(-4, 2, 1/10))
# check model residuals of model
# model <- heavyLme(elev.transform ~ time + thermokarst.presence,
#                   random = ~ time,
#                   groups = ~ id.factor,
#                   data = sub_karst_summary)
# 
# summary(model)
# 
# model.sqrt <- sqrt(abs(model$Resid$marginal))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model$Fitted$marginal, model$Resid$marginal, main='resid, model')
# plot(model$Fitted$marginal, model.sqrt, main='sqrt resid, model')
# qqnorm(model$Resid$marginal, main = 'model')
# qqline(model$Resid$marginal)
# par(mfrow=c(1,1))

# the qq plot shows that there are a lot more extreme values than are expected in a normal distribution
# I want to make sure that is not due to noise in the lidar data
# extract ids with very large or small subsidence values and see if it is just one cell or many
ids <- c(filter(sub_karst_summary, sub < -0.3 | sub > 0.3)$ID)
test_outliers <- sub_karst %>%
  filter(ID %in% ids & year > 2017) %>%
  group_by(ID, year) %>%
  summarise(max.sub = max(sub),
            min.sub = min(sub),
            mean.sub = mean(sub),
            sd = sd(sub))

hist(sub_karst$sub)

ggplot(filter(sub_karst_summary, thermokarst.presence == 0), aes(x = year, y = elev.transform, color = thermokarst.presence)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(group = ID), alpha = 0.25)

ggplot(filter(sub_karst_summary, thermokarst.presence == 1), aes(x = year, y = elev.transform, color = thermokarst.presence)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(group = ID), alpha = 0.25)

ggplot(sub_karst_summary, aes(x = year, y = elev.transform, color = thermokarst.presence)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(group = ID), alpha = 0.25)
########################################################################################################################


### EML Analysis
### Calculate Thermokarst Coverage #####################################################################################
# calculate thermokarst for eml watershed
# karst_eml <- mask(karst_1, as(eml_wtrshd, 'Spatial'))
# writeRaster(karst_eml, '/scratch/hgr7/analysis/eml_wtshd_karst.tif')
# writeRaster(karst_eml[[1]], '/scratch/hgr7/analysis/eml_wtshd_karst_2017.tif')
# writeRaster(karst_eml[[2]], '/scratch/hgr7/analysis/eml_wtshd_karst_2018.tif')
# writeRaster(karst_eml[[3]], '/scratch/hgr7/analysis/eml_wtshd_karst_2019.tif')
karst_eml <- brick('/scratch/hgr7/analysis/eml_wtshd_karst_2017.tif',
                   '/scratch/hgr7/analysis/eml_wtshd_karst_2018.tif',
                   '/scratch/hgr7/analysis/eml_wtshd_karst_2019.tif')
# karst_eml_mean <- calc(karst_eml, function(x)round(mean(x)))
# writeRaster(karst_eml_mean, '/scratch/hgr7/analysis/eml_wtshd_mean_karst.tif')
karst_eml_mean <- raster('/scratch/hgr7/analysis/eml_wtshd_mean_karst.tif')
karst_eml_mean_sp <- rasterToPolygons(karst_eml_mean)
writeOGR(karst_eml_mean_sp,
         dsn = '/scratch/hgr7/analysis/',
         layer = 'eml_wtshd_mean_karst.shp')

karst_eml_df <- karst_eml %>%
  as.data.frame(xy = TRUE) %>%
  rename(class.2017 = 3, class.2018 = 4, class.2019 = 5)

karst_eml_area <- karst_eml_df %>%
  filter(!is.na(class.2017)) %>%
  gather(key = 'year', value = 'thermokarst', 3:5) %>%
  mutate(year = as.numeric(str_sub(year, 7)),
         thermokarst = ifelse(thermokarst == 0,
                              'undisturbed',
                              ifelse(thermokarst == 1,
                                     'thermokarst',
                                     NA))) %>%
  group_by(year, thermokarst) %>%
  summarise(n = n())

karst_eml_cover <- karst_eml_area %>%
  filter(!is.na(thermokarst)) %>%
  spread(key = 'thermokarst', value = 'n') %>%
  mutate(percent.thermokarst = thermokarst/(thermokarst + undisturbed))
########################################################################################################################


### EC Tower Analysis
### Create EC Tower Footprint Slices ###################################################################################
# # point for ec tower location
# ec <- st_sfc(st_point(c(389398.2, 7085591), dim = 'XY'), crs = 32606)
# ec_sf <- st_sf(geometry = ec, crs = 32606)
# 
# # create a circle around the ec tower with radius = 200 m
# circle <- st_buffer(ec, dist = 225)
# circle_sf <- st_sf(geometry = circle)
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = ec_sf, aes(geometry = geometry)) + 
#   coord_sf(datum = st_crs(32606))
# 
# # create 360 lines at 1 degree angles around ec tower that reach to the circle
# # start by creating a single line the length of the diameter
# line <- st_sfc(st_linestring(matrix(c(389623.2, 389173.2, 7085591, 7085591), nrow = 2)), crs = 32606)
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = line, aes(geometry = geometry)) + 
#   coord_sf(datum = st_crs(32606))
# 
# # then rotate the line by 1 degree 179 times
# rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
# 
# line_sf <- st_sf(geometry = line, crs = 32606)
# for (i in 1:179) {
#   rad <- i*pi/180
#   line_rotate <- st_sf(geometry = (line - ec) * rot(rad) + ec, crs = 32606)
#   line_sf <- rbind(line_sf, line_rotate)
# }
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = line_sf, aes(geometry = geometry)) + 
#   coord_sf(datum = st_crs(32606))
# 
# # Snap the lines to the circle
# line_sf <- st_snap(line_sf, circle_sf, tol = 0.1)
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = line_sf, aes(geometry = geometry)) + 
#   coord_sf(datum = st_crs(32606))
# 
# wedges <- st_as_sf(st_collection_extract(st_split(circle_sf$geometry,
#                                                   line_sf$geometry),
#                                          "POLYGON"))
# 
# wedges_sf <- wedges %>%
#   mutate(n = seq(1:360))
# 
# st_write(wedges_sf, "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/wedges_poly_250.shp")
wedges_sf <- st_read("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/wedges_poly_250.shp")

ggplot() +
  geom_sf(data = wedges_sf, aes(color = n)) +
  coord_sf(datum = st_crs(32606))
########################################################################################################################

### Thermokarst Distribution at EC Tower ###############################################################################
# tower_extract <- raster::extract(karst_1, as(wedges_sf, 'Spatial'), layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame()
# tower_extract_neat <- tower_extract %>%
#   rename(karst.2017 = 3,
#          karst.2018 = 4,
#          karst.2019 = 5) %>%
#   group_by(ID) %>%
#   summarise(karst.2017 = sum(karst.2017)/n(),
#             karst.2018 = sum(karst.2018)/n(),
#             karst.2019 = sum(karst.2019)/n()) %>%
#   pivot_longer(cols = karst.2017:karst.2019, names_to = 'year', values_to = 'karst.percent') %>%
#   mutate(year = as.numeric(str_sub(year, 7)))
# # write.csv(tower_extract_neat, '/scratch/hgr7/analysis/tower_karst_extract.csv')
# 
# tower_karst_sf <- tower_extract %>%
#   rename(n = ID, karst.2017 = 3, karst.2018 = 4, karst.2019 = 5) %>%
#   group_by(n) %>%
#   summarise(karst.percent.2017 = sum(karst.2017)/n(),
#             karst.percent.2018 = sum(karst.2018)/n(),
#             karst.percent.2019 = sum(karst.2019)/n()) %>%
#   full_join(wedges_sf, by = c('n'))
# st_write(tower_karst_sf, '/scratch/hgr7/analysis/tower_karst_extract.shp')
tower_karst_sf <- st_read('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_karst_extract.shp') %>%
  rename(karst.percent.2017 = 2, karst.percent.2018 = 3, karst.percent.2019 = 4)

# ggplot(tower_extract_neat, aes(x = year, y = karst.percent, color = ID, group = ID)) +
#   geom_point() +
#   geom_line()


ggplot() + 
  geom_sf(data = tower_karst_sf,
          aes(geometry = geometry,
              color = karst.percent.2017,
              fill = karst.percent.2017)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.6)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.6))

ggplot() + 
  geom_sf(data = tower_karst_sf,
          aes(geometry = geometry,
              color = karst.percent.2018,
              fill = karst.percent.2018)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.6)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.6))

ggplot() + 
  geom_sf(data = tower_karst_sf,
          aes(geometry = geometry,
              color = karst.percent.2019,
              fill = karst.percent.2019)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.6)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.6))
########################################################################################################################

### Microtopography Distribution (Roughness) at EC Tower ###############################################################
mtopo_brick <- brick(mtopo[[1]][[1]],
                     mtopo[[2]][[1]],
                     mtopo[[3]][[1]])
# mtopo_extract <- raster::extract(mtopo_brick, as(wedges_sf, 'Spatial'), layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame()
# mtopo_extract_neat <- mtopo_extract %>%
#   rename(mtopo15.2017 = 3,
#          mtopo15.2018 = 4,
#          mtopo15.2019 = 5) %>%
#   group_by(ID) %>%
#   summarise(mtopo15.2017 = sum(mtopo15.2017)/n(),
#             mtopo15.2018 = sum(mtopo15.2018)/n(),
#             mtopo15.2019 = sum(mtopo15.2019)/n()) %>%
#   pivot_longer(cols = mtopo15.2017:mtopo15.2019, names_to = 'year', values_to = 'mtopo.15.percent') %>%
#   mutate(year = as.numeric(str_sub(year, 7)))
# 
# mtopo_sf <- mtopo_extract %>%
#   rename(n = ID, mtopo15.2017 = 3, mtopo15.2018 = 4, mtopo15.2019 = 5) %>%
#   group_by(n) %>%
#   summarise(mtopo15.sd.2017 = sd(mtopo15.2017),
#             mtopo15.sd.2018 = sd(mtopo15.2018),
#             mtopo15.sd.2019 = sd(mtopo15.2019)) %>%
#   full_join(wedges_sf, by = c('n'))
# st_write(mtopo_sf, '/scratch/hgr7/analysis/tower_mtopo15_extract.shp')
mtopo_sf <- st_read('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_mtopo15_extract.shp') %>%
  rename(mtopo15.sd.2017 = 2, mtopo15.sd.2018 = 3, mtopo15.sd.2019 = 4)

ggplot() + 
  geom_sf(data = mtopo_sf,
          aes(geometry = geometry,
              color = mtopo15.sd.2017,
              fill = mtopo15.sd.2017)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.25)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.25))

ggplot() + 
  geom_sf(data = mtopo_sf,
          aes(geometry = geometry,
              color = mtopo15.sd.2018,
              fill = mtopo15.sd.2018)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.25)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.25))

ggplot() + 
  geom_sf(data = mtopo_sf,
          aes(geometry = geometry,
              color = mtopo15.sd.2019,
              fill = mtopo15.sd.2019)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.25)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.25))
########################################################################################################################

### Subsidence Distribution at EC Tower ################################################################################
# The inter- and intra-year offset seems to be having to much of an impact over such a small area
# sub_ec_extract <- raster::extract(sub, as(wedges_sf, 'Spatial'), layer = 1, nl = 2, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame()
# 
# sub_ec_sf <- sub_ec_extract %>%
#   rename(n = ID, sub.2018 = 3, sub.2019 = 4) %>%
#   group_by(n) %>%
#   summarise(mean.sub.2018 = mean(sub.2018),
#             mean.sub.2019 = mean(sub.2019)) %>%
#   full_join(wedges_sf, by = c('n'))
# st_write(sub_ec_sf, '/scratch/hgr7/analysis/tower_sub_extract.shp')
sub_ec_sf <- st_read('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_sub_extract.shp') %>%
  rename(mean.sub.2018 = 2, mean.sub.2019 = 3)

ggplot() + 
  geom_sf(data = sub_ec_sf,
          aes(geometry = geometry,
              color = mean.sub.2018,
              fill = mean.sub.2018)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(limits = c(-0.05, 0.15)) +
  scale_fill_viridis(limits = c(-0.05, 0.15))

ggplot() + 
  geom_sf(data = sub_ec_sf,
          aes(geometry = geometry,
              color = mean.sub.2019,
              fill = mean.sub.2019)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(limits = c(-0.05, 0.15)) +
  scale_fill_viridis(limits = c(-0.05, 0.15))
########################################################################################################################

### ALT Thermokarst Microtopography Analysis ###########################################################################
### Clean ALT and GPS Points for joining
points_2017_clean <- points_2017 %>%
  select(Name, geometry) %>%
  filter(as.numeric(as.character(Name)) > 13000) %>%
  mutate(point = as.numeric(as.character(Name)) - 13000) %>%
  select(-Name)

points_2019_clean <- points_2019 %>%
  select(Name, geometry) %>%
  filter(as.numeric(as.character(Name)) > 14000) %>%
  mutate(point = as.numeric(as.character(Name)) - 14000) %>%
  select(-Name)

ec_alt_2017_clean <- ec_alt_2017 %>%
  filter(Experiment == 'Flux Tower') %>%
  mutate(year = 2017) %>%
  select(year, point = `Grid Point`, alt = ALT)

### Join All ALT and GPS Points Together
ec_alt_sf <- ec_alt_2017_clean %>%
  full_join(points_2017_clean, by = c('point')) %>%
  rbind.data.frame(ec_alt_2019 %>%
                     mutate(year = 2019) %>%
                     select(year, point, alt) %>%
                     full_join(points_2019_clean, by = c('point'))) %>%
  st_as_sf() %>%
  st_zm() %>%
  st_transform(crs = 32606)

### Extract Microtopography at ALT Points
ec_mtopo_extract_17 <- raster::extract(mtopo_brick,
                                       as(filter(ec_alt_sf, year == 2017),
                                          'Spatial'),
                                       layer = 1,
                                       nl = 1,
                                       cellnumbers = TRUE,
                                       df = TRUE) %>%
    as.data.frame() %>%
  rename(mtopo = 3) %>%
  cbind.data.frame(filter(ec_alt_sf, year == 2017))

ec_mtopo_extract_19 <- raster::extract(mtopo_brick, as(filter(ec_alt_sf, year == 2019), 'Spatial'), layer = 1, nl = 1, cellnumbers = TRUE, df = TRUE) %>%
  as.data.frame() %>%
  rename(mtopo = 3) %>%
  cbind.data.frame(filter(ec_alt_sf, year == 2019))

ec_mtopo_extract <- ec_mtopo_extract_17 %>%
  rbind.data.frame(ec_mtopo_extract_19) %>%
  mutate(mtopo = round(mtopo, 2),
         alt.sqrt = sqrt(alt),
         alt.log = log(alt))

hist(ec_mtopo_extract$mtopo)
hist(ec_mtopo_extract$alt)
hist(ec_mtopo_extract$alt.sqrt)
hist(ec_mtopo_extract$alt.log)

mtopo_alt_graph <- ggplot(ec_mtopo_extract, aes(x = mtopo, y = alt)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(method = 'gam',
              formula = y~s(x),
              color = 'black') +
  scale_y_continuous(name = 'ALT') +
  scale_x_continuous(name = 'Microtopography') +
  scale_color_manual(name = 'Year',
                     values = c('gray50', 'gray30'))
mtopo_alt_graph
ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/mtopo_alt_plot.jpg',
       mtopo_alt_graph)

# model <- lm(mtopo ~ alt.log, data = ec_mtopo_extract)
# summary(model)

### microtopography by thermokarst
karst_mtopo_sf <- tower_karst_sf %>%
  pivot_longer(cols = karst.percent.2017:karst.percent.2019,
               names_to = 'year',
               values_to = 'karst.percent') %>%
  mutate(year = as.numeric(str_sub(year, start = 15))) %>%
  select(-geometry) %>%
  full_join(mtopo_sf %>%
            pivot_longer(cols = mtopo15.sd.2017:mtopo15.sd.2019,
                         names_to = 'year',
                         values_to = 'mtopo.sd') %>%
            mutate(year = as.numeric(str_sub(year, start = 12))),
            by = c('n', 'year')) %>%
    st_as_sf()

karst_mtopo_2 <- karst_mtopo_sf %>%
  mutate(karst.percent.sqr = sqrt(karst.percent),
         color.group = as.factor(ifelse(n > 330 | n < 180,
                              'SW-S-E',
                              'W-N-NE')))

karst_roughness_graph <- ggplot(karst_mtopo_2, aes(x = karst.percent, y = mtopo.sd)) +
  geom_point(aes(color = color.group)) +
  geom_smooth(method = 'gam',
              formula = y ~s(x),
              color = 'black') +
  scale_y_continuous(name = 'Roughness') +
  scale_x_continuous(name = 'Percent Thermokarst') +
  scale_color_manual(name = 'Direction',
                     values = c('gray50', 'gray30'))
karst_roughness_graph
ggsave('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/roughness_karst_plot.jpg',
       karst_roughness_graph)

 # try to figure out why the mtopo values always have 999 or 000 in decimal places 3-5
# # I guess the raw LiDAR elevation somehow is only actually significant to 2 decimal places. Why?
# values_df <- data.frame(elev = getValues(elev[[1]]), median = getValues(median15))
# values_df <- mutate(values_df, diff = elev - median)
########################################################################################################################