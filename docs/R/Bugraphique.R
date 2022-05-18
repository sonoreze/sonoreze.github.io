###

#<h5>Niveaux de bruit interpolés</h5>

#  &nbsp;

r <- c(255,130,160,184,206,226,243,232,205,161,117,67)
g <- c(255,166,186,214,228,242,198,126,70,26,8,10)
b <- c(255,173,191,209,204,191,131,77,62,77,92,74)

sound <- function (n, name = c("soundcolors"))
{
  soundcolors = rgb(r, g, b, maxColorValue = 255)
  name = match.arg(name)
  orig = eval(parse(text = name))
  rgb = t(col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, ,length(orig))
  xg = seq(0, 1, , n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}

pie(rep(1, 100), col = palsound100)

palsound100 <- sound(n=100)

palbo <- colorNumeric(
  palette = palsound100,
  domain = stations$leq_mean
)

#```{r,message=FALSE,warning = FALSE,results='hide'}
Data_numeric <- data_nc[,c("Id","Date","x","y","leq_mean")]
SPDF_sf = st_as_sf(Data_numeric, coords = c("x", "y"), crs = 4326, agr = "constant", remove = F)
st_crs(SPDF_sf) <- 4326 #EPSG WGS84
SPDF_sf_2154 <- st_transform(SPDF_sf, 2154) #LAMBERT 93
st_write(SPDF_sf_2154, here("geo","pts2154_2.shp"), append=FALSE)
# ## -------------------------
# #read layer shapefile
# #PTS_ALL_2154 <- readOGR(dsn = here("geo"), layer = "PTS_ALL_2154_c") #Too much Lines
Pts_50000_2154 <- st_read(here("geo","pts2154_2.shp"))
#
# # add Time correction
# #Sys.time()
# # Ajout de la variable jour et de la variable heure
Pts_50000_2154 <- Pts_50000_2154 %>%
  mutate(DateJour=as.Date(Date)) %>%
  mutate(DateHeure=hour(as.POSIXct(Date))) %>%
  mutate(JourJ=wday(as.POSIXct(DateJour), label = TRUE, abbr=FALSE)) %>%
  mutate(JourSem = case_when(JourJ == "Samedi" ~ "S",
                             JourJ == "Dimanche" ~ "D",
                             TRUE ~ "LV")) %>%
  mutate(Correctif = case_when(JourSem == "LV" &&  DateHeure == 0 ~ 3.82629871477906,
                               JourSem == "LV" &&  DateHeure == 1 ~ 6.15994336459467,
                               JourSem == "LV" &&  DateHeure == 2 ~ 7.95864096513497,
                               JourSem == "LV" &&  DateHeure == 3 ~ 8.83512319743405,
                               JourSem == "LV" &&  DateHeure == 4 ~ 7.99185994519303,
                               JourSem == "LV" &&  DateHeure == 5 ~ 5.16928070062316,
                               JourSem == "LV" &&  DateHeure == 6 ~ 1.04663871023969,
                               JourSem == "LV" &&  DateHeure == 7 ~ -2.14561788976771,
                               JourSem == "LV" &&  DateHeure == 8 ~ -4.6464965444681,
                               JourSem == "LV" &&  DateHeure == 9 ~ -4.54152190004787,
                               JourSem == "LV" &&  DateHeure == 10 ~ -4.28640014209438,
                               JourSem == "LV" &&  DateHeure == 11 ~ -3.84647144855295,
                               JourSem == "LV" &&  DateHeure == 12 ~ -3.18634609926391,
                               JourSem == "LV" &&  DateHeure == 13 ~ -3.4288556614073,
                               JourSem == "LV" &&  DateHeure == 14 ~ -3.26224274719903,
                               JourSem == "LV" &&  DateHeure == 15 ~ -3.39232475189917,
                               JourSem == "LV" &&  DateHeure == 16 ~ -3.53448672074911,
                               JourSem == "LV" &&  DateHeure == 17 ~ -3.62662202332034,
                               JourSem == "LV" &&  DateHeure == 18 ~ -3.57040447264549,
                               JourSem == "LV" &&  DateHeure == 19 ~ -3.10009582047105,
                               JourSem == "LV" &&  DateHeure == 20 ~ -2.19209986542425,
                               JourSem == "LV" &&  DateHeure == 21 ~ -0.445100982591946,
                               JourSem == "LV" &&  DateHeure == 22 ~ 0.654308664171879,
                               JourSem == "LV" &&  DateHeure == 23 ~ 1.46642945533455,

                               JourSem == "S" &&  DateHeure == 0 ~ 1.48780692362609,
                               JourSem == "S" &&  DateHeure == 1 ~ 3.09915923846212,
                               JourSem == "S" &&  DateHeure == 2 ~ 4.62903516543763,
                               JourSem == "S" &&  DateHeure == 3 ~ 6.13993778082778,
                               JourSem == "S" &&  DateHeure == 4 ~ 6.40244175194746,
                               JourSem == "S" &&  DateHeure == 5 ~ 5.17642512147307,
                               JourSem == "S" &&  DateHeure == 6 ~ 2.9491430853308,
                               JourSem == "S" &&  DateHeure == 7 ~ -1.42561176025025,
                               JourSem == "S" &&  DateHeure == 8 ~ -0.440026522327699,
                               JourSem == "S" &&  DateHeure == 9 ~ -1.48013339524666,
                               JourSem == "S" &&  DateHeure == 10 ~ -2.29622099586781,
                               JourSem == "S" &&  DateHeure == 11 ~ -2.67927910001206,
                               JourSem == "S" &&  DateHeure == 12 ~ -2.27183464187897,
                               JourSem == "S" &&  DateHeure == 13 ~ -2.01266519396376,
                               JourSem == "S" &&  DateHeure == 14 ~ -2.05246937255653,
                               JourSem == "S" &&  DateHeure == 15 ~ -2.13855296898559,
                               JourSem == "S" &&  DateHeure == 16 ~ -2.32840417325521,
                               JourSem == "S" &&  DateHeure == 17 ~ -2.61733878769439,
                               JourSem == "S" &&  DateHeure == 18 ~ -2.7481531670295,
                               JourSem == "S" &&  DateHeure == 19 ~ -2.65541445542067,
                               JourSem == "S" &&  DateHeure == 20 ~ -2.31008690958376,
                               JourSem == "S" &&  DateHeure == 21 ~ -1.01611865483581,
                               JourSem == "S" &&  DateHeure == 22 ~ -0.203574157285303,
                               JourSem == "S" &&  DateHeure == 23 ~ 0.285955775220081,

                               JourSem == "D" &&  DateHeure == 0 ~ 1.07226853137743,
                               JourSem == "D" &&  DateHeure == 1 ~ 2.7327988750193,
                               JourSem == "D" &&  DateHeure == 2 ~ 4.35561515977382,
                               JourSem == "D" &&  DateHeure == 3 ~ 5.77008117491968,
                               JourSem == "D" &&  DateHeure == 4 ~ 6.33399045178363,
                               JourSem == "D" &&  DateHeure == 5 ~ 6.085218482836,
                               JourSem == "D" &&  DateHeure == 6 ~ 4.72012797591152,
                               JourSem == "D" &&  DateHeure == 7 ~ 3.68290925218579,
                               JourSem == "D" &&  DateHeure == 8 ~ 2.64380333439784,
                               JourSem == "D" &&  DateHeure == 9 ~ 1.09524976601212,
                               JourSem == "D" &&  DateHeure == 10 ~ -0.383394748825573,
                               JourSem == "D" &&  DateHeure == 11 ~ -0.857908225095848,
                               JourSem == "D" &&  DateHeure == 12 ~ -0.999227003048439,
                               JourSem == "D" &&  DateHeure == 13 ~ -0.905092243566273,
                               JourSem == "D" &&  DateHeure == 14 ~ -1.01840271281069,
                               JourSem == "D" &&  DateHeure == 15 ~ -1.59823569047699,
                               JourSem == "D" &&  DateHeure == 16 ~ -2.22671074482504,
                               JourSem == "D" &&  DateHeure == 17 ~ -2.16763540507277,
                               JourSem == "D" &&  DateHeure == 18 ~ -2.00389609002814,
                               JourSem == "D" &&  DateHeure == 19 ~ -1.79717136215053,
                               JourSem == "D" &&  DateHeure == 20 ~ -0.927374508864929,
                               JourSem == "D" &&  DateHeure == 21 ~ 0.384182112260127,
                               JourSem == "D" &&  DateHeure == 22 ~ 1.47996624855368,
                               TRUE ~ 2.66641002509156)) %>%
  mutate(leq_mean_corr = leq_mean + Correctif)
# #Sys.time()
#
Pts_50000_2154_sp <- as_Spatial(Pts_50000_2154)
proj4string(Pts_50000_2154_sp) <- CRS("+init=epsg:2154")
Pts_50000_WGS84 <- spTransform(Pts_50000_2154_sp ,CRS("+init=epsg:4326"))
proj4string(Pts_50000_WGS84) <- CRS("+init=epsg:4326")

# # NE FONCTIONNE PAS
# #STFDF_jour <- stConstruct(Pts_50000_2154, space=c('x','y'), time='Date')
# #spplot(STFDF_jour[,"2022-02-02",'leq_mean'],col.regions=brewer.pal(10,"Spectral"),cuts=10)
#
# ######################################
# # Spatial analysis - variogram - 50000 points
Pts_50000_2154$LEQ1s <- Pts_50000_2154$leq_mean_corr
#
# # NE FONCTIONNE PAS
# #var <- gstat::variogramST(leq_mean_corr~1,Pts_50000_WGS84,tunit="hours",assumeRegular=F,na.omit=T)
# #var <- gstat::variogramST(LEQ1s~1,Pts_50000_2154, tunit="hours", assumeRegular=F, na.omit=T)
#
# # NE SERT PAS
# #separableModel <- gstat::vgmST("separable", space = vgm(17.5, "Sph", 750,2.1), time = vgm(0.9, "Sph", 3.5, 0.1), sill = 124)
#
# #fit.StVariogram(empVgm, separableModel, fit.method = 7, stAni = 117.3,
# #                method = "L-BFGS-B",
# #               control = list(parscale = c(100, 1, 10, 1, 100)),
# #                lower = c(10, 0, 0.1, 0, 0.1), upper = c(2000, 1, 12, 1, 200))
#
var_50000 <- variogram(leq_mean_corr ~ 1, Pts_50000_2154, cutoff = 500, width = 25)
#
# #plot(var_50000)
#
# # INITIAL PIERRE
# # var_50000.Sph <- fit.variogram(var_50000, vgm(17.5, "Sph", 750,2.1))
# # attr(var_50000.Sph, "SSErr")
# # plot(var_50000,var_50000.Sph)
# #
# # #
# # var_50000.Exp <- fit.variogram(var_50000, vgm(sill = 120, "Exp", nugget = 60, range = 600))
# # attr(var_50000.Exp, "SSErr")
# # plot(var_50000,var_50000.Exp)
# #
# # # PAS DE CONVERGENCE
# # var_50000.Gau <- fit.variogram(var_50000, vgm(sill = 140, "Gau", nugget = 40, range = 800))
# # attr(var_50000.Gau, "SSErr")
# # plot(var_50000,var_50000.Gau)
# #
# # #
# # var_50000.Mat <- fit.variogram(var_50000, vgm(sill = 120, "Mat", nugget = 40, range = 600))
# # attr(var_50000.Mat, "SSErr")
# # plot(var_50000,var_50000.Mat)
#
# #lzn.fit = fit.variogram(var_50000, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
# #lzn.fit
#
var_test.fit <- fit.variogram(var_50000, vgm(sill = 95, "Mat", range = 50))
# #attr(var_test.fit, "SSErr")
# #plot(var_50000,var_test.fit)
#
var_50000.fit <- fit.variogram(var_50000, vgm(sill = 200, "Exp", nugget = 40, range = 600))
# #attr(var_50000.fit, "SSErr")
# #plot(var_50000,var_50000.fit)
#
# ######################################
# ## GRILLE DE PROJECTION
#grid
bb <- sp::bbox(Pts_50000_2154_sp)
cs <- c(1, 1)*25
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
grd
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(Pts_50000_2154_sp)))
#summary(sp_grd)

proj4string(sp_grd) <- CRS("+init=epsg:2154")
sp_grd84 <- spTransform(sp_grd ,CRS("+init=epsg:4326"))
sp_grd84 <-st_as_sf(sp_grd84)
#
# leaflet() %>%
#   #addTiles() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addCircleMarkers(
#     data = sp_grd84,
#     radius = 1,
#     stroke = FALSE, fillOpacity = 0.2
#   )
# #######################################
# ## spatial prediction
# #sel100 <- sample(1:78899,10000 )
# #Pts_50000_2154_sel <- Pts_50000_2154[sel100, ]
# #spatial prediction
#
sf_grd<- st_as_sf(sp_grd)
lz.sk <- krige(leq_mean_corr  ~ 1, Pts_50000_2154, sf_grd, var_50000.fit,nmin=5,nmax=10, maxdist =100) #maxdist =150
#lz.sk84 <- spTransform(lz.sk ,CRS("+init=epsg:4326"))
#lz.sk84<-st_as_sf(lz.sk84)
#dim(lz.sk[lz.sk$var1.var == "NA",])[1]/dim(lz.sk)[1]*100


lz.sk84 <- st_transform(lz.sk, 4326)
#lz.sk84_0 <- st_transform(lz.sk, 4326)
lz.sk84 <- na.omit(lz.sk84)

# #leaflet
map5 <- leaflet() %>%
  #addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    data = lz.sk84,
    color = ~palsound100(lz.sk84$var1.pred),
    radius = 2,
    stroke = FALSE, fillOpacity = 0.6
  ) %>%
  addLegend("bottomleft",
            pal = palsound100,
            #colors = ~pal2,
            values = lz.sk84$var1.pred,
            title = "Levels",
            labFormat = labelFormat(suffix = " dB",big.mark = " ", transform = identity),
            opacity = 1
  )
#```

#```{r,message=FALSE}
map5
#```
