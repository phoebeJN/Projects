
## 1. Backscatter - remote sensing in the ocean
<p><img align="right" width="400" height="1000" src="https://assets.datacamp.com/production/project_547/img/4167340394_cc0b979fac_b_crop.jpg"> 
Reflections. No, I’m not talking about contemplating your existence within the Tidyverse or understanding
what that spline did to your data. I’m talking about echoes. Specifically, acoustic echoes called “backscatter.” Marine scientists use acoustic backscatter to understand the distribution of organisms in the ocean.</p>
<p>In this analysis, we are going to wrangle active acoustic data and plot the mean volume backscatter associated with fish with swim-bladders in relation to bathymetry (depth of the seafloor).</p>
<p>These acoustic data were collected from a research vessel that crossed the shelf break in the Mid-Atlantic Bight (<a href="https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ngdc.mgg.wcd:HB1103_EK60">NOAA cruise HB1103</a>) and were preprocessed using the software, <a href="https://www.echoview.com/">Echoview</a>.</p>


```R
# Load the libraries
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(patchwork) # The Composer of Plots

# Read in the depth data
bottom <- read_csv("datasets/bottom_line.csv", 
                   col_types = cols(Ping_date = col_datetime(format = "%m/%d/%Y"))) %>% 
          rename_all(tolower)

# Glimpse the data
glimpse(bottom)
```

    Rows: 2,766
    Columns: 10
    $ ping_date         [3m[38;5;246m<dttm>[39m[23m 2011-06-18, 2011-06-18, 2011-06-18, 2011-06-18, ...
    $ ping_time         [3m[38;5;246m<time>[39m[23m 09:53:37, 09:53:42, 09:58:47, 09:58:52, 09:58:57...
    $ ping_milliseconds [3m[38;5;246m<dbl>[39m[23m 725, 741, 866, 866, 866, 866, 882, 882, 882, 882,...
    $ latitude          [3m[38;5;246m<dbl>[39m[23m 999.00000, 38.29771, 38.29429, 38.29424, 38.29418...
    $ longitude         [3m[38;5;246m<dbl>[39m[23m 999.00000, -74.00185, -73.99677, -73.99666, -73.9...
    $ position_status   [3m[38;5;246m<dbl>[39m[23m 4, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    $ depth             [3m[38;5;246m<dbl>[39m[23m 68.60377, 68.60024, 68.78515, 68.77859, 68.37986,...
    $ line_status       [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    $ ping_status       [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    $ altitude          [3m[38;5;246m<dbl>[39m[23m -9.9e+37, 0.0e+00, 0.0e+00, 0.0e+00, 0.0e+00, 0.0...



```R
# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

soln_bottom <- read_csv("datasets/bottom_line.csv", 
                        col_types = cols(Ping_date = col_datetime(format = "%m/%d/%Y"))) %>% 
               rename_all(tolower)

run_tests({
    test_that("packages are loaded", {
        expect_true("lubridate" %in% .packages(), info = "Did you load the lubridate package?")
        expect_true("patchwork" %in% .packages(), info = "Did you load the patchwork package?")
    })
    
    test_that("bottom data loaded correctly", {
        expect_equal(bottom, soln_bottom, info = "bottom contains the wrong values. Did you import the correct .csv file?")
        expect_identical(as.character(bottom$ping_date[1]), "2011-06-18", info = "Did you use the correct format in col_date()?" )
    })
})
```

    
    Attaching package: 'testthat'
    
    The following object is masked from 'package:dplyr':
    
        matches
    







    2/2 tests passed


## 2. What is the "shelf break"?
<p><img align="left" width="400" height="1000" src="https://assets.datacamp.com/production/project_547/img/map_trkln.png"></p>
<p>The red line in the map to the left is the ship’s track across the shelf break in the Mid-Atlantic Bight. But what is the “shelf break”? It’s the underwater version of a cliff. </p>
<p>In most marine ecosystems, the shelf break is also a highly dynamic and productive area that provides food and habitat for many species. From the smallest phytoplankton to the largest marine mammals, sharks, seabirds, tunas, and sea turtles - they all use this area at some point in their life cycles. And, we’re going to play with some active acoustic data from this fantastic region!</p>
<p>But first, let’s clean up the bathymetry (depth) data and get it ready for plotting. </p>


```R
# Clean the bottom data
bottom_clean <- bottom %>%
  filter(position_status == 1) %>%
  select(ping_date, ping_time, latitude, longitude, depth) %>%
  mutate(date_time = ping_date + ping_time,
       distance_between = c(0, 
                            geosphere::distHaversine(cbind(longitude[-n()], latitude[-n()]),
                                                     cbind(longitude[ -1], latitude[ -1]))),                         
       distance_along = cumsum(distance_between))
  
# Inspect the data
glimpse(bottom_clean)
```

    Rows: 2,764
    Columns: 8
    $ ping_date        [3m[38;5;246m<dttm>[39m[23m 2011-06-18, 2011-06-18, 2011-06-18, 2011-06-18, 2...
    $ ping_time        [3m[38;5;246m<time>[39m[23m 09:58:47, 09:58:52, 09:58:57, 09:59:02, 09:59:07,...
    $ latitude         [3m[38;5;246m<dbl>[39m[23m 38.29429, 38.29424, 38.29418, 38.29411, 38.29404, ...
    $ longitude        [3m[38;5;246m<dbl>[39m[23m -73.99677, -73.99666, -73.99653, -73.99641, -73.99...
    $ depth            [3m[38;5;246m<dbl>[39m[23m 68.78515, 68.77859, 68.37986, 68.37986, 68.37986, ...
    $ date_time        [3m[38;5;246m<dttm>[39m[23m 2011-06-18 09:58:47, 2011-06-18 09:58:52, 2011-06...
    $ distance_between [3m[38;5;246m<dbl>[39m[23m 0.00000, 11.47956, 12.77948, 13.24406, 14.17050, 1...
    $ distance_along   [3m[38;5;246m<dbl>[39m[23m 0.00000, 11.47956, 24.25904, 37.50310, 51.67360, 6...



```R
soln_bottom_clean <- soln_bottom %>%
  filter(position_status == 1)  %>%
  select(ping_date, ping_time, latitude, longitude, depth) %>%
  mutate(date_time = ping_date + ping_time, 
        distance_between = c(0, 
                             geosphere::distHaversine(cbind(longitude[-n()], latitude[-n()]),
                                                      cbind(longitude[ -1], latitude[ -1]))),                         
        distance_along = cumsum(distance_between))                                              


run_tests({
    test_that("correct columns were selected", {
        expect_identical(colnames(soln_bottom_clean), colnames(bottom_clean), 
                info = "Did you select the correct columns? Is there a typo?")
    })
    
    test_that("date_time is correct", {
        expect_identical(bottom_clean$date_time, soln_bottom_clean$date_time,
                         info = "date_time is not correct. Did you add ping_date and ping_time?")   
    })
})
    
```






    2/2 tests passed


## 3. Where ever you go, there you are
<p>Now that we have removed the bad data points and calculated the cumulative distance the ship traveled, let's plot the data. </p>
<p>A horizontal view of the ship's track will show us if the ship deviated from the track line or if there were any breaks in the data. </p>
<p>A plot of the depth of the seafloor along the track line will show us the position of the shelf break. In a spatial analysis of the track line data, we would typically work in the packages <code>sp</code> and <code>sf</code>, but that's a topic all its own. For now, we'll create a couple of track line plots with the latitude, longitude, depth, and distance along the track line.</p>


```R
# Reduce the size of the plots
options(repr.plot.width = 7, repr.plot.height = 5)

# Plot the ship's track
p_ship_track  <- ggplot(bottom_clean, aes(x = longitude, y = latitude)) +
  geom_point(size = 0.5) 
  labs(x = "Longitude", y = "Latitude")

# Plot the depth of the sea floor along the ship's track
p_bathymetry  <-  ggplot(bottom_clean, aes(x = distance_along, y = depth)) +
  geom_point(size = 0.5) +
  scale_y_reverse() +
  labs(x = "Distance along trackline (m)", y = "Depth (m)")

# Arrange the plots side by side for easier viewing
p_ship_track + p_bathymetry
```


    $x
    [1] "Longitude"
    
    $y
    [1] "Latitude"
    
    attr(,"class")
    [1] "labels"



![png](output_7_1.png)



```R
stud_LonLat <- p_ship_track
stud_bthy <- p_bathymetry

soln_LonLat <- ggplot(soln_bottom_clean, aes(longitude, latitude)) +
                  geom_point(size = 0.5) +
                 labs(x = "Longitude", y = "Latitude")

soln_bthy <- ggplot(soln_bottom_clean, aes(distance_along, depth)) +
                  geom_point(size = 0.5) +
                  scale_y_reverse() +
                  labs(x = "Distance along trackline (m)", y = "Depth (m)")

run_tests({    
    test_that("plots use correct x and y", {    
        expect_identical(deparse(stud_LonLat$mapping$x),deparse(soln_LonLat$mapping$x),
            info = 'The x aesthetic in p_ship_track is incorrect. Did you map it to longitude?')      
        expect_identical(deparse(stud_bthy$mapping$x),deparse(soln_bthy$mapping$x),
            info = 'The x aesthetic in p_bathymetry is incorrect. Did you map it to distance_along?')  
        expect_identical(deparse(stud_LonLat$mapping$y),deparse(soln_LonLat$mapping$y),
            info = 'The y aesthetic in p_ship_track is incorrect. Did you map it to latitude?')      
        expect_identical(deparse(stud_bthy$mapping$y),deparse(soln_bthy$mapping$y),
            info = 'The y aesthetic in p_bathymetry is incorrect. Did you map it to depth?')  
    })
    
    test_that("correct geoms were used", {
        expect_identical(class(stud_LonLat$layers[[1]]$geom)[1],class(soln_LonLat$layers[[1]]$geom)[1],
            info = 'There is no point layer in p_ship_track. Did you call geom_point()?')
        expect_identical(class(stud_bthy$layers[[1]]$geom)[1],class(soln_bthy$layers[[1]]$geom)[1],
            info = 'There is no point layer in p_bathymetry. Did you call geom_point()?')
        })
    
     test_that("the correct size parameter was used", {
        expect_identical(stud_LonLat$layers[[1]]$aes_params$size, soln_LonLat$layers[[1]]$aes_params$size,
            info = 'The size of the points in p_ship_track is incorrect. Did you set size = 0.5?')
        expect_identical(stud_bthy$layers[[1]]$aes_params$size, soln_bthy$layers[[1]]$aes_params$size,
            info = 'The size of the points in p_bathymetry is incorrect. Did you set size = 0.5?')
         })
    
    test_that("y axis was reversed", {
        expect_lt(ggplot_build(p_bathymetry)$layout$panel_scales_y[[1]]$range$range[1], 0,
                 label = "Did you reverse the y-axis? See the documentation link in the instructions.")
    })
})
   

```






    4/4 tests passed


## 4. Here fishy, fishy, fishy...
<p>Volume backscatter (Sv) is a measure of the relative density of organisms. In this case, because we preprocessed the data in <a href="https://www.echoview.com/">Echoview</a> to look for fish-like scattering, our final plot of the volume backscatter data, <code>Sv_mean</code>, will give us an indication of the distribution of fish along the track line.</p>
<p>Will there be sections of the track line with higher/lower densities if fish? Let's find out!</p>


```R
# Read in the acoustic data
acoustic <- read_csv("datasets/acoustic.csv", 
                 col_types = cols(Date_M = col_datetime(format = "%Y%m%d")))  %>% 
  filter(Lon_M != 999.0)

# Glimpse the data
glimpse(acoustic)
```

    Rows: 724
    Columns: 78
    $ Process_ID                           [3m[38;5;246m<dbl>[39m[23m 20216, 20216, 20216, 20216, 20...
    $ Interval                             [3m[38;5;246m<dbl>[39m[23m 4, 5, 6, 7, 8, 9, 10, 11, 12, ...
    $ Layer                                [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    $ Sv_mean                              [3m[38;5;246m<dbl>[39m[23m -67.97805, -67.65053, -66.6586...
    $ NASC                                 [3m[38;5;246m<dbl>[39m[23m 365.6001, 429.4046, 539.5769, ...
    $ Sv_max                               [3m[38;5;246m<dbl>[39m[23m -53.93325, -54.51390, -51.3186...
    $ Sv_min                               [3m[38;5;246m<dbl>[39m[23m -88.67275, -87.36100, -88.9946...
    $ Sv_noise                             [3m[38;5;246m<dbl>[39m[23m -967.8684, -967.6432, -967.623...
    $ NASC_noise                           [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ Height_mean                          [3m[38;5;246m<dbl>[39m[23m 53.25000, 58.00000, 58.00000, ...
    $ Depth_mean                           [3m[38;5;246m<dbl>[39m[23m 39.04617, 39.00000, 39.00000, ...
    $ Good_samples                         [3m[38;5;246m<dbl>[39m[23m 639, 522, 464, 464, 406, 464, ...
    $ Layer_depth_min                      [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ Layer_depth_max                      [3m[38;5;246m<dbl>[39m[23m 250, 250, 250, 250, 250, 250, ...
    $ Ping_S                               [3m[38;5;246m<dbl>[39m[23m 2, 14, 23, 31, 39, 46, 54, 61,...
    $ Ping_E                               [3m[38;5;246m<dbl>[39m[23m 13, 22, 30, 38, 45, 53, 60, 68...
    $ Ping_M                               [3m[38;5;246m<dbl>[39m[23m 7, 18, 26, 34, 42, 49, 57, 64,...
    $ Dist_S                               [3m[38;5;246m<dbl>[39m[23m 609.2889, 804.8728, 1010.5880,...
    $ Dist_E                               [3m[38;5;246m<dbl>[39m[23m 783.8686, 985.7557, 1188.6826,...
    $ Dist_M                               [3m[38;5;246m<dbl>[39m[23m 676.6425, 891.7338, 1086.3678,...
    $ VL_start                             [3m[38;5;246m<dbl>[39m[23m 600.4773, 785.7129, 986.4694, ...
    $ VL_end                               [3m[38;5;246m<dbl>[39m[23m 767.8645, 962.5547, 1159.6140,...
    $ VL_mid                               [3m[38;5;246m<dbl>[39m[23m 664.4884, 869.5859, 1060.3471,...
    $ Date_S                               [3m[38;5;246m<dbl>[39m[23m 20110618, 20110618, 20110618, ...
    $ Time_S                               [3m[38;5;246m<time>[39m[23m 09:58:47, 09:59:47, 10:00:32,...
    $ Date_E                               [3m[38;5;246m<dbl>[39m[23m 20110618, 20110618, 20110618, ...
    $ Time_E                               [3m[38;5;246m<time>[39m[23m 09:59:42, 10:00:27, 10:01:07,...
    $ Date_M                               [3m[38;5;246m<dttm>[39m[23m 2011-06-18, 2011-06-18, 2011-...
    $ Time_M                               [3m[38;5;246m<time>[39m[23m 09:59:12, 10:00:07, 10:00:47,...
    $ Lat_S                                [3m[38;5;246m<dbl>[39m[23m 38.29429, 38.29343, 38.29261, ...
    $ Lon_S                                [3m[38;5;246m<dbl>[39m[23m -73.99677, -73.99486, -73.9927...
    $ Lat_E                                [3m[38;5;246m<dbl>[39m[23m 38.29351, 38.29271, 38.29188, ...
    $ Lon_E                                [3m[38;5;246m<dbl>[39m[23m -73.99506, -73.99301, -73.9909...
    $ Lat_M                                [3m[38;5;246m<dbl>[39m[23m 38.29396, 38.29309, 38.29230, ...
    $ Lon_M                                [3m[38;5;246m<dbl>[39m[23m -73.99612, -73.99397, -73.9920...
    $ Exclude_below_line_depth_mean        [3m[38;5;246m<dbl>[39m[23m 68.43658, 68.25401, 68.22956, ...
    $ Alpha                                [3m[38;5;246m<dbl>[39m[23m 0.007856, 0.007856, 0.007856, ...
    $ Gain_constant                        [3m[38;5;246m<dbl>[39m[23m -9999, -9999, -9999, -9999, -9...
    $ Noise_Sv_1m                          [3m[38;5;246m<dbl>[39m[23m -999, -999, -999, -999, -999, ...
    $ Minimum_Sv_threshold_applied         [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    $ Minimum_integration_threshold        [3m[38;5;246m<dbl>[39m[23m -90, -90, -90, -90, -90, -90, ...
    $ Maximum_Sv_threshold_applied         [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ Maximum_integration_threshold        [3m[38;5;246m<dbl>[39m[23m 99, 99, 99, 99, 99, 99, 99, 99...
    $ Exclude_above_line_applied           [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    $ Exclude_above_line_depth_mean        [3m[38;5;246m<dbl>[39m[23m 10, 10, 10, 10, 10, 10, 10, 10...
    $ Exclude_below_line_applied           [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    $ Bottom_offset                        [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ Standard_deviation                   [3m[38;5;246m<dbl>[39m[23m 3.67224e-07, 3.45837e-07, 5.51...
    $ Skewness                             [3m[38;5;246m<dbl>[39m[23m 6.287088, 5.815767, 7.528268, ...
    $ Kurtosis                             [3m[38;5;246m<dbl>[39m[23m 50.453991, 45.173827, 76.99483...
    $ ABC                                  [3m[38;5;246m<dbl>[39m[23m 8.48232e-06, 9.96265e-06, 1.25...
    $ ABC_noise                            [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ Area_Backscatter_Strength            [3m[38;5;246m<dbl>[39m[23m -50.71486, -50.01625, -49.0243...
    $ Thickness_mean                       [3m[38;5;246m<dbl>[39m[23m 53.25000, 58.00000, 58.00000, ...
    $ Range_mean                           [3m[38;5;246m<dbl>[39m[23m 33.04617, 33.00000, 33.00000, ...
    $ Exclude_below_line_range_mean        [3m[38;5;246m<dbl>[39m[23m 62.43658, 62.25401, 62.22956, ...
    $ Exclude_above_line_range_mean        [3m[38;5;246m<dbl>[39m[23m 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
    $ Bad_data_no_data_samples             [3m[38;5;246m<dbl>[39m[23m 59, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    $ Beam_volume_sum                      [3m[38;5;246m<dbl>[39m[23m 7299.070, 5945.382, 5284.784, ...
    $ No_data_samples                      [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ C_good_samples                       [3m[38;5;246m<dbl>[39m[23m 639, 522, 464, 464, 406, 464, ...
    $ C_bad_data_no_data_samples           [3m[38;5;246m<dbl>[39m[23m 59, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    $ C_no_data_samples                    [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ Frequency                            [3m[38;5;246m<dbl>[39m[23m 38, 38, 38, 38, 38, 38, 38, 38...
    $ Grid_reference_line                  [3m[38;5;246m<chr>[39m[23m "\"Surface (depth of zero)\"",...
    $ Layer_top_to_reference_line_depth    [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ Layer_top_to_reference_line_range    [3m[38;5;246m<dbl>[39m[23m -6, -6, -6, -6, -6, -6, -6, -6...
    $ Layer_bottom_to_reference_line_depth [3m[38;5;246m<dbl>[39m[23m 250, 250, 250, 250, 250, 250, ...
    $ Layer_bottom_to_reference_line_range [3m[38;5;246m<dbl>[39m[23m 244, 244, 244, 244, 244, 244, ...
    $ Exclude_below_line_depth_min         [3m[38;5;246m<dbl>[39m[23m 68.28604, 68.21009, 68.20790, ...
    $ Exclude_below_line_range_min         [3m[38;5;246m<dbl>[39m[23m 62.28604, 62.21009, 62.20790, ...
    $ Exclude_below_line_depth_max         [3m[38;5;246m<dbl>[39m[23m 68.78515, 68.28604, 68.26494, ...
    $ Exclude_below_line_range_max         [3m[38;5;246m<dbl>[39m[23m 62.78515, 62.28604, 62.26494, ...
    $ Samples_Below_Bottom_Exclusion       [3m[38;5;246m<dbl>[39m[23m 2182, 1638, 1456, 1456, 1274, ...
    $ Samples_Above_Surface_Exclusion      [3m[38;5;246m<dbl>[39m[23m 48, 36, 32, 32, 28, 32, 28, 32...
    $ Samples_In_Domain                    [3m[38;5;246m<dbl>[39m[23m 2928, 2196, 1952, 1952, 1708, ...
    $ Bad_data_empty_water_samples         [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    $ C_bad_data_empty_water_samples       [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...



```R
soln_acoustic <- read_csv("datasets/acoustic.csv",col_types = cols(Date_M = col_datetime(format = "%Y%m%d")))  %>% 
  filter(Lon_M != 999.0)


run_tests({
    test_that("acoustic data loaded correctly", {
        expect_is(acoustic, "tbl_df", info = "Did you read in acoustic.csv with read_csv()?")
        expect_equal(acoustic, soln_acoustic, info = "acoustic contains the wrong values. Did you import the correct .csv file?")
        })
    
    test_that("acoustic data were filtered correctly", {
        expect_equal(nrow(acoustic), 724, info = "acoustic_clean does not have the correct number of rows.")
        })      
})
```






    2/2 tests passed


## 5. That's a lot of variables!
<p><strong>Wow! 724 observations of 78 variables!</strong> This is the full data export from Echoview, but remember, we’re only interested in the volume backscatter data, <code>Sv_mean</code>, and the few other variables needed to plot the data. </p>
<p>These backscatter data were integrated into grid cells that are 200 meters along the ship’s path (numbered in the variable, <code>Interval</code>), by 250 meters deep, (numbered in the variable, <code>Layer</code>), making a coordinate system referenced to the ship’s path and depth. We are going to explore the first depth layer.</p>


```R
# Create a list of variables to keep
variables_keep <- c("Interval", "Layer", "Sv_mean", "Frequency", 
               "Date_M", "Time_S", "Time_E", "Lat_M", "Lon_M")

# Select, rename, filter, mutate, and arrange the data 
Sv_layer1 <- acoustic %>%
    select(one_of(variables_keep)) %>% 
    rename(Spatial_interval = Interval, Date = Date_M) %>%
    filter(Layer == 1)  %>% 
    mutate(Datetime_start = Date + Time_S,
         Datetime_end = Date + Time_E)  %>% 
    arrange(Datetime_start) 

# Glimpse the cleaned acoustic data
glimpse(Sv_layer1)
```

    Rows: 362
    Columns: 11
    $ Spatial_interval [3m[38;5;246m<dbl>[39m[23m 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, ...
    $ Layer            [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    $ Sv_mean          [3m[38;5;246m<dbl>[39m[23m -67.97805, -67.65053, -66.65866, -68.24425, -69.02...
    $ Frequency        [3m[38;5;246m<dbl>[39m[23m 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38...
    $ Date             [3m[38;5;246m<dttm>[39m[23m 2011-06-18, 2011-06-18, 2011-06-18, 2011-06-18, 2...
    $ Time_S           [3m[38;5;246m<time>[39m[23m 09:58:47, 09:59:47, 10:00:32, 10:01:12, 10:01:52,...
    $ Time_E           [3m[38;5;246m<time>[39m[23m 09:59:42, 10:00:27, 10:01:07, 10:01:47, 10:02:22,...
    $ Lat_M            [3m[38;5;246m<dbl>[39m[23m 38.29396, 38.29309, 38.29230, 38.29147, 38.29067, ...
    $ Lon_M            [3m[38;5;246m<dbl>[39m[23m -73.99612, -73.99397, -73.99202, -73.98992, -73.98...
    $ Datetime_start   [3m[38;5;246m<dttm>[39m[23m 2011-06-18 09:58:47, 2011-06-18 09:59:47, 2011-06...
    $ Datetime_end     [3m[38;5;246m<dttm>[39m[23m 2011-06-18 09:59:42, 2011-06-18 10:00:27, 2011-06...



```R
soln_variables_keep <- c("Interval", "Layer", "Sv_mean", "Frequency", 
               "Date_M", "Time_S", "Time_E", "Lat_M", "Lon_M")
 
soln_Sv_layer1 <- soln_acoustic %>%
    select(one_of(variables_keep)) %>% 
    rename(Spatial_interval = Interval, Date = Date_M) %>%
    filter(Layer == "1")  %>% 
    mutate(Datetime_start = Date + Time_S,
           Datetime_end = Date + Time_E)  %>% 
    arrange(Datetime_start) 

run_tests({
    test_that("the columns and filter are correct", {
        expect_identical(colnames(soln_Sv_layer1), colnames(Sv_layer1), 
                info = "Did you select and rename the columns correctly? Is there a typo?")
        expect_true(all(Sv_layer1$Layer) == 1, info = "The Layer column is not correct. Did you filter for Layer == 1?")
    })
    test_that("datetimes were correctly created", {
        expect_identical(soln_Sv_layer1$Datetime_start, Sv_layer1$Datetime_start, info = "Something isn't correct with DT_S. Did you add the date and start time?")
        expect_identical(soln_Sv_layer1$Datetime_end, Sv_layer1$Datetime_end, info = "Something isn't correct with DT_S. Did you add the data and end time?")
        })
   })
```






    2/2 tests passed


## 6. A little more wrangling
<p>Great! All this is coming together nicely. In <code>bottom_clean</code> we have depth, distance along the track line, and timestamps. We now also have an almost clean acoustic dataset, <code>Sv_layer1</code>, with timestamps but no distance along the track line, and no depth information (we'll get to that in a minute). Also, <a href="https://www.echoview.com/">Echoview</a> uses -999.0 to indicate NAs. We need to remove those or our plot of backscatter will look a little wonky. </p>


```R
# More data wrangling...
Sv <- Sv_layer1 %>% 
  mutate(Distance_between = c(0, geosphere::distHaversine(cbind(Lon_M[-n()], Lat_M[-n()]),       
                                               cbind(Lon_M[  -1], Lat_M[  -1]))),
       Distance_along = cumsum(Distance_between)) %>%
  na_if(-999.0) %>% 
  mutate(Time_interval = interval(Datetime_start, Datetime_end))

# Glimpse the data
glimpse(Sv)

head(Sv, 3)
```

    Rows: 362
    Columns: 14
    $ Spatial_interval [3m[38;5;246m<dbl>[39m[23m 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, ...
    $ Layer            [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    $ Sv_mean          [3m[38;5;246m<dbl>[39m[23m -67.97805, -67.65053, -66.65866, -68.24425, -69.02...
    $ Frequency        [3m[38;5;246m<dbl>[39m[23m 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38...
    $ Date             [3m[38;5;246m<dttm>[39m[23m 2011-06-18, 2011-06-18, 2011-06-18, 2011-06-18, 2...
    $ Time_S           [3m[38;5;246m<time>[39m[23m 09:58:47, 09:59:47, 10:00:32, 10:01:12, 10:01:52,...
    $ Time_E           [3m[38;5;246m<time>[39m[23m 09:59:42, 10:00:27, 10:01:07, 10:01:47, 10:02:22,...
    $ Lat_M            [3m[38;5;246m<dbl>[39m[23m 38.29396, 38.29309, 38.29230, 38.29147, 38.29067, ...
    $ Lon_M            [3m[38;5;246m<dbl>[39m[23m -73.99612, -73.99397, -73.99202, -73.98992, -73.98...
    $ Datetime_start   [3m[38;5;246m<dttm>[39m[23m 2011-06-18 09:58:47, 2011-06-18 09:59:47, 2011-06...
    $ Datetime_end     [3m[38;5;246m<dttm>[39m[23m 2011-06-18 09:59:42, 2011-06-18 10:00:27, 2011-06...
    $ Distance_between [3m[38;5;246m<dbl>[39m[23m 0.0000, 211.7871, 192.3324, 204.8778, 209.1278, 18...
    $ Distance_along   [3m[38;5;246m<dbl>[39m[23m 0.0000, 211.7871, 404.1196, 608.9974, 818.1252, 10...
    $ Time_interval    [3m[38;5;246m<Interval>[39m[23m 2011-06-18 09:58:47 UTC--2011-06-18 09:59:42 ...



<table>
<caption>A tibble: 3 x 14</caption>
<thead>
	<tr><th scope=col>Spatial_interval</th><th scope=col>Layer</th><th scope=col>Sv_mean</th><th scope=col>Frequency</th><th scope=col>Date</th><th scope=col>Time_S</th><th scope=col>Time_E</th><th scope=col>Lat_M</th><th scope=col>Lon_M</th><th scope=col>Datetime_start</th><th scope=col>Datetime_end</th><th scope=col>Distance_between</th><th scope=col>Distance_along</th><th scope=col>Time_interval</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dttm&gt;</th><th scope=col>&lt;time&gt;</th><th scope=col>&lt;time&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dttm&gt;</th><th scope=col>&lt;dttm&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;Interval&gt;</th></tr>
</thead>
<tbody>
	<tr><td>4</td><td>1</td><td>-67.97805</td><td>38</td><td>2011-06-18</td><td>09:58:47</td><td>09:59:42</td><td>38.29396</td><td>-73.99612</td><td>2011-06-18 09:58:47</td><td>2011-06-18 09:59:42</td><td>  0.0000</td><td>  0.0000</td><td>2011-06-18 09:58:47 UTC--2011-06-18 09:59:42 UTC</td></tr>
	<tr><td>5</td><td>1</td><td>-67.65053</td><td>38</td><td>2011-06-18</td><td>09:59:47</td><td>10:00:27</td><td>38.29309</td><td>-73.99397</td><td>2011-06-18 09:59:47</td><td>2011-06-18 10:00:27</td><td>211.7871</td><td>211.7871</td><td>2011-06-18 09:59:47 UTC--2011-06-18 10:00:27 UTC</td></tr>
	<tr><td>6</td><td>1</td><td>-66.65866</td><td>38</td><td>2011-06-18</td><td>10:00:32</td><td>10:01:07</td><td>38.29230</td><td>-73.99202</td><td>2011-06-18 10:00:32</td><td>2011-06-18 10:01:07</td><td>192.3324</td><td>404.1196</td><td>2011-06-18 10:00:32 UTC--2011-06-18 10:01:07 UTC</td></tr>
</tbody>
</table>




```R
soln_Sv <- soln_Sv_layer1 %>% 
     mutate(Distance_between = c(0,
                                geosphere::distHaversine(cbind(Lon_M[-n()], Lat_M[-n()]),
                                                         cbind(Lon_M[  -1], Lat_M[  -1]))),
           Distance_along = cumsum(Distance_between)) %>%
    na_if(-999) %>% 
    mutate(Time_interval = interval(Datetime_start, Datetime_end))

run_tests({
    test_that("all columns are correctly named", {
        expect_equal(colnames(Sv), colnames(soln_Sv), 
                     info = "Did you use mutate to create Distance_between, Distance_along and Time_interval?")
    })
    test_that("all -999 changed to NA", {
        expect_equal(sum(Sv$Sv_mean == -999), 0, 
                     info = "Did you use na_if to replace all -999 with NA?")
        })
    test_that("Time_interval was correctly created", {
        expect_is(Sv$Time_interval, "Interval",
                 info = "tm_interval is not the correct class. Did you use lubridate's interva` to create tm_interval?")
        expect_equal(soln_Sv$tm_interval, Sv$tm_interval, 
                     info = "tm_interval is not correct. Did you use the start datetime and end datetime to create tm_interval?")
    })

})
```






    3/3 tests passed


## 7. Can't go spatial? Go temporal
<p>There is no depth information in the acoustic file. Well, that's not exactly true. One of those 78 variables was a mean depth, but it was an average of an average, and it was not as accurate as the depth data we have in <code>bottom_clean</code>. </p>
<p>You might have also noticed that the two data sets have different spatial resolutions. How can we pull the depth data out of <code>bottom_clean</code> and join it with the acoustic data? There are a few different spatial ways to do this, but because we are not working in the spatial packages, we'll write a function to figure out which data points from <code>bottom_clean</code> fall <em>within</em> the time interval, <code>Time_interval</code>, we just created.</p>


```R
# Function: assign Spatial_interval to bottom points that fall within Time_interval
get_Interval_by_time <- function(bottom_data){
  res <- Sv$Spatial_interval[bottom_data %within% Sv$Time_interval]
  if(length(res)==0) return(NA)         
  return(res)
}
 
# Map the track line interval value to bottom_clean
bottom_spatial_interval_segments <- bottom_clean  %>% 
    mutate(trackline_interval = purrr::map_dbl(date_time, get_Interval_by_time))

# Inspect the first 15 rows
head(bottom_spatial_interval_segments, 15)
```


<table>
<caption>A tibble: 15 x 9</caption>
<thead>
	<tr><th scope=col>ping_date</th><th scope=col>ping_time</th><th scope=col>latitude</th><th scope=col>longitude</th><th scope=col>depth</th><th scope=col>date_time</th><th scope=col>distance_between</th><th scope=col>distance_along</th><th scope=col>trackline_interval</th></tr>
	<tr><th scope=col>&lt;dttm&gt;</th><th scope=col>&lt;time&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dttm&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>2011-06-18</td><td>09:58:47</td><td>38.29429</td><td>-73.99677</td><td>68.78515</td><td>2011-06-18 09:58:47</td><td> 0.00000</td><td>  0.00000</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:58:52</td><td>38.29424</td><td>-73.99666</td><td>68.77859</td><td>2011-06-18 09:58:52</td><td>11.47956</td><td> 11.47956</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:58:57</td><td>38.29418</td><td>-73.99653</td><td>68.37986</td><td>2011-06-18 09:58:57</td><td>12.77948</td><td> 24.25904</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:02</td><td>38.29411</td><td>-73.99641</td><td>68.37986</td><td>2011-06-18 09:59:02</td><td>13.24406</td><td> 37.50310</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:07</td><td>38.29404</td><td>-73.99627</td><td>68.37986</td><td>2011-06-18 09:59:07</td><td>14.17050</td><td> 51.67360</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:12</td><td>38.29396</td><td>-73.99612</td><td>68.38039</td><td>2011-06-18 09:59:12</td><td>15.34577</td><td> 67.01937</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:17</td><td>38.29390</td><td>-73.99597</td><td>68.38039</td><td>2011-06-18 09:59:17</td><td>15.09648</td><td> 82.11585</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:22</td><td>38.29383</td><td>-73.99582</td><td>68.37777</td><td>2011-06-18 09:59:22</td><td>15.72843</td><td> 97.84428</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:27</td><td>38.29375</td><td>-73.99564</td><td>68.37777</td><td>2011-06-18 09:59:27</td><td>18.05774</td><td>115.90203</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:32</td><td>38.29368</td><td>-73.99545</td><td>68.44719</td><td>2011-06-18 09:59:32</td><td>18.00171</td><td>133.90374</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:37</td><td>38.29360</td><td>-73.99526</td><td>68.28604</td><td>2011-06-18 09:59:37</td><td>19.01490</td><td>152.91864</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:42</td><td>38.29351</td><td>-73.99506</td><td>68.28604</td><td>2011-06-18 09:59:42</td><td>19.74884</td><td>172.66748</td><td>4</td></tr>
	<tr><td>2011-06-18</td><td>09:59:47</td><td>38.29343</td><td>-73.99486</td><td>68.28604</td><td>2011-06-18 09:59:47</td><td>20.02998</td><td>192.69746</td><td>5</td></tr>
	<tr><td>2011-06-18</td><td>09:59:52</td><td>38.29335</td><td>-73.99464</td><td>68.26205</td><td>2011-06-18 09:59:52</td><td>20.51630</td><td>213.21376</td><td>5</td></tr>
	<tr><td>2011-06-18</td><td>09:59:57</td><td>38.29328</td><td>-73.99442</td><td>68.26305</td><td>2011-06-18 09:59:57</td><td>21.04787</td><td>234.26163</td><td>5</td></tr>
</tbody>
</table>




```R
soln_get_Interval_by_time <- function(bottom_data){
  res <- Sv$Spatial_interval[bottom_data %within% Sv$Time_interval]
  if(length(res)==0) return(NA)         # dealing with NAs
  return(res)
}
 
# Map the track line interval value to the bottom_clean data
soln_bottom_spatial_interval_segments <- soln_bottom_clean  %>% 
    mutate(trackline_interval = purrr::map_dbl(date_time, get_Interval_by_time))

run_tests({
    test_that("the answer is correct", {
    expect_equal(bottom_spatial_interval_segments, soln_bottom_spatial_interval_segments, 
        info = "bottom_spatial_interval_segments is not correct. Is there a typo in the name? Did you use `map_dbl()`?")
    })
})
```






    1/1 tests passed


## 8. Depth of an Interval
<p>Now that we have spatial track line intervals from the acoustic data assigned to each data point in <code>bottom_spatial_interval_segments</code>, we can calculate the mean depth for each <code>trackline_interval</code> along the track line. Then we'll need to join the two datasets on the <code>Spatial_interval</code> and <code>trackline_interval</code>. </p>
<p>Remember that we're only looking at the first depth layer (0 to 250 m). Because we do not want to insinuate that we're plotting data integrated over the entire water column, we will replace depths greater than 250 m with 250.</p>


```R
# Group bottom_clean and calculate the mean depth
bottom_intervals <- bottom_spatial_interval_segments %>%
    group_by(trackline_interval) %>%
    summarise(depth_mean = mean(depth)) %>%
    ungroup()

head(bottom_intervals, 3)

# Join the bottom intervals data to the acoustic data
Sv_and_depth <- Sv %>%
  left_join(bottom_intervals, by = c("Spatial_interval" = "trackline_interval")) %>% 
  mutate(depth_plot = ifelse(depth_mean >= 250, 250, depth_mean))

# Glimpse the data 
glimpse(Sv_and_depth)
```


<table>
<caption>A tibble: 3 x 2</caption>
<thead>
	<tr><th scope=col>trackline_interval</th><th scope=col>depth_mean</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>4</td><td>68.43658</td></tr>
	<tr><td>5</td><td>68.25401</td></tr>
	<tr><td>6</td><td>68.22956</td></tr>
</tbody>
</table>



    Rows: 362
    Columns: 16
    $ Spatial_interval [3m[38;5;246m<dbl>[39m[23m 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, ...
    $ Layer            [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    $ Sv_mean          [3m[38;5;246m<dbl>[39m[23m -67.97805, -67.65053, -66.65866, -68.24425, -69.02...
    $ Frequency        [3m[38;5;246m<dbl>[39m[23m 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38...
    $ Date             [3m[38;5;246m<dttm>[39m[23m 2011-06-18, 2011-06-18, 2011-06-18, 2011-06-18, 2...
    $ Time_S           [3m[38;5;246m<time>[39m[23m 09:58:47, 09:59:47, 10:00:32, 10:01:12, 10:01:52,...
    $ Time_E           [3m[38;5;246m<time>[39m[23m 09:59:42, 10:00:27, 10:01:07, 10:01:47, 10:02:22,...
    $ Lat_M            [3m[38;5;246m<dbl>[39m[23m 38.29396, 38.29309, 38.29230, 38.29147, 38.29067, ...
    $ Lon_M            [3m[38;5;246m<dbl>[39m[23m -73.99612, -73.99397, -73.99202, -73.98992, -73.98...
    $ Datetime_start   [3m[38;5;246m<dttm>[39m[23m 2011-06-18 09:58:47, 2011-06-18 09:59:47, 2011-06...
    $ Datetime_end     [3m[38;5;246m<dttm>[39m[23m 2011-06-18 09:59:42, 2011-06-18 10:00:27, 2011-06...
    $ Distance_between [3m[38;5;246m<dbl>[39m[23m 0.0000, 211.7871, 192.3324, 204.8778, 209.1278, 18...
    $ Distance_along   [3m[38;5;246m<dbl>[39m[23m 0.0000, 211.7871, 404.1196, 608.9974, 818.1252, 10...
    $ Time_interval    [3m[38;5;246m<Interval>[39m[23m 2011-06-18 09:58:47 UTC--2011-06-18 09:59:42 ...
    $ depth_mean       [3m[38;5;246m<dbl>[39m[23m 68.43658, 68.25401, 68.22956, 68.10563, 68.14753, ...
    $ depth_plot       [3m[38;5;246m<dbl>[39m[23m 68.43658, 68.25401, 68.22956, 68.10563, 68.14753, ...



```R
soln_bottom_intervals <- soln_bottom_spatial_interval_segments %>%
    group_by(trackline_interval) %>%
    summarize(depth_mean = mean(depth)) %>%
    ungroup()

soln_Sv_and_depth <- soln_Sv %>%
  left_join(soln_bottom_intervals, by = c("Spatial_interval" = "trackline_interval")) %>% 
  mutate(depth_plot = ifelse(depth_mean >= 250, 250, depth_mean))

run_tests({
    test_that("the answer is correct", {
    expect_equal(bottom_intervals, soln_bottom_intervals, 
        info = "The data frame is not correct. ")
    })
    test_that("the correct column was removed", {
    expect_equal(colnames(Sv_and_depth), colnames(soln_Sv_and_depth), 
        info = "The column names are incorrect. Did you remove `intvr` and create `depth_plot`?")
        })
    test_that("depth greater than 250 was reset to 250", {
        expect_equal(max(Sv_and_depth$depth_plot), 250, 
                     info = "The maximum plot depth is not 250 m. Check the ifelse() statement.")
        })     
})
```






    3/3 tests passed


## 9. Putting it all together
<p>Woohoo! We have done a lot of data wrangling! </p>
<p>Now it's time to plot <code>Sv_mean</code> in relation to the bathymetry (<code>depth_plot</code>) to find out where the high and low densities of fishes are along the track line. Because our y-axes are on two different scales, we'll create two plots and use the <code>patchwork</code> package again to put them in one figure.</p>


```R
# Top panel
Sv_mean_plot <- ggplot(Sv_and_depth, aes(x = Distance_along, y = Sv_mean)) +
  geom_line() +
  labs(y=expression(mean~volume~backscatter~S[v]~(dB))) +
  theme(axis.title.x=element_blank())

# Bottom panel
bathymetry <- ggplot(Sv_and_depth, aes(x = Distance_along, y = depth_plot)) +
  geom_line(size = 0.5) +
  scale_y_reverse() +
  labs(x = "Distance along trackline (m)", y = "Depth (m)")

# Display the two panels in one figure
Sv_mean_plot / bathymetry
```


![png](output_25_0.png)



```R
stud_Sv <- Sv_mean_plot
stud_bthy <- bathymetry

soln_Sv <- ggplot(soln_Sv_and_depth, aes(Distance_along, Sv_mean)) +
  geom_line() +
  labs(y=expression(mean~volume~backscatter~S[v]~(dB))) +
  theme(axis.title.x=element_blank())

soln_bthy <-ggplot(soln_Sv_and_depth, aes(Distance_along, depth_plot)) +
  geom_line(size = 0.5) +
  scale_y_reverse() +
  labs(x = "Distance along trackline (m)", y = "Depth (m)")

run_tests({
    test_that("plots use correct x and y", {    
        expect_identical(deparse(stud_Sv$mapping$x),deparse(soln_Sv$mapping$x),
            info = 'The x aesthetic in Sv_mean_plot is incorrect. Did you map it to Distance_along?')      
        expect_identical(deparse(stud_bthy$mapping$x),deparse(soln_bthy$mapping$x),
            info = 'The x aesthetic in bathymetry is incorrect. Did you map it to Distance_along?')  
        expect_identical(deparse(stud_Sv$mapping$y),deparse(soln_Sv$mapping$y),
            info = 'The y aesthetic in Sv_mean_plot is incorrect. Did you map it to Sv_mean?')      
        expect_identical(deparse(stud_bthy$mapping$x),deparse(soln_bthy$mapping$x),
            info = 'The y aesthetic in bathymetry is incorrect. Did you map it to depth_plot?')  
    })
    test_that("correct geoms were used", {
        expect_identical(class(stud_Sv$layers[[1]]$geom)[1],class(soln_Sv$layers[[1]]$geom)[1],
            info = 'There is no line layer in p_LonLat. Did you call `geom_line()`?')
        expect_identical(class(stud_bthy$layers[[1]]$geom)[1],class(soln_bthy$layers[[1]]$geom)[1],
            info = 'There is no line layer in p_bathymetry. Did you call `geom_line()`?')
        })
    
    test_that("y axis was reversed", {
        expect_lt(ggplot_build(stud_bthy)$layout$panel_scales_y[[1]]$range$range[1], 0,
                 label = "Did you reverse the y-axis? See the documentation link in the instructions.")
    })
})
   
```






    3/3 tests passed


## 10. So, where are the fishes?
<p>Nice looking plot!</p>
<p>If we assume that all the acoustic backscatter is only from fishes with swim bladders, and greater backscatter indicates higher densities of fish, where are most of the fish on this track line?</p>


```R
# Where do you think the fish are along this track line?

# Options: Shelf, Shelf Break, Offshore

(where_are_the_fishes <- "Shelf")
```


'Shelf'



```R
# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
    expect_true(where_are_the_fishes == "Shelf", 
        info = "The Shelf region has the most intense fish-like backscatter.")
    })
    # You can have more than one test
})
```






    1/1 tests passed

