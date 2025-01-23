# Biofouling research

### *Research from chapter 4 of my PhD* 

This research investigates contemporary climate change and non-native species in marine brackish ecosystems. Here, the focus is on extreme events such as warming and freshening (a sudden decline in salinity). 
With increased rainfall possibly altering the dynamics of tidal and riverine ebb and flow oscillations, this research set out to understand the repercussions on biofouling communities.

We deployed settlement panels in two locations in Plymouth Water (Millbay & Queen Anne's Battery Marinas) distinguished by different salinity regimes, one with stable salinity regimes and the other with greater salinity fluctuations. To detect salinity dynamics we deployed high-resolution salinity metres in the study sites. Other environmental factors were recorded and measured. The settlement panels were left for two years and photographed monthly (sort of) to understand community dynamics and dominance vs the environmental influences.


## Analytical Techniques 
<pre>
◾ R 
◾ Visualisations
◾ Generalised Additive Mixed Model with autocorrelation term
◾ Non-Metric Multidimensional scaling
</pre>

## Environmental data

Initial data cleaning and visualisations showed anomalies in the salinity and temperature data. Salinity was affected by drift. The temperature was affected by erroneous data points due to the hardware being removed for anti-fouling or data recovery.

Temperature was corrected by adding data from the light metre which recorded temperature. Metres where from [HOBO](https://www.onsetcomp.com/products/data-loggers/u24-002-c) & [HOBO](https://www.onsetcomp.com/products/data-loggers/ua-002-64).

The figure below shows a daily summary of a) minimum salinity, Millbay: grey & Queen Anne’s Battery: blue, b) mean temperature, Millbay: grey & Queen Anne’s Battery: red, and c) peaks show daily total precipitation.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/environs.png?raw=true)

To correct the salinity data, it was modelled over time and the residuals were extracted; these data were incorporated into the model. There were also periods of erroneous data due to metre malfunctioning causing the data to flat line at zero; these data were removed. Below are the salinity residuals from running a linear regression on the salinity measurements for (a) Millbay and (b) Queen Anne’s Battery. These data were implemented into the Generalised Additive Mixed Models.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/residuals.png?raw=true)

## Data Preparation
The measured abiotic parameters were organized into the periods between each point of sampling (when each photograph was taken). The parameters were calculated as: 
- total amount of rain
- maximum temperature
- minimum salinity (minimum residual salinity)
  
Then the difference between the time periods for these factors were calculated. This metric was used to indicate whether large increases or decreases occurred leading up to the point of photographing.  Some data was missing due to errors in the sampling metres and this was made as NA (see script).

Below shows the data for Queen Anne's Battery. Sample points plus the calculated measures and then the differences that occurred between them.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Environs_differenced.png?raw=true)

Finally, additional columns were added to this data frame. 

## Biological data

Using ImageJ software, photographs were analysed to identify species' spatial coverage.  Sessile organisms were identified to the lowest taxonomic level (predominantly to species level, but if not possible, to genus) and quantified by percentage cover for every individual (solitary taxa) or colony (colonial taxa) occurring in each panel. Species were identified as native or non-native to the region following the World Register of Introduced Marine Species [WRiMS](https://www.marinespecies.org/introduced/) and literature sources. We were unable to classify _Ciona spp_. as native or non-native due to overlap between the native species _Ciona intestinals_ and the introduced _Ciona robusta_ in this area of their geographical distributions - differentiating between the two species requires genetic analyses and / or dissections. Colonies or individuals that were heavily overgrown and clearly smothered by other encrusting taxa were assumed to be non-functional and were not counted. However, due to the 3-dimensional nature of the panels some percentage cover exceeded 100.

To explore relative differences in the response of species to environmental fluctuations, species percentage cover was standardised and scaled according to their maximum.

### Data Preparation Steps
- created a separate data frame for each site
- converted into a long data frame with species and per cent cover in single columns
- calculated the scaled percentage cover for each species.
- classified their native/NNS status

Here is a visual of the raw data

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Raw_biotic.png?raw=true)

This figure shows a stacked plot showing raw data of species percentage cover across individual panels. Native (grey) and non-native (red) species identified at (a) Millbay and (b) Queen Anne’s Battery over the 2-year sampling period. It should be noted that _Ciona spp_ was calculated as native for this figure.

Next, I reduced the number of replications. Currently, there are multiple panels across the grids (grid is a ladder structure that held 6 panels each, there were 5 grids). I grouped them according to grids and then summed the scaled percentage cover. This gave me 5 replicates per time point. Then with the species categorised, they were grouped according to the native/NNS status.
With these data I created a figure showing the change of scaled cover per native vs NNS over time.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Native_NNS_scaled.png?raw=true)

The figure above shows the scaled species cover averaged for all native (grey) and non-native (red) species, a) Millbay Marina and, b) Queens Anne's Battery Marina. Shaded area shows 95% confidence intervals. _Ciona spp_ was removed for this figure to show only the identifiable species status.

Next, I looked at the dominant species: _Ascidiella aspersa_, _Ciona spp_, _Watersipora subatra_, _Tricellaria inopinata_. Here I looked at the percentage cover over time. I also added the sampling points; the dates at which the photographs were taken.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Dominant%20species.png?raw=true)

## Generalised Additive Mixed Models

At this point all the data was combined. The difference in percentage cover per native/NNS/ciona spp on each replicate was calculated and noted as a change column. 

For the GAMMs, species cover was evaluated as a function of the environmental variables and nativeness (native or NNS or Ciona) was used as a fixed effect. Trends in cover were modelled as non-linear via smooth functions using penalized regression splines with minimum salinity, mean temperature and total precipitation. A stepwise deletion process was used by first reducing non-significant smooth parameters to a linear term, if a linear term was found to be non-significant then the parameter was removed from the model and the process was repeated. We included random effects of ‘grid’ nested in ‘site’, and month was modelled with a cyclic cubic spline (i.e., the temperature in
January is aligned with the temperature at the end of December). For this temporal spline, the basic dimensions were constrained for the smoothing functions, this allowed the natural cycle of seasons to be accounted for but left the additional variability to be explained by salinity, temperature or precipitation that fell outside the typical seasonal pattern. An autocorrelation
term was added to the model because the experimental design consisted of repeated measures. Day count (number of days since the experiment began) and temporal autocorrelation were modelled by fitting an auto-regressive moving average model (ARMA) to the residuals. The number of auto-regressive parameters were specified as p = 1 therefore modelling neighbouring days, and the ARMA was nested within each day count.

The full formula code prior to the step-wise deletion:
`mod <- gamm(Change ~ Site + Status + s(month, bs = "cc", k=6) + s(TempDiff) + s(SalResDiff) + s(RainDiff), random=list(Site=~1,Position=~1), method = 'REML', correlation = corARMA(form = ~ 1|DayCount, p = 1), data = alldat, knots = list(month = c(1, 12)))`

Variables were step-wise deleted based on significance. Below is the summary output.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/GAMM_alldat_output.png?raw=true)

The full model produced this output (base plots):
![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Gamm_full_mod.png?raw=true)

The above shows the best regression spline for the significant variables. Month, temperature and salinity were all significant. Nativeness (native, NNS and Ciona) were not significantly different from each other, however, I wanted to explore these factors in more detail as the model output does not separate the splines based on the fixed factor and I believe patterns are being lost in this model.

Native species GAMM 
![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Native_Gamm.png?raw=true)

NNS GAMM
![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/NNS_gamm.png?raw=true)

_Ciona spp_ Gamm
![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Ciona_gamm.png?raw=true)

For each of the above models (native, NNS and Ciona spp data), each model found month, temperature, and salinity to be significant. However, there does seem to be some differences in the final patterns of the splines when compared. Primarily the _Cion spp_ found temperature to have a negative linear relationship with the species cover change

Visualisations??

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Gamms_month_sep_species.png?raw=true)

Temporal trend of species cover for native, non-native and _Cion spp_ species analysed with Generalised Additive Mixed Models. Positive direction on the y-axis indicates an increase and negative indicates a decrease. Natives (black), non-natives (red) and _Cion spp_ (blue) are shown with sites combined. The solid line represents the best regression spline, and the shadowed line is the confidence interval at 95%. Patterns of species cover are compared to changes over time.

Altogether:

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Gamm_month_together.png?raw=true)

The above shows the output for species change over time modelling all of the data.

Temperature change _vs_ species change:

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Gamm_temp_allspp.png?raw=true)

The above figure shows the temporal trend of species cover for native, non-native and _Cion spp_ species analysed with Generalised Additive Mixed Models, splines were taken from each separate model. The solid lines represent the best regression splines, and the shadowed line is the confidence interval at 95%. Patterns of species cover are compared to changes in temperature.

Salinity change _vs_ species change

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Gamm_salinity_allspp.png?raw=true)

The above figure shows the temporal trend of species cover for native, non-native and _Cion spp_ species analysed with Generalised Additive Mixed Models, splines were taken from each separate model. The solid lines represent the best regression splines, and the shadowed line is the confidence interval at 95%. Patterns of species cover are compared to changes in salinity (residuals). This graph might be better as 3 separate figures. The figures show there is a difference in the regression splines (although not statistically significant) between the species types. _Discuss the interpretation_

## Non-Metric Multidimensional scaling

Multidimensional scaling was used to analyse community composition and distributions within the package ‘vegan’. To investigate the trajectory of the community structure through time, species were averaged across all grids to produce one sample point, per time and per site, then constructed into a Bray-Curtis dissimilarity matrix using 9999 permutations of the data. Non-metric multidimensional scaling was used as an ordination technique to graphically represent the temporal community composition.

The below shows Multi-Dimensional Scaling plots of Bray-Curtis similarities of community structure for both sites are shown with trajectory of temporal development. Colours show Queen Anne’s Battery as blue triangles and Millbay as grey circles.
The patterns show that succession is slow and as the community develops it never returns to the original state (barren panels). There are some large changes between data points where the direction and position shift. This represents an extreme change in the community composition. Also, there are numerous points clustered together, these representing time points where the community assemblage remained somewhat similar.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/NMDS.png?raw=true)

To be added: map of sites and panels



