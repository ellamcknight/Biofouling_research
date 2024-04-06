# Biofouling research

### *Research from chapter 4 of my PhD* 😃

This research investigates contemporary climate change and non-native species in marine brackish ecosystems. Here, the focus is on extreme events such as warming and freshening (a sudden decline in salinity). 
With increased rainfall possibly altering the dynamics of tidal and riverine ebb and flow oscillations, this research set out to understand the repercussions on biofouling communities.

We deployed settlement panels in two locations in Plymouth Water (Millbay & Queen Anne's Battery Marinas) distinguished by different salinity regimes, one with stable salinity regimes and the other with greater salinity fluctuations. To detect salinity dynamics we deployed high-resolution salinity metres in the study sites. Other environmental factors were recorded and measured. The settlement panels were left for two years and photographed monthly (sort of) to understand community dynamics and dominance vs the environmental influences.


## Analytical Techniques 
<pre>
◾ R 
◾ Generalised Additive Mixed Models
◾ Non-Metric Multidimensional scaling
</pre>

## Environmental data

Initial data cleaning and visualisations showed anomalies in the salinity and temperature data. Salinity was affected by drift. The temperature was affected by erroneous data points due to the hardware being removed for anti-fouling or data recovery.

Temperature was corrected by adding data from the light metre which recorded temperature. Metres where from [HOBO](https://www.onsetcomp.com/products/data-loggers/u24-002-c) & [HOBO](https://www.onsetcomp.com/products/data-loggers/ua-002-64).

The figure below shows a daily summary of a) minimum salinity, Millbay: grey & Queen Anne’s Battery: blue, b) mean temperature, Millbay: grey & Queen Anne’s Battery: red, and c) peaks show daily total precipitation.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/environs.png?raw=true)

To correct the salinity data I modelled salinity over time, extracted the residuals and incorporated these data into the model. There were also periods of erroneous data due to metre malfunctioning causing the data to flat line at zero; these data were removed. Below are the salinity residuals from running a linear regression on the salinity measurements for (a) Millbay and (b) Queen Anne’s Battery. These data were implemented into the Generalised Additive Mixed Models.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/residuals.png?raw=true)

## Data Preparation
The measured abiotic parameters were organized into the periods between each point of sampling (when each photograph was taken). The parameters were calculated as: 
- total amount of rain
- maximum temperature
- minimum salinity (minimum residual salinity)
  
Then the difference between the time periods for these factors were calculated. This metric was used to indicate whether large increases or decreases occurred leading up to the point of photographing.  Some data was missing due to errors in the sampling metres and this was made as NA (see script).

Below shows the data for Queen Anne's Battery. Sample points plus the calculated measures and then the differences that occurred between them.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Environs_differenced.png?raw=true)

Finally, additional columns were added to this data frame. Date, site etc
  month? season? add cols? row 332


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

Next I reduced the number of replications. Currently, there are multiple panels. I grouped across the panel ladders and summed the scaled abundance. This gave me 5 replicates per time point. Then with the species categorised, I grouped according the native/NNS status.
With these data I created a figure showing the change of scaled cover per native vs NNS over time.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Native_NNS_scaled.png?raw=true)

The figure above shows the scaled species cover averaged for all native (grey) and non-native (red) species, a) Millbay Marina and, b) Queens Anne's Battery Marina. Shaded area shows 95% confidence intervals. _Ciona spp_ was removed for this figure to show only the identifiable species status.

Next I looked at the dominant species: _Ascidiella aspersa_, _Ciona spp_, _Watersipora subatra_, _Tricellaria inopinata_. Here I looked at the percentage cover over time. I also added the sampling points; the dates at which the photographs were taken.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/Dominant%20species.png?raw=true)

