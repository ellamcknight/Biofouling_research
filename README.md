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






