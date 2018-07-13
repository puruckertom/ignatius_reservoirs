# ignatius_reservoirs

-raw_708reservoirs.xlxs contains data for 708 reservoirs. We'd like to see if any of the descriptive lake variables are good predictors of CyAN variables such as "bloom frequency" or "maximum bloom concentration" (highlighted in blue). Please note: some of the descriptive variables are "junk" such as unique ids, empty fields, etc. I need to spend a few days further cleaning up the data. The xlxs tab "metadata" may have some helpful information.

response variables are: MaxAreaLowIntensityBloom	MaxAreaModerateIntensityBloom	MaxAreaHighIntensityBloom	MaxCyanConcentration	Freq

-raw_708reservoir_timeseries.xlxs is weekly output for the 708 reservoirs. We flagged snow/ice dates using GEE.

# running jupyter
    
    git clone ...
    cd ignatius_reservoirs
    jupyter lab
