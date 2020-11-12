# VitalRates
Measuring vital rates from fixed-site photomosaics.

The Patch_And_Colony_Data_20201103.rdata is the vital rates dataset created in partnership with the Logan Lab at CSUMB and NOAA PIFSC. This file contains the ColonyLevel dataframe and PatchLevel dataframe.

The VitalRates_stats.Rmd code takes the vital rates dataset to reproduce the analyses and figures for analyzing site-level and year-interval vital rate data.


**Data dictionary**

| Variable | Description |
|---|---|
| Site | Location where imagery was collected |
| DataorError | DATA = imagery collected >1 month, Error = imagery collected <1 month for error analysis |
| ColonyID | Unique identifer for each colony (Site_SpecCode_C_UniquePatchID_Annotator) |
| Spec_Code | Species code (PMEA, PGRA, MCAP, MPAT, PLIC, PLOB, PLUT) |
| Genus_Code | Genus code (POCS, MOSP, POSP) |
| StartingDate | Year the oldest colony was delineated |
| EndingDate | Year the colony was delineated |
| Interval_Years | Time interval between patches (older colony date - younger colony date / 365.25) |
| N_t0 | # of fragments in the oldest time point |
| N_t1 | # of fragments in the newer time point |
| StartingSize | Size of oldest colony (cm^2) |
| EndingSize | Size of newer colony (cm^2) |
| TransitionMagnitude | Change in Area in cm^2 (Area of newer-older colony * 10^4) |
| TransitionType | Growth, Shrink, Mortality, Recruitment, Fission, Fusion, and combinations of Growth, Shrink, Fission and Fusion |
| PercentChange | |
| Log2Ratio_Change | |
| TransitionTypeSimple | |

