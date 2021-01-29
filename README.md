# VitalRates
Measuring vital rates from fixed-site photomosaics.

The Patch_And_Colony_Data_20201103.rdata is the vital rates dataset created in partnership with the Logan Lab at CSUMB and NOAA PIFSC. This file contains the ColonyLevel dataframe and PatchLevel dataframe.

The VitalRates_stats.Rmd code takes the vital rates dataset to reproduce the analyses and figures for analyzing site-level and year-interval vital rate data.
The VitalRateFunctions_CR.Rmd script uses the vital rates dataset to model different vital rate functions, which is the first step in building an Integral Projection Model


**Data Dictionary**

| Variable | Description |
|---|---|
| Site | Location where imagery was collected |
| DataorError | DATA = imagery collected >1 month, Error = imagery collected <1 month for error analysis |
| ColonyID | Unique identifer for each colony (Site_SpecCode_C_UniquePatchID_Annotator) |
| Spec_Code | Species code (PMEA, PGRA, MCAP, MPAT, PLIC, PLOB, PLUT) |
| Genus_Code | Genus code (POCS, MOSP, POSP) |
| StartingDate | Date the older colony was delineated (YYYY-MM-DD) |
| EndingDate | Date the colony was delineated in the next time point (YYYY-MM-DD) |
| Interval_Years | Time interval between patches (older colony date - younger colony date / 365.25) |
| N_t0 | # of fragments in the oldest time point |
| N_t1 | # of fragments in the newer time point |
| StartingSize | Size of older colony (cm^2) |
| EndingSize | Size of newer colony (cm^2) |
| TransitionMagnitude | Change in Area in cm^2 (Area of newer-older colony * 10^4) |
| TransitionType | Growth, Shrink, Mortality, Recruitment, Fission, Fusion, and combinations of Growth, Shrink, Fission and Fusion |
| PercentChange | Percent change, growth or shrinkage, for each colony (100 * TransitionMagnitude/StartingSize) |
| Log2Ratio_Change | The log transformed amount of change from the ending size to the starting size of the colony (log2(EndingSize/StartingSize)) |
| TransitionTypeSimple | Overall positive change in area = GROWTH, overall negative change in area = SHRINK, and RECR or MORT |
| Fragmented | Whether or not coral broke into pieces (if N_t0 or N_t1 are >1, return TRUE to signify fragmentation) |
| Genus | Genus (Pocillopora sp., Montipora sp., Porites sp.) |
| Recruit | Indicates whether there was a recruitment event, 1 = recruitment |
| Mortality | Indicates whether there was a mortality event, 1 = mortality |
| Fragmentation | Indicates whether a colony fragmented, 1 = fragmentation |
| ln_SS | Log starting colony size |
| ln_ES | Log ending colony size |
| StartingYear | Year the oldest colony was delineated |
| Ending Year | Year the colony was delineated in the next time point |
| Interval | Older and newer time point for the colony (YY-YY) |
| SiteInterval | Site and interval (Site YY-YY) |
| Island | Island where imagery was collected |
| PropMagnitude | Magnitude of change (EndingSize/StartingSize) |
| AnnualPropRate_E | Magnitude of change for each time interval (PropMagnitude^(1/IntervalYears)) |
| TransitionRate_L | Change in area per time interval (TransitionMagnitude/Interval_Years) |
| AnnualEndingSize_E | (StartingSize*AnnualPropRate_E) |
| TriennialEndingSize_E | (StartingSize*AnnualPropRate_E^3) |
| ln_AES | Log AnnualEndingSize |
| ln_3ES | Log TriennialEndingSize |


**To Do List**
- [ ] ANCOVA to compare growth rates b/w 2 species. Use avg growth in ANCOVA test.
- [X] Re-plot at colony level
- [ ] Calculate the # of growth, recruitment, and mortality events per year and per species
- [X] Set-up GitHub page


