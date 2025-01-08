# HeFTy_PlotR
Customize HeFTy-generated tT plots for thermochronology interpretation

HeFty_plotR is an R script that plots time-temperature (tT) paths created using the HeFty thermochronologic data modeling software (Ketcham, 2005; 2024). The script can plot paths produced in any version of HeFty but options for plotting tT paths produced using HeFTy v1 are more limited than for paths produced using HeFTy v2.

For models produced in HeFTy v1, paths can be plotted using the default discrete acceptance criteria used in v1 based on the minimum goodness of fit of path predictions to individual observed dates or using a continuous color scale corresponding to the minimum goodness of fit to individual observed dates for each path (e.g., Peak et al., 2021 Fig. 3). For models produced in HeFTy v2, paths can be plotted using the default acceptance criteria used in v1, the default discrete acceptance criteria used in v2 based on combined goodness of fit of predictions to all observed dates, or using continuous color scales corresponding to the minimum individual date goodness of fit or combined dates goodness of fit. For information about the acceptance criteria used in each version of HeFTy see Ketcham, 2005 (v1) and Ketcham, 2024 (v2).

The script also plots tT path date predictions as frequency distributions compared with the observed dates in a manner similar to McDonnell and Keller (2022). If this option is selected, the script will output two histograms: one lumping all date predictions and one color coded using the same color scheme adopted for depicting prediction goodness of fit in the tT path plot.

Required Inputs:

1. HeFTy inverse path results exported from HeFTy as a .txt file.
  	- This is read into the script as "file.”

2. An .xlsx or .xls file of the modeled individual or binned thermochronologic dates and uncertainties.
	- This is read into the script as “data_file.”
 	- This is used to make histogram plots of the predicted vs. observed dates.
	- IMPORTANT! Dates should match exactly with dates input into and modeled in HeFTy - e.g., if uncorrected dates 	were modeled, the date input file should list uncorrected dates; if corrected dates were modeled, the 	file should list corrected dates. Dates must be listed in the same order they were input into HeFTy.

Examples of each input file using data published in Peak et al. (2023) are included in this repository. Example tT paths were generated in HeFTy v2. Example script outputs using this data are also included.

To report bugs or suggest improvements please email Barra at barra.peak@jsg.utexas.edu.

References:
Ketcham, R.A., 2024. Thermal history inversion from thermochronometric data and complementary information: New methods and recommended practices. Chemical Geology 653, 122042. https://doi.org/10.1016/j.chemgeo.2024.122042

Ketcham, R.A., 2005. Forward and Inverse Modeling of Low-Temperature Thermochronometry Data. Reviews in Mineralogy and Geochemistry 58, 275–314. https://doi.org/10.2138/rmg.2005.58.11

McDannell, K.T., Keller, C.B., 2022. Cryogenian glacial erosion of the central Canadian Shield: The “late” Great Unconformity on thin ice. Geology 50, 1336–1340. https://doi.org/10.1130/G50315.1

Peak, B.A., Flowers, R.M., Macdonald, F.A., Cottle, J.M., 2021. Zircon (U-Th)/He thermochronology reveals pre-Great Unconformity paleotopography in the Grand Canyon region, USA. Geology 49, 1462–1466. https://doi.org/10.1130/G49116.1

Peak, B.A., Flowers, R.M., Macdonald, F.A., 2023. Ediacaran-Ordovician tectonic and geodynamic drivers of Great Unconformity exhumation on the southern Canadian Shield. Earth and Planetary Science Letters 619, 118334. https://doi.org/10.1016/j.epsl.2023.118334

