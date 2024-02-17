# Problem
Quantitative size analysis of nucleic acids or proteins often requires precise concentration measurements. SDS-PAGE is an electrophoretic separation method that sorts a heterogenous nucleic acid or protein mixture based on size. However, unlike orthogonal, quantitative methods like chromatography, SDS-PAGE is a semi-quantitative method that can't measure concentration directly. In molecular biology labs where chromatographic equipment is unavailable, SDS-PAGE may be a versatile option to obtain separation data.

## SDS-PAGE Backgronud
In SDS-PAGE, samples are usually prepared using a series of enzymatic treatments and then separated by molecular weight using an electric current on a gel matrix. After staining, the molecular weight of each separated species, as indicated by a band, can be imaged on an gel imaging instrument and processed using an imaging software and the position of a known molecular weight ladder. While the molecular weight ladder can estimate a sample's molecular weight based on vertical position on the gel, I always thought it'd be useful if analyte concentration could be obtained from band intensity to calculate the approximate sample composition.

# Solution
This code approaches concentration determination using band intensity to convert a HEX code into grayscale intensity using the luminosity method. Using a linear calibration curve, this code interpolates concentration from grayscale intensity. To ensure that the band of interest can in fact be interpolated, I made a calibration plot and plotted the estimated concentration of the band of interest as well as the standard deviation using a dot and whisker aesthetic.

## Wet Lab Set-up
Assuming a relatively predictable response and similar image processing protocol, my experimental approach to this task would be to designate 2 wells in the SDS-PAGE gel to obtain the 2 calibration points at 1 mg/mL and 15 mg/mL. Using this calibration curve, I'd extract the HEX code from the band of interest, plug it into this program, and estimate concentration ± standard deviation.

## Modifications
This code uses the ggplot2 library to make the calibration plot with the interpolated band of intensity point. Depending on your needs, one modification to this code might include changing the HEX codes of the 1 mg/mL and 15 mg/mL points depending on the color saturation of your imaged gel. I assumed a linear calibration curve for ease of analysis, but if you don't think your band intensities increase linearly with concentration, you could always add a few intermediate, checkpoint concentrations to the calibration curve. If you're unsure of whether your calibration curve is linear, you could designate wells on your gels as internal standards by injecting a known concentration onto the gel, assessing the calculated concentration, and adjusting your calibration function accordingly.