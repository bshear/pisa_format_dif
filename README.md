Code to accompany the paper ["Gender Bias in Test Item Formats: Evidence from PISA 2009, 2012, and 2015 Math and Reading Tests"](https://onlinelibrary.wiley.com/doi/full/10.1111/jedm.12372).

1: item format prep

2: PISA item response data prep

3: OSM descriptives

4: US DIF models and regressions

5: International DFF models

6: Paper tables and figures

Online Supplementary Materials PDF [here](https://github.com/bshear/pisa_format_dif/blob/main/submit_item_formats_osm_accepted.pdf).

Technical documentation for item format information are available at the following links.

PISA 2009: https://www.oecd.org/pisa/pisaproducts/50036771.pdf (Annex A and Cognitive Item codebook)

PISA 2012: https://www.oecd.org/pisa/pisaproducts/PISA-2012-technical-report-final.pdf (Annex A)

PISA 2015: https://www.oecd.org/pisa/data/2015-technical-report/ (Annex A)

Original PISA data are publicly available at the following links.

PISA 2009: https://www.oecd.org/pisa/pisaproducts/pisa2009database-downloadabledata.htm

PISA 2012: https://www.oecd.org/pisa/pisaproducts/pisa2012database-downloadabledata.htm

PISA 2015: https://www.oecd.org/pisa/data/2015database/

For 2009 .sav files are produced by running the .sps files provided by OECD
- INT_STQ09_DEC11.txt       -> INT_STQ09_SPSS_DEC11.sps    -> INT_STQ09_DEC11.sav
- NT_COG09_S_SPSS_DEC11.txt -> INT_COG09_S_SPSS_DEC11.sps  -> NT_COG09_S_SPSS_DEC11.sav
- INT_COG09_TD_DEC11.txt    -> INT_COG09_TD_SPSS_DEC11.sps -> INT_COG09_TD_DEC11.sav

For 2012 .sav filees are produced by running the .sps files provided by OECD
- INT_STU12_DEC03.txt   -> PISA2012_SPSS_student.sps               -> INT_STU12_DEC03.sav
- INT_COG12_S_DEC03.txt -> PISA2012_SPSS_scored_cognitive_item.sps -> INT_COG12_S_DEC03.sav
- INT_COG12_DEC03.txt   -> PISA2012_SPSS_cognitive_item.sps        -> INT_COG12_DEC03.sav

For 2015 OECD provided .sav files that can be read in directly without processing with .sps
- CY6_MS_CMB_STU_QQQ.sav
- CY6_MS_CMB_STU_COG.sav
