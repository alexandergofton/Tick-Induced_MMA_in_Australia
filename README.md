# National patterns of alpha-gal sensitisation in Australia

Interactive web application for exploring laboratory surveillance of alpha-gal-specific IgE (α-Gal sIgE) testing in Australia, 2014-2024.

**View it here:** https://alexandergofton.github.io/Tick-Induced_MMA_in_Australia

## About

This repository contains the source document, public data and analysis notebooks behind an interactive application presenting national surveillance data on α-Gal sensitisation (the laboratory marker of tick-induced mammalian meat allergy / alpha-gal syndrome) in Australia. It accompanies the manuscript:

**"National patterns of alpha-gal sensitisation in Australia: Laboratory surveillance of alpha-gal-specific IgE testing, 2014-2024"**

[Link to published manuscript](https://doi.org/10.XXXX/XXXXXX)

### Summary of findings

Retrospective analysis of α-Gal sIgE ImmunoCAP™ results from 14,075 individuals tested between 1 January 2014 and 31 December 2024, with residential postcodes mapped to Statistical Areas Level 3 (SA3). Overall 35.7% (5,025) tested positive (≥0.1 kU/L). Testing volume rose 330% over the period and detections accelerated from around 2022; decomposition attributed 79% of case growth to expanded testing, leaving up to 21% unexplained by measurable surveillance changes. Sensitised individuals clustered along the eastern seaboard within the range of *Ixodes holocyclus*, with three SA3 regions accounting for over a quarter of nationally detected sensitisation. Among 1,017 repeatedly tested individuals, α-Gal sIgE declined in 73.7% by a median of 20.4% annually.

## Contents of the application

Every manuscript figure is reproduced panel by panel as an interactive plotly or leaflet widget, with zoom, pan and hover detail. Figure S7 is the one exception and is shown as published, because its national-plus-inset map layout has no interactive equivalent; Figure 3 is the zoomable version of the same data.

### Main figures

| Figure | Content | Panels |
| --- | --- | --- |
| **Figure 1** | Annual trends in α-Gal sIgE testing and its decomposition | A. Total tests · B. Testing regions · C. Sources of testing growth · D. Within-region testing intensity |
| **Figure 2** | Annual α-Gal sensitisation detections and test positivity | A. Sensitised individuals detected · B. Positivity rate · C. Sources of detection growth |
| **Figure 3** | Geographic distribution of α-Gal sensitisation | Full-screen leaflet map of SA3 regions with the *I. holocyclus* western distribution limit overlaid, an opacity slider, and a sortable/searchable regional data table |
| **Figure 4** | Geographic concentration of α-Gal sensitisation | A. Lorenz curve · B. Pareto chart |
| **Figure 5** | Longitudinal changes in α-Gal sIgE levels | A. Annual rate of change · B. First vs last test |

### Supplementary figures

| Figure | Content | Panels |
| --- | --- | --- |
| **Figure S1** | Distribution of α-Gal sIgE results | Single panel |
| **Figure S2** | Age distribution by sex | A. All tested · B. α-Gal sIgE positive |
| **Figure S3** | Positivity ratios by age and sex | A. Male vs female · B. Age vs 25-34 reference · C. Age-sex vs female 25-34 |
| **Figure S4** | Sensitivity of the testing-volume split to the attribution rule | Single panel |
| **Figure S5** | Patient-to-clinician distance | Single panel |
| **Figure S6** | Geographic concentration standardised by testing volume | A. Lorenz curves vs population · B. Lorenz curve vs testing volume · C. Standardised positivity ratios · D. Sensitisation vs testing rate |
| **Figure S7** | Population- and testing-standardised burden (static, as published) | A. Sensitisation rate · B. Testing intensity · C. Testing-standardised burden |

The page also carries a light/dark theme toggle (cosmo/darkly), a left-hand table of contents, and folded code for every figure.

## Repository structure

```
index.qmd          Quarto source for the application (all figures and text)
index.html         Rendered self-contained application (also served by GitHub Pages)
public_data/       CSV, shapefile and GeoJSON inputs read by index.qmd
analysis_code/     Rendered notebooks for the eight analysis steps behind the figures
authors.md         Author list and affiliations
licence.md         Full CC BY 4.0 licence text
```

`public_data/` and `analysis_code/` are present in the working tree but are not tracked by git, so they are not part of the GitHub repository. Contact the corresponding author for access.

The analysis notebooks in `analysis_code/` cover, in order: `01_process_data`, `02_demographics`, `03_testing_trends`, `04_cases_per_sa3`, `05_case_concentration`, `06_risk_ratios`, `07_IgE_levels`, `08_mapping`.

## Quick start

### Viewing

Visit https://alexandergofton.github.io/Tick-Induced_MMA_in_Australia, or open [index.html](index.html) in any browser. The file is self-contained and needs no server or setup.

### Rebuilding

Rendering requires [Quarto](https://quarto.org/) and R, with `public_data/` in place:

```bash
quarto render index.qmd --to html
```

R packages used: `leaflet`, `sf`, `dplyr`, `readr`, `DT`, `ggplot2`, `tidyr`, `plotly`, `geojsonsf`, `smoothr`, `scales`, `htmltools`, `htmlwidgets`.

The data-loading chunk is cached (`index_cache/`), so the first render is considerably slower than subsequent ones.

## Data sources

Deidentified data were obtained from all [α-Gal sIgE ImmunoCAP™](https://www.allergy.org.au/patients/food-allergy/mammalian-meat-tick-faq) tests submitted to the following pathology service providers between 1 January 2014 and 31 December 2024:

- [QML Pathology](https://www.qml.com.au/)
- [Sullivan Nicolaides Pathology](https://www.snp.com.au/)
- [Douglas Hanley Moir Pathology](https://www.dhm.com.au/)
- [Laverty Pathology](https://www.laverty.com.au/)

Patients' residential locations were aggregated to Australian Bureau of Statistics (ABS) Statistical Areas Level 3 (SA3) as defined by the [Australian Statistical Geography Standard Edition 3](https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/latest-release), which creates functional areas of regional towns and cities with populations between 30,000 and 130,000.

Sensitised individuals are reported as cases per 1 million population per year (1M PPY), adjusting for both regional population size and the sampled period, using annual regional population estimates from the [ABS](https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/latest-release) as denominators:

```
Sensitised individuals per 1M PPY = (cumulative_incidence_2014-2024 / cumulative_annual_population_2014-2024) * 1000000
```

## Authors

Emily Smith¹˒²˒³, Paul Campbell⁴, Carl Kennedy⁵, Karl Baumgart⁶, Lucinda Williams⁷, Stephen Barker⁸, Sheryl van Nunen⁹˒¹⁰˒¹¹˒¹², Andrew Walker¹˒³, and Alexander W. Gofton²˒¹²˒*

### Affiliations

1. Institute for Molecular Bioscience, The University of Queensland, Brisbane, Australia
2. CSIRO Health and Biosecurity, Brisbane, Australia
3. Australian Research Council Centre of Excellence for Innovations in Peptide and Protein Science, The University of Queensland, Brisbane, Australia
4. QML Pathology, Brisbane, Australia
5. Sullivan Nicolaides Pathology, Brisbane, Australia
6. Douglas Hanley Moir Pathology, Sydney, Australia
7. Laverty Pathology, Sydney, Australia
8. School of Chemistry and Molecular Biology, The University of Queensland, Brisbane, Australia
9. National Allergy Centre of Excellence, Australia
10. Faculty of Medicine and Health, The University of Sydney, Sydney, Australia
11. Faculty of Medicine and Health Sciences, Macquarie University, Sydney, Australia
12. Tick-induced Allergies Research and Awareness (TiARA), Australia

\* Corresponding author: alexander.gofton@csiro.au

See [authors.md](authors.md).

## Citation

If you use this application or data, please cite:

```
[Citation details to be added upon publication]
```

## Licence

This work is licensed under a [Creative Commons Attribution 4.0 International Licence](licence.md) (CC BY 4.0).

You are free to:

- **Share** — copy and redistribute the material in any medium or format
- **Adapt** — remix, transform, and build upon the material for any purpose, even commercially

Under the following terms:

- **Attribution** — you must give appropriate credit, provide a link to the licence, and indicate if changes were made

See [licence.md](licence.md) for the full text.

## Disclaimer

**Preprint research notice:** The findings and analyses displayed in this application are part of ongoing research and are provisional. They may be incomplete, subject to revision, or contain errors.

## Contact

**Alexander W. Gofton** — alexander.gofton@csiro.au

## Acknowledgments

We thank the pathology providers for contributing deidentified testing data, and all patients whose data contributed to this research.
