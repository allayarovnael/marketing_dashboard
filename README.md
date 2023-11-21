# Marketing Mix Analysis Dashboard 
This python script uses data from [Ferienwiki](https://www.ferienwiki.de/). The goal of this project is to collect all available data (2014-2023) and bring it to timeseries form, which can be easily fed to common machine learning models. Very often the calendar factors such as school holidays can explain the sales behaviour.

## Introduction
This repository contains a Python script for scraping, processing, and organizing holiday (Ferien) data from German states (Bundesländer). The data is scraped from the website [Ferienwiki](https://www.ferienwiki.de/), which provides detailed holiday information for different years and regions.

## Features
<ul>
<li>Scraping holiday data for German states from 2014 to 2023.</li>
<li>Parsing and formatting the date intervals for different types of holidays.</li>
<li>Creating a comprehensive DataFrame for easy analysis and visualization.</li>
<li>Data includes various holiday types like Winterferien, Osterferien, Pfingstferien, Sommerferien, Herbstferien, and Weihnachtsferien.</li>
</ul>

## Requirements
<ul>
<li>R.</li>
<li>Libraries: 'shiny', 'semantic.dashboard','shiny.semantic','car','highcharter','stargazer','dplyr','tidyr','purrr','forecast','lubridate','broom','openxlsx'.</li>
</ul>


## Function Descriptions
<ul>
<li>get_raw_yearly_ferien_data(year: int): Scrapes raw holiday data for a given year.</li>
<li>parse_time_interval(interval: str, year: int): Parses and formats the raw date intervals.</li>
<li>parse_table_data(years=list(range(2014, 2024))): Processes raw data for a list of years.</li>
<li>create_df_from_crawled_data(years = list(range(2014, 2024))): Creates a DataFrame from the crawled data.</li>
</ul>


## Data Structure
<ul>
<li>FERIEN_NAMES: Dictionary mapping German holiday names to their abbreviations.</li>
<li>BUNDESLAENDER: Dictionary containing German states and their abbreviations.</li>
</ul>


## Output foramt
The final output has the following form:

![alt text](https://github.com/allayarovnael/marketing_project/blob/main/images/export_example.png "model_fit")

![alt text](https://github.com/allayarovnael/marketing_project/blob/main/images/export_example.png "impacts")

![alt text](https://github.com/allayarovnael/marketing_project/blob/main/images/export_example.png "media_adstock")

![alt text](https://github.com/allayarovnael/marketing_project/blob/main/images/export_example.png "diagnostics")
