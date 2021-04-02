# hack-the-bay

# Overview
Submission for Challenge 2 of the [Hack The Bay 2020 Hackathon](https://github.com/Hack-the-Bay/hack-the-bay). One of four [winning teams](https://hack-the-bay.devpost.com/project-gallery).

## In the News
- [CMC - Hackathon summary](https://www.chesapeakemonitoringcoop.org/hackthebay/) (Oct 2020)
- [CMC - Hack the Bay Winners Create New Ways to Use CMC Data](https://www.chesapeakemonitoringcoop.org/news/hack-the-bay-winners-create-new-ways-to-use-cmc-data/) (Oct 5, 2020)

# Slides
- [pptx](HackTheBay_Slides.pptx)
- [pdf](HackTheBay_Slides.pdf)

# How To

## initialize.R (initalize the data locally) 
- This is to load data into a data/ folder, since the data is too big to store in GitHub.
- Download from [Hack the Bay](https://github.com/Hack-the-Bay/hack-the-bay) the 'Water Quality' data and store 'Final_WATER.csv' into a /data directory.
- Download HUC-12 boundary map from [USGS WBD Data Store](https://nrcs.app.box.com/v/huc/folder/39640323180) and store as 'wbdhu12_a_us.gdb' in a /data directory.
- Run initialize.R to change 'Final_WATER.csv' to 'Final_WATER2.rds'

## combined_huc_visual.R (initial EDA with HUCs)
- Sources import.R, which brings in packages and data frames.
- Visual for the combined huc comparison across both databases.
- Example:<br/>
![Combined HUC Visual](/images/combined_huc_visual.png)

## huc_visuals.R
- Sources import.R, which brings in packages and data frames.
- Visuals for core of the presentation (tables, graphics).
- Example:<br/>
![table graphic](/images/table_graphic.png)
