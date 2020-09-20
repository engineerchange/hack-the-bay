# hack-the-bay

# Slides
- [pptx](HackTheBay_Slides.pptx)
- [pdf](HackTheBay_Slides.pdf)

# How To

## Initalize the data locally (see initialize.R)
- This is to load data into a data/ folder, since the data is too big to store in GitHub.
- Download from [Hack the Bay](https://github.com/Hack-the-Bay/hack-the-bay) the 'Water Quality' data and store 'Final_WATER.csv' into a /data directory.
- Download HUC-12 boundary map from same repo, see "HUC12 Boundary Maps" and store as 'wbdhu12_a_us_september2019.gdb' in a /data directory.

## combined_huc_visual.R (initial EDA with HUCs)
- Sources import.R, which brings in packages and data frames.
- Visual for the combined huc comparison across both databases.
- Example:
![Combined HUC Visual](/images/combined_huc_visual.png)

## huc_visuals.R
- Sources import.R, which brings in packages and data frames.
- Visuals for core of the presentation (tables, graphics).
- Example:
![table graphic](/images/table_graphic.png)