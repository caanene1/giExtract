# giExtract
A universal framework for the extraction of features from digital H&E images using multiple CNN pretrained models. 
Extracting from multiple CNNs models enables a wider range of features that could be functionally relevant.

The core of this tool is built in python3.8 with tensorflow backend and keras functional API, 
while the downstream analysis is implemented in R programming language. 

# Installation and running the tool
The best way to get giExtract along with all the dependencies is to install the release from python package installer (pip).

```pip install giExtract```
This will add two command line scripts:

| Script | Context | Usage |
| ---    | --- | --- |
| giCube | Gene set analysis | ```giCube -h``` |
| giExtract | Single gene analysis | ```giExtract -h``` |

Utility functions can be imported using conventional python system like ```from giExtract.util import generator```

# Input giCube
The main input here is the path to the H&E images slides (.jpg or .png), specified by ```-p``` to load and create patches.
All other arguments are optional and have been set to reasonable default. User can use ```giCube -h``` 
to reveal the options and the default settings.

# Output giCube
Image patches format the H&E slides, which will be saved under "cubes" inside the path provided in the input.

# Input giExtract
The two main input here is the path to the H&E cubes generated by giCube (.jpg), specified by ```-p``` and path to the meta file (.csv)
to flow the patches for feature extraction ```-c```. The context file must have a column with file names matching the patches in the path. 
All other arguments are optional and have been set to reasonable default. User can use ```giExtract -h``` 
to reveal the options and the default settings.

# Output giExtract
Table for histological features extracted by the different CNN models, where patches are in rows and histological 
features in columns.

| Name  | feature 1 | feature 2 | feature 2 |
| --- | --- | --- | --- |
| patch 1 | 0.2 | 0.1 | 0.6 |
| patch 2  |  5.2  | 0.14  |  0.6  |
| patch 3  |  0.6  | 0.1  |  0.7 |

The output is named to indicate CNN origin of the feature example "inception_46"


# Extras
R script to analyse the output of giExtract above and identify differential features (see Manuscript) is included under R, 
with a README file on usage. The script giFeature.R takes two mandatory inputs, including:
- Path to a csv file with meta information. This table must have only 3 columns Name, slide and Group. 
- Path to csv file with cnn features to analyse. This table must be an output of giExtract.
Details about the optional arguments are given inside the read file.
  The R script assumes you have R and tidyverse package installed.

# Manuscript analysis
To reproduce the analysis reported in the manuscript user can execute run.sh script inside the manuscript folder. 
This assumes you have installed the giExtract package using standard pip install as stated above and have R installed.
The run.sh script will perform the three core analysis 1) patch generation 2) feature extraction and 3) differential feature analysis.

To generate the plots and automatically extract images, user can run the codes in downstream.R.

# Example data
Example datasets are provided inside manuscript/data to give a user visualisation of the input/output files. 
Note only a subset of the data is provided due size requirement. Full dataset can be downloaded from the manuscript supplemental file.

# To clone the source repository
git clone https://github.com/caanene1/giExtract