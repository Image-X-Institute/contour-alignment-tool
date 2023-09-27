<a name="readme-top"></a>

<div align="center">

  <h3 align="center">contour-alignment-tool</h3>

  <p align="center">
    An application for visualising and aligning contours in kV projections. 
    <br />
    <br />
    <a href="https://github.com/Image-X-Institute/contour-alignment-tool/issues">Report Bug</a>
    ·
    <a href="https://github.com/Image-X-Institute/contour-alignment-tool/issues">Request Feature</a>
  </p>
</div>



<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#system-requirements-and-prerequisites">System Requirements and Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#data">Data</a></li>
      </ul>
    </li>
    <li>
		<a href="#usage">Usage</a>
		<ul>
			<li><a href="#loading-data">Loading Data</a></li>
			<li><a href="#contour-alignment">Contour Alignment</a></li>
			<li><a href="#display">Display</a></li>
			<li><a href="#data-export">Data Export</a></li>
		</ul>
	</li>
    <li><a href="#build">Build</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>




## About
The Contour Alignment Tool is an application designed for aligning 3D contour structures with 2D kilovoltage projections. Users can load projections along with the associated CT, structure set, and plan files. The tool generates 2D contours from the selected 3D structure and allows users to manually align the 2D contour to the correct position. The aligned contours can be exported as masks facilitating the creation of labeled data for machine learning projects. Furthermore, the tool offers the ability to load a large range of 2D projection types, serving a wide range of general usage scenarios.

![image](https://github.com/Image-X-Institute/contour-alignment-tool/assets/63682590/c008bb15-f33d-45a2-8128-b81f123d3a53)


<br />

## Getting Started
### System Requirements and Prerequisites
- Windows
- MATLAB Runtime Version 9.10 can be downloaded [here](https://ssd.mathworks.com/supportfiles/downloads/R2021a/Release/8/deployment_files/installer/complete/win64/MATLAB_Runtime_R2021a_Update_8_win64.zip). Visit the [MathWorks website](https://au.mathworks.com/products/compiler/matlab-runtime.html) for more details.

### Installation
The [latest release](https://github.com/Image-X-Institute/contour-alignment-tool/releases/latest) of the Contour Alignment Tool can be downloaded [here](https://github.com/Image-X-Institute/contour-alignment-tool/releases/latest/download/ContourAlignmentTool.zip). 

Once downloaded, extract the contents to a local directory.

### Data
The following set of files are required to use the application:
- CT DICOMS
- Structure DICOM
- Plan DICOM
- Kilovoltage images (.tiff, .xim, .hnd, .hnc, or .his)

Suitable data to test the application can be downloaded from the [SPARK Database](https://ses.library.usyd.edu.au/handle/2123/31090). 

<p align="right">(<a href="#readme-top">back to top</a>)</p>


## Usage
### Loading Data
1.	Run the ContourAlignmentTool executable. Once the application loads, the data processing screen will be displayed as shown below.
	> [!IMPORTANT]  
	> Depending on the system, the application can take several minutes to open.
 
	![image](https://github.com/Image-X-Institute/contour-alignment-tool/assets/63682590/c083d152-6e8d-49ae-9d19-ca49de5fb2fc)
2.	The data can be loaded by using as shown below the clinical data browse or invidual browse:
    - Individual browse: Select each file or folder individually using the respective browse buttons. 
	> [!NOTE]
 	> If the CT, RS, and RP are all within the same folder, the RS and RP files will load automatically when the CT folder is selected.
    - Clinical data browse: This option is useful for bulk loading of several patients and fractions. The data must be in the required folder structure. The selected folder should contain a folder for each patient. For each patient, there should be a "Patient Plans" folder containing the plan DICOMs and "Pateint Images" folder containing a folder for each fraction.
3.	Set the number of projections, pixel spacing, SID, SDD, and detector offset if they differe from the defaults. A warning is displayed if the parameters can not be determined from the file headers.

	![image](https://github.com/Image-X-Institute/contour-alignment-tool/assets/63682590/056855ba-4241-4db8-9f68-d51157f54e7f)
4.	Select the Structure of Interest from the dropdown menu.
5.	Click Proceed, once all data has been selected. The execution time can vary depending on the file type, number of projections, and whether it is executed on a GPU.
6.	Once the data is processed, labelling of the data can be performed.
	![image](https://github.com/Image-X-Institute/contour-alignment-tool/assets/63682590/c008bb15-f33d-45a2-8128-b81f123d3a53)

<br/>

### Contour Alignment
**Navigation**<br/>
The shorcut buttons can be used to go to the first, previous, next, or last projection.

The navigation panel provides a list of all projections sorted by alignment confidence. Click on the projection name to load the selected projection.

**Moving the contour**<br/>
The contour can be moved using:<br/>
-	the arrow buttons on the main screen
-	`arrow keys`
-	`mouse click` + `drag`

The contour position can be reset using the reset button in the middle of the arrows buttons.

**Confidence in alignment**<br/>
For each projection, the confidence in alignment can be selected.

<br/>

### Display
Options under the `Display` menu:
- 	`DRR Viewer`: The DRR viewer displays a full DRR view with the true location of the target contour based on the plan.
-	`Contour Fill`: Change the contour between a border and fill
-	`Contour Colour`: Change the colour of the contour
-	`Invert Intensity`: Invert the intensity of the projections.

**Contrast Adjustment**<br/>
The projection contrast can be adjusted using the panel on the main screen. The contrast can be manually changed using the sliders. Additional options include auto adjustment, adjustment based on contour location and adjustment based on an ROI selection.


<br/>

### Data Export
The contours can be exported as binary PNG files. To export the updated contours, go `File` -> `Export` or `Export As...`
- `Export` will save the contours to the default location or the location set during data loading.
- `Export As...` will save the contours to a new location of choice.

<p align="right">(<a href="#readme-top">back to top</a>)</p>



## Build
[MATLAB App Designer](https://au.mathworks.com/products/matlab/app-designer.html) is required to edit and export the application:

- [ContourAlignmentTool.mlapp](ContourAlignmentTool.mlapp) is the main application project that can be opened in MATLAB App Designer.
- [ContourAlignmentTool.prj](ContourAlignmentTool.prj) is the file that contains information for the packaging.

### Supporting Files

- [ContourAlignmentTool_resources](./ContourAlignmentTool_resources) directory contains graphics for the application.
- [Dependencies](./Dependencies) directory contains files required for data processing.
- [Supporting Apps](./Supporting%20Apps) directory contains the project files for the supporting windows.

<br />

## Acknowledgments
The geometry simulation and forward projections are performed using the [Reconstruction Toolkit (RTK)](https://www.openrtk.org/).

<br />

## Contact

Adam Mylonas | adam.mylonas@sydney.edu.au

**The University of Sydney**<br/>
Image X Institute<br/>
Faculty of Medicine and Health

<p align="right">(<a href="#readme-top">back to top</a>)</p>

