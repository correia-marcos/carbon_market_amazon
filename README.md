This project aim to study REDD+ projects in the Brazilian Amazon


We took the location of each project from the VERRA registry: https://registry.verra.org/
We thus download the KML file from the registry, merge every layer of the file into a unique geometry
and convert it to Shapefile format. We did this using QGIS software. Must of the KML files need only
one process: Colect geometries. Some of them, though, needed also to have a "merge scattered layers"
process.
--> In particular, the project 1663 needed to first be converted to polygon since, the project
contained five different shapes and the actual project area was the shape defined as a line


** Getting Started ** 

You need R version at least 4.3.1
Packages:
	- Groundhog
	- Here

The other will be installed if needed!

** Ignoring files ** 

Do to large size of some files, we needed to set them in .gitignore. In the 'results' directory,
only large 'tenure_projects_data' needed to be ignored. This is the datasets created by
'merge_property_data.R' script. 