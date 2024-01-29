# -*- coding: utf-8 -*-
"""
The idea of this program is to create KML files when not available.

Some projects in VERRA (and other registry companies) have no KML files, but
the project description does have some crude latitude and longitude
specifications - for almost all projects. So I've searched across all documents
in each project that doesn't have an KML file to find the latitude and
londitude points in order to create the KML file for those projects.

*Important*: Many coordinates are in different coordination systems, so we
must create a conversion for the coordination system of the google maps:
    WGS 84 -- WGS84 - World Geodetic System 1984

From the official site of EPSG conversion (https://epsg.io) we have:
EPSG:29191 --> SAD69 / UTM zone 21S
EPSG:31982 --> SIRGAS 2000 / UTM zone 22S
EPSG:4326 --> WGS 84 - WGS84 - World Geodetic System 1984


@Date: jun 2023.
@author: marcos
"""
# Importing required libraries

import simplekml
import pyproj
import csv


# We defined two functions

def convert_coordinate(list_from, epsg_from, epsg_to):
    """
    Convert a list of 2-dimension tuple into the desired ESPG coordination.

    Parameters
    ----------
    tuple_from: list
        Tuple containing 2-dimension tuple. Can be of any given size.
    epsg_from: integer
        Integer with the EPSG code for the coordinate data is in.
    epsg_to: integer
        Integer with the EPSG code you want data to be.

    Returns
    -------
    List of the same size, but with coordinates of the desired ESPG.

    """
    coordinates = []
    transform = pyproj.Transformer.from_crs(epsg_from, epsg_to, always_xy=True)
    for i in range(len(list_from)):
        coordinates.append(transform.transform(*list_from[i]))

    return coordinates


def create_kml(coordinates, name_project=None, name_polygon=None):
    """
    Create the kml class of simplekml package.

    Parameters
    ----------
    coordinates: List
        The geolocation of the project in WGS 84 (GPS) - WORLD.
    name_project : String
        Name of the the kml file you want to create.
    name_polygon: String
        Name of the polygon of the project.

    Returns
    -------
    kml : simplekml.kml.Kml
        Object that can be saved as a kml file.

    """
    kml = simplekml.Kml(name=name_project)

    verra = kml.newpolygon(name=name_polygon,
                           outerboundaryis=coordinates,
                           gxballoonvisibility=0
                           )
    verra.style.polystyle.color = simplekml.Color.red
    verra.style.polystyle.fill = 0
    verra.style.polystyle.outline = 1

    return kml


# =============================================================================
# (1) project Verra 665
# =============================================================================

# =============================================================================
# (a) Full property size
# =============================================================================

# We found this points from their documents in:
# https://registry.verra.org/app/projectDetail/VCS/665

# First, we define de full property size
full_property_665 = [(-58.212222, -9.798611),
                     (-58.328056, -9.798611),
                     (-58.328056, -9.891944),
                     (-58.212222, -9.891944),
                     (-58.212222, -9.798611)]

property_full = create_kml(full_property_665,
                           name_project='Verra Project 665',
                           name_polygon='Fazenda São Nicolau')


# Now, we save the KML file
property_full.save('Data/kml/verra(full_property)_665.kml')

# =============================================================================
# b) Only project size
# =============================================================================

# Creating the KML for just the geospatial values of the REDD project
# This geo references were made based on pdf file in VERRA website:
# https://registry.verra.org/app/projectDetail/VCS/665
# but the pdf had coordinates in SAD 1969 UTM Zone 21S system.
# We had to convert with some imprecison in order to make a full sqaure.

redd_project_665 = [(359420.971, 8911923.882),
                    (366409.081, 8911923.882),
                    (366409.081, 8907556.071),
                    (359420.971, 8907556.071),
                    (359420.971, 8911923.882)
                    ]

converted_665 = convert_coordinate(redd_project_665, 29191, 4326)

project_full = create_kml(converted_665,
                          name_project='Verra Project 665',
                          name_polygon='Fazenda São Nicolau')


# Now, we save the KML file
project_full.save('Data/kml/verra_665.kml')


# =============================================================================
# 2) Project Verra 1115
# =============================================================================

# We found the geospacial points in the following document:
# https://www.biofilica.com.br/docs/redd/jari-amapa/monitoring-report/amapa-monitoring-report-2011.pdf
# and we saved it on the Data/raw_data folder. So we must read it.

with open('Data/raw_data/verra_1115_raw.csv', newline='') as fout:
    reader = csv.reader(fout, quoting=csv.QUOTE_NONNUMERIC)
    redd_project_1115 = list(tuple(line) for line in reader)

converted_1115 = convert_coordinate(redd_project_1115, 31982, 4326)


project_1115 = create_kml(converted_1115,
                          name_project='Verra Project 1115',
                          name_polygon='JARI/AMAPÁ REDD+ PROJECT')

project_1115.save('Data/kml/verra_1115.kml')
