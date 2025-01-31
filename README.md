Modelo espacialmente explícito COVID19
================

## Introducción

Este repositorio tiene los datos y códigos necesarios para generar el
modelo espacialmente explicito para la propagación del COVID19 en chile,
a continuación se describen los distintos archivos que hay en el
repositorio.

## Bases de datos

##### Comunas.rds

Simple feature obtenido de los archivos de la biblioteca nacional con
los poligonos de cada comuna, mas la población de cada una de acuerdo al
último censo

##### Regiones.rds

Simple feature obtenido de los archivos de la biblioteca nacional con
los poligonos de cada Región, mas la población de cada una de acuerdo al
último censo, el número de infectados al 19 de marzo y la prevalencia
calculada según ambos valores

![](README_files/figure-gfm/Mapa-1.png)<!-- -->

##### Viajes\_Regiones.rds

data.frame con el numero de viajes promedio realizados entre regiones
por día, sacado de un promedio de 21 días. El script para genererlo es
`MigracionRegion.R`. A continuación vemos las primeras 20 entradas en
una tabla.

| origen                                   | destino                                  | n\_personas |
| :--------------------------------------- | :--------------------------------------- | ----------: |
| Región Metropolitana de Santiago         | Región Metropolitana de Santiago         |  2675058.81 |
| Región de Valparaíso                     | Región de Valparaíso                     |   377275.14 |
| Región del Libertador Bernardo O’Higgins | Región del Libertador Bernardo O’Higgins |   267375.19 |
| Región del Bío-Bío                       | Región del Bío-Bío                       |   195334.00 |
| Región de La Araucanía                   | Región de La Araucanía                   |   144441.86 |
| Región del Maule                         | Región del Maule                         |   142386.00 |
| Región de Coquimbo                       | Región de Coquimbo                       |   100768.43 |
| Región de Los Lagos                      | Región de Los Lagos                      |    92601.90 |
| Región de Ñuble                          | Región de Ñuble                          |    75999.29 |
| Región de Los Ríos                       | Región de Los Ríos                       |    32448.67 |
| Región de Valparaíso                     | Región Metropolitana de Santiago         |    27176.10 |
| Región Metropolitana de Santiago         | Región de Valparaíso                     |    26876.81 |
| Región de Atacama                        | Región de Atacama                        |    23237.86 |
| Región de Tarapacá                       | Región de Tarapacá                       |    21928.19 |
| Región Metropolitana de Santiago         | Región del Libertador Bernardo O’Higgins |    20039.00 |
| Región del Libertador Bernardo O’Higgins | Región Metropolitana de Santiago         |    19877.19 |
| Región de Antofagasta                    | Región de Antofagasta                    |    18892.10 |
| Región Metropolitana de Santiago         | Región de Atacama                        |    13219.24 |
| Región de Atacama                        | Región Metropolitana de Santiago         |    11941.95 |
| Región del Libertador Bernardo O’Higgins | Región del Maule                         |    10899.86 |

![](README_files/figure-gfm/red-1.png)<!-- -->

## Modelos

## Outputs

### Lineas de tiempo de infección

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Tablas con peaks de infección

| Region                                    | Time       | Proporcion\_Infectados |
| :---------------------------------------- | :--------- | ---------------------: |
| Región de Antofagasta                     | 2020-06-23 |                   0.33 |
| Región de Arica y Parinacota              | 2020-03-18 |                   0.00 |
| Región de Arica y Parinacota              | 2020-03-19 |                   0.00 |
| Región de Atacama                         | 2020-06-07 |                   0.34 |
| Región de Aysén del Gral.Ibañez del Campo | 2020-06-25 |                   0.26 |
| Región de Coquimbo                        | 2020-04-09 |                   0.00 |
| Región de La Araucanía                    | 2020-06-08 |                   0.33 |
| Región de Los Lagos                       | 2020-06-02 |                   0.33 |
| Región de Los Ríos                        | 2020-06-25 |                   0.25 |
| Región de Magallanes y Antártica Chilena  | 2020-05-15 |                   0.00 |
| Región de Ñuble                           | 2020-05-25 |                   0.33 |
| Región de Tarapacá                        | 2020-06-25 |                   0.13 |
| Región de Valparaíso                      | 2020-06-18 |                   0.33 |
| Región del Bío-Bío                        | 2020-06-06 |                   0.33 |
| Región del Libertador Bernardo O’Higgins  | 2020-03-27 |                   0.00 |
| Región del Maule                          | 2020-06-05 |                   0.33 |
| Región Metropolitana de Santiago          | 2020-05-29 |                   0.33 |

![](Test2.gif)<!-- -->
