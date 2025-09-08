Problem Set 1 — Predicting Income Septiembre 2025
======================================================

Autores - Grupo 6
-------

- Juan Esteban Moncada 202021566
- Marcel Montesdoca 202012736
- Mariana Correa 202020924
- Rodrigo Iriarte 202022852

------------------------------------------------------

Propósito del repositorio
-------------------------
Este repositorio contiene la solución al **Problem Set 1**, de la clase: Big Data Y Machine Learning para la Economía Aplicada 
El propósito del trabajo es **definir y estimar modelos econométricos que permitan, primero, hacer inferencia sobre el ingreso laboral y, después, evaluar su capacidad predictiva**.  

El análisis se divide en dos grandes objetivos:
1. **Inferencia**: estimar relaciones entre edad, género, educación y otras características con los ingresos laborales horarios.
2. **Predicción**: evaluar el desempeño de distintos modelos al predecir ingresos, usando métricas como el RMSE, validación con particiones de datos y validación cruzada (LOOCV).

------------------------------------------------------

Origen de los datos
-------------------
Los datos provienen de la **Gran Encuesta Integrada de Hogares (GEIH) 2018** para Bogotá.  
Esta base se obtiene mediante web scraping del portal:  
https://ignaciomsarmiento.github.io/GEIH2018_sample/  

El dataset original corresponde a la medición de pobreza monetaria y desigualdad del DANE y contiene información sobre empleo, ingresos y características sociodemográficas de los hogares.

------------------------------------------------------

Estructura del repositorio
--------------------------
- `/scripts/`
  - `taller1.R` — Script principal con todo el flujo de trabajo.
- `/stores/`
  - Base de datos ( `base_geih.xlsx`).
- `/views/`
  - Gráficas y tablas exportadas (ej. `salario_por_sexo_edad.png`, `salario_educacion.png`, etc.).
- `taller_1.Rproj`
  - Proyecto de R para ejecutar con rutas relativas.`
  - Archivos de configuración y entorno.
- `README` (este archivo).

------------------------------------------------------

Divisiones del código
---------------------
El script `taller1.R` está dividido en secciones principales:

1. **Web scraping**  
   Descarga y consolidación de la base GEIH 2018 (Bogotá) desde la página web.

2. **Limpieza de datos**  
   - Conversión de variables a factores o numéricas.  
   - Creación de variables cuadráticas y logaritmos.  
   - Filtrado de individuos relevantes (mayores de 18 años, ocupados).  

3. **Imputación y tratamiento de missing values**  
   - Exploración de datos faltantes.  
   - Filtrado de registros no válidos.  

4. **Análisis descriptivo**  
   - Tablas de estadísticos básicos.  
   - Gráficas de importancia.  

5. **Perfil edad–salario**  
   - Estimación de un modelo cuadrático de log-ingresos sobre la edad.  
   - Construcción de intervalos de confianza mediante bootstrap.  
   - Identificación de la edad pico de ingresos.  

6. **Brecha de género**  
   - Estimación del gap incondicional (modelo simple).  
   - Estimación del gap condicional con controles mediante FWL.  
   - Bootstrap para comparar errores estándar.  
   - Perfiles edad–salario por sexo (ceteris paribus).  

7. **Modelos predictivos**  
   - Separación train/test (70/30).  
   - Ocho modelos de creciente complejidad (interacciones, polinomios, efectos no lineales).  
   - Evaluación con RMSE (in-sample y out-of-sample).  
   - Validación cruzada (LOOCV).  
   - Análisis de errores, gráficas de residuales, cobertura de intervalos de predicción.  

------------------------------------------------------

Requisitos y configuración
--------------------------
**Versión sugerida:** R (≥ 4.3) y RStudio.

**Paquetes R**
El script usa `pacman::p_load()` para instalar/cargar automáticamente:
- `tidyverse`, `rvest`, `writexl`, `readxl`
- `gt`, `gtsummary`, `modelsummary`
- `caret`, `boot`, `stargazer`
- `skimr`, `car`, `forcats`, `scales`

Instalación rápida (si no se tiene `pacman`):

install.packages(c(
  "pacman","tidyverse","rvest","writexl","readxl",
  "gt","gtsummary","modelsummary",
  "caret","boot","stargazer","skimr","car","forcats","scales"
))

**Reproducibilidad**
- set.seed(07092025) para procesos generales.
- set.seed(10101) para partición train/test. 

------------------------------------------------------

Instrucciones de uso
--------------------
1. Clonar el repositorio:
   git clone <url-del-repo>

2. Abrir el proyecto en RStudio:
   Abrir el archivo `taller_1.Rproj`. Esto asegura que las rutas relativas a `/scripts`, `/stores` y `/views` funcionen correctamente.

3. Ejecutar el script principal:
   source("scripts/taller1.R")

   Nota: la ejecución completa puede tardar un largo tiempo (especialmente scraping y bootstraps).

4. Outputs generados:
   - stores/base_geih.xlsx — base consolidada tras el scraping y limpieza.
   - Gráficas en /views (ej. salario_por_sexo_edad.png).
   - Tablas exportadas en LaTeX (.tex) en /views.

------------------------------------------------------

Resultados esperados / ejemplos
-------------------------------
Ejemplo de outputs clave:

- Gráfica Edad–Salario por Sexo (ceteris paribus)
  Archivo generado: views/salario_por_sexo_edad.png

- Tablas en LaTeX exportadas con stargazer y modelsummary,
  listas para incluir en reportes académicos.

------------------------------------------------------




