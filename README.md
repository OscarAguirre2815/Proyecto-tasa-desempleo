
# Modelo estructural de la tasa de desempleo

Modelo de **espacio de estados (State-Space)** para analizar y pronosticar la tasa de desempleo mediante una descomposición en **nivel, tendencia y estacionalidad**.

## Metodología

- Transformación logarítmica de la tasa de desempleo.
- Modelo de **tendencia local lineal + componente estacional**.
- Estimación de parámetros mediante **Máxima Verosimilitud (MLE)**.
- Estimación de estados mediante **Filtro y Suavizamiento de Kalman**.
- Diagnóstico de innovaciones mediante ACF, PACF e histograma.
- Pronóstico a **18 períodos** con intervalos de confianza del 95 %.

## Herramientas

- **R**
- `dlm`
- `readxl`
- `forecast`

## Resultados

El proyecto permite visualizar:

- Tasa de desempleo y niveles filtrados/suavizados.
- Componente estacional.
- Señal estimada del modelo.
- Innovaciones y sus diagnósticos.
- Pronósticos futuros con intervalos de confianza.

## Autor

**Oscar Aguirre**  
Estudiante de Estadística — Universidad Nacional de Colombia
