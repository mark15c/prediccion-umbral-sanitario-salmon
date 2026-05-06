# 📊 Datos del proyecto

## Fuente

Los datos provienen del **Departamento de Pesca y Océanos de Canadá** (*Fisheries and Oceans Canada – DFO*) y son de **acceso público**.

🔗 **Portal oficial:** [(https://open.canada.ca/)](https://open.canada.ca/data/en/dataset/3cafbe89-c98b-4b44-88f1-594e8d28838d)

## Archivo

**`DFO_farm_abundance.csv`**

Contiene reportes operacionales de centros de cultivo de salmón en el Pacífico canadiense durante el periodo 2011–2024.

### Variables principales

| Variable | Descripción |
|----------|-------------|
| `year` | Año del reporte |
| `month` | Mes del reporte |
| `num_pens_sampled` | Número de corrales muestreados |
| `chalimus_ab` | Abundancia promedio de estadios chalimus de *L. salmonis* por pez |
| `lep_motile_ab` | Abundancia promedio de estadios móviles no reproductivos de *L. salmonis* por pez |
| `lep_af_ab` | Abundancia promedio de hembras adultas de *L. salmonis* por pez |
| `cal_motile_ab` | Abundancia promedio de estadios móviles del género *Caligus* por pez |

### Variable derivada

`y` = 1 si `lep_af_ab > 3` (incumplimiento del umbral sanitario), 0 en caso contrario.

## Cita

Si utilizas estos datos, considera citar la fuente original del DFO.
