# datapaperchecks

`datapaperchecks` é um pacote R com funções auxiliares para checagem de qualidade de dados no fluxo do datapaper **WildCrossData Latin America**.

## Para que serve

O pacote centraliza funções reutilizáveis para:

- leitura padronizada de planilhas Excel (`read_sheet`);
- aplicação consistente de tipos de coluna (`set_column_types`);
- validações de estrutura e datas (`unique_id`, `dttm_update`, etc.);
- checagens específicas do projeto (ex.: `check_problem_intervals`).

A ideia é evitar duplicação de código entre capítulos Quarto e scripts de revisão.

## Instalação

Instale diretamente do GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("nerf-ufrgs/datapaperchecks")
```

## Uso

Use as funções com namespace explícito:

```r
ct <- datapaperchecks::read_sheet(
  path = "Example/13",
  sheet = "Camera_trap",
  recurse = FALSE,
  na = c("NA", "na")
)
```

## Autores

- Dornas, R.
- Franceschi, I.
- Dasoler, B.
