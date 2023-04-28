dados <- haven::read_sav("data-raw/banco_infancia_kj_0318_kim.sav") |> 
  janitor::clean_names()

dplyr::glimpse(dados)

# Escore total da IEP = iep_total
# IEP positivo = iep_p
# IEP negativo = iep_n
# Idade materna = idade
# Tempo fora de casa = tradia
# Quem mais cuida da criança = ccuida
# Vive com companheiro = vivecom
# Cor da pele = corpele

# Que transtornos vão ser considerados como "ansiedade"?
# Guia de correção do SDQ (como corrigir e quais domínios vamos usar)
# IEN = índice econômico nacional (escore ou itens individuais?)
# Criança dorme sozinha = ?
# Escolaridade = ? (escola1)

dados |>
  dplyr::filter(filter_mae == 1) |> 
  nrow()

dados |> 
  dplyr::filter(filter_mae == 1) |> 
  ggplot2::ggplot(ggplot2::aes(x = escola1, y = iep_total)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")
  

dados_mae <- dplyr::filter(dados, filter_mae == 1)

dados_limpos <- dados_mae |>
  dplyr::mutate(
    dplyr::across(c(tag, fsa, astp, tpsa, tpca),
                  \(x) tidyr::replace_na(x, 0))
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    sdq_emocional = sdq1 + sdq2 + sdq3 + sdq4 + sdq5,
    sdq_comportamento = sdq6 + sdq7 + sdq8 + sdq9 + sdq10,
    sdq_hiperatividade = sdq11 + sdq12 + sdq13 + sdq14 + sdq15,
    sdq_relacionamento = sdq16 + sdq17 + sdq18 + sdq19 + sdq20,
    sdq_prosocial = sdq21 + sdq22 + sdq23 + sdq24 + sdq25,
    sdq_total = sdq_emocional + sdq_comportamento + sdq_hiperatividade +
      sdq_relacionamento,
    ien_total = ien1 + ien2 + ien3 + ien4 + ien5 + ien6 + ien7 + ien8 + ien9 +
      ien10 + ien11 + ien12,
    ansiedade_soma = tag + fsa + astp + tpsa + tpca,
    ansiedade_dic = as.factor(dplyr::case_when(
      tag == 1 | fsa == 1 | astp == 1 | tpsa == 1 | tpca == 1 ~ "Sim",
      .default = "Não")
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    ien_tercil = dplyr::ntile(ien_total, 3),
    ien_quintil = dplyr::ntile(ien_total, 5),
  ) |> 
  dplyr::select(
    iep_negativo = iep_n,
    iep_positivo = iep_p,
    iep_total,
    idade_materna = idade,
    tempo_fora = tradia,
    cuida_crianca = ccuida,
    vive_com_companheiro = vivecom,
    cor_pele = corpele,
    dplyr::starts_with("sdq_"),
    dplyr::starts_with("ansiedade"),
    dplyr::starts_with("ien_")
  ) |>
  haven::as_factor() |> 
  dplyr::mutate(
    cor_pele = dplyr::case_when(
      cor_pele == "Branca" ~ "Branca",
      cor_pele %in% c("Preta",
                      "Mulata",
                      "Amarela",
                      "Indígena") ~ "Não-branca"
    )
  )

dados_limpos |> 
  readr::write_rds(file = "data-processed/dados_limpos.rds")

dados_limpos |> 
  dplyr::count(ansiedade_dic)

set.seed(1)
dados_limpos |> 
  ggplot2::ggplot(ggplot2::aes(x = iep_total,
                               y = sdq_total)) +
  ggplot2::geom_jitter() +
  ggplot2::geom_smooth(method = "lm",
                       se = FALSE,
                       linewidth = 1.5)

dados_limpos |> 
  dplyr::summarise(
    dplyr::across(dplyr::starts_with("sdq"),
                  list(
                    media = \(x) mean(x, na.rm = TRUE),
                    dp = \(x) sd(x, na.rm = TRUE)
                  )),
    .by = "ansiedade_dic"
  )

testes_ansiedade <- dados_limpos |>
  dplyr::select(dplyr::starts_with("sdq_"),
                ansiedade_dic) |>
  tidyr::pivot_longer(
    cols = c(dplyr::everything(), -ansiedade_dic),
    names_to = "outcome",
    values_to = "score"
  ) |>
  dplyr::nest_by(outcome) |>
  dplyr::mutate(teste = list(infer::t_test(data, score ~ ansiedade_dic,
                                           order = c("Sim", "Não")))) |> 
  dplyr::reframe(teste)

testes_ansiedade
