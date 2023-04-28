#### IMPORTAÇÃO DE DADOS ####

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

#### QUANTAS OBSERVAÇÕES AO FILTRAR PELOS DADOS COM INFORMAÇÃO MATERNA? ####

dados |>
  dplyr::filter(filter_mae == 1) |> 
  nrow()

#### CRIAR SUBCONJUNTO COM APENAS DADOS VALIDADOS ACIMA ####  

dados_mae <- dplyr::filter(dados, filter_mae == 1)

#### LIMPEZA AO RECODIFICAR E CALCULAR ESCORES ####

dados_limpos <- dados_mae |>
  # Substituir NA por 0 nas variáveis de transtorno de ansiedade
  dplyr::mutate(
    dplyr::across(c(tag, fsa, astp, tpsa, tpca),
                  \(x) tidyr::replace_na(x, 0))
  ) |> 
  # Colocar dataset pelas linhas para calcular escores
  dplyr::rowwise() |> 
  dplyr::mutate(
    # Escala de sintomas emocionais
    sdq_emocional = sdq1 + sdq2 + sdq3 + sdq4 + sdq5,
    # Escala de problemas de comportamento
    sdq_comportamento = sdq6 + sdq7 + sdq8 + sdq9 + sdq10,
    # Escala de hiperatividade
    sdq_hiperatividade = sdq11 + sdq12 + sdq13 + sdq14 + sdq15,
    # Escala de problemas de relacionamento com colegas
    sdq_relacionamento = sdq16 + sdq17 + sdq18 + sdq19 + sdq20,
    # Escala de comportamento pró-social
    sdq_prosocial = sdq21 + sdq22 + sdq23 + sdq24 + sdq25,
    # Calcular escore total da SDQ (não incluir pró-social)
    sdq_total = sdq_emocional + sdq_comportamento + sdq_hiperatividade +
      sdq_relacionamento,
    # Calcular soma dos itens da IEN
    ien_total = ien1 + ien2 + ien3 + ien4 + ien5 + ien6 + ien7 + ien8 + ien9 +
      ien10 + ien11 + ien12,
    # Calcular "escore" dos transtornos de ansiedade
    ansiedade_soma = tag + fsa + astp + tpsa + tpca,
    # Criar variável dicotômica dos transtornos de ansiedade
    ansiedade_dic = as.factor(dplyr::case_when(
      tag == 1 | fsa == 1 | astp == 1 | tpsa == 1 | tpca == 1 ~ "Sim",
      .default = "Não")
    )
  ) |> 
  # Desagrupar a base de dados
  dplyr::ungroup() |> 
  dplyr::mutate(
    # Criar variável dos tercis da IEN
    ien_tercil = dplyr::ntile(ien_total, 3),
    # Criar variável dos quintis da IEN
    ien_quintil = dplyr::ntile(ien_total, 5),
  ) |> 
  dplyr::select(
    # Escore negativo da IEP
    iep_negativo = iep_n,
    # Escore positivo da IEP
    iep_positivo = iep_p,
    # Escore total da IEP
    iep_total,
    # Idade materna
    idade_materna = idade,
    # Tempo que a mãe fica fora de casa
    tempo_fora = tradia,
    # Qual o principal cuidador da criança?
    cuida_crianca = ccuida,
    # Mãe vive com companheiro?
    vive_com_companheiro = vivecom,
    # Cor da pele
    cor_pele = corpele,
    # Selecionar variáveis dos domínios e total da SDQ
    dplyr::starts_with("sdq_"),
    # Selecionar escore de ansiedade e variável dicotômica
    dplyr::starts_with("ansiedade"),
    # Selecionar total, tercis e quintis da IEN
    dplyr::starts_with("ien_")
  ) |>
  # Puxar os labels das categóricas para fator
  haven::as_factor() |> 
  dplyr::mutate(
    # Dicotomizar cor da pele entre branca e não-branca
    cor_pele = dplyr::case_when(
      cor_pele == "Branca" ~ "Branca",
      cor_pele %in% c("Preta",
                      "Mulata",
                      "Amarela",
                      "Indígena") ~ "Não-branca"
    )
  )

#### EXPORTAR DADOS PROCESSADOS ####

dados_limpos |> 
  readr::write_rds(file = "data-processed/dados_limpos.rds")

#### QUANTAS MÃES COM PELO MENOS UM TRANSTORNO DE ANSIEDADE? ####

dados_limpos |> 
  dplyr::count(ansiedade_dic)

#### SCATTERPLOT IEP E SDQ ####

set.seed(1)
dados_limpos |> 
  ggplot2::ggplot(ggplot2::aes(x = iep_total,
                               y = sdq_total)) +
  ggplot2::geom_jitter() +
  ggplot2::geom_smooth(method = "lm",
                       se = FALSE,
                       linewidth = 1.5)

#### ANSIEDADE E SDQ ####

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

#### MEDIAÇÃO ####
