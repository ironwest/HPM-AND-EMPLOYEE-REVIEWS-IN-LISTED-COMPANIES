library(tidyverse)
library(readxl)

# 経産省の健康経営度調査公開エクセルファイルを読み込む-----
fl <- list.files("data/kenko_keiei/raw",pattern = "xlsx",full.names = TRUE)

d23 <- read_excel("data/kenko_keiei/raw/result2023.xlsx") 
d24 <- read_excel("data/kenko_keiei/raw/result2024.xlsx") 
d25 <- read_excel("data/kenko_keiei/raw/result2025_dai.xlsx") 
d26 <- read_excel("data/kenko_keiei/raw/result2026_dai_v2.xlsx") 

## 読み込んだエクセルファイルの列名が年度毎にどのように変化しているかを確認-----
colmatch <- bind_rows(
  enframe(colnames(d23)) |> select(value) |> mutate(nendo = "n23"),
  enframe(colnames(d24)) |> select(value) |> mutate(nendo = "n24"),
  enframe(colnames(d25)) |> select(value) |> mutate(nendo = "n25"),
  enframe(colnames(d26)) |> select(value) |> mutate(nendo = "n26"),
) |> 
  mutate(val = 1) |> 
  pivot_wider(id_cols = value, names_from = nendo, values_from = val, values_fill = 0)

## 列名の一致を確認----
colmatch |> filter(if_all(n23:n26, ~ .x==1)) #|> View()
colmatch |> filter(if_any(n23:n26, ~ .x==0)) #|> View()
# NOTE:年度事に細かいQ列が示すものや基準の内容が大きく違うため、複数年度にまたがる回答のマッチングを行うなら調査票で何を聞いているかを確認することが必要。

## 「Q」で始まる列名の整備------------
colmatch |> 
  select(value) |> 
  filter(str_detect(value,"^Q")) |> 
  pull(value)

# NOTE:
# Q1            … 単独設問
# Q2SQ3         … Q2 のサブ設問 SQ3
# Q2SQ3_a_1     … Q2-SQ3 の選択肢(_a_1)
# Q5課題内容①  … Q5 + 日本語 suffix
# のような表記が多数存在しており、ソートが意図どおりにならないため、Q番号int(整数列)を別途用意して、年度内では Q番号 → SQ番号 → suffix の階層で正しく並ぶようにする
columnmapper <- bind_rows(
  tibble(nendo = rep("23", length(colnames(d23))), columnname = colnames(d23)),
  tibble(nendo = rep("24", length(colnames(d24))), columnname = colnames(d24)),
  tibble(nendo = rep("25", length(colnames(d25))), columnname = colnames(d25)),
  tibble(nendo = rep("26", length(colnames(d26))), columnname = colnames(d26))
) |> 
  filter(str_detect(columnname,"^Q\\d+")) |> 
  mutate(
    Qnum = str_extract(columnname,"^Q\\d+"),
    Qint = as.integer(str_remove(Qnum,"Q")),
    SQnum = str_extract(columnname,"SQ\\d+"),
    Qsuffix = str_remove(columnname,"^Q\\d+(SQ\\d+)?")
  ) |> 
  arrange(nendo, Qint, SQnum, Qsuffix)
# FOOTNOTE: SQ番号がなければNA、説明文が特になければ""

# 質問のマスターデータを調査票PDFから取得する----------------
# NOTE: 健康経営度調査のエクセルデータのQ番号は各年度で同じ番号でも全く別の概念の質問をしている。対比表が健康経営度調査票PDFに掲載されているため、PDFを読み込んで、エクセルで抽出したQ番号を突合してマッピングを行い、年度間で同じ設問についての対応をつけられるようにする。（MarkitdownでMD化したが処理がうまくいかないためPDFデータを読み込む形に変更）(尚、突合は主に調査票目次に記載されているQ番号の今年、昨年の筋を披露形で対応)

pdfpath_23 <- "data/kenko_keiei/questionnaire/r4.kenkoukeieidotyousahyou.sample.pdf"
pdfpath_24 <- "data/kenko_keiei/questionnaire/r5.kenkoukeieidotyousahyou.sample.pdf"
pdfpath_25 <- "data/kenko_keiei/questionnaire/kk2025sample_dai.pdf"
pdfpath_26 <- "data/kenko_keiei/questionnaire/kk2026sample_dai.pdf"

mokuji_page23 <- 7
mokuji_page24 <- 8
mokuji_page25 <- 8
mokuji_page26 <- 9
# FOOTNOTE: PDFファイルを確認すると、「調査票 目 次」で始まるページが対象。目視でページを確認してmokuji_pagennに設定。

naiyou_x23  <- c(94, 352)
naiyou_x24  <- c(98, 358)
naiyou_x25  <- c(98, 358)
naiyou_x26  <- c(97, 360)
# FOOTNOTE: 目次の表は左右にわかれており、「内容」が該当するテキストのx座標は機械的に取得するよりも目視で個別設定したほうが早いので、個別設定とする。

library(pdftools)

extract_mokuji <- function(pdfpath_nn, mokuji_pagenn, naiyou_xnn, nn){
  
  pdfnn <- pdftools::pdf_data(pdfpath_nn)
  tgtnn <- pdfnn[[mokuji_pagenn]]
  
  #「今年」列のX座標の位置を確定（2箇所あるはず）
  current_year_x <- tgtnn |> filter(text=="今年") |> pull(x)
  
  #「昨年」列のX座標の位置を確定（2箇所あるはず）
  past_year_x <- tgtnn |> filter(text=="昨年") |> pull(x)

  
  leftnn <- tgtnn |> 
    filter(x %in% c(current_year_x[1], past_year_x[1], naiyou_xnn[1])) |> 
    select(x, y, text) |>
    mutate(x = factor(x, levels = c(current_year_x[1], past_year_x[1], naiyou_xnn[1]),
                      labels = c("c1current","c2past","c3text"))) |> 
    pivot_wider(id_cols = y, names_from = x, values_from = text)
  
  rightnn <- tgtnn |> 
    filter(x %in% c(current_year_x[2], past_year_x[2], naiyou_xnn[2])) |> 
    select(x, y, text) |>
    mutate(x = factor(x, levels = c(current_year_x[2], past_year_x[2], naiyou_xnn[2]),
                      labels = c("c1current","c2past","c3text"))) |> 
    pivot_wider(id_cols = y, names_from = x, values_from = text)
  
  fintable <- bind_rows(leftnn, rightnn) |> 
    fill(c1current, .direction = "down") |> 
    filter(str_detect(c1current,"^Q\\d+") & !is.na(c3text)) |> 
    mutate(nendo = nn) |> 
    select(nendo, c1current, c2past, c3text)
  
  return(fintable)
}

mokuji23 <- extract_mokuji(pdfpath_23, mokuji_page23, naiyou_x23, 23) 
mokuji24 <- extract_mokuji(pdfpath_24, mokuji_page24, naiyou_x24, 24) 
mokuji25 <- extract_mokuji(pdfpath_25, mokuji_page25, naiyou_x25, 25) 
mokuji26 <- extract_mokuji(pdfpath_26, mokuji_page26, naiyou_x26, 26) 

# NOTE: 24年度において、目次の設問数がQ78で終わっているが、実際はQ79とQ80があるため、手作業で追記しておく。
mokuji24 <- mokuji24 |> 
  add_row(qmap24 = "Q79", qtext24 = "健康経営への取り組みで実感する効果") |> 
  add_row(qmap24 = "Q80", qtext24 = "健康経営優良法人認定によるメリット")

# NOTE:ここまでで、PDFから取得した各年度が一つ前の年度のどの設問と対応しているかのデータを取得できたため、Excelの列名に対してquestionidをふりたい。Q番号は年度毎に変わるため、「q_最初に登場した年度における、text説明文」を便宜上のIDとする
mokuji23 <- mokuji23 |> rename(qmap23 = c1current, qmap22 = c2past, qtext23 = c3text) |> select(!nendo)
mokuji24 <- mokuji24 |> rename(qmap24 = c1current, qmap23 = c2past, qtext24 = c3text) |> select(!nendo)
mokuji25 <- mokuji25 |> rename(qmap25 = c1current, qmap24 = c2past, qtext25 = c3text) |> select(!nendo)
mokuji26 <- mokuji26 |> rename(qmap26 = c1current, qmap25 = c2past, qtext26 = c3text) |> select(!nendo)

# NOTE: 質問それぞれの内容毎のIDをふる。IDは、一番最初の年度に出現したテクストを利用する
# （一番最初にしないと、今後新しい年度を追加した場合にIDが変わってしまう可能性がある）
joinedmokuji <- mokuji26 |>
  full_join(mokuji25, by="qmap25") |>
  full_join(mokuji24, by="qmap24") |>
  full_join(mokuji23 |> select(!qmap22), by="qmap23") |>
  relocate(qmap26,qtext26,qmap25,qtext25,qmap24,qtext24,qmap23,qtext23)

joinedmokuji <- joinedmokuji |> 
  mutate(
    qid = case_when(
      !is.na(qtext23) ~ qtext23, 
      !is.na(qtext24) ~ qtext24, 
      !is.na(qtext25) ~ qtext25, 
      !is.na(qtext26) ~ qtext26 
    )
  )

# NOTE: それぞれの年度、質問番号、QIDを縦持ちデータとして保持する(columnmapperと結合するため)
qiddata <- joinedmokuji |> 
  select(qid, matches("map")) |> 
  pivot_longer(cols = !qid, names_to = "qmap", values_to = "Qnum") |> 
  mutate(nendo = str_extract(qmap,"\\d+")) |> 
  select(nendo, Qnum, qid)

# NOTE: Excelの列名に、Qnumを基準としてIDを付与する
question_map <- columnmapper |> left_join(qiddata, by=c("nendo","Qnum"))
  
write_rds(question_map,file="middledata/question_map.rds")


