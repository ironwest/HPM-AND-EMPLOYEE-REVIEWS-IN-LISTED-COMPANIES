library(tidyverse)
library(readxl)

# NOTE: 健康経営度調査のデータを一つのパネルデータに結合する

question_map <- read_rds("middledata/question_map.rds")
# FOOTNOTE: 質問のマッピングデータを読み込む（01_question_map.Rの結果）

# NOTE:法人IDの重複や名前衝突の処理について。健康経営度調査のデータにおいて名前衝突や、割り振られているIDに不整合な部分あるため確認する

## 経産省の健康経営度調査公開エクセルファイルを読み込む-----
fl <- list.files("data/kenko_keiei/raw",pattern = "xlsx",full.names = TRUE)

d23 <- read_excel("data/kenko_keiei/raw/result2023.xlsx") 
d24 <- read_excel("data/kenko_keiei/raw/result2024.xlsx") 
d25 <- read_excel("data/kenko_keiei/raw/result2025_dai.xlsx") 
d26 <- read_excel("data/kenko_keiei/raw/result2026_dai_v2.xlsx") 

# NOTE:コード列の重複確認（なし）
d23 |> count(`コード`) |> filter(n > 1)
d24 |> count(`コード`) |> filter(n > 1)
d25 |> count(`コード`) |> filter(n > 1)
d26 |> count(`コード`) |> filter(n > 1)

# NOTE: 法人名の重複確認（あり）
d23 |> count(`法人名`) |> filter(n > 1)
d24 |> count(`法人名`) |> filter(n > 1)
d25 |> count(`法人名`) |> filter(n > 1)
d26 |> count(`法人名`) |> filter(n > 1)
# FOOTNOTE:株式会社システムリサーチが全年度で、社会副法人慶生会が２５年度と２６年度で出現している。
# フィードバックシートのURLからそれぞれの内容を確認して原因を調査する

d26 |> filter(`法人名` %in% c("株式会社システムリサーチ", "社会福祉法人慶生会")) |> pull(`フィードバックシート`)
# FOOTNOTE：フィードバックシートに記載されていたURLからは株式会社システムリサーチは全く別法人で２社存在している。
# 一社は名古屋で創業（A03771：1981年設立。3771（東証プライム市場）、従業員1000名超）、もう一社は昭和六〇年設立（従業員数160名前後）
# 転職会議から提示されたデータはスナップショット時点の口コミ評価数からはA03771で一致していると考えられるため、分析の際はA03771を採用。A00261の株式会社システムリーサーチは分析から除外する。
#（そもそも、A00261のシステムリサーチは上場していないため、論文1の上場企業を対象としたデータからは除外される 
# 社会福祉法人慶生会も大阪と鹿児島で別事業で同じ名前。これらはいずれも上場企業ではないため除外されるので問題なし。
# 提供をうけた口コミデータについては、大阪の社会福祉法人慶生会を受領しているためA12125を採用する。
# 尚、感度分析で全事業場を対象とする場合は、口コミデータであたりがつけられるなら採用。怪しいものは除外して分析を行う。

# NOTE: コード列のフォーマットが２５年度からはAが頭についている。目視で確認した範囲では、単純に２３年度と２４年度のコードの頭の０をAに置き換えただけの様子。それで２３年度と２４年度も統一する。
d23 |> count(`コード`)
d24 |> count(`コード`)
d25 |> count(`コード`)
d26 |> count(`コード`)

d23 <- d23 |> mutate(code = str_c("A",str_remove(`コード`,"^0")))
d24 <- d24 |> mutate(code = str_c("A",str_remove(`コード`,"^0")))
d25 <- d25 |> mutate(code = `コード`)
d26 <- d26 |> mutate(code = `コード`)

companydata <- bind_rows(
  d23 |> select(`業種番号`:`業種名`,code) |> mutate(nendo = 23),
  d24 |> select(`業種番号`:`業種名`,code) |> mutate(nendo = 24),
  d25 |> select(`業種番号`:`業種名`,code) |> mutate(nendo = 25),
  d26 |> select(`業種番号`:`業種名`,code) |> mutate(nendo = 26),
)

# NOTE: code毎に社名の表記ゆれがないかをチェックする
checkdupcompany <- companydata |> count(code, `法人名`)

duplicatednames <- checkdupcompany |> count(`法人名`) |> filter(n > 1) |> pull(`法人名`)
# FOOTNOTE: ２０社、コードは同じなのに社名が違う会社がある。

# NOTE: 以下の表で確認
companydata |> 
  select(code, `法人名`, nendo) |> 
  filter(`法人名` %in% duplicatednames) |> 
  arrange(`法人名`)

companydata |> 
  filter(`法人名` %in% duplicatednames) |> 
  select(code, `法人名`, nendo, `フィードバックシート`) |> 
  mutate(res = str_c(nendo,":",code)) |> 
  select(`法人名`, res, `フィードバックシート`) |> 
  arrange(`法人名`) |> 
  clipr::write_clip()

check_code_func <- function(codelist){
  print(d23 |> filter(`コード` == codelist[1]) |> select(matches("総合偏差値")))
  print(d24 |> filter(`コード` == codelist[2]) |> select(matches("総合偏差値")))
  print(d25 |> filter(`コード` == codelist[3]) |> select(matches("総合偏差値")))
  print(d26 |> filter(`コード` == codelist[4]) |> select(matches("総合偏差値")))
  
  print(d23 |> filter(`コード` == codelist[1]) |> select(`保険者名`,`業種名`))
  print(d24 |> filter(`コード` == codelist[2]) |> select(`保険者名`,`業種名`))
  print(d25 |> filter(`コード` == codelist[3]) |> select(`保険者名`,`業種名`))
  print(d26 |> filter(`コード` == codelist[4]) |> select(`保険者名`,`業種名`))
}


replace_code <- tibble(from = as.character(NULL), to = as.character(NULL))

#以下、重複企業のデータクリーニング

# 法人名	res	フィードバックシート
# イオン琉球株式会社	25:A15278	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A15278.pdf
# イオン琉球株式会社	26:A14154	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A14154.pdf
#   →フィードバックシートを確認したところ、２６年度のシートではA14154では、23,25,26年度にデータがある様子（２５年度の偏差値とA15278は一致）
#   していることから、A15278は振り間違え？
check_code_func(c("","","A15278","A14154"))
companydata |> filter(`コード` == "A14154")
companydata |> filter(`コード` == "A15278")

d23 |> filter(`総合偏差値` == 47.7) #該当なし。
#特に社名変更などが行われた形跡もHPの企業沿革からは確認できないこと
#また、23年度実施のあと、24年度実施なし、25年度と26年度ありという状況からは
#23年度に公表を不可としていたあと、24年度に参加せず、25年度から再度参加（新しく振り直し）
#26年度に23年度分との突合に切り替えたために、23年度に付与されたコードが復活した。とみなして、
#A14154にすべて統一する処理を行う。
replace_code <- replace_code |> add_row(from = "A15278", to = "A14154")

# ウェルネス・コミュニケーションズ株式会社	24:A14792	
# ウェルネス・コミュニケーションズ株式会社	25:A14792	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A14792.pdf
# ウェルネス・コミュニケーションズ株式会社	26:A0366A	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A0366A.pdf
# ホームページからは、2025年に上場となっている。東証GRTでの株価コードが366Aなので、上場にともなうコード変更。
# 実際に、25年度のフィードバックレポートの数字は26年度の過去の数値と一致している。上場非上場は別の列に保持されているため、
# A0366Aに統一する処理とする
check_code_func(c("","014792","A14792","A0366A"))
replace_code <- replace_code |> add_row(from = "A14792", to = "A0366A")

# エムディフード東北株式会社	24:A14762	
# エムディフード東北株式会社	26:A16210	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A16210.pdf
# HPがあるのは「エムディフード株式会社」のみ同一法人に思われるが、そもそも口コミデータの提供で同一法人名がない。1つにしておく
check_code_func(c("","014762","","A16210"))
replace_code <- replace_code |> add_row(from = "A14762", to = "A16210")

# キオクシアホールディングス株式会社	24:A14630	
# キオクシアホールディングス株式会社	25:A14630	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A14630.pdf
# キオクシアホールディングス株式会社	26:A0285A	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A0285A.pdf
# 上場が2024年12月であることや、26年度のフィードバックシートの過去の偏差値と25年度の過去の偏差値が一致していること、住所も一致していることから同一法人で上場前後でコードが変わったと判断。
check_code_func(c("","014630","A14630","A0285A")) 
replace_code <- replace_code |> add_row(from = "A14630", to = "A0285A")

# グロースエクスパートナーズ株式会社	23:A11003	
# グロースエクスパートナーズ株式会社	24:A11003	
# グロースエクスパートナーズ株式会社	25:A0244A	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A0244A.pdf
# 2024年9月上場。以下の通り総合偏差値も一致しているので同一企業と判断
check_code_func(c("011003","011003","A0244A",""))
replace_code <- replace_code |> add_row(from = "A11003", to = "A0244A")

# ビークルエナジージャパン株式会社	25:A15313	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A15313.pdf
# ビークルエナジージャパン株式会社	26:A11301	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A11301.pdf
# フィードバックシートの健康経営宣言のHPが一致しているため、同一企業と判断
replace_code <- replace_code |> add_row(from = "A15313", to = "A11301")

# ヤマトボックスチャーター株式会社	25:A15555	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A15555.pdf
# ヤマトボックスチャーター株式会社	26:A12195	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A12195.pdf
# 過去の偏差値の一致と、HPも一致しているため同一企業と判断
replace_code <- replace_code |> add_row(from = "A15555", to = "A12195")

# 協和医科器械株式会社	23:A13973	
# 協和医科器械株式会社	24:A10684	
# 協和医科器械株式会社	25:A10684	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A10684.pdf
# 協和医科器械株式会社	26:A10684	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A10684.pdf
check_code_func(c("013973","010684","A10684","A10684"))
#総合偏差値の数値が一致していることと、保険者名と業種名が一致していることから同一企業と判断
replace_code <- replace_code |> add_row(from = "A13973", to = "A10684")

# 東京地下鉄株式会社	23:A00001	
# 東京地下鉄株式会社	24:A00001	
# 東京地下鉄株式会社	25:A00001	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A00001.pdf
# 東京地下鉄株式会社	26:A09023	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A09023.pdf
check_code_func(c("000001","000001","A00001","A09023"))
#総合偏差値の数値が一致していることと、保険者名と業種名が一致していることから同一企業と判断
replace_code <- replace_code |> add_row(from = "A00001", to = "A09023")

# 株式会社インターメスティック	24:A14577	
# 株式会社インターメスティック	25:A14577	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A14577.pdf
# 株式会社インターメスティック	26:A0262A	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A0262A.pdf
# 上場に伴うコード変更と判断
check_code_func(c("","014577","A14577","A0262A"))
replace_code <- replace_code |> add_row(from = "A14577", to = "A0262A")

# 株式会社オービ―システム	23:A12083	
# 株式会社オービ―システム	24:A05576	
# 株式会社オービ―システム	25:A05576	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A05576.pdf
# 株式会社オービ―システム	26:A05576	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A05576.pdf
check_code_func(c("012083","005576","A05576","A05576"))
replace_code <- replace_code |> add_row(from = "A12083", to = "A05576")
#偏差値一致、上場にともなうコード変換

# 株式会社システムリサーチ	23:A00261	
# 株式会社システムリサーチ	24:A00261	
# 株式会社システムリサーチ	25:A00261	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A00261.pdf
# 株式会社システムリサーチ	26:A00261	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A00261.pdf
# 株式会社システムリサーチ	23:A03771	
# 株式会社システムリサーチ	24:A03771	
# 株式会社システムリサーチ	25:A03771	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A03771.pdf
# 株式会社システムリサーチ	26:A03771	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A03771.pdf
#システムリサーチは同名他社で問題なし。

# 株式会社ヒノキヤグループ	25:A15135	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A15135.pdf
# 株式会社ヒノキヤグループ	26:A01413	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A01413.pdf
check_code_func(c("","","A15135","A01413"))
#偏差値と健保一致。同じ企業と判断
replace_code <- replace_code |> add_row(from = "A15135", to = "A01413")

# 株式会社ユタカファーマシー	23:A13866	
# 株式会社ユタカファーマシー	24:A12392	
# 株式会社ユタカファーマシー	26:A12392	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A12392.pdf
check_code_func(c("013866","012392","","A12392"))
#偏差値と健保一致。同じ企業と判断
replace_code <- replace_code |> add_row(from = "A13866", to = "A12392")

# 株式会社日立ハイシステム２１	23:A12871	
# 株式会社日立ハイシステム２１	24:A12439	
# 株式会社日立ハイシステム２１	25:A12439	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A12439.pdf
# 株式会社日立ハイシステム２１	26:A12439	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A12439.pdf
check_code_func(c("012871","012439","A12439","A12439"))
#偏差値と健保一致。同じ企業と判断
replace_code <- replace_code |> add_row(from = "A12871", to = "A12439")


# 株式会社近鉄エクスプレス	24:A14288	
# 株式会社近鉄エクスプレス	25:A09375	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A09375.pdf
# 株式会社近鉄エクスプレス	26:A09375	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A09375.pdf
check_code_func(c("","014288","A09375","A09375"))
#偏差値と健保一致。同じ企業と判断
replace_code <- replace_code |> add_row(from = "A14288", to = "A09375")

# 株式会社ＫＯＫＵＳＡＩ　ＥＬＥＣＴＲＩＣ	24:A12317	
# 株式会社ＫＯＫＵＳＡＩ　ＥＬＥＣＴＲＩＣ	25:A06525	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A06525.pdf
# 株式会社ＫＯＫＵＳＡＩ　ＥＬＥＣＴＲＩＣ	26:A06525	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A06525.pdf
check_code_func(c("","012317","A06525","A06525"))
#偏差値と健保一致。同じ企業と判断
replace_code <- replace_code |> add_row(from = "A12317", to = "A06525")

# 社会福祉法人慶生会	23:A12125	
# 社会福祉法人慶生会	25:A12125	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A12125.pdf
# 社会福祉法人慶生会	26:A12125	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A12125.pdf
# 社会福祉法人慶生会	25:A15464	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A15464.pdf
# 社会福祉法人慶生会	26:A15464	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A15464.pdf
#→同名他社で問題なし。

# 社会福祉法人健生会	24:A14744	
# 社会福祉法人健生会	26:A16247	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A16247.pdf
check_code_func(c("","014744","","A16247"))
#協会けんぽの支部は一緒。同一企業と判断
replace_code <- replace_code |> add_row(from = "A14744", to = "A16247")

# 近畿中央ヤクルト販売株式会社	24:A14625	
# 近畿中央ヤクルト販売株式会社	25:A15393	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2025/A15393.pdf
# 近畿中央ヤクルト販売株式会社	26:A14625	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A14625.pdf
check_code_func(c("","014625","A15393","A14625"))
#同一企業と判断。
replace_code <-replace_code |> add_row(from = "A15393", to="A14625")

#NOTE: 03の処理を行うなかで、健康経営データの中で、株式会社ＧＥＮＯＡＶＡ（全角）と株式会社GENOVA（半角）が混在して、かつ上場してたためコードが変わっている企業を発見したため、ここで置き換えておく 
check_code_func(c("014072","009341","A09341","A09341"))
replace_code <-replace_code |> add_row(from = "A14072", to="A09341")
# NOTE: ここまで作成したreplace_codeを利用して、codeを置き換える。
# 尚、最新から過去にさかのぼった際に過去で出現しない会社もいくつかあるため、
# パネルデータ作成の際は、最新年度のデータを過去に適応して作成する方針とする

d23 <- d23 |>  left_join(replace_code, by=c("code"="from")) |> mutate(fixedcode = if_else(is.na(to),code,to)) |> select(!c(to))
d24 <- d24 |>  left_join(replace_code, by=c("code"="from")) |> mutate(fixedcode = if_else(is.na(to),code,to)) |> select(!c(to))
d25 <- d25 |>  left_join(replace_code, by=c("code"="from")) |> mutate(fixedcode = if_else(is.na(to),code,to)) |> select(!c(to))
d26 <- d26 |>  left_join(replace_code, by=c("code"="from")) |> mutate(fixedcode = if_else(is.na(to),code,to)) |> select(!c(to))

d23 <- d23 |> mutate(nendo = 23)
d24 <- d24 |> mutate(nendo = 24)
d25 <- d25 |> mutate(nendo = 25)
d26 <- d26 |> mutate(nendo = 26)

write_rds(d23, file="middledata/d23.rds")
write_rds(d24, file="middledata/d24.rds")
write_rds(d25, file="middledata/d25.rds")
write_rds(d26, file="middledata/d26.rds")
