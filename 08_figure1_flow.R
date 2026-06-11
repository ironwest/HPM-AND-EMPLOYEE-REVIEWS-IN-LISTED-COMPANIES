# NOTE: Figure 1 — データフローチャート
rm(list=ls())
library(tidyverse)

master <- read_rds("middledata/master_cross_v3.rds")

#各段階のnを集計

#2023年度から2026年度の健康経営の公開データに登場した企業数
n1 <- nrow(master) #n1 3464

#2026年度（最新の公開データに出現している）
d2 <- master |> filter(final_appear_year == 2025)
n2 <- nrow(d2) #n2 2938

#2026年度の調査で上場している企業
d3 <- d2 |> filter(latest_jojo == 1) 
n3 <- nrow(d3) #n3 874

#口コミデータがある企業
d4 <- d3 |> filter(!is.na(`総合評点`) & !is.na(`口コミ件数`))
n4 <- nrow(d4) #n4 842
d5 <- d4 |> filter(`口コミ件数`>0)
n5 <- nrow(d5) #842 

#Edinetデータがあり、分析可能な企業
d6 <- d5 |> 
  filter(!is.na(従業員数_単体)|!is.na(従業員数_単体or連結)) |> 
  filter(!is.na(当期純利益_単体)) |> 
  filter(!is.na(総資産_単体)) |> 
  filter(!is.na(平均年間給与)) |> 
  filter(!is.na(平均年齢)) |> 
  filter(!is.na(平均勤続年数)) 

d5 |> filter(is.na(従業員数_単体) & is.na(従業員数_単体or連結))|> nrow()
d5 |> filter(is.na(当期純利益_単体))|> select(latest_name,当期純利益_単体) |> nrow()
d5 |> filter(is.na(総資産_単体))|> nrow()
d5 |> filter(is.na(平均年間給与))|> nrow()
d5 |> filter(is.na(平均年齢))|> nrow()
d5 |> filter(is.na(平均勤続年数)) |> nrow()
d5 |> filter(is.na(従業員数_単体)) |> nrow()

edinetmissingcompanies <- bind_rows(
  d5 |> filter(is.na(従業員数_単体))   ,
  d5 |> filter(is.na(当期純利益_単体)) ,
  d5 |> filter(is.na(総資産_単体))     ,
  d5 |> filter(is.na(平均年間給与))    ,
  d5 |> filter(is.na(平均年齢))        ,
  d5 |> filter(is.na(平均勤続年数))    
) |> 
  distinct()

colnames(edinetmissingcompanies)

tgts <- c("従業員数_単体","従業員数_単体or連結","当期純利益_単体","総資産_単体","平均年間給与",
          "平均年齢","平均勤続年数","関係会社株式_単体")

edinetmissingcompanies |> 
  select(fixedcode, edinetCode, latest_name, final_appear_year, all_of(tgts)) |> clipr::write_clip()

#1回目の実行時、この時点でEDINETデータがないのはおかしい：理由を確認したところ、いずれも上場廃止となっている
#企業。今回、取得対象がEDINETのコードに含まれていないため、CSVデータが入手できていないことが判明。
#EDINETの閲覧用ページから社名検索を利用すれば4年以内程度前の財務諸表の取得が可能なため、追加でダウンロードする
#(data/edinet/extradocsに保存して処理をおこなう)
#
# fixedcode	edinetCode	latest_name	final_appear_year	従業員数_単体	当期純利益_単体	総資産_単体	平均年間給与	平均年齢	平均勤続年数	関係会社株式比率_単体	決算年度	資本金	提出者業種
# A01821		三井住友建設株式会社	2025										
# A03141		ウエルシアホールディングス株式会社	2025										
# A03341		日本調剤株式会社	2025										
# A05017		富士石油株式会社	2025										
# A05191		住友理工株式会社	2025										
# 
# A06293		日精樹脂工業株式会社	2025										
# A06937		古河電池株式会社	2025										
# A06973		協栄産業株式会社	2025										
# A07092		株式会社FASTFITNESSJAPAN	2025										
# A07205		日野自動車株式会社	2025										
# 
# A07250		太平洋工業株式会社	2025										
# A07450		株式会社サンデー	2025										
# A07718		スター精密株式会社	2025										
# A07817		パラマウントベッドホールディングス株式会社	2025										
# A09600		株式会社アイネット	2025										
# 
# A09719		SCSK株式会社	2025										
# A13443		センコー商事株式会社	2025										
# A14595		株式会社ゾフ	2025										
# 
# A03544	E32381	サツドラホールディングス株式会社	2025		1.56e+08	8.967e+09				93.5206869633099	2025	1000	小売業
# A06557	E33557	AIAIグループ株式会社	2025		80122000	5885179000				24.8218278492464	2025	20	サービス業
# A07570	E02879	橋本総業ホールディングス株式会社	2025		7.6e+08	3.1538e+10				6.95985794914072	2025	542	卸売業
# A03050	E03489	DCMホールディングス株式会社	2025		1.6869e+10	4.72198e+11				53.0999707749715	2026	10000	小売業
# A07181	E31755	株式会社かんぽ生命保険	2025	17952	1.24093e+11	5.9555517e+13					2025	5e+05	保険業
			

#処理後とれていなかった会社はなくなっている。残りはデータの中で一部欠損があるもの。これ以上の救出は難しい
#fixedcode	edinetCode	latest_name	final_appear_year	従業員数_単体	従業員数_単体or連結	当期純利益_単体	総資産_単体	平均年間給与	平均年齢	平均勤続年数	関係会社株式_単体
# A03050	E03489	DCMホールディングス株式会社	2025		4982	1.6869e+10	4.72198e+11				2.50737e+11
# A03544	E32381	サツドラホールディングス株式会社	2025		1095	1.56e+08	8.967e+09				8.386e+09
# A06557	E33557	AIAIグループ株式会社	2025		1119	80122000	5885179000				1460809000
# A07570	E02879	橋本総業ホールディングス株式会社	2025		938	7.6e+08	3.1538e+10				2.195e+09
# A07181	E31755	株式会社かんぽ生命保険	2025	17952	18656	1.24093e+11	5.9555517e+13				

n6 <- nrow(d6)

n6-n5
colnames(d6)
#ホールディングス系にフラグを立てる
d7 <- d6 |> 
  mutate(`従業員数_単体or連結` = if_else(
    is.na(`従業員数_単体or連結`) , `従業員数_単体`, `従業員数_単体or連結`
  )) |> 
  mutate(kankei_div_sousisan = `関係会社株式_単体`/`総資産_単体`) |> 
  mutate(empratio = `従業員数_単体`/`従業員数_単体or連結`)
d7 |> filter(is.na(従業員数_単体)) |> nrow() #0

d6$従業員数_単体 |> summary()
d6$従業員数_単体or連結 |> summary()


d7 |> filter(kankei_div_sousisan >= 0.7) |> nrow() #HDっぽい？
d7 |> filter(kankei_div_sousisan >= 0.7) |> pull(latest_name) #HDっぽい？

d7 |> filter(empratio < 0.01) |> nrow() #HDっぽい？
d7 |> filter(empratio < 0.02) |> nrow() #HDっぽい？

d7 |> filter(empratio < 0.01) |> pull(latest_name) #HDっぽい？
d7 |> filter(empratio < 0.02) |> pull(latest_name) #HDっぽい？

d7 |> filter(str_detect(latest_name, "ホールディングス")) |> 
  arrange(desc(empratio)) |> 
  select(latest_name, empratio) |> 
  print(n = 50)

hist(d7$kankei_div_sousisan)
hist(d7$empratio, n=50)

#ヒストグラム上は2%でスパイクがあってそこ以外は区別できなさそう
#

write_rds(x = d7, file = "middledata/datafor_analysis.rds")

sprintf("#2023年度から2026年度の健康経営の公開データに登場した企業数 = %d",n1)
sprintf("2026年度（最新の公開データに出現している） = %d",n2)
sprintf("2026年度の調査で上場している企業 = %d",n3)
sprintf("口コミデータがある企業 = %d",n5)
sprintf("Edinetデータがあり、分析可能 = %d",n6)




