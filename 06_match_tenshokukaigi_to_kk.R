# NOTE: 転職会議の返却データに tkid を振り、健康経営パネルとの突合表を作る。

library(tidyverse)
library(readxl)

# 入力読み込み--------------------------
# 03 の出力(健康経営パネル、各年度の社名と上場有無)
kkpanel <- read_rds("middledata/panel_kenkoukeiei.rds")

# 転職会議返却データ(2026-05-25 受領、3,091 社、シート "result")
tk_raw <- read_excel(
  "data/kuchikomi/tenshokukaigi/転職会議データ_西田典充様（ファクトリーヘルス株式会社）.xlsx",
  sheet = "result"
)

# データ確認--------------------------
nrow(tk_raw)   # 3091
colnames(tk_raw)
summary(tk_raw)
tk_raw |> filter(`口コミ件数` > 10000)

# 転職会議側に tkid を振る--------------------------
# NOTE: tkid は TK + 5桁0埋め(TK00001〜TK03091)
tk <- tk_raw |>
  mutate(tkid = sprintf("tk%05d", row_number()), .before=1)

# 正規化関数--------------------------
# NOTE: scripts_by_hand/04 の normalize_name に加えて、転職会議側で必要な処理:
# -小書きカナ → 大書きカナ(ェ→エ、ッ→ツ等)
# -ローマ数字 → ラテン文字(Ⅰ→I、ⅰ→i 等)


normalize_name <- function(x) {
  x |>
    coalesce("") |>
    # 全角英数 → 半角
    str_replace_all("[Ａ-Ｚａ-ｚ０-９]", \(s) {
      chartr("ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ０１２３４５６７８９",
             "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", s)
    }) |>
    # 全角スペース除去
    str_replace_all("　", "") |>
    # 小書きカナ → 大書きカナ
    str_replace_all("[ァィゥェォッャュョヮヵヶ]", \(s) {
      chartr("ァィゥェォッャュョヮヵヶ", "アイウエオツヤユヨワカケ", s)
    }) |>
    # ローマ数字 → ラテン
    str_replace_all("Ⅰ","I") |> 
    str_replace_all("Ⅱ","II") |>
    str_replace_all("Ⅲ","III") |> 
    str_replace_all("Ⅳ","IV") |>
    str_replace_all("Ⅴ","V") |> 
    str_replace_all("Ⅵ","VI") |>
    str_replace_all("Ⅶ","VII") |> 
    str_replace_all("Ⅷ","VIII") |>
    str_replace_all("Ⅸ","IX") |> 
    str_replace_all("Ⅹ","X") |>
    str_replace_all("ⅰ","i") |>
    str_replace_all("ⅱ","ii") |>
    str_replace_all("ⅲ","iii") |> 
    str_replace_all("ⅳ","iv") |>
    str_replace_all("ⅴ","v") |>
    #旧仮名
    str_replace_all("ヱ","エ") |> 

    # 連続スペース・前後スペース・全空白除去
    str_replace_all("\\s+", "") |>
    # 大文字化(英字)
    str_to_upper() |>
    # 句読点・記号
    str_replace_all("[・･,，.\\.\\-‐－—–=＝/／&＆\\(\\)（）「」『』\\[\\]【】<>＜＞]", "")
}


# 転職会議側に normname を付与--------------------------
tk <- tk |> mutate(normname = normalize_name(企業名), .before=2)

# 転職会議側の同名(返却データ内で重複)を確認
tk |> count(normname) |> filter(n > 1) #0件

# 健康経営パネル側の社名を long に展開--------------------------
# NOTE: 03 の出力(panel_kenkoukeiei.rds)から 4 年度分の社名を縦持ちに展開し、
# 各年度の社名を正規化する。1 fixedcode に対し最大 4 つの normname がありうる(社名変更を考慮)。distinct で重複削除して fixedcode × normname。
kk_long <- kkpanel |>
  select(fixedcode, matches("companyname")) |>
  pivot_longer(cols = !fixedcode, names_to = "nendo", values_to = "name") |>
  filter(!is.na(name)) |>
  mutate(normname = normalize_name(name)) |>
  distinct(fixedcode, normname)

cat("健康経営側 fixedcode 数:", n_distinct(kk_long$fixedcode), "\n")
cat("健康経営側 normname 数:", n_distinct(kk_long$normname), "\n")

# 健康経営側 normname の重複
kk_dups <- kk_long |> count(normname) |> filter(n > 1)
#想定通りの2社だけ


kk_dups #これらは後から処理をするのでtkから除外しておく。
tk_dups <- tk |> filter(normname %in% kk_dups$normname) #後処理：未
tk_this <- tk |> filter(!normname %in% kk_dups$normname)

# 一次マッチ(完全一致 join)--------------------------
matched <- kk_long |> left_join(tk_this |> select(tkid,normname), by="normname")

#kk(健康経営）のfixedcodeに紐付けられたtkidを除外して残った口コミが既存のkkデータにマッチしそうなものがないか、目視で確認する
tk_this |> filter(!tkid %in% matched$tkid) #2社だけ！
tk_this |> filter(!tkid %in% matched$tkid) |> clipr::write_clip()

# tkid	normname	企業名	総合評点	口コミ件数	仕事のやりがい	給与水準	教育・研修制度	福利厚生	面接_選考	企業の成長性・将来性	社員の魅力	ワークライフバランス	女性の働きやすさ	入社後のギャップ	退職理由	社長の魅力
# tk00067	株式会社大氣社	株式会社大氣社	3.48	279	2.96	3.69	2.75	3.23	3.17	3.1	2.88	1.94	2.33	2.69	2.22	
# tk01099	株式会社テイアイシー	株式会社ティ・アイ・シー	2.89	12	3		1									
#FOOTNOTE: 多分、「氣」の時と「TIC」の表記方法が微妙に違う？

matched |> filter(str_detect(normname,"氣")) #HIT: tk00067:A01979
matched |> filter(str_detect(normname,"テイアイ")) #HIT: tk01099:A12604

# NOTE: 後は、同名法人があるとわかっているところをあわせる
# 転職会議に2026年6月3日にアクセスして、口コミ件数・総合評点が近い企業の住所・職種を確認したうえで、
# 健康経営度調査の法人とマッチングさせる
tk_dups |> clipr::write_clip()
# tkid	  normname	                企業名	                  総合評点	口コミ件数	
# tk00207	株式会社システムリサーチ	株式会社システムリサーチ	3.8	      503        	
#  https://jobtalk.jp/companies/1066：　ソフトウェア/ハードウェア開発業界 / 愛知県名古屋市岩塚本通２丁目１２番 評点：3.8 件数：505件
#  https://jobtalk.jp/companies/78926：  専門商社業界 / 兵庫県豊岡市日高町国分寺１５８番地１ 評点：3.5 件数：29件 
#     ほかも数社あるが、口コミ件数1件などで上記二社のいずれか。フィードバックシートの情報からは、愛知県のシステムリサーチと判断して、突合する。
# 株式会社システムリサーチ	26:A00261（HPからは兵庫県豊岡市）	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A00261.pdf
# 株式会社システムリサーチ	26:A03771	(HPからは、本社名古屋。こっち)https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A03771.pdf 
# tk00207：A03771     

# tk02925	社会福祉法人慶生会	      社会福祉法人慶生会	      3.29	    28	        
# https://jobtalk.jp/companies/7113495 医療・福祉・介護業界 / 鹿児島県鹿児島市下福元町字松ケ尾１７３２番地 評点：2.8 口コミ件数；56
# https://jobtalk.jp/companies/5129414 医療・福祉・介護業界 / 大阪府大阪市巽東４丁目１１番１０号  評価：3.29 口コミ：28件 こっち。
# 社会福祉法人慶生会	26:A12125（HPより大阪の法人。こっち）	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A12125.pdf
# 社会福祉法人慶生会	26:A15464 ((フィードバックシート：保険者が協会けんぽ鹿児島)	https://kenko-keiei.jp/wp-content/themes/kenko_keiei_cms/files/fb/2026/1126_1_A15464.pdf
#tk02925:A12125

#以上の調査より、未マッチだった4件もすべてマッチ（転職会議より提供を受けたデータを全てマッチング完了）
matched <- matched |> 
  mutate(tkid = case_when(
    !is.na(tkid) ~ tkid,
    fixedcode == "A01979" ~ "tk00067", 
    fixedcode == "A12604" ~ "tk01099", 
    fixedcode == "A03771" ~ "tk00207", 
    fixedcode == "A12125" ~ "tk02925"
  ))

#念のため、fixedcodeとtkidで互いに重複がないかを確認する
matched |> select(fixedcode, tkid) |> filter(!is.na(tkid)) |> count(fixedcode) |> filter(n>1)
#3社、同じfixedcodeに口コミデータが別々にあたっている。

#1社目：
matched |> filter(fixedcode == "A00312")
# 日本情報通信株式会社とNTTインテグレーション株式会社は社名変更前後
# 転職会議のサイトを確認したところ、日本情報通信株式会社の口コミは残存しているが、個々のコメントは5年以上前のものばかり。
# tkid"tk02853"は除外して分析する。

#2社目
matched |> filter(fixedcode == "A06594")
# 日本電産株式会社とニデック株式会社は社名変更の関係。
# 転職会議のサイトを確認したところ、日本電産株式会社の口コミ情報は古いことからニデックのデータを利用する
# tkid: tk02858 は削除する

#3社目 
matched |> filter(fixedcode == "A13835")
#社会医療法人東和会に2024年に変更。ほか2つと同様。口コミも1件のみであるので
#tkid:tk02765 を削除する

matched <- matched |> 
  filter(!tkid %in% c("tk02853","tk02858","tk02765"))

#念のため、同じtkidに複数のfixedcodeがあたっていないかも確認。
matched |> select(tkid, fixedcode) |> filter(!is.na(tkid)) |> count(tkid, fixedcode) |> filter(n > 1)
#問題なし。

#パネルデータ用のID対応表：
kktotk <- matched |> 
  filter(!is.na(tkid)) |> 
  select(fixedcode, tkid) |> 
  distinct()
tk
write_rds(kktotk, "middledata/kktotk.rds")
write_rds(tk, "middledata/tk.rds")
