# EDINET Indexの取得--------------------------
library(tidyverse)
library(httr2)

API_KEY <- Sys.getenv("EDINET_API_KEY")
BASE_URL <- "https://api.edinet-fsa.go.jp/api/v2/documents.json"
DATE_FROM <- as.Date("2017-01-01") 
DATE_TO   <- as.Date("2026-06-01") #Sys.Date()
TARGET_DOC_TYPES <- c("120", "130")
# FOOTNOTE: 取得対象の書類コード(EDINET docTypeCode)
#   120: 有価証券報告書(年次)               ← 本研究の主要書類
#   130: 訂正有価証券報告書                   ← 修正履歴の把握用

OUT_INDEX <- "output/edinet_index.csv"
OUT_PROGRESS <- "output/edinet_index_progress.csv"
SLEEP_SEC <- 2

## 進捗管理 ------------------------------------------------------------
# NOTE: OUT_PROGRESSファイルに処理済みの日付記載あり。
# todo_datesで未処理日付を取得する。
if (file.exists(OUT_PROGRESS)) {
  done <- read_csv(OUT_PROGRESS, col_types = "Dii") |>
    pull(date) |> as.Date()
} else {
  done <- as.Date(character(0))
}

all_dates <- seq.Date(DATE_FROM, DATE_TO, by = "day")
todo_dates <- setdiff(all_dates, done) |> as.Date(origin = "1970-01-01")

## API 呼び出し関数 ----------------------------------------------------
# NOTE:
# fetch_one_day(date): 指定日の書類一覧を取得し、対象タイプに絞って返す
# 1日分の EDINET 書類一覧API を呼んで、120/130 のメタデータ tibble に変換する。

fetch_one_day <- function(date) {
  req <- request(BASE_URL) |>
    req_url_query(date = format(date, "%Y-%m-%d"),
                  type = "2",  # type=2: メタデータ含む完全な書類一覧
                  `Subscription-Key` = API_KEY) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_timeout(60)
  
  resp <- tryCatch(req_perform(req),
                   error = function(e) {
                     warning("Request failed for ", date, ": ", conditionMessage(e))
                     return(NULL)
                   })
  
  if (is.null(resp)) return(NULL)
  
  body <- resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(body$results) || length(body$results) == 0) {
    return(tibble(date = date, n_results = 0L))
  }
  
  results <- as_tibble(body$results) |> mutate(取得日 = date)
  
  # 対象書類タイプ(120/130)にフィルタ
  results_filtered <- results |>
    filter(docTypeCode %in% TARGET_DOC_TYPES)
  
  list(
    progress = tibble(date = date,
                      n_results = nrow(results),
                      n_target = nrow(results_filtered)),
    records = results_filtered
  )
}

# 増分書き込み --------------------------------------------------------
append_csv <- function(df, path) {
  if (nrow(df) == 0) return(invisible(NULL))
  if (!file.exists(path)) {
    write_csv(df, path)
  } else {
    write_csv(df, path, append = TRUE)
  }
}

# メインループ --------------------------------------------------------
t0 <- Sys.time()

for (i in seq_along(todo_dates)) {
  d <- todo_dates[i]
  res <- fetch_one_day(d)
  
  if (is.null(res)) {
    cat(sprintf("[%4d/%4d] %s: SKIP (request failed)\n", i, length(todo_dates), d))
    Sys.sleep(SLEEP_SEC)
    next
  }
  
  if (is.data.frame(res)) {
    append_csv(res |> mutate(n_target = 0L), OUT_PROGRESS)
  } else {
    append_csv(res$progress, OUT_PROGRESS)
    if (nrow(res$records) > 0) {
      append_csv(res$records, OUT_INDEX)
    }
  }
  
  if (i %% 20 == 0 || i == length(todo_dates)) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    eta_min <- (elapsed / i) * (length(todo_dates) - i) / 60
    cat(sprintf("[%4d/%4d] %s: 経過 %.0fs / 残り ETA ≒ %.1f分\n",
                i, length(todo_dates), d, elapsed, eta_min))
  }
  
  Sys.sleep(SLEEP_SEC)
}

# EDINETから取得する企業を同定する---------------------------------------
# NOTE：健康経営度調査のパネルデータの上場企業の企業名とEDINETの企業名をマッチングさせて、必要な企業データを取得する

library(tidyverse)
library(readxl)
library(openxlsx)

#健康経営度調査に含まれる企業リストを取得
dat <- read_rds("middledata/panel_kenkoukeiei.rds")

dat |> count() #method記載
dat |> count(n2025_jojo) #method記載

# 金融庁が公開する「EDINETコード集約一覽）を読み込む
ec <- read_csv("data/edinet/edinet_codes.csv",
               skip = 1,
               locale = locale(encoding = "CP932"),
               show_col_types = FALSE)

ec |> filter(str_detect(`提出者名`,"ホールディングス"))  |> count(`提出者業種`) |> print(n = 40)

# NOTE:最終年度の企業名とEDINETの対応を作成して、EDINETコードをパネルデータに追加する
d25 <- dat |> select(fixedcode, name = n2025_companyname, jojo = n2025_jojo) |> filter(jojo == 1)
d24 <- dat |> select(fixedcode, name = n2024_companyname, jojo = n2024_jojo) |> filter(jojo == 1)
d23 <- dat |> select(fixedcode, name = n2023_companyname, jojo = n2023_jojo) |> filter(jojo == 1)
d22 <- dat |> select(fixedcode, name = n2022_companyname, jojo = n2022_jojo) |> filter(jojo == 1)

# NOTE: EDINETの企業名と健康経営度調査の企業名をマッチングするための社名を正規化する関数
normalize_name <- function(x) {
  x |>
    # NA -> ""
    coalesce("") |>
    # 全角英数 → 半角
    str_replace_all("[Ａ-Ｚａ-ｚ０-９]", \(s) {
      chartr("ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ０１２３４５６７８９",
             "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", s)
    }) |>
    # 全角スペース・記号
    str_replace_all("　", "") |>
    # 連続スペース・前後スペース・全空白除去
    str_replace_all("\\s+", "") |>
    # 大文字化(英字)
    str_to_upper() |>
    # 句読点・記号
    str_replace_all("[・･,，.\\.\\-‐－—–=＝/／&＆\\(\\)（）「」『』\\[\\]【】<>＜＞]", "")
}

# NOTE: 正規化した名前を追加
d25 <- d25 |> mutate(normname = normalize_name(name))
d24 <- d24 |> mutate(normname = normalize_name(name))
d23 <- d23 |> mutate(normname = normalize_name(name))
d22 <- d22 |> mutate(normname = normalize_name(name))


ec2 <- ec |>
  rename(code = `ＥＤＩＮＥＴコード`, name = `提出者名`) |> 
  filter(上場区分 == "上場") |>  
  mutate(normname = normalize_name(name)) |> 
  relocate(code, normname, name)


# NOTE: 正規化した後に同じ名前になっている会社がないかをチェック
d25 |> count(normname) |> filter(n > 1) #OK
d24 |> count(normname) |> filter(n > 1) #OK
d23 |> count(normname) |> filter(n > 1) #OK
d22 |> count(normname) |> filter(n > 1) #OK

unique(c(d25$normname,d24$normname,d23$normname,d22$normname)) |> length()
#健康経営パネルデータに含まれる、上場フラグのある法人名、社名変更前後を含む

ec2 |> count(normname) |> filter(n > 1) #NG
ec2 |> count(name) |> filter(n > 1) #NG　#EDINET収載企業で同名の企業が2社ある。

ec2 |> distinct(normname) |> nrow()
ec2 |> distinct(name) |> nrow() #EDINETデータに収載された法人名数

# 1 株式会社アルファ         2
# 2 株式会社バッファロー     2
# の2社が同じ名前。
checkvec <- c("株式会社アルファ","株式会社バッファロー")
d25 |> filter(normname %in% checkvec)
d24 |> filter(normname %in% checkvec)
d23 |> filter(normname %in% checkvec)
d22 |> filter(normname %in% checkvec)
#健康経営パネルに含まれている企業名のためにチェックが必要

match_by_hand <- tibble(kk = character(0), edinet = character(0))

ec2 |> filter(normname %in% checkvec) |> clipr::write_clip()
# code	  normname	            name	                提出者種別	    上場区分	連結の有無	資本金	決算日	  所在地	                        提出者業種	証券コード	提出者法人番号
# E02086	株式会社バッファロー	株式会社バッファロー	内国法人・組合	上場	    有	        1000	  3月31日		千代田区丸の内一丁目１１番１号	電気機器	  66760	      6180001048602
# E02245	株式会社アルファ	    株式会社アルファ	    内国法人・組合	上場	    有	        2760	  3月31日		横浜市金沢区福浦一丁目６番８号	金属製品	  34340	      1020001005862
# E03447	株式会社バッファロー	株式会社バッファロー	内国法人・組合	上場	    有	        653	    3月31日		川口市本町四丁目１番８号	      小売業	    33520	      1030001076804
# E05083	株式会社アルファ	    株式会社アルファ	    内国法人・組合	上場	    無	        409 	  8月31日		岡山市桑野７０９番地６	        サービス業	47600	      6260001000380

raw25 <- read_rds("middledata/d26.rds") |> 
  filter(`法人名` %in% checkvec)
d25 |> filter(normname %in% checkvec)
d24 |> filter(normname %in% checkvec)
d23 |> filter(normname %in% checkvec)
d22 |> filter(normname %in% checkvec)
# 健康経営度調査で、codeが26年度よりA証券番号という表記であること、業種などから以下の対応を作成。
match_by_hand <- match_by_hand |> 
  add_row(kk = "A03352", edinet = "E03447") |> 
  add_row(kk = "A03434", edinet = "E02245") 

# NOTE:以下のJOINで複数行マッチになるため、対応していないアルファとバッファローは消しておく。
ec3 <- ec2 |> 
  select(code, normname) |> 
  filter(code != "E02086") |> 
  filter(code != "E05083")

# NOTE: 完全一致でジョインする
makematch <- function(dnn,ecx){
  leftjoined <- dnn |> select(fixedcode, normname) |>  left_join(ecx,by="normname")
  leftjoined |> count(normname) |> filter(n > 1) #EDINETでの重複なし
  nonmatched <- leftjoined |> filter(is.na(code)) 
  matched    <- leftjoined |> filter(!is.na(code))
  return(list(nonmatched = nonmatched, matched = matched))
}
 
m25 <- makematch(d25,ec3)
m24 <- makematch(d24,ec3)
m23 <- makematch(d23,ec3)
m22 <- makematch(d22,ec3)

#NOTE: matchされた内容を確認する
matchdat <- bind_rows(m25$matched,m24$matched,m23$matched,m22$matched)

matchdat |> 
  distinct() |> 
  count(fixedcode, code) |> 
  filter(n > 1)

matchdat |> distinct() |> count(fixedcode, code) |> nrow() 
#FOOTNOTE： METHOD EDINET KKパネルマッチ数

matchdat <- matchdat |> rename(matchname = normname)

#FOOTNOTE: 重複なくあたっている

# NOTE: nonmatchの内容を確認する
nonmatch <- bind_rows(m25$nonmatched,m24$nonmatched,m23$nonmatched,m22$nonmatched) |> distinct()

nonmatch |> nrow()
#FOOTNOTE： METHOD EDINET KKパネル アンマッチ数

needtolookup <- nonmatch |> 
  left_join(matchdat, by=c("fixedcode")) |> 
  select(fixedcode, normname, matchname) |> 
  filter(is.na(matchname))
needtolookup
#FOOTNOTE: 社名変更でもない96社を抽出

#NOTE:以下の96社すべてをネット検索等で対象会社を可能な限り抽出する
needtolookup |> 
  mutate(res = str_c('"',fixedcode,'"',',',
                     '"',normname,'"',',',
                     '"',"",'"',',',
                     '# spot(','"',normname,'")'
                     )) |> 
  select(res) |> 
  clipr::write_clip()


google_search <- function(query) {
  q   <- utils::URLencode(query, reserved = TRUE)
  url <- paste0("https://www.google.com/search?q=", q)
  
  os <- Sys.info()[["sysname"]]
  if (os == "Darwin") {                 # macOS
    system2("open", args = c("-a", shQuote("Google Chrome"), shQuote(url)))  # ← ここを修正
  } else if (os == "Windows") {
    chrome <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
    system2(chrome, args = shQuote(url), wait = FALSE)
  } else {
    system2("google-chrome", args = shQuote(url), wait = FALSE)
  }
  invisible(url)
}

spot <- function(txt){
  res <- ec2 |> filter(str_detect(normname,txt)) |> select(code, normname,`所在地`,`提出者業種`)
  if(nrow(res)==0){
    google_search(txt)
  }else{
    print(res)
    clipr::write_clip(res$code)  
  }
}

matchbysearch <- tribble(
  ~kkcode, ~normname, ~edinetcode,
  "A01821",	"三井住友建設株式会社",	"REMOVE",　#spot("三井住友建設株式会社")
  # spot("インフロニア") 親会社はあり。除外
  "A01979",	"株式会社大氣社",	"E00183",　      #spot("株式会社大氣社")
  #spot("株式会社大気社")
  "A02587","サントリー食品インターナショナル株式会社","E27622",# spot("サントリー食品インターナショナル株式会社")
  # spot("サントリービバレッジ") サントリー食品インターナショナルは28日、2026年4月1日に社名を「サントリービバレッジ&フード」に変更する
  "A02805","エスビー食品株式会社","E00452",# spot("エスビー食品株式会社")
  # spot("ヱスビー")
  "A05191","住友理工株式会社","REMOVE",# spot("住友理工株式会社")
  # spot("住友") HP:当社株式は、2026 年1月 29日をもって、東京証券取引所プライム市場および名古屋証券取引所プレミア市場において上場廃止 除外
  "A05333","日本碍子株式会社","E01137",# spot("日本碍子株式会社")
  # spot("NGK")
  "A03422","株式会社JMAX","E01452",# spot("株式会社JMAX")
  # spot("MAX")
  "A06293","日精樹脂工業株式会社","REMOVE",# spot("日精樹脂工業株式会社") 上場廃止
  "A07718","スター精密株式会社","REMOVE",# spot("スター精密株式会社") 当社株式は、2026年3月13日をもちまして、東京証券取引所プライム市場において上場廃止となりました。
  "A06937","古河電池株式会社","REMOVE",# spot("古河電池株式会社")2025年12月22日をもって上場廃止となりました。
  
  "A07205","日野自動車株式会社","REMOVE",# spot("日野自動車株式会社")2026年4月1日の三菱ふそうとの経営統合をもちまして上場廃止となりました
  "A07250","太平洋工業株式会社","REMOVE",# spot("太平洋工業株式会社") 2026 年４月 13 日をもって上場廃止とな
  "A07702","株式会社ジェイエムエス","E02303",# spot("株式会社ジェイエムエス")
  "A07817","パラマウントベッドホールディングス株式会社","REMOVE",# spot("パラマウントベッドホールディングス株式会社")当社株式は2026年2月5日付で上場廃止となりました。
  "A09044","南海電気鉄道株式会社","REMOVE",# spot("南海電気鉄道株式会社") #株式会社NANKAIが親会社？REMOVE
  "A09600","株式会社アイネット","REMOVE",# spot("株式会社アイネット")当社株式は、2026年2月26日をもちまして、東京証券取引所プライム市場において上場廃止となりました。 
  "A09719","SCSK株式会社","REMOVE",# spot("SCSK株式会社") 親会社である住友商事による完全子会社化に伴い、2026年3月12日をもって東証プライム市場での上場を廃止
  "A13443","センコー商事株式会社","REMOVE",# spot("センコー商事株式会社")
  # spot("センコ") 親会社のみREMOVE
  "A03141","ウエルシアホールディングス株式会社","REMOVE",# spot("ウエルシアホールディングス株式会社")株式会社ツルハホールディングスとの経営統合に伴い当社株式は、2025年11月27日をもちまして、東京証券取引所プライム市場において上場廃止
  "A03341","日本調剤株式会社"                  ,"REMOVE",# spot("日本調剤株式会社")当社株式は、2025年12月19日をもちまして、東京証券取引所プライム市場において上場廃止となりました。
  
  "A07450","株式会社サンデー"                  ,"REMOVE",# spot("株式会社サンデー") 株式会社サンデーは、2026年4月1日をもちまして東京証券取引所の上場を廃止し、同年4月3日付で親会社であるイオン株式会社の完全子会社となりました。
  "A08173","上新電機株式会社"                  ,"E03052",# spot("上新電機株式会社") spot("JOSHIN")
  "A08750","第一生命ホールディングス株式会社"  ,"E06141",# spot("第一生命ホールディングス株式会社") spot("第一ライフ")
  "A04666","パーク二四株式会社"                ,"E04979",# spot("パーク二四株式会社")
  "A07092","株式会社FASTFITNESSJAPAN"          ,"REMOVE",# spot("株式会社FASTFITNESSJAPAN") 当社株式は、2026年4月20日をもちまして、東京証券取引所プライム市場において上場廃止となりました。
  "A04922","株式会社コーセー"                  ,"E01049",# spot("株式会社コーセー")
  "A07105","三菱ロジスネクスト株式会社"        ,"E02136",# spot("三菱ロジスネクスト株式会社") spot("ロジスネクスト")
  "A05576","株式会社オービ―システム"           ,"E38645",# spot("株式会社オービ―システム") spot("オービ")
  "A14595","株式会社ゾフ"                      ,"REMOVE",# spot("株式会社ゾフ")
  "A06973","協栄産業株式会社"                  ,"REMOVE",# spot("協栄産業株式会社") spot("加賀")
  
  "A05017","富士石油株式会社"                  ,"REMOVE",# spot("富士石油株式会社") 当社株式は2026年1月20日をもって上場廃止となりました。
  "A06022","株式会社赤阪鉄工所"                ,"E01475",# spot("赤阪鐵工所")
  "A15970","宮城中央ヤクルト販売株式会社"      ,"REMOVE",# spot("ヤクルト")
  "A03521","エコナックホールディングス株式会社","E00576",# spot("テルマー")
  "A01775","富士電機EC株式会社"                ,"REMOVE",# spot("富士電機")
  "A08114","株式会社デサント"                  ,"REMOVE",# spot("BS")
  "A04921","株式会社ファンケル"                ,"REMOVE",# spot("株式会社ファンケル")当社株式は、2024年12月18日をもちまして、東京証券取引所プライム市場において上場廃止となりました。
  "A06482","株式会社ユーシン精機"              ,"E01710",# spot("YUSHIN")
  "A06755","株式会社富士通ゼネラル"            ,"E31321",# spot("株式会社ゼネラル")
  "A07739","キヤノン電子株式会社"              ,"REMOVE",# spot("キヤノン") 2026年4月21日付で東京証券取引所プライム市場の上場を廃止
  
  "A09055","株式会社アルプス物流"              ,"REMOVE",# spot("株式会社アルプス物流") アルプス物流は、ロジスティード株式会社（旧日立物流）によるTOB（株式公開買付）の成立に伴い、2024年12月17日をもって東京証券取引所プライム市場における上場を廃止
  "A01973","NECネッツエスアイ株式会社"         ,"REMOVE",# spot("NECネッツエスアイ株式会社")親会社である日本電気（NEC）の完全子会社化に伴い、2025年3月21日をもって東京証券取引所プライム市場での上場を廃止し
  "A03857","株式会社ラック"                    ,"REMOVE",# spot("株式会社ラック")
  "A04435","株式会社カオナビ"　　　　　　　　　,"REMOVE",# spot("株式会社カオナビ") 2025年6月11日に東証グロース市場から上場廃止
  "A04820","株式会社イーエムシステムズ"　　　　,"E05155",# spot("株式会社イーエムシステムズ")
  "A09613","株式会社NTTデータグループ"　　　　 ,"REMOVE",# spot("株式会社NTTデータグループ")2025年9月26日をもって東京証券取引所（プライム市場）での上場を廃止
  "A09749","富士ソフト株式会社"　　　　　　　　,"REMOVE",# spot("富士ソフト株式会社")2025年5月16日付で上場廃止と
  "A07451","三菱食品株式会社"　　　　　　　　　,"REMOVE",# spot("三菱食品株式会社")2025年9月26日をもって上場廃止
  "A07623","株式会社サンオータス"　　　　　　　,"REMOVE",# spot("株式会社サンオータス") 2025年8月18日付で東京証券取引所スタンダード市場において上場廃止
  "A08182","株式会社いなげや"　　　　　　　　　,"REMOVE",# spot("株式会社いなげや")2024年11月28日付で東京証券取引所プライム市場での上場を廃止
  
  "A08905","イオンモール株式会社"　　　　　　　,"REMOVE",# spot("イオンモール株式会社") 廃止
  "A09787","イオンディライト株式会社"　　　　　,"REMOVE",# spot("イオンディライト株式会社") 廃止
  "A07958","天馬株式会社"　　　　　　　　　    ,"REMOVE",# spot("天馬株式会社") 廃止
  "A04551","鳥居薬品株式会社"　　　　　　　　　,"REMOVE",# spot("鳥居薬品株式会社")
  "A07518","ネットワンシステムズ株式会社"　　　,"REMOVE",# spot("ネットワンシステムズ株式会社")
  "A02487","株式会社CDG"　　　　　　　　　     ,"REMOVE",# spot("株式会社CDG")
  "A09161","IDEホールディングス株式会社"　　　 ,"REMOVE",# spot("IDEホールディングス株式会社")
  "A02830","アヲハタ株式会社"　　　　　　　　　,"REMOVE",# spot("アヲハタ株式会社")
  "A03978","株式会社マクロミル"　　　　　　　　,"REMOVE",# spot("株式会社マクロミル")
  "A02715","エレマテック株式会社"　　　　　　　,"REMOVE",# spot("エレマテック株式会社")
  "A01775","富士古河EC株式会社"　　　　　　　  ,"REMOVE",# spot("富士電機")
  
  "A01805","飛島建設株式会社"　　　　　　　    ,"REMOVE",# spot("飛島建設株式会社")
  "A04185","JSR株式会社"　　　　　　        　 ,"REMOVE",# spot("JSR株式会社")
  "A04987","株式会社寺岡製作所"　　　　　　 　 ,"REMOVE",# spot("株式会社寺岡製作所")
  "A04581","大正製薬ホールディングス株式会社"　,"REMOVE",# spot("大正製薬ホールディングス株式会社")
  "A05481","山陽特殊製鋼株式会社"　　　　　　　,"REMOVE",# spot("山陽特殊製鋼株式会社")
  "A06502","株式会社東芝"　　　　　　　        ,"REMOVE",# spot("株式会社東芝")
  "A07905","大建工業株式会社"　　　　　　　    ,"REMOVE",# spot("大建工業株式会社")
  "A03784","株式会社ヴィンクス"　　　　　　　    ,"REMOVE",# spot("株式会社ヴィンクス")
  "A04739","伊藤忠テクノソリューションズ株式会社","REMOVE",# spot("伊藤忠テクノソリューションズ株式会社")
  "A09995","株式会社グローセル"　　　　　　　    ,"REMOVE",# spot("株式会社グローセル")
  "A02651","株式会社ローソン"　　　　　　　      ,"REMOVE",# spot("株式会社ローソン")
  
  "A07676","株式会社グッドスピード"　　　　　　　,"REMOVE",# spot("株式会社グッドスピード")
  "A08168","株式会社ケーヨー"　　　　　　　      ,"REMOVE",# spot("株式会社ケーヨー")
  "A02412","株式会社ベネフィットワン"　　　　　　,"REMOVE",# spot("株式会社ベネフィットワン")
  "A06532","株式会社ベイカレントコンサルティング","E32549",# spot("ベイカレント")
  "A06704","岩崎通信機株式会社"　　　　　　　    ,"REMOVE",# spot("岩崎通信機株式会社")
  "A02309","シミックホールディングス株式会社"　　,"REMOVE",# spot("シミックホールディングス株式会社")
  "A02613","株式会社Jオイルミルズ"　　　　　　　 ,"E00434",# spot("オイルミルズ")
  "A13507","中部グリコ栄食株式会社"　　　　　　　,"REMOVE",# spot("中部グリコ栄食株式会社")
  "A05008","東亜石油株式会社"　　　　　　　      ,"REMOVE",# spot("東亜石油株式会社")
  "A05486","株式会社プロテリアル"　　　　　　　  ,"REMOVE",# spot("株式会社プロテリアル")
  "A05805","昭和電線ホールディングス株式会社"　　,"E01336",# spot("SWCC")
  
  "A09086","株式会社日立物流"　　　　　　　      ,"REMOVE",# spot("株式会社日立物流")
  "A03924","株式会社ランドコンピュータ"　　　　　,"REMOVE",# spot("株式会社ランドコンピュータ")
  "A09418","株式会社USENNEXTHOLDINGS"　　　　　　,"E31052",# spot("NEXT")
  "A09422","コネクシオ株式会社"　　　　　　　    ,"REMOVE",# spot("コネクシオ株式会社")
  "A09613","株式会社エヌティティデータ"　　　　　,"REMOVE",# spot("株式会社エヌティティデータ")
  "A07442","中山福株式会社"　　　　　　　        ,"E02805",# spot("中山")
  "A09810","日鉄物産株式会社"　　　　　　　      ,"REMOVE",# spot("日鉄物産株式会社")
  "A08369","株式会社京都銀行"　　　　　　　      ,"E38714",# spot("京都フィナ")
  "A08521","株式会社長野銀行"　　　　　　　      ,"REMOVE",# spot("株式会社長野銀行")
  "A01954","日本工営株式会社"　　　　　　　      ,"REMOVE",# spot("日本工営株式会社")
  
  "A02196","株式会社エスクリ"　　　　　　　      ,"REMOVE",# spot("株式会社エスクリ")
  "A04745","株式会社東京個別指導学院"　　　　　　,"REMOVE",# spot("株式会社東京個別指導学院")
  "A06172","株式会社メタップス"　　　　　　　    ,"REMOVE" # spot("株式会社メタップス")
)


matchbysearch |> filter(edinetcode == "REMOVE") |> nrow()
matchbysearch |> filter(edinetcode != "REMOVE") |> nrow()

# 株式会社コーセー
#raw26 |> filter(str_detect(`法人名`,"コーセー")) |> pull(フィードバックシート) #化学

#NOTE: 健康経営データとEDINETのコードのマッチング結果
kktoedinet <- bind_rows(
  matchdat |> 
    select(fixedcode, code),
  needtolookup |>
    select(fixedcode) |> 
    distinct() |> 
    left_join(matchbysearch, by = c("fixedcode"="kkcode")) |> 
    select(fixedcode, code = edinetcode)
) |> 
  distinct()

kktoedinet |> filter(code == "REMOVE") |> nrow() #71社が上場廃止等でマッチせず
kktoedinet |> filter(code != "REMOVE") |> nrow() #マッチは976社

kktoedinet <- kktoedinet |> filter(code != "REMOVE")


#上場廃止企業がEDINETのコードに掲載されていないため、以下のものを手動でマッチングしておく）Figure1からの手戻り作業）
# A01821		三井住友建設株式会社	2025										
# A03141		ウエルシアホールディングス株式会社	2025										
# A03341		日本調剤株式会社	2025										
# A05017		富士石油株式会社	2025										
# A05191		住友理工株式会社	2025										
# A06293		日精樹脂工業株式会社	2025										
# A06937		古河電池株式会社	2025										
# A06973		協栄産業株式会社	2025										
# A07092		株式会社FASTFITNESSJAPAN	2025										
# A07205		日野自動車株式会社	2025										
# A07250		太平洋工業株式会社	2025										
# A07450		株式会社サンデー	2025										
# A07718		スター精密株式会社	2025										
# A07817		パラマウントベッドホールディングス株式会社	2025										
# A09600		株式会社アイネット	2025										
# A09719		SCSK株式会社	2025										
# A13443		センコー商事株式会社	2025										
# A14595		株式会社ゾフ	2025

# 05より：
# edinetCode	companyName
# 	三井住友建設株式会社
# 	ウエルシアホールディングス株式会社
# 	日本調剤株式会社
# 	富士石油株式会社
# 	日精樹脂工業株式会社
# 	古河電池株式会社
# 	協栄産業株式会社
# 	株式会社Fast Fitness Japan
# 	日野自動車株式会社
# 	太平洋工業株式会社
# 	株式会社サンデー
# 	スター精密株式会社
# 	パラマウントベッドホールディングス株式会社
# 	株式会社アイネット
# 	ＳＣＳＫ株式会社
# 	センコーグループホールディングス株式会社
# 	株式会社インターメスティック

extrac_matching <- tribble(
  ~fixedcode, ~code, 
  "A01821",	"E00085"	, #"三井住友建設株式会社	"
  "A03141",	"E21035"	, #"ウエルシアホールディングス株式会社	"
  "A03341",	"E05422"	, #"日本調剤株式会社	"
  "A05017",	"E01082"	, #"富士石油株式会社	"
  "A05191",	"E01097"	, #"住友理工株式会社	"
  "A06293",	"E01695"	, #"日精樹脂工業株式会社	"
  "A06937",	"E01917"	, #"古河電池株式会社	"
  "A06973",	"E01619"	, #"協栄産業株式会社	"
  "A07092",	"E35318"	, #"株式会社FASTFITNESSJAPAN	"
  "A07205",	"E02146"	, #"日野自動車株式会社	"
  "A07250",	"E02178"	, #"太平洋工業株式会社	"
  "A07450",	"E03245"	, #"株式会社サンデー	"
  "A07718",	"E02302"	, #"スター精密株式会社	"
  "A07817",	"E25664"	, #"パラマウントベッドホールディングス株式会社	"
  "A09600",	"E04919"	, #"株式会社アイネット	"
  "A09719",	"E04830"	, #"SCSK株式会社	"
  "A13443",	"E04179"	, #"センコー商事株式会社	"
  "A14595",	"E03492"	, #"株式会社ゾフ"
)

#これらのファイルはAPIで自動取得できないため、EDINETサイトで検索して直接CSVファイルを含んだZIPファイルをダウンロードする必要がある。
#

kktoedinet <- kktoedinet |> 
  bind_rows(extrac_matching) |> 
  distinct()

write_rds(kktoedinet,"middledata/kktoedinetcode.rds")

## EDINETよりデータ取得 ------------------

library(tidyverse)
library(httr2)

API_KEY <- Sys.getenv("EDINET_API_KEY")
DOC_BASE_URL <- "https://api.edinet-fsa.go.jp/api/v2/documents/"
DOWNLOAD_DIR <- "data/edinet/docs"
PROGRESS_FILE <- "output/edinet_downloads_progress.csv"
SLEEP_SEC <- 2

if (!dir.exists(DOWNLOAD_DIR)) dir.create(DOWNLOAD_DIR, recursive = TRUE)

# 1. 対象 docID の決定 --------------------------------------------
idx <- read_csv("output/edinet_index.csv", show_col_types = FALSE,
                col_types = cols(.default = "c"))

DOWNLOAD_MODE <- "all"  # "latest_only" / "employee_panel" / "all"

# 対象書類: 有価証券報告書(120)のみ、本パネル対象社のもの
#
# フィルタ条件の根拠:
#   docTypeCode == "120": 訂正(130)は除外。最初の有報のみで十分(本研究の
#                         分析精度では訂正を追わなくても結論は変わらない)
#
# periodEnd は "YYYY-MM-DD" 形式の文字列なので、先頭4文字を取って integer 化。

target_codes <- kktoedinet$code

targets_all <- idx |>
  filter(docTypeCode == "120",
         edinetCode %in% target_codes,
         withdrawalStatus == "0",        # 取下げ済を除外
         disclosureStatus == "0") |>     # 開示済のみ
  mutate(periodEndYear = as.integer(substr(periodEnd, 1, 4))) |>
  filter(periodEndYear >= 2017)          

if (DOWNLOAD_MODE == "latest_only") {
  # 各 edinetCode について最新 periodEnd の書類を選ぶ
  # 同じ periodEnd で複数(訂正含む)あれば最新 submitDateTime を採用
  # arrange + slice(1) で1社1書類に絞る。
  targets <- targets_all |>
    arrange(edinetCode, desc(periodEnd), desc(submitDateTime)) |>
    group_by(edinetCode) |>
    slice(1) |>
    ungroup()
  
} else if (DOWNLOAD_MODE == "employee_panel") {
  # periodEndYear 2022 以降の全有報(従業員情報を各年度パネル化するため)
  targets <- targets_all |> filter(periodEndYear >= 2022)
  
} else {
  targets <- targets_all
  
}

print(targets |> count(periodEndYear))

# ---- 2. 進捗管理 ---------------------------------------------------------
if (file.exists(PROGRESS_FILE)) {
  done <- read_csv(PROGRESS_FILE, show_col_types = FALSE) |>
    filter(status %in% c("ok", "skip_no_data"))
} else {
  done <- tibble(docID = character(), kind = character(), status = character(),
                 size_bytes = integer(), msg = character())
}

todo <- targets |> anti_join(done, by = "docID")
cat("\n総計", nrow(targets), "件 / 既処理", nrow(done), "件 / 未処理", nrow(todo), "件\n")

# ダウンロード関数 -------------------------------------------------
fetch_one_doc <- function(docID, prefer_csv = TRUE, csvFlag = "0", xbrlFlag = "0") {
  # type=5: CSV版(タグ別 TSV/UTF-16LE 構成のZIPアーカイブ)
  # type=1: XBRL本体(複数 XML 構成のZIPアーカイブ)
  if (prefer_csv && csvFlag == "1") {
    type_param <- "5"; kind <- "csv"
  } else if (xbrlFlag == "1") {
    type_param <- "1"; kind <- "xbrl"
  } else {
    # どちらも提供がない書類(古い書類や提出形式の問題で発生する)
    return(list(status = "skip_no_data", kind = NA_character_,
                size_bytes = 0L, msg = "no xbrl/csv available"))
  }
  
  out_path <- file.path(DOWNLOAD_DIR, paste0(docID, "_", kind, ".zip"))
  
  # 既に存在(再開時)のスキップ判定
  # file.size > 0 で「空ファイルでない」ことを確認する。0バイトの残骸は
  # 前回失敗の可能性があるので再取得する。
  if (file.exists(out_path) && file.size(out_path) > 0) {
    return(list(status = "ok", kind = kind,
                size_bytes = as.integer(file.size(out_path)), msg = "already_exists"))
  }
  
  # /api/v2/documents/<docID> エンドポイントへ GET
  # クエリで type と Subscription-Key を渡す
  req <- request(paste0(DOC_BASE_URL, docID)) |>
    req_url_query(type = type_param,
                  `Subscription-Key` = API_KEY) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_timeout(120)
  
  # tryCatch でリクエスト例外を吸収(接続切断、タイムアウト)
  resp <- tryCatch(req_perform(req),
                   error = function(e) {
                     return(list(error = conditionMessage(e)))
                   })
  
  if (!is.null(resp$error)) {
    return(list(status = "error", kind = kind, size_bytes = 0L, msg = resp$error))
  }
  
  if (resp_status(resp) != 200) {
    return(list(status = "http_error", kind = kind, size_bytes = 0L,
                msg = paste0("HTTP ", resp_status(resp))))
  }
  
  body <- resp_body_raw(resp)
  if (length(body) < 100) {
    # 空または極小レスポンス(EDINETがエラー時に短いHTMLを返すことがある)
    return(list(status = "empty_response", kind = kind,
                size_bytes = length(body), msg = "response too small"))
  }
  
  # zip ファイルとして保存(バイナリそのまま書き出し)
  writeBin(body, out_path)
  list(status = "ok", kind = kind,
       size_bytes = as.integer(file.size(out_path)), msg = "")
}

# progress.csv に1行追記。append=TRUE はファイル存在時のみ(初回はヘッダ付き)。
append_progress <- function(row) {
  exists_before <- file.exists(PROGRESS_FILE)
  write_csv(row, PROGRESS_FILE, append = exists_before)
}

# メインループ -----------------------------------------------------
# todo を順次ダウンロードし、毎回 progress.csv に1行記録する。
t0 <- Sys.time()
n_total <- nrow(todo)
n_ok <- 0L; n_err <- 0L; n_skip <- 0L

for (i in seq_len(n_total)) {
  row <- todo[i, ]
  res <- fetch_one_doc(row$docID,
                       prefer_csv = TRUE,
                       csvFlag = row$csvFlag,
                       xbrlFlag = row$xbrlFlag)
  
  # 1行分の進捗ログを組み立て(後の集計・デバッグに必須の情報)
  log_row <- tibble(
    docID = row$docID,
    edinetCode = row$edinetCode,
    periodEnd = row$periodEnd,
    kind = res$kind,
    status = res$status,
    size_bytes = res$size_bytes,
    msg = res$msg,
    ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  append_progress(log_row)
  
  if (res$status == "ok") n_ok <- n_ok + 1L
  else if (res$status == "skip_no_data") n_skip <- n_skip + 1L
  else n_err <- n_err + 1L
  
  # 50件ごとの進捗表示(進捗・経過秒・残り ETA分)
  # ETA = (経過秒 / 進捗i) × (残りタスク) / 60
  if (i %% 50 == 0 || i == n_total) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    eta_min <- (elapsed / i) * (n_total - i) / 60
    cat(sprintf("[%5d/%5d] ok=%d skip=%d err=%d 経過%.0fs ETA≒%.1f分\n",
                i, n_total, n_ok, n_skip, n_err, elapsed, eta_min))
  }
  
  Sys.sleep(2)
}
