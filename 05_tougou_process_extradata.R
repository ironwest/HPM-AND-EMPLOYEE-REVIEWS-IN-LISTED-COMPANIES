# EDINETのCSVデータから必要な変数を抽出する処理をここで行う。

elem_map <- tribble(
  ~field,                
  ~element_id,
  ~context_id,
  # 単体 PL:当期純利益(3 タグ候補、企業/年度ごとに 1 つだけ存在)──
  "当期純利益_単体",
  "jppfs_cor:ProfitLoss",
  "CurrentYearDuration_NonConsolidatedMember",
  
  # 単体 BS:総資産・関係会社株式 ───────────────────────────────
  "総資産_単体",
  "jppfs_cor:Assets",
  "CurrentYearInstant_NonConsolidatedMember",
  
  "関係会社株式_単体",
  "jppfs_cor:StocksOfSubsidiariesAndAffiliates",
  "CurrentYearInstant_NonConsolidatedMember",
  
  # 従業員数:連結(bare ctx)と単体(NonConsolidated ctx)を別 field に ──
  "従業員数_単体or連結",    
  "jpcrp_cor:NumberOfEmployees",
  "CurrentYearInstant",
  
  "従業員数_単体",
  "jpcrp_cor:NumberOfEmployees",
  "CurrentYearInstant_NonConsolidatedMember",
  
  # 提出会社の従業員情報──
  "平均年齢",
  "jpcrp_cor:AverageAgeYearsInformationAboutReportingCompanyInformationAboutEmployees",
  "CurrentYearInstant_NonConsolidatedMember",

  "平均勤続年数",
  "jpcrp_cor:AverageLengthOfServiceYearsInformationAboutReportingCompanyInformationAboutEmployees",
  "CurrentYearInstant_NonConsolidatedMember",
  
  "平均年間給与",
  "jpcrp_cor:AverageAnnualSalaryInformationAboutReportingCompanyInformationAboutEmployees",
  "CurrentYearInstant_NonConsolidatedMember",
  
  "companyName",
  "jpdei_cor:FilerNameInJapaneseDEI",
  "FilingDateInstant",
  
  "edinetCode",
  "jpdei_cor:EDINETCodeDEI",
  "FilingDateInstant",
  
  "endofperiod",
  "jpdei_cor:CurrentPeriodEndDateDEI",
  "FilingDateInstant",
  
  "typeofperiod",
  "jpdei_cor:TypeOfCurrentPeriodDEI",
  "FilingDateInstant"
)

zip_paths1 <- list.files("data/edinet/extradocs", full.names = TRUE) #一部上場廃止で自動でおちなかったものは手動でDL
zip_paths2 <- list.files("data/edinet/docs", full.names = TRUE)
zip_paths <- c(zip_paths1,zip_paths2)

extract_onefile <- function(zip_path){
  if (!file.exists(zip_path)) return(NULL)
  files <- unzip(zip_path, list = TRUE)$Name
  csv_name <- files[str_detect(files, "jpcrp.*-asr-\\d+_.*\\.csv$")][1]
  if (is.na(csv_name)) return(NULL)
  
  tmpdir <- tempfile(); dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  unzip(zip_path, files = csv_name, exdir = tmpdir)
  
  d <- tryCatch(
    read_tsv(file.path(tmpdir, csv_name),
             locale = locale(encoding = "UTF-16LE"),
             col_types = cols(.default = "c"),
             show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  if (is.null(d) || nrow(d) == 0) return(NULL)
  
  #d2 |> filter(str_detect(item_name, "EDINET")) |> View()
  
  d2<- d |>
    rename(element_id        = 要素ID,
           item_name         = 項目名,
           relative_period   = `相対年度`,
           consolidated_flag = `連結・個別`,
           context_id        = `コンテキストID`,
           value_str         = 値)
  
  d3 <- d2 |> inner_join(elem_map, by=c("element_id","context_id"))
  
  targetcodes <- c(
    "jpdei_cor:FilerNameInJapaneseDEI",
    "jpdei_cor:EDINETCodeDEI",
    "jpdei_cor:CurrentPeriodEndDateDEI",
    "jpdei_cor:TypeOfCurrentPeriodDEI"
  )
  
  companyName   <- d3 |> filter(element_id %in% targetcodes[1]) |> pull(value_str)
  edinetCode    <- d3 |> filter(element_id %in% targetcodes[2]) |> pull(value_str)
  endofperiod   <- d3 |> filter(element_id %in% targetcodes[3]) |> pull(value_str)
  typeofperiod  <- d3 |> filter(element_id %in% targetcodes[4]) |> pull(value_str)
  
  d3 |> 
    filter(!element_id %in% targetcodes) |> 
    mutate(edinetCode = edinetCode, 
           companyName = companyName, 
           endOfPeriod = endofperiod,
           typeOfPeriod = typeofperiod,
           .before = 1) |> 
    select(edinetCode, companyName, endOfPeriod, typeOfPeriod, `ユニットID`, field, value_str) |> 
    distinct() |> 
    mutate(zippath = zip_path)
}


#res <- map_dfr(zip_paths, extract_onefile, .progress=TRUE)
parallelly::availableCores()
library(furrr)
plan(multisession)
rrr <- furrr::future_map_dfr(zip_paths, extract_onefile, .progress=TRUE)

write_rds(rrr,"middledata/edinetlong.rds")

res <- read_rds("middledata/edinetlong.rds")

agecheck <- res |> 
  select(zippath, field, value_str) |> 
  pivot_wider(id_cols = zippath, names_from = field, values_from = value_str) |> 
  filter(is.na(`平均年齢`) & is.na(`平均年齢`)) |> pull(zippath)
  #filter(is.na(`従業員数_単体or連結`) & is.na(`従業員数_単体`))

zip_path <- agecheck[1] 
#詳細みたがない。
#
res |> 
  select(zippath, field, value_str) |> 
  pivot_wider(id_cols = zippath, names_from = field, values_from = value_str) |> 
  filter(is.na(`当期純利益_単体`))

res |> count(`ユニットID`, field) #ユニット違いは無さそう

res |> 
  select(edinetCode, companyName, field, value_str) |> 
  mutate(value_num = as.numeric(value_str)) |> 
  filter(!is.na(value_num) & is.na(value_num)) #変換失敗はNAだけ

res |> count(typeOfPeriod)
res |> filter(typeOfPeriod == "FY40")

res2 <- res |> 
  mutate(value_num = as.numeric(value_str)) |> 
  select(edinetCode, companyName, endOfPeriod, field, value = value_num) 

res2 |>
  distinct() |> 
  dplyr::summarise(n = dplyr::n(), .by = c(edinetCode, companyName, endOfPeriod, field)) |>
  dplyr::filter(n > 1L) 

wide <- res2 |> 
  distinct() |> 
  pivot_wider(id_cols = c("edinetCode","companyName",endOfPeriod), names_from = field, values_from = value)

#離職率を簡易計算してみる
# risyoku <- wide |> 
#   select(edinetCode, endOfPeriod, matches("従業員数")) |> 
#   arrange(edinetCode,endOfPeriod) |> 
#   group_by(edinetCode) |> 
#   slice_tail(n = 4) |> 
#   nest()
#   
# risyoku2 <- risyoku |> 
#   mutate(num = map_dbl(data,nrow)) |> 
#   filter(num == 2) |> 
#   mutate(data2 = map(data, ~{
#     . |> 
#       arrange(endOfPeriod) |> 
#       mutate(order = 1:n())
#   })) |> 
#   select(edinetCode, data2) |> 
#   unnest(data2) |> 
#   select(edinetCode,order,endOfPeriod, num_conornon = `従業員数_単体or連結`, num_non = `従業員数_単体`)
# 
# risyoku3 <- risyoku2 |> 
#   mutate(numthis = case_when(
#     !is.na(num_non) ~ num_non,
#      is.na(num_non) ~ num_conornon
#   ))
# 
# risyokudat <- risyoku3 |> 
#   select(edinetCode, order, numthis) |> 
#   pivot_wider(id_cols = edinetCode, names_from = order, values_from = numthis, names_prefix = "ord") |> 
#   mutate(
#     risyoku_n3yr = ord4 - ord1,
#     risyoku_n2yr = ord4 - ord2,
#     risyoku_n1yr = ord4 - ord3,
#   
#     risyoku_p3yr = ord4/ord1,
#     risyoku_p2yr = ord4/ord2,
#     risyoku_p1yr = ord4/ord3
#   ) |> 
#   ungroup() |> 
#   rename(empnum_pre3yr = ord1, empnum_pre2yr = ord2, empnum_pre1yr = ord3, empnum_thisyr = ord4)

wide2 <- wide |> 
  group_by(edinetCode) |>
  mutate(endOfPeriod = lubridate::ymd(endOfPeriod)) |> 
  arrange(endOfPeriod) |> 
  slice_tail(n=1) |> 
  left_join(risyokudat, by="edinetCode")

write_rds(wide2, "middledata/wideedinet.rds")


#zippaths1のデータ、04でEDINETと健康経営のコード統合の歳に目視マッチングが必要（上場廃止のため、EDINETの目録に掲載されていない）ため、ここで作業しておく。
zip_paths1 <- list.files("data/edinet/extradocs", full.names = TRUE)
tempres <- map_dfr(zip_paths1, extract_onefile, .progress=TRUE)

tempres |> distinct(edinetCode, companyName) |> clipr::write_clip()
  
