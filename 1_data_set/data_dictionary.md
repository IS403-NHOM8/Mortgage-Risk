# IS403 — Data Dictionary & Handover Guide

**Dự án:** Mortgage-Risk — Dự đoán xác suất vỡ nợ khoản vay thế chấp  
**Nguồn phương pháp:** [SpaceTimeML](https://github.com/pkuendig/SpaceTimeML) — [script tạo dữ liệu của tác giả](https://github.com/pkuendig/SpaceTimeML/blob/main/1_data_set/generate_data_set.R)  
**Dataset:** Freddie Mac Single-Family Loan-Level Dataset, Annual Sample, Release 47 (theo bộ file nhóm đã audit)  
**Mức độ tái lập:** *Adapted empirical study* — không phải toàn bộ quần thể, lịch sử hay thiết lập gốc của bài báo.

> **Đọc trước khi huấn luyện:** Đơn vị dữ liệu là **loan-year**, không phải khoản vay duy nhất. `default` là nhãn, `loan_id` và `date_versioning` là định danh/thời gian, `original_loan_to_value_aux` không phải predictor. Các cột còn thiếu dữ liệu phải được xử lý theo **train-only preprocessing**; tuyệt đối không fit imputer/scaler trên train + validation + test cùng lúc.

## 1. Tài liệu này dùng để làm gì?

Tài liệu giải thích nguồn và phạm vi dữ liệu, ý nghĩa **toàn bộ 30 cột trong** `loan_year_master.csv`, cấu trúc hai RDS, cách tạo train/validation/test, các quyết định tiền xử lý, thống kê kiểm tra và hợp đồng dữ liệu cho các thành viên làm `2_hyperparameter_tuning`, `3_training_and_prediction`, `4_evaluation`, `5_external_validation`.

## 2. Phạm vi, nguồn, cấu trúc dữ liệu

| Hạng mục | Thiết lập hiện tại |
|---|---|
| Origination vintages **được đọc thực sự** | **2010–2015**, sáu Annual Sample ZIP; `sample_2016.zip` đang có trên máy nhưng **không được V4 đọc** |
| File gốc bên trong ZIP | `sample_orig_YYYY.txt` (31 trường), `sample_perf_YYYY.txt` (35 trường) |
| Performance cutoff | Giữ kỳ báo cáo `period < 201701`, tức đến **12/2016** |
| Ngày tạo snapshot | **01/01/2011 ... 01/01/2016** (6 snapshots) |
| Đối tượng chọn | `property_type = SF`, `amortization_type = FRM`, `loan_term = 360` tháng |
| Đơn vị một dòng | **Một khoản vay (`loan_id`) tại một ngày snapshot (`date_versioning`)** |
| Khóa duy nhất | Cặp `(loan_id, date_versioning)`; cùng `loan_id` có thể xuất hiện nhiều năm |
| Horizon nhãn | Một năm: `[date_versioning, date_versioning + 1 năm)` |
| Mục tiêu | Dự đoán lần đầu khoản vay có delinquency **>= 03** trong khoảng một năm tiếp theo |
| Quy ước thiếu | `NA` trong R, ô trống trong CSV; một số sentinel gốc được chuẩn hóa, riêng AUX bảo toàn LTV raw |

### 2.1. Nguồn file đầu vào

| Tệp | Nguồn/chức năng | Vị trí nhóm hiện dùng |
|---|---|---|
| `sample_2010.zip` ... `sample_2015.zip` | [Freddie Mac Loan-Level Dataset](https://www.freddiemac.com/research/datasets/sf-loanlevel-dataset), origination + performance | `1_data_set/data/raw/freddie_mac/` |
| `sample_2016.zip` | Đã tải nhưng **không cần** cho snapshot đúng ngày 01/01/2016 | Cùng thư mục raw |
| `ZIP_centroids.csv` | [SpaceTimeML — dữ liệu centroid](https://github.com/pkuendig/SpaceTimeML/tree/main/1_data_set/data), nối ZIP3 -> tọa độ | `1_data_set/data/raw/freddie_mac/` |
| `macro_data.csv` | [SpaceTimeML — dữ liệu macro](https://github.com/pkuendig/SpaceTimeML/tree/main/1_data_set/data), nối năm và bang | Như trên |
| `MORTGAGE30US.csv` | [FRED — 30-Year Fixed Rate Mortgage Average](https://fred.stlouisfed.org/series/MORTGAGE30US) | Như trên |

`MORTGAGE30US.csv` xuất từ FRED có thể có cột ngày `observation_date` hoặc `DATE`: bản V4 đã dùng chấp nhận hai tên và chuẩn hóa thành `DATE`. Không để bản ZIP raw hoặc thông tin bị hạn chế phân phối lên GitHub công khai trước khi kiểm tra điều khoản sử dụng của Freddie Mac.

### 2.2. Quy tắc chọn và tạo loan-year (theo script nhóm)

1. Đọc vintage 2010–2015, lọc SF + FRM + 360 tháng; lấy performance liên quan trước 01/2017.
2. Chuẩn hóa ZIP3, loại `000`/`001`, nối centroid thuộc vùng Hoa Kỳ lục địa; loại khoản vay không có centroid phù hợp.
3. Chỉ giữ khoản vay có bản ghi performance **chưa modified** (flag trống) tại loan age 0–3 tháng. `Y`/`P` không được xem là chưa modified.
4. Xác định `date_default`: kỳ đầu tiên với delinquency dạng số >= 3; `RA` và `XX` không được tự động quy thành `03` hay `00`. `RA` có audit riêng.
5. Xác định ngày bắt đầu khoản vay từ tháng thanh toán đầu tiên trừ một tháng; kết thúc bằng ngày sớm nhất giữa default/zero balance/maturity.
6. Tại 01/01 từng năm: chọn khoản vay active và thông tin performance cuối cùng **trước** ngày snapshot; loại thông tin có tuổi quá 6 tháng. Nếu không có record trước snapshot, dùng các giá trị origination theo logic fallback của V4.
7. Tạo predictor origination, hiện trạng khoản vay, tọa độ và macro năm trước; lấy mức lãi suất FRED cuối cùng **trước** snapshot.
8. Gắn nhãn `default` nếu `date_default` xảy ra trước 01/01 năm kế tiếp đối với khoản vay active tại snapshot. Ghi RDS tích lũy và master.

**Khác biệt có chủ đích so với GitHub gốc:** lấy cohort vintage 2010–2015 thay vì 1999–2022, snapshot/test đến 2016; thay schema raw sang Release 47; chưa làm mean/mode imputation toàn dữ liệu trong generator để tránh leakage. Do đó không so sánh trực tiếp số liệu benchmark với paper như thể là cùng mẫu.

## 3. File đầu ra và hợp đồng dữ liệu

Đường dẫn tương đối tính từ **repository root** `Mortgage-Risk/`:

| File | Nội dung | Ai dùng |
|---|---|---|
| `1_data_set/data/processed/freddie_mac/loan_year_master.csv` | **348.692 dòng × 30 cột**, đầy đủ snapshots 2011–2016; nguồn chung để chia tập | Cả sáu mô hình |
| `1_data_set/data/processed/freddie_mac/data_window_2015.rds` | **255.003 dòng**: tích lũy 2011–2015 | Tuning (train 2011–2014, validation 2015) |
| `1_data_set/data/processed/freddie_mac/data_window_2016.rds` | **348.692 dòng**: tích lũy 2011–2016 | Train cuối + test |
| `.../audit/data_quality_by_year.csv` | Tổng loan-year, số default, tỷ lệ default và số LTV thiếu theo snapshot | Báo cáo, kiểm toán |
| `.../audit/vintage_audit.csv` | Số khoản vay raw đủ tiêu chí ban đầu và số match performance theo vintage | Báo cáo, kiểm toán |

Trong danh sách thư mục đã gửi, `splits/train.csv`, `validation.csv`, `test.csv` **không còn được lưu**. Đây là lựa chọn hợp lệ nếu mỗi thành viên chia động bằng code chung theo `date_versioning`. V4 có `WRITE_SPLITS <- TRUE`, vì vậy **khi chạy lại generator** có thể tái tạo thư mục `splits/` và xuất thêm audit CSV vào `processed/freddie_mac/` (không tự động chuyển vào `audit/`).

**RDS format (tương thích về hình dạng, chưa bảo đảm tương thích trực tiếp với mọi script gốc):**

```r
r <- readRDS("1_data_set/data/processed/freddie_mac/data_window_2016.rds")
names(r)  # Y, X, ID, AUX
# r$Y: binary integer label; r$X: data.frame predictor, bao gồm date_versioning
# r$ID: loan ID theo đúng thứ tự hàng trong X/Y
# r$AUX$original_loan_to_value_raw: LTV cột 12 nguyên gốc, cùng chiều dài
```

`X` trong RDS có **27 cột**, trong đó `date_versioning` là trường thời gian để chia tập. Trong master có thêm `loan_id`, `default` và `original_loan_to_value_aux` (3 cột) = tổng **30 cột**. Không đưa bất kỳ trường định danh, nhãn, thời gian hoặc AUX vào vector predictor mặc định.

## 4. Từ điển đầy đủ các cột `loan_year_master.csv`

**Quy ước:** `numeric` = số thực, `integer` = số nguyên, `categorical` = biến phân loại, `date` = ngày ISO `YYYY-MM-DD`, `ID` = định danh. Kiểu dữ liệu trong CSV được suy ra sau khi đọc; RDS đã ép các biến categorical thành factor. Ký hiệu `[X]` nghĩa là cột thuộc bảng predictor `r$X`, không có nghĩa luôn được đưa vào tất cả mô hình mà không biến đổi.

### 4.1. Khóa, nhãn và thời điểm

| Cột | Kiểu | Ý nghĩa / cách tạo | Vai trò |
|---|---|---|---|
| `loan_id` | ID (character) | Mã khoản vay Freddie Mac (`loan_number`). Có thể lặp giữa các năm; duy nhất khi ghép với `date_versioning`. | **Không làm predictor** |
| `date_versioning` | date | Ngày snapshot 01/01/năm, từ 2011 đến 2016; trục thời gian chính để chia tập. | **Không đưa vào predictor mặc định**; dùng để split / time index riêng nếu mô hình yêu cầu |
| `default` | integer {0,1} | **1** nếu lần đầu đạt delinquency dạng số từ `03` trở lên trong kỳ `[snapshot, snapshot+1 năm)`; **0** nếu không có sự kiện được xác định trong kỳ theo dữ liệu/logic V4. | **Nhãn Y**, không dùng làm predictor |

> **Diễn giải nhãn:** `default = 0` có nghĩa *không ghi nhận default theo định nghĩa/nguồn dữ liệu đang dùng*, không bảo đảm khoản vay không gặp mọi hình thức distress. `RA` không tự động là default. Với dữ liệu chấm dứt tại 12/2016, cần thận trọng về missing/censoring và nhãn chưa được audit từng hồ sơ.

### 4.2. Các đặc trưng tại origination và không gian — 13 cột trong X

| Cột | Kiểu dự kiến | Định nghĩa / mã hóa | Đơn vị / lưu ý |
|---|---|---|---|
| `X_centroid` | numeric | Kinh độ centroid gắn với ZIP3; lấy từ `ZIP_centroids.csv`. | Độ kinh (tọa độ địa lý); âm đối với US |
| `Y_centroid` | numeric | Vĩ độ centroid gắn với ZIP3. | Độ vĩ |
| `credit_score` | numeric | Điểm tín dụng gốc. | Score; `9999` -> NA, kiểm tra khoảng 301–850 |
| `occupancy` | categorical | Loại hình cư trú: **P** = primary, **I** = investment, **S** = second home. | `9` -> NA; cần encode khi mô hình yêu cầu |
| `nr_units` | categorical | Số đơn vị nhà trên bất động sản: 1,2,3,4. | `99` -> NA; là factor trong RDS |
| `loan_purpose` | categorical | **P** = purchase; **C** = cash-out refinance; **N** = no-cash-out refinance; **R** = refinance chưa xác định loại. | `9` -> NA; không tự gộp R với N/C |
| `first_time_homebuyer` | categorical | Người mua nhà lần đầu: raw `Y` -> **"1"**, `N` -> **"0"**. | `9` -> NA; **factor**, không phải thang số liên tục |
| `MSA` | categorical | **"1"** nếu MSA gốc có giá trị, **"0"** nếu MSA gốc thiếu; script V4 chỉ ghi *có/không có MSA*, không lưu mã MSA đầy đủ. | Không diễn giải 1/0 là đô thị/nông thôn nếu chưa xác minh thêm |
| `insurance_percent` | numeric | Phần trăm mortgage insurance ở origination. | Điểm phần trăm; `999` -> NA |
| `original_dept_to_income` | numeric | Original debt-to-income ratio (tên `dept` là tên theo repo/script, về ý nghĩa là **debt**). | %; `999` -> NA; có missing đáng kể |
| `original_combined_loan_to_value` | numeric | CLTV ban đầu, có thể bao gồm dư nợ thế chấp thứ cấp. | %; `999` -> NA; Relief Refinance có thể vượt 200 |
| `original_upb` | numeric | Original unpaid principal balance khi phát hành. | USD |
| `number_of_borrowers` | categorical | Với vintage trước 2018Q2: **"1"** = một người vay, **"2"** = **nhiều hơn một** người vay. | `99` -> NA; không hiểu "2" là đúng hai người |

### 4.3. Đặc trưng biến động theo snapshot — 4 cột trong X

| Cột | Kiểu | Định nghĩa / công thức thực hiện trong V4 | Đơn vị / lưu ý |
|---|---|---|---|
| `current_loan_to_value` | numeric | `current_upb / original_value * 100`, với `original_value = original_upb / original_loan_to_value * 100`. | %; **không** cập nhật định giá nhà theo HPI; có thể NA nếu LTV gốc thiếu |
| `ir_spread` | numeric | `current_interest_rate` (bản ghi trước snapshot) **trừ** mức `MORTGAGE30US` cuối cùng trước snapshot. | Điểm phần trăm nếu hai nguồn cùng thang %; không dùng rate sau snapshot |
| `n_months` | numeric/integer | Số tháng tuổi khoản vay xấp xỉ theo `interval(date_loan_start, snapshot) %/% months(1) - 1`. | Tháng; là cách tính theo V4, không đồng nhất một cách máy móc với `loan_age` raw |
| `current_upb` | numeric | Dư nợ gốc chưa thanh toán gần snapshot nhất trước 01/01; fallback `original_upb` khi thiếu bản ghi. | USD |

**LTV quan trọng:** Cột `original_loan_to_value` ở **vị trí 12** trong Origination được dùng tính `original_value`; **không dùng CLTV ở vị trí 9 thay thế**. Nếu sửa imputation của LTV gốc thì downstream cần **tính lại** `current_loan_to_value` bằng `current_upb` và LTV gốc đã impute, tránh giữ nguyên LTV hiện thời tính từ NA.

### 4.4. Đặc trưng kinh tế vĩ mô — 9 cột trong X

Các biến đến từ `macro_data.csv`; script V4 join **năm trước snapshot** (`snapshot_year - 1`) với `property_state`. Tên biến giữ nguyên tên file nguồn.

| Cột | Kiểu | Ý nghĩa |
|---|---|---|
| `gdp_growth` | numeric | Tăng trưởng GDP |
| `ln_income_per_capita` | numeric | Log của thu nhập bình quân đầu người |
| `ln_expenditures_per_capita` | numeric | Log của chi tiêu bình quân đầu người |
| `unemployment_rate` | numeric | Tỷ lệ thất nghiệp |
| `hpi_growth` | numeric | Tăng trưởng chỉ số giá nhà (House Price Index) |
| `gdp_construction_growth` | numeric | Tăng trưởng GDP ngành xây dựng |
| `gross_operating_surplus_growth` | numeric | Tăng trưởng thặng dư hoạt động gộp |
| `inflation_rate` | numeric | Tỷ lệ lạm phát |
| `DJIA_growth` | numeric | Tăng trưởng Dow Jones Industrial Average |

**Đơn vị, cách log và cách tính growth thực tế** cần đối chiếu với tài liệu xây dựng `macro_data.csv` của repo gốc; V4 **đọc và ghép nguyên giá trị**, không tự tính lại tốc độ tăng trưởng, không tự chuyển tỷ lệ thập phân <-> phần trăm.

### 4.5. Cột AUX — 1 cột ngoài X

| Cột master | Đối tượng RDS | Ý nghĩa | Vai trò |
|---|---|---|---|
| `original_loan_to_value_aux` | `AUX$original_loan_to_value_raw` | Original LTV **nguyên thủy từ cột 12** của Origination, trước chuyển sentinel `999` -> NA. | **Không phải predictor**. Dùng khi cần fit imputer LTV trên training loans, áp dụng và tính lại current LTV. |

Trong raw/AUX, **999 là sentinel**, không phải 999% LTV hợp lệ. Nếu downstream xử lý AUX, phải đổi `999` -> NA **trước** fit imputer.

## 5. Các mã/giá trị đặc biệt của raw đã xử lý

| Raw field | Quy tắc V4 / Ý nghĩa |
|---|---|
| `current_loan_delinquency_status` | `00`–`99` chuyển numeric; lần đầu >=`03` là thời điểm default; `RA` và `XX` giữ riêng trong audit, không chuyển thành default |
| `modification_flag` | Trống = chưa modified; `Y` hoặc `P` là đã modified; V4 yêu cầu bản ghi chưa modified trong loan age 0–3 |
| `postal_code` | ZIP3 là chuỗi 3 chữ số; giữ số 0 đầu; `000` unknown và `001` loại theo logic không gian |
| `credit_score` | `9999` = NA |
| `insurance_percent`, `original_dept_to_income`, `original_combined_loan_to_value`, `original_loan_to_value` | `999` = NA (trong X); raw original LTV vẫn lưu trong AUX |
| `nr_units`, `number_of_borrowers` | `99` = NA |
| `occupancy`, `loan_purpose`, `first_time_homebuyer`, `channel` | `9` = NA |
| `first_time_homebuyer` | `Y/N` -> `1/0` như biến phân loại |
| `MSA` | Chuyển thành chỉ báo có/không có mã MSA gốc |

**Numeric domain guard:** V4 chấp nhận LTV và CLTV trong miền rộng 1–998 (999 missing), đồng thời cảnh báo khi vượt ngưỡng non-Relief thông thường (LTV 105, CLTV 200). **Không mặc định mọi quan sát vượt ngưỡng là Relief Refinance** — cần kiểm tra chỉ báo khoản vay nếu muốn khẳng định.

## 6. Phân chia Train / Validation / Test — giao thức bắt buộc

**Không random-split các dòng loan-year.** Dùng năm của `date_versioning`:

| Giai đoạn | Năm | Số dòng | Số `default=1` | Vai trò |
|---|---|---:|---:|---|
| Train khi tuning | **2011–2014** | **173.306** | **1.075** | Fit preprocessing và model thử nghiệm |
| Validation | **2015** | **81.697** | **413** | Chọn siêu tham số/cấu hình |
| Train cuối | **2011–2015** | **255.003** | **1.488** | Fit lại preprocessing + model sau khi chọn tham số |
| Test | **2016** | **93.689** | **411** | Đánh giá ngoài mẫu cuối cùng |

Test 2016 phải được giữ ngoài việc chọn mô hình, feature, threshold, hyperparameter và preprocessing. Các mô hình dùng **cùng hàng train, val, test**; riêng thuật toán có thể mã hóa/scaling khác nhau.

**Code chia động từ master (dùng tại repo root):**

```r
library(data.table)
path <- "1_data_set/data/processed/freddie_mac/loan_year_master.csv"
df <- fread(path)
yr <- as.integer(substr(df$date_versioning, 1L, 4L))
train <- df[yr %in% 2011:2014]
validation <- df[yr == 2015L]
train_final <- df[yr %in% 2011:2015]
test <- df[yr == 2016L]

stopifnot(nrow(train) == 173306L, nrow(validation) == 81697L,
          nrow(train_final) == 255003L, nrow(test) == 93689L)
```

**Feature contract:** Không dùng `loan_id`, `date_versioning`, `default`, `original_loan_to_value_aux` như predictor thông thường. `date_versioning` chỉ đi vào mô hình nếu mô hình được thiết kế rõ cho time-index effect và tránh thông tin tương lai.

**Preprocessing chống leakage:**

- **Trong tuning:** fit imputer/encoder/scaler trên `train` 2011–2014; chỉ **transform** `validation` 2015. Không fit trên validation/test.
- **Trong final training:** fit lại trên `train_final` 2011–2015; chỉ transform `test` 2016.
- Biến tĩnh từ origination (score, occupancy, LTV, DTI...) nên xem xét fit mean/mode trên **loan ID duy nhất có mặt trong train**, rồi map sang các dòng loan-year. Đây là lựa chọn của nhóm nhằm gần với grain origination và giảm lệch trọng số do khoản vay tồn tại lâu.
- Biến thời gian (current UPB, interest spread...) có thể fit trên quan sát loan-year của tập train theo đúng định nghĩa pipeline. Mọi biến đổi phải được lưu để tái sử dụng khi test.
- Với `original_loan_to_value_aux`, chuẩn hóa `999` -> NA, fit imputer trên training loans, sau đó tính lại `current_loan_to_value` cho từng tập nếu cần; **không đưa AUX vào X**.
- `number_of_borrowers` là **categorical**, với `2` = nhiều hơn một người vay; `occupancy`, `loan_purpose`, `nr_units`, `first_time_homebuyer`, `MSA` cũng là categorical. Mô hình không hỗ trợ factor/character cần encode theo train.
- **Cùng tập quan sát**, nhưng cách xử lý missing, biến giả, scaling hoặc GPBoost spatial/random effects có thể khác tùy mô hình. Không dùng validation/test để chọn thủ công các phương án có lợi cho test.
- Không sử dụng thông tin performance từ hoặc sau snapshot để tạo **predictor** tại snapshot. Nhãn 12 tháng tương lai chỉ xuất hiện trong `default`.

## 7. Quy mô dữ liệu và kết quả audit đã xác nhận

Nguồn: tám file audit được sinh từ bản V4 đã chạy của nhóm; dữ liệu này là số liệu thực nghiệm nhóm, **không phải** số liệu paper gốc.

| Snapshot | Loan-year | Defaults | Default rate | Missing current LTV |
|---|---:|---:|---:|---:|
| 2011 | 20.397 | 106 | 0,520% | 0 |
| 2012 | 38.200 | 285 | 0,746% | 0 |
| 2013 | 50.413 | 329 | 0,653% | 1 |
| 2014 | 64.296 | 355 | 0,552% | 1 |
| 2015 | 81.697 | 413 | 0,506% | 2 |
| 2016 | 93.689 | 411 | 0,439% | 2 |
| **Tổng** | **348.692** | **1.899** | **0,545%** | **6 loan-year** |

**Giải thích số dòng:** một vintage có 50.000 khoản vay raw *trước lọc*; mỗi khoản vay đủ điều kiện có thể xuất hiện qua nhiều snapshot, nên **348.692 là số loan-year, không phải 348.692 khoản vay khác nhau**. Không suy ra `uniqueN(loan_id)` từ số dòng; cần tính trực tiếp từ master nếu muốn báo cáo unique loans.

**Các kiểm tra thực tế:**

- `vintage_audit.csv`: mỗi năm 2010–2015 có 50.000 `perf_unique_loans`; sau điều kiện SF/FRM/360, số `eligible` lần lượt **23.459, 20.854, 20.995, 21.827, 23.780, 23.289**. Đây là số ở **bước lọc sớm**, không phải số cuối cùng sau ZIP/early-performance/snapshot.
- `zip3_join_audit.csv`: 134.204 eligible origination sau lọc ban đầu; **133.358 matched**, 846 unmatched; eligible match rate **99,3696%** (chưa tính các bước lọc sau đó).
- `macro_missing_audit.csv`: không thấy thiếu trong 9 biến × 6 snapshot = 54 nhóm năm-biến đã audit.
- `numeric_domain_audit.csv`: không còn giá trị bị đánh dấu invalid theo **miền rộng** V4; các khoản LTV/CLTV vượt miền non-Relief vẫn được theo dõi riêng, **chưa tự xác minh loại refinance**.
- `borrower_domain_audit.csv`: các vintage đã audit dùng mã borrower 1/2; đối với 2010–2015, 2 = *more than one borrower*.
- `ra_loan_audit.csv`: 283 khoản có RA; 1 khoản bị flag không có mã delinquency >=03 **trước RA**; nên giữ audit để truy vết (đã phát hiện trường hợp `F12Q20153989`).
- Đã chạy kiểm tra `stopifnot` trên máy nhóm: RDS 2015 **255.003** dòng; RDS 2016 **348.692** dòng; `length(Y) = nrow(X) = length(ID) = length(AUX LTV)`; không trùng `(loan_id,date_versioning)` trong master; ID và nhãn giữa master và RDS 2016 khớp **đúng thứ tự**.

**Phần chưa được xác nhận chỉ từ audit:** khớp từng predictor giữa CSV và hai RDS, tổng unique loans, lỗi feature leakage hiếm gặp, tính đại diện dân số so với paper, và tương thích trực tiếp với code GPBoost của tác giả (vì RDS hiện còn NA).

## 8. Hướng dẫn cho từng phần tiếp theo

| Thư mục | Cần lấy gì | Deliverable / lưu ý |
|---|---|---|
| `2_hyperparameter_tuning/` | Master hoặc RDS 2015; lọc `2011–2014` vs `2015` | Chọn siêu tham số **chỉ dựa validation 2015**; lưu params, seed, preprocessing, metrics |
| `3_training_and_prediction/` | Master hoặc RDS 2016; lọc `2011–2015` vs `2016` | Huấn luyện cuối và ghi dự đoán xác suất test 2016 kèm `loan_id`, `date_versioning`, `default`, `pred_prob`, `model` |
| `4_evaluation/` | Kết quả dự đoán trên **cùng test 2016** | ROC-AUC, PR-AUC, Brier Score, confusion matrix/recall theo threshold được chọn từ validation, calibration/ECE nếu tính nhất quán |
| `5_external_validation/` | Chưa có Fannie Mae processed trong cây thư mục hiện tại | Chỉ làm khi có dữ liệu ngoài và mapping feature/label tương thích; không mặc định đã có dữ liệu cho bước này |

**Sáu mô hình nhóm đang hướng tới:** Logistic Regression (Independent Linear Hazard), GPBoost Spatial, GPBoost Spatio-temporal, SVC, Random Forest, LightGBM. GPBoost dùng cấu trúc không gian/thời gian có khác biệt; không chỉ chuyển `X` sang matrix bằng cách mù quáng.

### 8.1. Ví dụ đọc an toàn trong R

```r
library(data.table)
root <- "1_data_set/data/processed/freddie_mac"
master <- fread(file.path(root, "loan_year_master.csv"))
print(dim(master))
print(master[, .(rows=.N, defaults=sum(default)),
             by=.(year=as.integer(substr(date_versioning, 1, 4)))])

r15 <- readRDS(file.path(root, "data_window_2015.rds"))
r16 <- readRDS(file.path(root, "data_window_2016.rds"))
stopifnot(nrow(r15$X) == 255003L, nrow(r16$X) == nrow(master),
          identical(as.character(r16$ID), as.character(master$loan_id)),
          identical(as.integer(r16$Y), as.integer(master$default)))
```

**Đường dẫn repo root:** Mở `Mortgage-Risk.Rproj` trong RStudio và kiểm tra `getwd()` là thư mục root. Nếu chạy `source("1_data_set/generate_data_set.R")`, script V4 hiện chứa `ROOT <- "C:/Users/24521/Downloads/Mortgage-Risk/1_data_set"`; **trên máy đồng đội phải sửa ROOT** hoặc refactor về đường dẫn project-relative trước khi chạy lại generator. Nếu chỉ **đọc processed** thì dùng các đường dẫn tương đối trên, không cần chạy generator.

## 9. Bàn giao GitHub — tối thiểu cần có

**Bắt buộc để thành viên khác chạy tiếp từ processed:**

1. `1_data_set/data/processed/freddie_mac/loan_year_master.csv` **hoặc** cả hai `data_window_2015.rds`, `data_window_2016.rds` tùy pipeline, nhưng **nên giao cả ba** để thuận tiện so sánh với repo và dùng chung.
2. `1_data_set/data_dictionary.md` (file này) và một README ngắn ghi đường dẫn/điều kiện sử dụng.
3. `1_data_set/generate_data_set.R` **phải là bản đã tạo dữ liệu hiện tại**, bao gồm sửa cột FRED, LTV/CLTV và `OUT`. Nếu không chắc, đối chiếu file local đang chạy với file commit trước khi push.
4. `audit/data_quality_by_year.csv`, `audit/vintage_audit.csv` để truy vết; lưu thêm các audit khác nếu tiện.
5. Script split/preprocess thống nhất hoặc quy ước split phía trên. **Chỉ có master/RDS là chưa đủ cho mô hình chạy trực tiếp nếu thiếu các bước xử lý NA và category.**

**Cần làm trước khi công bố repo:**

- Kiểm tra `.gitignore`: không vô tình bỏ qua 3 file processed nếu định commit chúng; đồng thời không commit ZIP raw khi chưa kiểm tra giấy phép/điều khoản Freddie Mac. File CSV master khoảng 59 MB trong máy, lớn cho Git thông thường nhưng dưới giới hạn một file 100 MB của GitHub; nên cân nhắc Git LFS hoặc cách chia sẻ nội bộ nếu chính sách dữ liệu yêu cầu. Không push dữ liệu bị hạn chế phân phối chỉ vì file có kích thước hợp lệ.
- Đừng để `1_data_set/data/processed/fannie_mae/` hoặc `5_external_validation/` trống bị hiểu nhầm là bước đã hoàn tất.
- Lưu `sessionInfo()`/phiên bản R và các package vào README, và chốt một seed/tuning protocol cho cả nhóm.
- Nếu cần tái tạo chính xác: lưu phiên bản generator, nguồn ZIP/auxiliary và thời điểm tải; kiểm tra checksum khi có thể.


---

**Nguồn tham khảo chính:** [Repo SpaceTimeML](https://github.com/pkuendig/SpaceTimeML), [generate_data_set.R của tác giả](https://github.com/pkuendig/SpaceTimeML/blob/main/1_data_set/generate_data_set.R), [Freddie Mac Single-Family Loan-Level Dataset](https://www.freddiemac.com/research/datasets/sf-loanlevel-dataset), [FRED MORTGAGE30US](https://fred.stlouisfed.org/series/MORTGAGE30US). Mọi số liệu 2011–2016 trong tài liệu này lấy từ audit và kết quả kiểm tra do nhóm chạy với generator V4.
