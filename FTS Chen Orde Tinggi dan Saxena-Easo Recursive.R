#IMPORT DATA
Data <- read_excel("SKRIPSI KAKAK/FILE SKRIPSI SEMHAS/DepositoBerjangka.xlsx")
View(Data)

#4.1 ANALISIS DESKRIPTIF DATA
Datats = as.ts(Data) 
ts.plot(Datats) 

#4.2 PENGUJIAN KARAKTERISTIK DATA
#1. Uji Autokorelasi
#Plot ACF
acf(Data$Deposito_Berjangka, main = "Plot ACF Data Deposito Berjangka") 
acf_result <- acf(Data$Deposito_Berjangka, plot = FALSE)
acf_result$acf

#2. Uji Stasioneritas
#Uji Stasioneritas Terhadap Varians 
lambda_opt <- BoxCox.lambda(Data$Deposito_Berjangka) 
lambda_opt

#Uji Stasioneritas Terhadap Mean
adftest <- adf.test(Data$Deposito_Berjangka)
adf <- ur.df(Data$Deposito_Berjangka,
             type = "drift",
             lags = 4)
summary(adf)

#3. Uji Non-Linieritas
#Uji BDS
bds.test(Data$Deposito_Berjangka, m =5)

#4.3 PEMBAGIAN DATA TRAINING DAN TESTING
Data$t <- 1:nrow(Data)
#Data training
train <- Data[1:84, ]
# Data testing
test <- Data[85:108, ]

#4.4 PERAMALAN METODE FTS CHEN
#4.4.1 PEMBENTUKAN MODEL FTS CHEN ORDE 1 MENGGUNAKAN DATA TRAINING
#1. Pembentukan Himpunan Semesta
Dmin <- min(train$Deposito_Berjangka)
Dmin
Dmax <- max(train$Deposito_Berjangka)
Dmax
D1 <- 60166.96211
D2 <- 5187.48977999389
U <- c(Dmin - D1, Dmax + D2)
U

#2. Pembentukan Interval
#a. Menentukan Banyak Interval
N <- length(train$Deposito_Berjangka)
N
n <- 1 + 3.322*log10(N)
n
n <- round(n)
n

#b. Menghitung Panjang Interval
l <- ((Dmax+D2)-(Dmin-D1))/n
l
#c. Interval yang Terbentuk
bawah <- U[1] + (0:(n-1))*l
atas  <- U[1] + (1:n)*l

#Interval terakhir tepat sama dengan batas atas himpunan semesta
atas[n] <- U[2]

#d. Menghitung Nilai Tengah Interval
nilai_tengah <- (bawah + atas)/2

#Tabel Interval yang Terbentuk dan Nilai Tengah
tabel_interval <- data.frame(
  Interval = paste0("u",1:n),
  Batas_Bawah = bawah,
  Batas_Atas = atas,
  Nilai_Tengah = nilai_tengah
)

tabel_interval

#3. Melakukan Fuzzifikasi
#Mendefinisikan Fungsi Fuzzifikasi
fuzzifikasi <- function(x, bawah, atas){
  n <- length(bawah)
  for(i in 1:n){
    #Interval 1 sampai n-1
    if(i < n){
      if(x >= bawah[i] & x < atas[i]){
        return(paste0("A",i))
      }
    }
    #Interval terakhir
    if(i == n){
      if(x >= bawah[i] & x <= atas[i]){
        return(paste0("A",i))
      }
    }
  }
}

#Fuzzifikasi Data Training
fuzzy_train <- sapply(
  train$Deposito_Berjangka,
  fuzzifikasi,
  bawah = bawah,
  atas = atas
)

fuzzy_train

#Tabel Fuzzifikasi Data Training
tabel_fuzzifikasi <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Deposito_Berjangka = train$Deposito_Berjangka,
  Fuzzy_set = fuzzy_train
)

View(tabel_fuzzifikasi)
tabel_fuzzifikasi

#4. Membentuk FLR dan FLRG
#A. FLR
FLR_orde1 <- rep("-", length(fuzzy_train))

for(i in 2:length(fuzzy_train)){
  
  FLR_orde1[i] <- paste(
    fuzzy_train[i-1],
    "→",
    fuzzy_train[i]
  )
}

tabel_FLR_orde1 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Fuzzifikasi = fuzzy_train,
  FLR = FLR_orde1
)

#B. FLRG
FLRG_data_orde1 <- data.frame(
  Current_State = fuzzy_train[1:(length(fuzzy_train)-1)],
  Next_State = fuzzy_train[2:length(fuzzy_train)]
)

library(dplyr)

FLRG_orde1 <- FLRG_data_orde1 %>%
  group_by(Current_State) %>%
  summarise(
    Next_State = paste(
      sort(unique(Next_State)),
      collapse = ", "
    ),
    .groups = "drop"
  )

FLRG_orde1

#5. Defuzzifikasi Nilai Ramalan

defuzzifikasi <- function(Next_State, nilai_tengah){
  state <- unlist(strsplit(Next_State, ","))
  state <- trimws(state)
  indeks <- as.numeric(
    gsub("A", "", state)
  )
  mean(nilai_tengah[indeks])
}

tabel_defuzzifikasi_orde1 <- FLRG_orde1

tabel_defuzzifikasi_orde1$Nilai_Ramalan <- sapply(
  FLRG_orde1$Next_State,
  defuzzifikasi,
  nilai_tengah = nilai_tengah
)

tabel_defuzzifikasi_orde1

#6. Peramalan Data Training

ramalan_training_orde1 <- rep(
  NA,
  length(fuzzy_train)
)

for(i in 2:length(fuzzy_train)){
  
  state_sebelumnya <- fuzzy_train[i-1]
  
  ramalan_training_orde1[i] <-
    tabel_defuzzifikasi_orde1$Nilai_Ramalan[
      FLRG_orde1$Current_State ==
        state_sebelumnya
    ]
}

fuzzy_sebelumnya_orde1 <- c(
  "-",
  fuzzy_train[1:(length(fuzzy_train)-1)]
)

tabel_peramalan_training_orde1 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Fuzzifikasi_Sebelumnya = fuzzy_sebelumnya_orde1,
  Hasil_Peramalan = ramalan_training_orde1
)

tabel_peramalan_training_orde1

#4.4.2 PERAMALAN ONE-STEP AHEAD FTS CHEN ORDE 1
#Fuzzifikasi Data Testing
fuzzy_test_orde1 <- sapply(
  test$Deposito_Berjangka,
  fuzzifikasi,
  bawah = bawah,
  atas = atas
)

tabel_fuzzifikasi_testing_orde1 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  Data_Aktual = test$Deposito_Berjangka,
  Fuzzifikasi = fuzzy_test_orde1
)

#One-Step Ahead
fuzzy_sebelumnya_test_orde1 <- c(
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-1)]
)

ramalan_testing_orde1 <- numeric(
  length(fuzzy_sebelumnya_test_orde1)
)

for(i in 1:length(fuzzy_sebelumnya_test_orde1)){
  
  state <- fuzzy_sebelumnya_test_orde1[i]
  
  ramalan_testing_orde1[i] <-
    tabel_defuzzifikasi_orde1$Nilai_Ramalan[
      FLRG_orde1$Current_State == state
    ]
}

tabel_onestep_orde1 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  Data_Aktual = test$Deposito_Berjangka,
  Fuzzifikasi_Sebelumnya = fuzzy_sebelumnya_test_orde1,
  Hasil_Peramalan = ramalan_testing_orde1
)

tabel_onestep_orde1

#4.4.3 PERAMALAN RECURSIVE MULTI-STEP AHEAD FTS CHEN ORDE 1

state_awal_orde1 <- fuzzy_train[length(fuzzy_train)]

h_orde1 <- nrow(test)

recursive_state_orde1 <- character(h_orde1)
recursive_forecast_orde1 <- numeric(h_orde1)
fuzzy_prediksi_orde1 <- character(h_orde1)

current_state_orde1 <- state_awal_orde1

for(i in 1:h_orde1){
  
  recursive_state_orde1[i] <- current_state_orde1
  
  recursive_forecast_orde1[i] <-
    tabel_defuzzifikasi_orde1$Nilai_Ramalan[
      FLRG_orde1$Current_State ==
        current_state_orde1
    ]
  
  fuzzy_prediksi_orde1[i] <- fuzzifikasi(
    recursive_forecast_orde1[i],
    bawah,
    atas
  )
  
  current_state_orde1 <- fuzzy_prediksi_orde1[i]
  
}

tabel_recursive_orde1 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  Data_Aktual = test$Deposito_Berjangka,
  Fuzzifikasi_Sebelumnya = recursive_state_orde1,
  Hasil_Peramalan = recursive_forecast_orde1,
  Fuzzifikasi_Prediksi = fuzzy_prediksi_orde1
)

tabel_recursive_orde1

#4.4.4 MENGHITUNG UKURAN KETEPATAN PERAMALAN FTS CHEN ORDE 1
#1. One-Step Ahead

aktual_orde1 <- test$Deposito_Berjangka
prediksi_orde1 <- ramalan_testing_orde1

SE_orde1 <- (aktual_orde1 - prediksi_orde1)^2
AE_orde1 <- abs(aktual_orde1 - prediksi_orde1)
APE_orde1 <- abs((aktual_orde1 - prediksi_orde1)/aktual_orde1)*100

akurasi_onestep_orde1 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_orde1,
  Ft = prediksi_orde1,
  SE = SE_orde1,
  APE = APE_orde1,
  AE = AE_orde1
)

RMSE_onestep_orde1 <- sqrt(mean(SE_orde1))
MAPE_onestep_orde1 <- mean(APE_orde1)
MAE_onestep_orde1 <- mean(AE_orde1)
RMSE_onestep_orde1
MAPE_onestep_orde1
MAE_onestep_orde1

#2. Recursive Multi-Step Ahead

aktual_r_orde1 <- test$Deposito_Berjangka
prediksi_r_orde1 <- recursive_forecast_orde1

SE_r_orde1 <- (aktual_r_orde1 - prediksi_r_orde1)^2
AE_r_orde1 <- abs(aktual_r_orde1 - prediksi_r_orde1)
APE_r_orde1 <- abs((aktual_r_orde1 - prediksi_r_orde1)/aktual_r_orde1)*100

akurasi_recursive_orde1 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_r_orde1,
  Ft = prediksi_r_orde1,
  SE = SE_r_orde1,
  APE = APE_r_orde1,
  AE = AE_r_orde1
)

RMSE_recursive_orde1 <- sqrt(mean(SE_r_orde1))
MAPE_recursive_orde1 <- mean(APE_r_orde1)
MAE_recursive_orde1 <- mean(AE_r_orde1)
RMSE_recursive_orde1
MAPE_recursive_orde1
MAE_recursive_orde1

#4.5 PERAMALAN METODE FTS CHEN ORDE 2
#4.5.1 PEMBENTUKAN MODEL FTS CHEN ORDE 2 MENGGUNAKAN DATA TRAINING
#1. Membentuk FLR dan FLRG
#A. FLR
FLR_orde2 <- rep("-", length(fuzzy_train))

for(i in 3:length(fuzzy_train)){
  
  FLR_orde2[i] <- paste(
    "(",
    fuzzy_train[i-2], ", ",
    fuzzy_train[i-1],
    ") → ",
    fuzzy_train[i],
    sep=""
  )
}

tabel_FLR_orde2 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Fuzzifikasi = fuzzy_train,
  FLR = FLR_orde2
)
tabel_FLR_orde2

#B. FLRG
FLRG_data_orde2 <- data.frame(
  State1 = fuzzy_train[1:(length(fuzzy_train)-2)],
  State2 = fuzzy_train[2:(length(fuzzy_train)-1)],
  Next_State = fuzzy_train[3:length(fuzzy_train)]
)

FLRG_orde2 <- FLRG_data_orde2 %>%
  group_by(State1, State2) %>%
  summarise(
    Next_State = paste(
      sort(unique(Next_State)),
      collapse = ", "
    ),
    .groups = "drop"
  )

FLRG_orde2

#5. Defuzzifikasi Nilai Ramalan

defuzzifikasi <- function(Next_State, nilai_tengah){
  state <- unlist(strsplit(Next_State, ","))
  state <- trimws(state)
  indeks <- as.numeric(
    gsub("A", "", state)
  )
  mean(nilai_tengah[indeks])
}

tabel_defuzzifikasi_orde2 <- FLRG_orde2

tabel_defuzzifikasi_orde2$Nilai_Ramalan <- sapply(
  FLRG_orde2$Next_State,
  defuzzifikasi,
  nilai_tengah = nilai_tengah
)

tabel_defuzzifikasi_orde2

#6. Peramalan Data Training

ramalan_training_orde2 <- rep(
  NA,
  length(fuzzy_train)
)

for(i in 3:length(fuzzy_train)){
  
  state1 <- fuzzy_train[i-2]
  state2 <- fuzzy_train[i-1]
  
  idx <- which(
    FLRG_orde2$State1 == state1 &
      FLRG_orde2$State2 == state2
  )
  
  if(length(idx)>0){
    
    ramalan_training_orde2[i] <-
      tabel_defuzzifikasi_orde2$Nilai_Ramalan[idx]
    
  }
  
}

tabel_peramalan_training_orde2 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Hasil_Peramalan = ramalan_training_orde2
)

tabel_peramalan_training_orde2

#4.5.2 PERAMALAN ONE-STEP AHEAD FTS CHEN ORDE 2
#One-Step Ahead
state1_test <- c(
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-2)]
)

state2_test <- c(
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-1)]
)

ramalan_testing_orde2 <- rep(
  NA,
  length(fuzzy_test_orde1)
)

for(i in 1:length(fuzzy_test_orde1)){
  
  idx <- which(
    FLRG_orde2$State1 == state1_test[i] &
      FLRG_orde2$State2 == state2_test[i]
  )
  
  if(length(idx)>0){
    
    ramalan_testing_orde2[i] <-
      tabel_defuzzifikasi_orde2$Nilai_Ramalan[idx]
    
  }else{
    
    s1 <- as.numeric(
      gsub("A","",state1_test[i])
    )
    
    s2 <- as.numeric(
      gsub("A","",state2_test[i])
    )
    
    ramalan_testing_orde2[i] <-
      (1*nilai_tengah[s1] +
         2*nilai_tengah[s2]) /
      (1+2)
  }
}

tabel_onestep_orde2 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = state1_test,
  State2 = state2_test,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = ramalan_testing_orde2
)

tabel_onestep_orde2

#4.5.3 PERAMALAN RECURSIVE MULTI-STEP AHEAD FTS CHEN ORDE 2

state1_awal_orde2 <- fuzzy_train[length(fuzzy_train)-1]
state2_awal_orde2 <- fuzzy_train[length(fuzzy_train)]

h_orde2 <- nrow(test)

recursive_state1_orde2 <- character(h_orde2)
recursive_state2_orde2 <- character(h_orde2)

recursive_forecast_orde2 <- numeric(h_orde2)

fuzzy_prediksi_orde2 <- character(h_orde2)

state1 <- state1_awal_orde2
state2 <- state2_awal_orde2

for(i in 1:h_orde2){
  
  recursive_state1_orde2[i] <- state1
  recursive_state2_orde2[i] <- state2
  
  idx <- which(
    FLRG_orde2$State1 == state1 &
      FLRG_orde2$State2 == state2
  )
  
  if(length(idx) > 0){
    
    recursive_forecast_orde2[i] <-
      tabel_defuzzifikasi_orde2$Nilai_Ramalan[idx]
    
  } else {
    
    
    s1 <- as.numeric(
      gsub("A","",state1)
    )
    
    s2 <- as.numeric(
      gsub("A","",state2)
    )
    
    recursive_forecast_orde2[i] <-
      (1*nilai_tengah[s1] +
         2*nilai_tengah[s2]) /
      (1+2)
    
  }
  
  fuzzy_prediksi_orde2[i] <- fuzzifikasi(
    recursive_forecast_orde2[i],
    bawah,
    atas
  )
  
  state1 <- state2
  state2 <- fuzzy_prediksi_orde2[i]
  
}

tabel_recursive_orde2 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = recursive_state1_orde2,
  State2 = recursive_state2_orde2,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = recursive_forecast_orde2,
  Fuzzifikasi_Prediksi = fuzzy_prediksi_orde2
)

tabel_recursive_orde2

#4.5.4 MENGHITUNG UKURAN KETEPATAN PERAMALAN FTS CHEN ORDE 2
#1. One-Step Ahead

aktual_orde2 <- test$Deposito_Berjangka
prediksi_orde2 <- ramalan_testing_orde2

SE_orde2 <- (aktual_orde2 - prediksi_orde2)^2
AE_orde2 <- abs(aktual_orde2 - prediksi_orde2)
APE_orde2 <- abs((aktual_orde2 - prediksi_orde2)/aktual_orde2)*100

akurasi_onestep_orde2 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_orde2,
  Ft = prediksi_orde2,
  SE = SE_orde2,
  APE = APE_orde2,
  AE = AE_orde2
)

RMSE_onestep_orde2 <- sqrt(mean(SE_orde2))
MAPE_onestep_orde2 <- mean(APE_orde2)
MAE_onestep_orde2 <- mean(AE_orde2)
RMSE_onestep_orde2
MAPE_onestep_orde2
MAE_onestep_orde2

#2. Recursive Multi-Step Ahead

aktual_r_orde2 <- test$Deposito_Berjangka
prediksi_r_orde2 <- recursive_forecast_orde2

SE_r_orde2 <- (aktual_r_orde2 - prediksi_r_orde2)^2
AE_r_orde2 <- abs(aktual_r_orde2 - prediksi_r_orde2)
APE_r_orde2 <- abs((aktual_r_orde2 - prediksi_r_orde2)/aktual_r_orde2)*100

akurasi_recursive_orde2 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_r_orde2,
  Ft = prediksi_r_orde2,
  SE = SE_r_orde2,
  APE = APE_r_orde2,
  AE = AE_r_orde2
)

RMSE_recursive_orde2 <- sqrt(mean(SE_r_orde2))
MAPE_recursive_orde2 <- mean(APE_r_orde2)
MAE_recursive_orde2 <- mean(AE_r_orde2)
RMSE_recursive_orde2
MAPE_recursive_orde2
MAE_recursive_orde2

#4.6 PERAMALAN METODE FTS CHEN ORDE 3
#4.6.1 PEMBENTUKAN MODEL FTS CHEN ORDE 3 MENGGUNAKAN DATA TRAINING
#1. Membentuk FLR dan FLRG
#A. FLR
FLR_orde3 <- rep("-", length(fuzzy_train))

for(i in 4:length(fuzzy_train)){
  
  FLR_orde3[i] <- paste(
    "(",
    fuzzy_train[i-3], ", ",
    fuzzy_train[i-2], ", ",
    fuzzy_train[i-1],
    ") → ",
    fuzzy_train[i],
    sep=""
  )
  
}

tabel_FLR_orde3 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Fuzzifikasi = fuzzy_train,
  FLR = FLR_orde3
)

tabel_FLR_orde3

#B. FLRG
FLRG_data_orde3 <- data.frame(
  State1 = fuzzy_train[1:(length(fuzzy_train)-3)],
  State2 = fuzzy_train[2:(length(fuzzy_train)-2)],
  State3 = fuzzy_train[3:(length(fuzzy_train)-1)],
  Next_State = fuzzy_train[4:length(fuzzy_train)]
)

FLRG_orde3 <- FLRG_data_orde3 %>%
  group_by(State1, State2, State3) %>%
  summarise(
    Next_State = paste(
      sort(unique(Next_State)),
      collapse = ", "
    ),
    .groups = "drop"
  )
FLRG_orde3
View(FLRG_orde3)

#5. Defuzzifikasi Nilai Ramalan

defuzzifikasi <- function(Next_State, nilai_tengah){
  state <- unlist(strsplit(Next_State, ","))
  state <- trimws(state)
  indeks <- as.numeric(
    gsub("A", "", state)
  )
  mean(nilai_tengah[indeks])
}

tabel_defuzzifikasi_orde3 <- FLRG_orde3

tabel_defuzzifikasi_orde3$Nilai_Ramalan <- sapply(
  FLRG_orde3$Next_State,
  defuzzifikasi,
  nilai_tengah = nilai_tengah
)

tabel_defuzzifikasi_orde3

#6. Peramalan Data Training

ramalan_training_orde3 <- rep(
  NA,
  length(fuzzy_train)
)

for(i in 4:length(fuzzy_train)){
  
  state1 <- fuzzy_train[i-3]
  state2 <- fuzzy_train[i-2]
  state3 <- fuzzy_train[i-1]
  
  idx <- which(
    FLRG_orde3$State1 == state1 &
      FLRG_orde3$State2 == state2 &
      FLRG_orde3$State3 == state3
  )
  
  if(length(idx)>0){
    
    ramalan_training_orde3[i] <-
      tabel_defuzzifikasi_orde3$Nilai_Ramalan[idx]
    
  }
}

tabel_peramalan_training_orde3 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Hasil_Peramalan = ramalan_training_orde3
)

tabel_peramalan_training_orde3

#4.6.2 PERAMALAN ONE-STEP AHEAD FTS CHEN ORDE 3
#One-Step Ahead
state1_test <- c(
  fuzzy_train[length(fuzzy_train)-2],
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-3)]
)

state2_test <- c(
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-2)]
)

state3_test <- c(
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-1)]
)

ramalan_testing_orde3 <- rep(
  NA,
  length(fuzzy_test_orde1)
)

for(i in 1:length(fuzzy_test_orde1)){
  
  idx <- which(
    FLRG_orde3$State1 == state1_test[i] &
      FLRG_orde3$State2 == state2_test[i] &
      FLRG_orde3$State3 == state3_test[i]
  )
  
  if(length(idx)>0){
    
    ramalan_testing_orde3[i] <-
      tabel_defuzzifikasi_orde3$Nilai_Ramalan[idx]
    
  }else{
    
    s1 <- as.numeric(gsub("A","",state1_test[i]))
    s2 <- as.numeric(gsub("A","",state2_test[i]))
    s3 <- as.numeric(gsub("A","",state3_test[i]))
    
    ramalan_testing_orde3[i] <-
      (1*nilai_tengah[s1] +
         2*nilai_tengah[s2] +
         3*nilai_tengah[s3]) /
      (1+2+3)
  }
}

tabel_onestep_orde3 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = state1_test,
  State2 = state2_test,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = ramalan_testing_orde3
)

tabel_onestep_orde3

#4.6.3 PERAMALAN RECURSIVE MULTI-STEP AHEAD FTS CHEN ORDE 3

state1_awal_orde3 <- fuzzy_train[length(fuzzy_train)-2]
state2_awal_orde3 <- fuzzy_train[length(fuzzy_train)-1]
state3_awal_orde3 <- fuzzy_train[length(fuzzy_train)]

h_orde3 <- nrow(test)

recursive_state1_orde3 <- character(h_orde2)
recursive_state2_orde3 <- character(h_orde2)
recursive_state3_orde3 <- character(h_orde2)

recursive_forecast_orde3 <- numeric(h_orde3)

fuzzy_prediksi_orde3 <- character(h_orde3)

state1 <- state1_awal_orde3
state2 <- state2_awal_orde3
state3 <- state3_awal_orde3

for(i in 1:h_orde3){
  
  idx <- which(
    FLRG_orde3$State1 == state1 &
      FLRG_orde3$State2 == state2 &
      FLRG_orde3$State3 == state3
  )
  
  if(length(idx)>0){
    
    recursive_forecast_orde3[i] <-
      tabel_defuzzifikasi_orde3$Nilai_Ramalan[idx]
    
  }else{
    
    s1 <- as.numeric(gsub("A","",state1))
    s2 <- as.numeric(gsub("A","",state2))
    s3 <- as.numeric(gsub("A","",state3))
    
    recursive_forecast_orde3[i] <-
      (1*nilai_tengah[s1] +
         2*nilai_tengah[s2] +
         3*nilai_tengah[s3]) /
      (1+2+3)
    
  }
  
  fuzzy_prediksi_orde3[i] <- fuzzifikasi(
    recursive_forecast_orde3[i],
    bawah,
    atas
  )
  
  state1 <- state2
  state2 <- state3
  state3 <- fuzzy_prediksi_orde3[i]
  
}

tabel_recursive_orde3 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = recursive_state1_orde3,
  State2 = recursive_state2_orde3,
  State3 = recursive_state3_orde3,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = recursive_forecast_orde3,
  Fuzzifikasi_Prediksi = fuzzy_prediksi_orde3
)

tabel_recursive_orde3

#4.6.4 MENGHITUNG UKURAN KETEPATAN PERAMALAN FTS CHEN ORDE 3
#1. One-Step Ahead

aktual_orde3 <- test$Deposito_Berjangka
prediksi_orde3 <- ramalan_testing_orde3

SE_orde3 <- (aktual_orde3 - prediksi_orde3)^2
AE_orde3 <- abs(aktual_orde3 - prediksi_orde3)
APE_orde3 <- abs((aktual_orde3 - prediksi_orde3)/aktual_orde3)*100

akurasi_onestep_orde3 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_orde3,
  Ft = prediksi_orde3,
  SE = SE_orde3,
  APE = APE_orde3,
  AE = AE_orde3
)

RMSE_onestep_orde3 <- sqrt(mean(SE_orde3))
MAPE_onestep_orde3 <- mean(APE_orde3)
MAE_onestep_orde3 <- mean(AE_orde3)
RMSE_onestep_orde3
MAPE_onestep_orde3
MAE_onestep_orde3

#2. Recursive Multi-Step Ahead

aktual_r_orde3 <- test$Deposito_Berjangka
prediksi_r_orde3 <- recursive_forecast_orde3

SE_r_orde3 <- (aktual_r_orde3 - prediksi_r_orde3)^2
AE_r_orde3 <- abs(aktual_r_orde3 - prediksi_r_orde3)
APE_r_orde3 <- abs((aktual_r_orde3 - prediksi_r_orde3)/aktual_r_orde3)*100

akurasi_recursive_orde3 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_r_orde3,
  Ft = prediksi_r_orde3,
  SE = SE_r_orde3,
  APE = APE_r_orde3,
  AE = AE_r_orde3
)

RMSE_recursive_orde3 <- sqrt(mean(SE_r_orde3))
MAPE_recursive_orde3 <- mean(APE_r_orde3)
MAE_recursive_orde3 <- mean(AE_r_orde3)
RMSE_recursive_orde3
MAPE_recursive_orde3
MAE_recursive_orde3

#4.7 PERAMALAN METODE FTS CHEN ORDE 4
#4.7.1 PEMBENTUKAN MODEL FTS CHEN ORDE 4 MENGGUNAKAN DATA TRAINING
#1. Membentuk FLR dan FLRG
#A. FLR
FLR_orde4 <- rep("-", length(fuzzy_train))

for(i in 5:length(fuzzy_train)){
  
  FLR_orde4[i] <- paste(
    "(",
    fuzzy_train[i-4], ", ",
    fuzzy_train[i-3], ", ",
    fuzzy_train[i-2], ", ",
    fuzzy_train[i-1],
    ") → ",
    fuzzy_train[i],
    sep=""
  )
  
}

tabel_FLR_orde4 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Fuzzifikasi = fuzzy_train,
  FLR = FLR_orde4
)

tabel_FLR_orde4

#B. FLRG
FLRG_data_orde4 <- data.frame(
  State1 = fuzzy_train[1:(length(fuzzy_train)-4)],
  State2 = fuzzy_train[2:(length(fuzzy_train)-3)],
  State3 = fuzzy_train[3:(length(fuzzy_train)-2)],
  State4 = fuzzy_train[4:(length(fuzzy_train)-1)],
  Next_State = fuzzy_train[5:length(fuzzy_train)]
)

FLRG_orde4 <- FLRG_data_orde4 %>%
  group_by(State1, State2, State3, State4) %>%
  summarise(
    Next_State = paste(
      sort(unique(Next_State)),
      collapse = ", "
    ),
    .groups = "drop"
  )

FLRG_orde4
View(FLRG_orde4)

#5. Defuzzifikasi Nilai Ramalan

tabel_defuzzifikasi_orde4 <- FLRG_orde4

tabel_defuzzifikasi_orde4$Nilai_Ramalan <- sapply(
  FLRG_orde4$Next_State,
  defuzzifikasi,
  nilai_tengah = nilai_tengah
)

tabel_defuzzifikasi_orde4

#6. Peramalan Data Training

ramalan_training_orde4 <- rep(
  NA,
  length(fuzzy_train)
)

for(i in 5:length(fuzzy_train)){
  
  state1 <- fuzzy_train[i-4]
  state2 <- fuzzy_train[i-3]
  state3 <- fuzzy_train[i-2]
  state4 <- fuzzy_train[i-1]
  
  idx <- which(
    FLRG_orde4$State1 == state1 &
      FLRG_orde4$State2 == state2 &
      FLRG_orde4$State3 == state3 &
      FLRG_orde4$State4 == state4
  )
  
  if(length(idx) > 0){
    
    ramalan_training_orde4[i] <-
      tabel_defuzzifikasi_orde4$Nilai_Ramalan[idx]
    
  }
  
}

tabel_peramalan_training_orde4 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Hasil_Peramalan = ramalan_training_orde4
)

tabel_peramalan_training_orde4

#4.7.2 PERAMALAN ONE-STEP AHEAD FTS CHEN ORDE 4
#One-Step Ahead
state1_test <- c(
  fuzzy_train[length(fuzzy_train)-3],
  fuzzy_train[length(fuzzy_train)-2],
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-4)]
)

state2_test <- c(
  fuzzy_train[length(fuzzy_train)-2],
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-3)]
)

state3_test <- c(
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-2)]
)

state4_test <- c(
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-1)]
)

ramalan_testing_orde4 <- rep(
  NA,
  length(fuzzy_test_orde1)
)

for(i in 1:length(fuzzy_test_orde1)){
  
  idx <- which(
    FLRG_orde4$State1 == state1_test[i] &
      FLRG_orde4$State2 == state2_test[i] &
      FLRG_orde4$State3 == state3_test[i] &
      FLRG_orde4$State4 == state4_test[i]
  )
  
  if(length(idx) > 0){
    
    ramalan_testing_orde4[i] <-
      tabel_defuzzifikasi_orde4$Nilai_Ramalan[idx]
    
  } else {
    
    s1 <- as.numeric(gsub("A","",state1_test[i]))
    s2 <- as.numeric(gsub("A","",state2_test[i]))
    s3 <- as.numeric(gsub("A","",state3_test[i]))
    s4 <- as.numeric(gsub("A","",state4_test[i]))
    
    ramalan_testing_orde4[i] <-
      (
        1*nilai_tengah[s1] +
          2*nilai_tengah[s2] +
          3*nilai_tengah[s3] +
          4*nilai_tengah[s4]
      )/(1+2+3+4)
    
  }
  
}

tabel_onestep_orde4 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = state1_test,
  State2 = state2_test,
  State3 = state3_test,
  State4 = state4_test,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = ramalan_testing_orde4
)

tabel_onestep_orde4

#4.7.3 PERAMALAN RECURSIVE MULTI-STEP AHEAD FTS CHEN ORDE 4

state1_awal_orde4 <- fuzzy_train[length(fuzzy_train)-3]
state2_awal_orde4 <- fuzzy_train[length(fuzzy_train)-2]
state3_awal_orde4 <- fuzzy_train[length(fuzzy_train)-1]
state4_awal_orde4 <- fuzzy_train[length(fuzzy_train)]

h_orde4 <- nrow(test)

recursive_state1_orde4 <- character(h_orde4)
recursive_state2_orde4 <- character(h_orde4)
recursive_state3_orde4 <- character(h_orde4)
recursive_state4_orde4 <- character(h_orde4)

recursive_forecast_orde4 <- numeric(h_orde4)
fuzzy_prediksi_orde4 <- character(h_orde4)

state1 <- state1_awal_orde4
state2 <- state2_awal_orde4
state3 <- state3_awal_orde4
state4 <- state4_awal_orde4

for(i in 1:h_orde4){
  
  idx <- which(
    FLRG_orde4$State1 == state1 &
      FLRG_orde4$State2 == state2 &
      FLRG_orde4$State3 == state3 &
      FLRG_orde4$State4 == state4
  )
  
  if(length(idx) > 0){
    
    recursive_forecast_orde4[i] <-
      tabel_defuzzifikasi_orde4$Nilai_Ramalan[idx]
    
  } else {
    
    s1 <- as.numeric(gsub("A","",state1))
    s2 <- as.numeric(gsub("A","",state2))
    s3 <- as.numeric(gsub("A","",state3))
    s4 <- as.numeric(gsub("A","",state4))
    
    recursive_forecast_orde4[i] <-
      (
        1*nilai_tengah[s1] +
          2*nilai_tengah[s2] +
          3*nilai_tengah[s3] +
          4*nilai_tengah[s4]
      )/(1+2+3+4)
    
  }
  
  fuzzy_prediksi_orde4[i] <- fuzzifikasi(
    recursive_forecast_orde4[i],
    bawah,
    atas
  )
  
  state1 <- state2
  state2 <- state3
  state3 <- state4
  state4 <- fuzzy_prediksi_orde4[i]
  
}

tabel_recursive_orde4 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = recursive_state1_orde4,
  State2 = recursive_state2_orde4,
  State3 = recursive_state3_orde4,
  State4 = recursive_state4_orde4,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = recursive_forecast_orde4,
  Fuzzifikasi_Prediksi = fuzzy_prediksi_orde4
)

tabel_recursive_orde4

#4.7.4 MENGHITUNG UKURAN KETEPATAN PERAMALAN FTS CHEN ORDE 4

#1. One-Step Ahead

aktual_orde4 <- test$Deposito_Berjangka
prediksi_orde4 <- ramalan_testing_orde4

SE_orde4 <- (aktual_orde4 - prediksi_orde4)^2
AE_orde4 <- abs(aktual_orde4 - prediksi_orde4)
APE_orde4 <- abs((aktual_orde4 - prediksi_orde4)/aktual_orde4)*100

akurasi_onestep_orde4 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_orde4,
  Ft = prediksi_orde4,
  SE = SE_orde4,
  APE = APE_orde4,
  AE = AE_orde4
)

RMSE_onestep_orde4 <- sqrt(mean(SE_orde4))
MAPE_onestep_orde4 <- mean(APE_orde4)
MAE_onestep_orde4 <- mean(AE_orde4)

RMSE_onestep_orde4
MAPE_onestep_orde4
MAE_onestep_orde4

#2. Recursive Multi-Step Ahead

aktual_r_orde4 <- test$Deposito_Berjangka
prediksi_r_orde4 <- recursive_forecast_orde4

SE_r_orde4 <- (aktual_r_orde4 - prediksi_r_orde4)^2
AE_r_orde4 <- abs(aktual_r_orde4 - prediksi_r_orde4)
APE_r_orde4 <- abs((aktual_r_orde4 - prediksi_r_orde4)/aktual_r_orde4)*100

akurasi_recursive_orde4 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan,test$Tahun),
  Xt = aktual_r_orde4,
  Ft = prediksi_r_orde4,
  SE = SE_r_orde4,
  APE = APE_r_orde4,
  AE = AE_r_orde4
)

RMSE_recursive_orde4 <- sqrt(mean(SE_r_orde4))
MAPE_recursive_orde4 <- mean(APE_r_orde4)
MAE_recursive_orde4 <- mean(AE_r_orde4)

RMSE_recursive_orde4
MAPE_recursive_orde4
MAE_recursive_orde4

#4.8 PERAMALAN METODE FTS CHEN ORDE 5
#4.8.1 PEMBENTUKAN MODEL FTS CHEN ORDE 5 MENGGUNAKAN DATA TRAINING
#1. Membentuk FLR dan FLRG
#A. FLR
FLR_orde5 <- rep("-", length(fuzzy_train))

for(i in 6:length(fuzzy_train)){
  
  FLR_orde5[i] <- paste(
    "(",
    fuzzy_train[i-5], ", ",
    fuzzy_train[i-4], ", ",
    fuzzy_train[i-3], ", ",
    fuzzy_train[i-2], ", ",
    fuzzy_train[i-1],
    ") → ",
    fuzzy_train[i],
    sep=""
  )
  
}

tabel_FLR_orde5 <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Fuzzifikasi = fuzzy_train,
  FLR = FLR_orde5
)

tabel_FLR_orde5

#B. FLRG
FLRG_data_orde5 <- data.frame(
  State1 = fuzzy_train[1:(length(fuzzy_train)-5)],
  State2 = fuzzy_train[2:(length(fuzzy_train)-4)],
  State3 = fuzzy_train[3:(length(fuzzy_train)-3)],
  State4 = fuzzy_train[4:(length(fuzzy_train)-2)],
  State5 = fuzzy_train[5:(length(fuzzy_train)-1)],
  Next_State = fuzzy_train[6:length(fuzzy_train)]
)

FLRG_orde5 <- FLRG_data_orde5 %>%
  group_by(State1, State2, State3, State4, State5) %>%
  summarise(
    Next_State = paste(
      sort(unique(Next_State)),
      collapse = ", "
    ),
    .groups = "drop"
  )

FLRG_orde5
View(FLRG_orde5)

#5. Defuzzifikasi Nilai Ramalan

tabel_defuzzifikasi_orde5 <- FLRG_orde5

tabel_defuzzifikasi_orde5$Nilai_Ramalan <- sapply(
  FLRG_orde5$Next_State,
  defuzzifikasi,
  nilai_tengah = nilai_tengah
)

tabel_defuzzifikasi_orde5

#6. Peramalan Data Training

ramalan_training_orde5 <- rep(
  NA,
  length(fuzzy_train)
)

for(i in 6:length(fuzzy_train)){
  
  state1 <- fuzzy_train[i-5]
  state2 <- fuzzy_train[i-4]
  state3 <- fuzzy_train[i-3]
  state4 <- fuzzy_train[i-2]
  state5 <- fuzzy_train[i-1]
  
  idx <- which(
    FLRG_orde5$State1 == state1 &
      FLRG_orde5$State2 == state2 &
      FLRG_orde5$State3 == state3 &
      FLRG_orde5$State4 == state4 &
      FLRG_orde5$State5 == state5
  )
  
  if(length(idx) > 0){
    
    ramalan_training_orde5[i] <-
      tabel_defuzzifikasi_orde5$Nilai_Ramalan[idx]
    
  }
  
}

#4.8.2 PERAMALAN ONE-STEP AHEAD FTS CHEN ORDE 5
#One-Step Ahead
state1_test <- c(
  fuzzy_train[length(fuzzy_train)-4],
  fuzzy_train[length(fuzzy_train)-3],
  fuzzy_train[length(fuzzy_train)-2],
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-5)]
)

state2_test <- c(
  fuzzy_train[length(fuzzy_train)-3],
  fuzzy_train[length(fuzzy_train)-2],
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-4)]
)

state3_test <- c(
  fuzzy_train[length(fuzzy_train)-2],
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-3)]
)

state4_test <- c(
  fuzzy_train[length(fuzzy_train)-1],
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-2)]
)

state5_test <- c(
  fuzzy_train[length(fuzzy_train)],
  fuzzy_test_orde1[1:(length(fuzzy_test_orde1)-1)]
)

ramalan_testing_orde5 <- rep(
  NA,
  length(fuzzy_test_orde1)
)

for(i in 1:length(fuzzy_test_orde1)){
  
  idx <- which(
    FLRG_orde5$State1 == state1_test[i] &
      FLRG_orde5$State2 == state2_test[i] &
      FLRG_orde5$State3 == state3_test[i] &
      FLRG_orde5$State4 == state4_test[i] &
      FLRG_orde5$State5 == state5_test[i]
  )
  
  if(length(idx) > 0){
    
    ramalan_testing_orde5[i] <-
      tabel_defuzzifikasi_orde5$Nilai_Ramalan[idx]
    
  } else {
    
    s1 <- as.numeric(gsub("A","",state1_test[i]))
    s2 <- as.numeric(gsub("A","",state2_test[i]))
    s3 <- as.numeric(gsub("A","",state3_test[i]))
    s4 <- as.numeric(gsub("A","",state4_test[i]))
    s5 <- as.numeric(gsub("A","",state5_test[i]))
    
    ramalan_testing_orde5[i] <-
      (
        1*nilai_tengah[s1] +
          2*nilai_tengah[s2] +
          3*nilai_tengah[s3] +
          4*nilai_tengah[s4] +
          5*nilai_tengah[s5]
      ) / (1+2+3+4+5)
    
  }
  
}

tabel_onestep_orde5 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = state1_test,
  State2 = state2_test,
  State3 = state3_test,
  State4 = state4_test,
  State5 = state5_test,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = ramalan_testing_orde5
)

tabel_onestep_orde5

#4.8.3 PERAMALAN RECURSIVE MULTI-STEP AHEAD FTS CHEN ORDE 5

state1_awal_orde5 <- fuzzy_train[length(fuzzy_train)-4]
state2_awal_orde5 <- fuzzy_train[length(fuzzy_train)-3]
state3_awal_orde5 <- fuzzy_train[length(fuzzy_train)-2]
state4_awal_orde5 <- fuzzy_train[length(fuzzy_train)-1]
state5_awal_orde5 <- fuzzy_train[length(fuzzy_train)]

h_orde5 <- nrow(test)

recursive_state1_orde5 <- character(h_orde5)
recursive_state2_orde5 <- character(h_orde5)
recursive_state3_orde5 <- character(h_orde5)
recursive_state4_orde5 <- character(h_orde5)
recursive_state5_orde5 <- character(h_orde5)

recursive_forecast_orde5 <- numeric(h_orde5)
fuzzy_prediksi_orde5 <- character(h_orde5)

state1 <- state1_awal_orde5
state2 <- state2_awal_orde5
state3 <- state3_awal_orde5
state4 <- state4_awal_orde5
state5 <- state5_awal_orde5

for(i in 1:h_orde5){
  
  recursive_state1_orde5[i] <- state1
  recursive_state2_orde5[i] <- state2
  recursive_state3_orde5[i] <- state3
  recursive_state4_orde5[i] <- state4
  recursive_state5_orde5[i] <- state5
  
  idx <- which(
    FLRG_orde5$State1 == state1 &
      FLRG_orde5$State2 == state2 &
      FLRG_orde5$State3 == state3 &
      FLRG_orde5$State4 == state4 &
      FLRG_orde5$State5 == state5
  )
  
  if(length(idx) > 0){
    
    recursive_forecast_orde5[i] <-
      tabel_defuzzifikasi_orde5$Nilai_Ramalan[idx]
    
  } else {
    
    s1 <- as.numeric(gsub("A","",state1))
    s2 <- as.numeric(gsub("A","",state2))
    s3 <- as.numeric(gsub("A","",state3))
    s4 <- as.numeric(gsub("A","",state4))
    s5 <- as.numeric(gsub("A","",state5))
    
    recursive_forecast_orde5[i] <-
      (
        1*nilai_tengah[s1] +
          2*nilai_tengah[s2] +
          3*nilai_tengah[s3] +
          4*nilai_tengah[s4] +
          5*nilai_tengah[s5]
      )/(1+2+3+4+5)
    
  }
  
  fuzzy_prediksi_orde5[i] <- fuzzifikasi(
    recursive_forecast_orde5[i],
    bawah,
    atas
  )
  
  state1 <- state2
  state2 <- state3
  state3 <- state4
  state4 <- state5
  state5 <- fuzzy_prediksi_orde5[i]
  
}

tabel_recursive_orde5 <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  State1 = recursive_state1_orde5,
  State2 = recursive_state2_orde5,
  State3 = recursive_state3_orde5,
  State4 = recursive_state4_orde5,
  State5 = recursive_state5_orde5,
  Data_Aktual = test$Deposito_Berjangka,
  Hasil_Peramalan = recursive_forecast_orde5,
  Fuzzifikasi_Prediksi = fuzzy_prediksi_orde5
)

tabel_recursive_orde5

#4.8.4 MENGHITUNG UKURAN KETEPATAN PERAMALAN FTS CHEN ORDE 5

#1. One-Step Ahead

aktual_orde5 <- test$Deposito_Berjangka
prediksi_orde5 <- ramalan_testing_orde5

SE_orde5 <- (aktual_orde5 - prediksi_orde5)^2
AE_orde5 <- abs(aktual_orde5 - prediksi_orde5)
APE_orde5 <- abs((aktual_orde5 - prediksi_orde5)/aktual_orde5)*100

akurasi_onestep_orde5 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan, test$Tahun),
  Xt = aktual_orde5,
  Ft = prediksi_orde5,
  SE = SE_orde5,
  APE = APE_orde5,
  AE = AE_orde5
)

RMSE_onestep_orde5 <- sqrt(mean(SE_orde5))
MAPE_onestep_orde5 <- mean(APE_orde5)
MAE_onestep_orde5 <- mean(AE_orde5)

RMSE_onestep_orde5
MAPE_onestep_orde5
MAE_onestep_orde5

#2. Recursive Multi-Step Ahead

aktual_r_orde5 <- test$Deposito_Berjangka
prediksi_r_orde5 <- recursive_forecast_orde5

SE_r_orde5 <- (aktual_r_orde5 - prediksi_r_orde5)^2
AE_r_orde5 <- abs(aktual_r_orde5 - prediksi_r_orde5)
APE_r_orde5 <- abs((aktual_r_orde5 - prediksi_r_orde5)/aktual_r_orde5)*100

akurasi_recursive_orde5 <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan, test$Tahun),
  Xt = aktual_r_orde5,
  Ft = prediksi_r_orde5,
  SE = SE_r_orde5,
  APE = APE_r_orde5,
  AE = AE_r_orde5
)

RMSE_recursive_orde5 <- sqrt(mean(SE_r_orde5))
MAPE_recursive_orde5 <- mean(APE_r_orde5)
MAE_recursive_orde5 <- mean(AE_r_orde5)

RMSE_recursive_orde5
MAPE_recursive_orde5
MAE_recursive_orde5

#4.9 PEMILIHAN ORDE TERBAIK FTS CHEN ORDE TINGGI DENGAN PENDEKATAN RECURSIVE

perbandingan_recursive <- data.frame(
  Orde = c(1,2,3,4,5),
  RMSE = c(
    RMSE_recursive_orde1,
    RMSE_recursive_orde2,
    RMSE_recursive_orde3,
    RMSE_recursive_orde4,
    RMSE_recursive_orde5
  ),
  MAPE = c(
    MAPE_recursive_orde1,
    MAPE_recursive_orde2,
    MAPE_recursive_orde3,
    MAPE_recursive_orde4,
    MAPE_recursive_orde5
  ),
  MAE = c(
    MAE_recursive_orde1,
    MAE_recursive_orde2,
    MAE_recursive_orde3,
    MAE_recursive_orde4,
    MAE_recursive_orde5
  )
)

perbandingan_recursive$Ranking_MAPE <-
  rank(perbandingan_recursive$MAPE)

perbandingan_recursive$Ranking_RMSE <-
  rank(perbandingan_recursive$RMSE)

perbandingan_recursive$Ranking_MAE <-
  rank(perbandingan_recursive$MAE)

perbandingan_recursive

#Model Terbaik FTS Chen Orde 3 dengan RMSE, MAPE, MAE Terkecil

#4.10 PERAMALAN METODE FTS SACENA-EASO BERBASIS FLR DAN FLRG
#4.10.1 PEMBENTUKAN MODEL FTS SAXENA-EASO BERBASIS FLR DAN FLRG PADA DATA TRAINING
#1. Menghitung Persentase Perubahan Data
persentase_perubahan_train <- c(
  NA,
  ((train$Deposito_Berjangka[-1] -
      train$Deposito_Berjangka[-nrow(train)]) /
     train$Deposito_Berjangka[-nrow(train)]) * 100
)

persentase_perubahan_train

tabel_persentase_perubahan_train <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Deposito_Berjangka = train$Deposito_Berjangka,
  Persentase_Perubahan = persentase_perubahan_train
)

tabel_persentase_perubahan_train

#2. Pembentukan Himpunan Semesta

Pmin <- min(
  persentase_perubahan_train,
  na.rm = TRUE
)

Pmax <- max(
  persentase_perubahan_train,
  na.rm = TRUE
)

Pmin
Pmax

D1 <- 0.2984242266594
D2 <- 0.2930333

U <- c(
  Pmin - D1,
  Pmax + D2
)

U

#3. Membentuk Interval Awal

#a. Menentukan Banyak Interval
N <- length(train$Deposito_Berjangka)
N

n <- 1 + 3.322*log10(N)
n

n <- round(n)
n

#b. Menghitung Panjang Interval
l <- ((Dmax+D2)-(Dmin-D1))/n
l

#c. Interval yang Terbentuk
bawah <- U[1] + (0:(n-1))*l
atas  <- U[1] + (1:n)*l

#Interval terakhir tepat sama dengan batas atas himpunan semesta
atas[n] <- U[2]

#Tabel Interval yang Terbentuk
tabel_interval_awal <- data.frame(
  Interval = paste0("u",1:n),
  Batas_Bawah = bawah,
  Batas_Atas = atas
)

tabel_interval_awal

#4. Membentuk Subinterval
#a. Menghitung frekuensi data tiap interval
frekuensi <- numeric(n)

for(i in 1:n){
  
  if(i < n){
    
    frekuensi[i] <- sum(
      persentase_perubahan_train >= bawah[i] &
        persentase_perubahan_train < atas[i],
      na.rm = TRUE
    )
    
  }else{
    
    frekuensi[i] <- sum(
      persentase_perubahan_train >= bawah[i] &
        persentase_perubahan_train <= atas[i],
      na.rm = TRUE
    )
    
  }
  
}

tabel_frekuensi <- data.frame(
  Interval = paste0("u",1:n),
  Frekuensi = frekuensi
)

tabel_frekuensi

#b. Subinterval yang Terbentuk dan Nilai Tengah
subinterval <- data.frame()

nomor_sub <- 1

for(i in 1:n){
  
  f <- frekuensi[i]
  
  # jika frekuensi 0 gunakan 1 subinterval
  if(f == 0){
    f <- 1
  }
  
  panjang_sub <- (atas[i] - bawah[i]) / f
  
  for(j in 1:f){
    
    batas_bawah_sub <- bawah[i] + (j-1)*panjang_sub
    batas_atas_sub <- bawah[i] + j*panjang_sub
    
    nilai_tengah_sub <-
      (batas_bawah_sub + batas_atas_sub)/2
    
    subinterval <- rbind(
      subinterval,
      data.frame(
        Interval_Awal = paste0("u",i),
        Subinterval = paste0("u",nomor_sub),
        Batas_Bawah = batas_bawah_sub,
        Batas_Atas = batas_atas_sub,
        Nilai_Tengah = nilai_tengah_sub
      )
    )
    
    nomor_sub <- nomor_sub + 1
    
  }
  
}

subinterval

#5. Fuzzifikasi Berdasarkan Subinterval Kelas
fuzzifikasi_saxena <- function(x, subinterval){
  
  n_sub <- nrow(subinterval)
  
  for(i in 1:n_sub){
    
    if(i < n_sub){
      
      if(x >= subinterval$Batas_Bawah[i] &
         x < subinterval$Batas_Atas[i]){
        
        return(paste0("A", i))
        
      }
      
    } else {
      
      if(x >= subinterval$Batas_Bawah[i] &
         x <= subinterval$Batas_Atas[i]){
        
        return(paste0("A", i))
        
      }
      
    }
    
  }
  
}

fuzzy_pt_train <- c(
  "-",
  sapply(
    persentase_perubahan_train[-1],
    fuzzifikasi_saxena,
    subinterval = subinterval
  )
)

fuzzy_pt_train

tabel_fuzzifikasi_saxena <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Persentase_Perubahan = c(
    "-",
    round(
      persentase_perubahan_train[-1],
      3
    )
  ),
  Fuzzifikasi = fuzzy_pt_train
)

tabel_fuzzifikasi_saxena
View(tabel_fuzzifikasi_saxena)

#6. Membentuk FLR dan FLRG
#FLR
FLR_saxena <- rep(
  "-",
  length(fuzzy_pt_train)
)

for(i in 3:length(fuzzy_pt_train)){
  
  FLR_saxena[i] <- paste(
    fuzzy_pt_train[i-1],
    "→",
    fuzzy_pt_train[i]
  )
  
}

tabel_FLR_saxena <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Persentase_Perubahan = c(
    "-",
    round(
      persentase_perubahan_train[-1],
      3
    )
  ),
  Fuzzifikasi = fuzzy_pt_train,
  FLR = FLR_saxena
)

tabel_FLR_saxena
View(tabel_FLR_saxena)

#FLRG
FLRG_data_saxena <- data.frame(
  Current_State = fuzzy_pt_train[2:(length(fuzzy_pt_train)-1)],
  Next_State = fuzzy_pt_train[3:length(fuzzy_pt_train)]
)

FLRG_data_saxena

library(dplyr)

FLRG_saxena <- FLRG_data_saxena %>%
  group_by(Current_State) %>%
  summarise(
    Next_State = paste(
      sort(unique(Next_State)),
      collapse = ", "
    ),
    .groups = "drop"
  )

FLRG_saxena
View(FLRG_saxena)

#7. Menentukan Nilai Peramalan
#a. Menghitung Bobot Keanggotaan Peramalan Tiap Himpunan Fuzzy

options(digits = 9)

# Nilai tengah subinterval
mi <- subinterval$Nilai_Tengah

p <- length(mi)

wi <- numeric(p)

for(i in 1:p){
  
  # i = 1
  if(i == 1){
    
    wi[i] <- (1 + 0.5) /
      ((1/mi[i]) + (0.5/mi[i+1]))
    
  }
  
  # 2 ≤ i ≤ p-1
  else if(i < p){
    
    wi[i] <- (0.5 + 1 + 0.5) /
      ((0.5/mi[i-1]) +
         (1/mi[i]) +
         (0.5/mi[i+1]))
    
  }
  
  # i = p
  else{
    
    wi[i] <- (0.5 + 1) /
      ((0.5/mi[i-1]) +
         (1/mi[i]))
    
  }
  
}

# Tabel Bobot Keanggotaan
tabel_bobot_keanggotaan <- data.frame(
  Ai = paste0("A",1:p),
  mi = mi,
  wi = wi
)

tabel_bobot_keanggotaan

View(tabel_bobot_keanggotaan)

#b. Menentukan Peramalan Persentase Perubahan

forecast_pt <- function(
    state,
    FLRG_saxena,
    tabel_bobot_keanggotaan){
  
  idx <- which(
    FLRG_saxena$Current_State == state
  )
  
  # Aturan 1
  if(length(idx) == 0){
    
    indeks_state <- as.numeric(
      gsub("A","",state)
    )
    
    return(
      tabel_bobot_keanggotaan$wi[
        indeks_state
      ]
    )
    
  }
  
  consequent <- FLRG_saxena$Next_State[idx]
  
  state_tujuan <- unlist(
    strsplit(consequent,",")
  )
  
  state_tujuan <- trimws(
    state_tujuan
  )
  
  indeks <- as.numeric(
    gsub("A","",state_tujuan)
  )
  
  # Aturan 2 dan 3
  return(
    mean(
      tabel_bobot_keanggotaan$wi[indeks]
    )
  )
  
}

ramalan_pt_training <- rep(
  NA,
  length(fuzzy_pt_train)
)

for(i in 3:length(fuzzy_pt_train)){
  
  state_sebelumnya <-
    fuzzy_pt_train[i-1]
  
  ramalan_pt_training[i] <-
    forecast_pt(
      state_sebelumnya,
      FLRG_saxena,
      tabel_bobot_keanggotaan
    )
  
}

# Fuzzifikasi sebelumnya
fuzzifikasi_sebelumnya <- c(
  "-",
  fuzzy_pt_train[1:(length(fuzzy_pt_train)-1)]
)

# Tabel Peramalan Persentase Perubahan
tabel_peramalan_pt_training <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Persentase_Perubahan = c(
    NA,
    persentase_perubahan_train[-1]
  ),
  Fuzzifikasi_Sebelumnya =
    fuzzifikasi_sebelumnya,
  Peramalan_Persentase =
    ramalan_pt_training
)

tabel_peramalan_pt_training

View(tabel_peramalan_pt_training)

#c. Transformasi Peramalan Persentase Menjadi Nilai Ramalan Akhir

ramalan_training_saxena <- rep(
  NA,
  nrow(train)
)

for(i in 3:nrow(train)){
  
  ramalan_training_saxena[i] <-
    train$Deposito_Berjangka[i-1] *
    (1 + ramalan_pt_training[i]/100)
  
}

# Hasil Peramalan Data Training

tabel_peramalan_training_saxena <- data.frame(
  t = train$t,
  Tahun = train$Tahun,
  Bulan = train$Bulan,
  Data_Aktual = train$Deposito_Berjangka,
  Persentase_Perubahan = c(
    NA,
    persentase_perubahan_train[-1]
  ),
  Fuzzifikasi_Sebelumnya =
    fuzzifikasi_sebelumnya,
  Peramalan_Persentase =
    ramalan_pt_training,
  Hasil_Peramalan =
    ramalan_training_saxena
)

tabel_peramalan_training_saxena
View(tabel_peramalan_training_saxena)

#4.10.2 PERAMALAN ONE-STEP AHEAD FTS SAXENA-EASO BERBASIS FLR DAN FLRG PADA DATA TESTING
#1. Menghitung Persentase Perubahan Data Testing

persentase_perubahan_test <- numeric(nrow(test))

# Januari 2024 dibandingkan Desember 2023
persentase_perubahan_test[1] <-
  ((test$Deposito_Berjangka[1] -
      train$Deposito_Berjangka[nrow(train)]) /
     train$Deposito_Berjangka[nrow(train)]) * 100

# Periode berikutnya dibandingkan data testing sebelumnya
for(i in 2:nrow(test)){
  
  persentase_perubahan_test[i] <-
    ((test$Deposito_Berjangka[i] -
        test$Deposito_Berjangka[i-1]) /
       test$Deposito_Berjangka[i-1]) * 100
  
}

persentase_perubahan_test

#Fuzzifikasi Persentase Perubahan Data Testing
fuzzy_pt_test <- sapply(
  persentase_perubahan_test,
  fuzzifikasi_saxena,
  subinterval = subinterval
)

fuzzy_pt_test

#Tabel persentase perubahan dan fuzzifikasi data testing
tabel_fuzzifikasi_testing_saxena <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  Data_Aktual = test$Deposito_Berjangka,
  Persentase_Perubahan = round(
    persentase_perubahan_test,
    3
  ),
  Fuzzifikasi = fuzzy_pt_test
)

tabel_fuzzifikasi_testing_saxena
View(tabel_fuzzifikasi_testing_saxena)

#2. Menghitung Peramalan Persentase Perubahan One-Step Ahead Data Testing FTS Saxena-Easo
fuzzifikasi_sebelumnya_test <- c(
  fuzzy_pt_train[length(fuzzy_pt_train)],
  fuzzy_pt_test[1:(length(fuzzy_pt_test)-1)]
)

fuzzifikasi_sebelumnya_test

#Peramalan Persentase Perubahan One-Step Pada Data Testing
ramalan_pt_test <- numeric(
  length(fuzzifikasi_sebelumnya_test)
)

for(i in 1:length(fuzzifikasi_sebelumnya_test)){
  
  ramalan_pt_test[i] <-
    forecast_pt(
      fuzzifikasi_sebelumnya_test[i],
      FLRG_saxena,
      tabel_bobot_keanggotaan
    )
  
}
tabel_peramalan_pt_testing <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  Persentase_Perubahan = round(
    persentase_perubahan_test,
    3
  ),
  Fuzzifikasi_Sebelumnya =
    fuzzifikasi_sebelumnya_test,
  Peramalan_Persentase =
    round(
      ramalan_pt_test,
      3
    )
)

tabel_peramalan_pt_testing
View(tabel_peramalan_pt_testing)

#8.3 Transformasi Menjadi Nilai Peramalan Akhir One-Step Ahead

ramalan_akhir_test <- numeric(
  nrow(test)
)

# Januari 2024 menggunakan Desember 2023
ramalan_akhir_test[1] <-
  train$Deposito_Berjangka[nrow(train)] *
  (1 + ramalan_pt_test[1]/100)

# Februari 2024 s.d. Desember 2025
for(i in 2:nrow(test)){
  
  ramalan_akhir_test[i] <-
    test$Deposito_Berjangka[i-1] *
    (1 + ramalan_pt_test[i]/100)
  
}

ramalan_akhir_test

tabel_onestep_saxena <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  Peramalan_Persentase = round(
    ramalan_pt_test,
    3
  ),
  Nilai_Peramalan = round(
    ramalan_akhir_test,
    2
  )
)

tabel_onestep_saxena
View(tabel_onestep_saxena)

#4.10.3 PERAMALAN RECURSIVE MULTI-STEP AHEAD FTS SAXENA-EASO BERBASIS FLR DAN FLRG PADA DATA TESTING
h <- nrow(test)

recursive_state <- character(h)
recursive_pt <- numeric(h)
recursive_forecast <- numeric(h)

state_awal <- fuzzy_pt_train[length(fuzzy_pt_train)]
current_state <- state_awal

for(i in 1:h){
  
  # state yang digunakan
  recursive_state[i] <- current_state
  
  # forecast persentase perubahan
  recursive_pt[i] <-
    forecast_pt(
      current_state,
      FLRG_saxena,
      tabel_bobot_keanggotaan
    )
  
  # periode pertama menggunakan data aktual terakhir training
  if(i == 1){
    
    recursive_forecast[i] <-
      train$Deposito_Berjangka[nrow(train)] *
      (1 + recursive_pt[i]/100)
    
  } else {
    
    # recursive menggunakan hasil prediksi sebelumnya
    recursive_forecast[i] <-
      recursive_forecast[i-1] *
      (1 + recursive_pt[i]/100)
    
  }
  
  # hitung persentase perubahan hasil prediksi
  if(i == 1){
    
    pt_prediksi <-
      ((recursive_forecast[i] -
          train$Deposito_Berjangka[nrow(train)]) /
         train$Deposito_Berjangka[nrow(train)]) * 100
    
  } else {
    
    pt_prediksi <-
      ((recursive_forecast[i] -
          recursive_forecast[i-1]) /
         recursive_forecast[i-1]) * 100
    
  }
  
  # fuzzifikasi persentase perubahan hasil prediksi
  current_state <-
    fuzzifikasi_saxena(
      pt_prediksi,
      subinterval
    )
  
}

tabel_recursive_saxena <- data.frame(
  t = test$t,
  Tahun = test$Tahun,
  Bulan = test$Bulan,
  Fuzzifikasi_Sebelumnya = recursive_state,
  Peramalan_Persentase = round(
    recursive_pt,
    3
  ),
  Nilai_Peramalan_Akhir = round(
    recursive_forecast,
    2
  )
)

tabel_recursive_saxena

View(tabel_recursive_saxena)

#4.10.4 MENGHITUNG UKURAN KETEPATAN PERAMALAN FTS SAXENA-EASO BERBASIS FLR DAN FLRG
# One-Step Ahead

aktual_saxena <- test$Deposito_Berjangka
prediksi_saxena <- ramalan_akhir_test

SE_saxena <- (aktual_saxena - prediksi_saxena)^2
AE_saxena <- abs(aktual_saxena - prediksi_saxena)
APE_saxena <- abs(
  (aktual_saxena - prediksi_saxena) /
    aktual_saxena
) * 100

akurasi_onestep_saxena <- data.frame(
  t = test$t,
  Bulan = paste(test$Bulan, test$Tahun),
  Xt = aktual_saxena,
  Ft = prediksi_saxena,
  SE = SE_saxena,
  APE = APE_saxena,
  AE = AE_saxena
)

akurasi_onestep_saxena

RMSE_onestep_saxena <- sqrt(
  mean(SE_saxena)
)

MAPE_onestep_saxena <- mean(
  APE_saxena
)

MAE_onestep_saxena <- mean(
  AE_saxena
)

RMSE_onestep_saxena
MAPE_onestep_saxena
MAE_onestep_saxena

# Recursive Multi-Step Ahead

aktual_recursive_saxena <- test$Deposito_Berjangka

prediksi_recursive_saxena <- recursive_forecast

SE_recursive_saxena <-
  (aktual_recursive_saxena -
     prediksi_recursive_saxena)^2

AE_recursive_saxena <-
  abs(
    aktual_recursive_saxena -
      prediksi_recursive_saxena
  )

APE_recursive_saxena <-
  abs(
    (aktual_recursive_saxena -
       prediksi_recursive_saxena) /
      aktual_recursive_saxena
  ) * 100

akurasi_recursive_saxena <- data.frame(
  t = test$t,
  Bulan = paste(
    test$Bulan,
    test$Tahun
  ),
  Xt = aktual_recursive_saxena,
  Ft = prediksi_recursive_saxena,
  SE = SE_recursive_saxena,
  APE = APE_recursive_saxena,
  AE = AE_recursive_saxena
)

akurasi_recursive_saxena

RMSE_recursive_saxena <- sqrt(
  mean(SE_recursive_saxena)
)

MAPE_recursive_saxena <- mean(
  APE_recursive_saxena
)

MAE_recursive_saxena <- mean(
  AE_recursive_saxena
)

RMSE_recursive_saxena
MAPE_recursive_saxena
MAE_recursive_saxena

#4.11 PERBANDINGAN FTS CHEN ORDE TERBAIK DAN FTS SAXENA EASO BERBASIS FLR DAN FLRG DENGAN PENDEKATAN RECURSIVE MULTI-STEP AHEAD SERTA PEMILIHAN MODEL TERBAIK
RMSE_recursive_orde3
MAPE_recursive_orde3
MAE_recursive_orde3

RMSE_recursive_saxena
MAPE_recursive_saxena
MAE_recursive_saxena


#MODEL TERBAIK ADALAH FTS CHEN ORDE TIGA
#4.12 FUTURE FORECASTING DENGAN FTS CHEN ORDE 3
#1. Pembentukan Himpunan Semesta
Dmin <- min(Data$Deposito_Berjangka)
Dmin
Dmax <- max(Data$Deposito_Berjangka)
Dmax
D1 <- 60166.96211
D2 <- 5187.48977999389
U <- c(Dmin - D1, Dmax + D2)
U

#2. Pembentukan Interval
#a. Menentukan Banyak Interval
N <- length(Data$Deposito_Berjangka)
N
n <- 1 + 3.322*log10(N)
n
n <- round(n)
n

#b. Menghitung Panjang Interval
l <- ((Dmax+D2)-(Dmin-D1))/n
l
#c. Interval yang Terbentuk
bawah <- U[1] + (0:(n-1))*l
atas  <- U[1] + (1:n)*l

#Interval terakhir tepat sama dengan batas atas himpunan semesta
atas[n] <- U[2]

#d. Menghitung Nilai Tengah Interval
nilai_tengah <- (bawah + atas)/2

#Tabel Interval yang Terbentuk dan Nilai Tengah
tabel_interval <- data.frame(
  Interval = paste0("u",1:n),
  Batas_Bawah = bawah,
  Batas_Atas = atas,
  Nilai_Tengah = nilai_tengah
)

tabel_interval

#3. Melakukan Fuzzifikasi
#Mendefinisikan Fungsi Fuzzifikasi
fuzzifikasi <- function(x, bawah, atas){
  n <- length(bawah)
  for(i in 1:n){
    #Interval 1 sampai n-1
    if(i < n){
      if(x >= bawah[i] & x < atas[i]){
        return(paste0("A",i))
      }
    }
    #Interval terakhir
    if(i == n){
      if(x >= bawah[i] & x <= atas[i]){
        return(paste0("A",i))
      }
    }
  }
}

#Fuzzifikasi Data
fuzzy_future <- sapply(
  Data$Deposito_Berjangka,
  fuzzifikasi,
  bawah = bawah,
  atas = atas
)

fuzzy_future[95] <- "A2"
fuzzy_future

#Tabel Fuzzifikasi Future Forecasting
tabel_fuzzifikasi_future <- data.frame(
  t = Data$t,
  Tahun = Data$Tahun,
  Bulan = Data$Bulan,
  Deposito_Berjangka = Data$Deposito_Berjangka,
  Fuzzy_set_future = fuzzy_future
)

tabel_fuzzifikasi_future

#4. Membentuk FLR dan FLRG
#A. FLR
FLR_orde3_future <- rep("-", length(fuzzy_future))

for(i in 4:length(fuzzy_future)){
  
  FLR_orde3_future[i] <- paste(
    "(",
    fuzzy_future[i-3], ", ",
    fuzzy_future[i-2], ", ",
    fuzzy_future[i-1],
    ") → ",
    fuzzy_future[i],
    sep=""
  )
  
}

tabel_FLR_orde3_future <- data.frame(
  t = Data$t,
  Tahun = Data$Tahun,
  Bulan = Data$Bulan,
  Data_Aktual = Data$Deposito_Berjangka,
  Fuzzifikasi = fuzzy_future,
  FLR = FLR_orde3_future
)

tabel_FLR_orde3_future

#B. FLRG
FLRG_data_orde3_future <- data.frame(
  State1 = fuzzy_future[1:(length(fuzzy_future)-3)],
  State2 = fuzzy_future[2:(length(fuzzy_future)-2)],
  State3 = fuzzy_future[3:(length(fuzzy_future)-1)],
  Next_State = fuzzy_future[4:length(fuzzy_future)]
)

FLRG_orde3_future <- FLRG_data_orde3_future %>%
  group_by(State1, State2, State3) %>%
  summarise(
    Next_State = paste(
      sort(unique(Next_State)),
      collapse = ", "
    ),
    .groups = "drop"
  )
FLRG_orde3_future
View(FLRG_orde3_future)

#5. Defuzzifikasi Nilai Ramalan

defuzzifikasi_future <- function(Next_State, nilai_tengah){
  state <- unlist(strsplit(Next_State, ","))
  state <- trimws(state)
  indeks <- as.numeric(
    gsub("A", "", state)
  )
  mean(nilai_tengah[indeks])
}

tabel_defuzzifikasi_orde3_future <- FLRG_orde3_future

tabel_defuzzifikasi_orde3_future$Nilai_Ramalan_future <- sapply(
  FLRG_orde3_future$Next_State,
  defuzzifikasi_future,
  nilai_tengah = nilai_tengah
)

tabel_defuzzifikasi_orde3_future

#4.6.3 PERAMALAN RECURSIVE MULTI-STEP AHEAD FTS CHEN ORDE 3
# Future Forecasting Recursive Tahun 2026 (12 Periode)

# Future Forecasting FTS Chen Orde 3 (Jan–Des 2026)

h_future <- 12

state1_awal_orde3 <- fuzzy_future[length(fuzzy_future)-2]
state2_awal_orde3 <- fuzzy_future[length(fuzzy_future)-1]
state3_awal_orde3 <- fuzzy_future[length(fuzzy_future)]

future_state1_orde3 <- character(h_future)
future_state2_orde3 <- character(h_future)
future_state3_orde3 <- character(h_future)

future_forecast_orde3 <- numeric(h_future)

fuzzy_prediksi_orde3 <- character(h_future)

state1 <- state1_awal_orde3
state2 <- state2_awal_orde3
state3 <- state3_awal_orde3

for(i in 1:h_future){
  
  # simpan state yang digunakan
  future_state1_orde3[i] <- state1
  future_state2_orde3[i] <- state2
  future_state3_orde3[i] <- state3
  
  idx <- which(
    FLRG_orde3_future$State1 == state1 &
      FLRG_orde3_future$State2 == state2 &
      FLRG_orde3_future$State3 == state3
  )
  
  if(length(idx) > 0){
    
    future_forecast_orde3[i] <-
      tabel_defuzzifikasi_orde3_future$Nilai_Ramalan_future[idx]
    
  } else {
    
    s1 <- as.numeric(gsub("A","",state1))
    s2 <- as.numeric(gsub("A","",state2))
    s3 <- as.numeric(gsub("A","",state3))
    
    future_forecast_orde3[i] <-
      (
        1*nilai_tengah[s1] +
          2*nilai_tengah[s2] +
          3*nilai_tengah[s3]
      )/(1+2+3)
    
  }
  
  # penting: bulatkan dulu agar tepat di batas interval
  fuzzy_prediksi_orde3[i] <- fuzzifikasi(
    round(future_forecast_orde3[i],0),
    bawah,
    atas
  )
  
  # update state untuk periode berikutnya
  state1 <- state2
  state2 <- state3
  state3 <- fuzzy_prediksi_orde3[i]
  
}

tabel_future_orde3 <- data.frame(
  t = 109:120,
  Tahun = rep(2026,12),
  Bulan = c(
    "Januari","Februari","Maret","April",
    "Mei","Juni","Juli","Agustus",
    "September","Oktober","November","Desember"
  ),
  Fuzzifikasi_Sebelumnya = paste(
    future_state1_orde3,
    future_state2_orde3,
    future_state3_orde3,
    sep = ", "
  ),
  Hasil_Peramalan = round(
    future_forecast_orde3,
    0
  ),
  Fuzzifikasi_Prediksi = fuzzy_prediksi_orde3
)

tabel_future_orde3

View(tabel_future_orde3)

