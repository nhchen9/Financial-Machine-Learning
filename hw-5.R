library(keras)
library(Hmisc)
library(lubridate)
library(gtools)
library(data.table)
library(onehot)

setwd("C:/Users/nhche/Development/stat-430")
dat = read.csv("datasets/2017_M1_IEX.csv", header =F)

dat = dat[,-3]
names(dat) = c("tStamp", "ticker", "O", "H", "L", "C", "V")

## impute missing values by 0
dat$C = zoo::na.locf(dat$C)
dat$V = na.replace(dat$V, 0)

TICKER = c("SPY", "AAPL", "MSFT", "GOOGL","FB","GOOG", "INTC","AMZN", "VZ", "BRK.B",  "BAC","JNJ", "JPM", "XOM", 
              "PFE", "UNH", "V", "T", "WFC", "CVX", "HD")

pdat = NULL
vdat =NULL
allDat <- NULL
for(iTicker in TICKER){
  tmpDat <- subset(dat, ticker==iTicker)
  pdat <- cbind(pdat, tmpDat$C)
  vdat <- cbind(vdat, tmpDat$V)
}
allDat = cbind(pdat, vdat)
nMin <- 390
nDay <- dim(allDat)[1] / nMin


train_days <- 1:150
val_days <- 151:200
test_days <- 201:251
train_min <- 1:(150*nMin)
val_min <- (150*nMin+1):(200*nMin)
test_min <- (200*nMin+1):(nDay*nMin)


col_included <- 1:(length(TICKER)*2)

x_train <- allDat[train_min, col_included]
x_val <- allDat[val_min, col_included]
x_test <- allDat[test_min, col_included]

w=60
avgMprice <- c(rep(NA, w-1), zoo::rollmean(allDat[,1], k=w, align="left"))
preMP <- avgMprice
postMP <- c(avgMprice[-(1:w)], rep(NA,w))
price_change <- postMP - preMP


r <- 0.08
Y <- rep(1, length(preMP)) # stable
Y[price_change > r] <- 2 # increase
Y[price_change < - r] <- 0 # decrease

kept_min <- ( (1:nrow(allDat)) %% nMin > w ) & ( (1:nrow(allDat)) %% nMin <= nMin - w )
Y[!kept_min] <- NA

length(which(Y==1))
length(which(Y==2))
length(which(Y==0))

y_train <- Y[train_min]
y_val <- Y[val_min]
y_test <- Y[test_min]



for(i in dim(x_train)[2]){
  m = mean(x_train[,i])
  s = sd(x_train[,i])
  
  x_train = (x_train - m)/s
  x_val = (x_val-m)/s
  x_test = (x_test-m)/s
}


sampling_generator <- function(X_data, Y_data, batch_size, w)
{
  function()
  {
    rows_with_up_down <- w:nrow(X_data)
    rows_with_up_down <- intersect(rows_with_up_down, which( Y_data %in% c(0,1,2)))  # only use labels 0 and 1
    
    rows <- sample( rows_with_up_down, batch_size, replace = TRUE )
    
    X = array(dim=c(batch_size, w, 21,2))
  
    Ylist = list()
    for(i in 1:length(rows)){
      X[i,,,1]=X_data[(rows[i]-w+1):rows[i],1:21]
      X[i,,,2]=X_data[(rows[i]-w+1):rows[i],22:42]
      Ylist[[i]] = c(0,0,0)
      Ylist[[i]][Y_data[rows[i]]+1] = 1
    }
    #print(Ylist)
    Y <- array(abind::abind(Ylist, along = 0), c(batch_size, 3))
    list(X, Y)
  }
}

k_clear_session()

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 6, kernel_size = c(4, 4), activation = "relu",input_shape = c(60, 21, 2)) %>% 
  layer_conv_2d(filters = 8, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 8, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 

  layer_flatten() %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% 
  layer_dense(units = 3, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 5e-4),
  metrics = c("accuracy") 
)

w = 60
batch_size = 500
epochs = 20

his <- model %>% fit_generator(sampling_generator(x_train, y_train, batch_size = batch_size, w=w),
                               steps_per_epoch = 100, epochs = epochs,
                               validation_data = sampling_generator(x_val, y_val, batch_size = batch_size, w=w),
                               validation_steps = 50)


k_clear_session()

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 12, kernel_size = c(8, 4), strides = c(1,1),activation = "relu",input_shape = c(60, 21, 2)) %>% 
  layer_conv_2d(filters = 12, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 12, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  
  layer_flatten() %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% 
  layer_dense(units = 3, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 5e-4),
  metrics = c("accuracy") 
)
earlyStop <- callback_early_stopping(monitor = "val_acc", patience = 5)

checkPoint <- callback_model_checkpoint(filepath = file.path("C:/Users/nhche/Development/stat-430/iex_CNN.h5"),
                                        monitor = "val_acc", save_best_only = TRUE)

schedule <- function(epoch,lr) (lr)*(0.8^(epoch))
schedulLr <- callback_learning_rate_scheduler(schedule)

w = 60
batch_size = 500
epochs = 20

his <- model %>% fit_generator(sampling_generator(x_train, y_train, batch_size = batch_size, w=w),
                               steps_per_epoch = 100, epochs = epochs, callbacks = list(checkPoint, schedulLr),
                               validation_data = sampling_generator(x_val, y_val, batch_size = batch_size, w=w),
                               validation_steps = 50)

fitted <- load_model_hdf5(file.path("C:/Users/nhche/Development/stat-430/iex_CNN5.h5"))

results <- fitted %>% evaluate_generator(sampling_generator(x_test, y_test, batch_size = batch_size, w=w), 
                                         steps = 100)
results
