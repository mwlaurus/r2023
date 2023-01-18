#zad dom 1

#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.

AvgSpeed1 <- 120    #Average predkosc z pierwszego odcinka w km/h
AvgSpeed2 <- 90      #Average predkosc z drugiego odcinka w km/h

#AvgSpeed to całkowita droga przez całkowity czas czyli AvgSpeed = (S+S)/(timeA+timeB)
#połowa pierwsza i druga były równe, wiec S=AvgSpeed1*timeA i S=AvgSpeed2*timeB  => timeA = (AvgSpeed2/AvgSpeed1)*timeB i 2S = 2*AvgSpeed2*timeB
#podstawiając poniższe zalezności do gornego rowania: AvgSpeed = (2*AvgSpeed2*timeB) / ((AvgSpeed2/AvgSpeed1)*timeB + timeB)

AvgSpeed <- (2*AvgSpeed2) / (AvgSpeed2/AvgSpeed1 +1)

print(paste("Average predkosc pociagu to:", AvgSpeed))  # 102 km/h


#-------------------------------------------------------------------------------------------------------------------

#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
# Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.


df <-read.csv("dane.csv",header = TRUE, sep=";")
df <-df[rowSums(is.na(df)) != ncol(df),] #usuwamy wierszy z nullami

df

X <-as.list(df[['wzrost']]) #tablica reprezentująca wektor wzrost
Y <-as.list(df[['waga']]) #tablica reprezentująca wektor waga



dev_func <- function(arg_list) {
  Sum <- 0
  Average <-mean(unlist(arg_list))
  for (i in 1:length(arg_list)) {
    Sum <- Sum + (as.numeric(arg_list[i])-Average)^2
  }
  return (sqrt(Sum))
}


Cor <- function(listimeA, listimeB) {
  Sum_cov <- 0
  Average1 <- mean(unlist(listimeA))
  Average2 <- mean(unlist(listimeB))
  for (i in 1:length(listimeA)) {
    Sum_cov <- Sum_cov + ((as.numeric(listimeA[i])-Average1)*(as.numeric(listimeB[i])-Average2))
  }
  return (Sum_cov / (dev_func(listimeA) * dev_func(listimeB)))
}

Cor(X,Y) # =0.9793459 / wartość zbliżone do 1 - bardzo silna dodatnia korelacja


plot(df$wzrost, df$waga, xlab="Height", ylab="Weight", pch=18)
#co potwierdza wykres- wzrost wartości jednej cechy (wzrostu) implikuje proporcjonalne zmiany średnich wartości drugiej cechy (wagi)


#-----------------------------------------------------------------------------------------------------------

#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika
#stworzDataFrame <- function(how_many=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych (tyle wierszy how_many podaliśmy w argumencie how_many. how_many=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

data_collection <- function(how_many_rows=1) {
  print('Enter column names. [Enter] tab without column name - quits')
  col_list <-list()
  response <- "wrong value"
  while (response != "") {
    response <- readline("Enter column name: ") 
    if (response != "") {col_list <- append(col_list, response)}
  }
  
  Dframe = data.frame(matrix(nrow=how_many_rows, ncol=length(col_list),0)) #tworzymy frame
  names(Dframe)<-c(col_list)     #podmieniamy nazwy kolumn
  
  for (i in 1:how_many_rows) {   #w pętli nadpisujemy zera - kolejnymi wartościami
    for (j in 1:length(col_list)) {
      response2 <- readline(paste("Enter cell value for column:[",toString(col_list[j]),"], line: ->",toString(i),", :",sep=" "))
      if (response2 != "") {Dframe[i,toString(col_list[j])] <- response2}
    }
  }
  show(Dframe)
}    

#przykład użycia: data_collection(2)

#-----------------------------------------------------------------------------------------------------------

#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.

calcAggr <- function(filePath="C:\\Cloud\\Maciek\\PJWSTK\\GitHub_r2023\\r2023\\smogKrakow2",colName="3_temperature",aggrFunc="mean",howManyFiles=1) {
  
  #sprawdzenie ile mamy plikow i sprawdzenie maksymalnej ilości
  fileList <- list.files(filePath, "*.csv")
  if (howManyFiles > length(fileList)) {
    howManyFiles <- length(fileList)
    print("There are less files than set in parameter. All will take into consideration.")}
  
  all_elements <- list()
  
  #Dodawanie do ww listy odczytów z kolumn z wybranych plików, z pominięciem NULLi
  for (i in 1:howManyFiles){
    data <- read.csv(paste(filePath,"\\",fileList[i],sep=""), sep=",")
    print(paste("Reading file: " , fileList[i]))
    seria <- as.list(data[[paste("X",colName,sep="")]])
    seria <- seria[!is.na(seria)]
    all_elements <- append(all_elements,seria)
  }  
  
  #Zwracanie wybranego agregatu
  if (aggrFunc=="mean") {return (print(paste("Mean: ",mean(unlist(all_elements)),sep=" ")))}
  if (aggrFunc=="median") {return (print(paste("Median: ",median(unlist(all_elements)),sep=" ")))}
  if (aggrFunc=="min") {return (print(paste("Minimum: ",min(unlist(all_elements)),sep=" ")))}
  if (aggrFunc=="max") {return (print(paste("Maximum: ",max(unlist(all_elements)),sep=" ")))}
}

#Przykład uzycia
calcAggr("C:\\Cloud\\Maciek\\PJWSTK\\GitHub_r2023\\r2023\\smogKrakow2","3_temperature","mean",10)
