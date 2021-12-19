
**Wstęp**

Dokument opisuje metody przeprowadzenia analizy przeżycia na podstawie danych zrekontruowanych do poziomu pacjenta. 
W procesie rekontrukcji wykorzystano algorytm interacyjny poprawiający prezycję rekonstrukcji danych \footnote{Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves. P. Guyot 2012}. Uzyskane indywidualne dane pacjenta *(ang. IPD - indyvidual patient data)* posłużyły do przeprowadzenia wtórnej analizy przeżycia. Następnie przeprowadzono ekstrapolację przy użyciu rozkładu parametrycznego. W pracy odtworzono przebieg krzywych Kaplana-Meiera reprezentujących prawdopodobieństwo przeżycia wraz przedziałami ufności (95% CI) oraz oszacowano mediany czasu przeżycia dla pacjentów. W celu oszacowania ryzyka zgonu zastosowano model proporcjonalnego hazardu Coxa, na podstawie którego uzyskano wartość proporcji hazardu *(ang. HR - hazard ratio)*. Wielkość efektu interwencji oceniono analizując pola pod krzywymi Kaplana-Meiera uzyskując miary ograniczonych średnich: czasu przeżycia *(ang. restricted mean survival time, RMST)* oraz czasu utraconego *(ang. restricted mean time lost, RMTL)*. Chcąc oszacować pełniejsze korzyści wynikające w ocenianej interwencji przeprowadzono parametryczną ekstrapolację funkcji przeżycia. 

Zastosowana ścieżka analityczna obejmuje 3 główne etapy: proces rekonstrukcji danych do poziomu pacjenta, przeprowadzenie analiz na podstawie uzyskanych danych oraz ekstrapolację. Każdy z etapów wykorzystuje funkcje właściwe dla swoich przeznaczeń, stąd też dostosowano przepływ pracy w sposób nie powodujący trudności analitykom nie pracującym na codzień w środowisku R.

Dodatkowym celem niniejszego dokumentu jest wykazanie korzyści wynikające z przeprowadzenia całej procedury w jednym środowisku analitycznym - R. Mając na uwadze trudności interpretacyjne wynikające oceny wizualnej przedstawiono narzędzia pozwalające oszacować efekt interwencji uzyskany po obserwacji wynikającej z badania oraz ocenić dobór właściwego modelu przy ekstrapolacji przeżycia. Dokument uzupełnia interaktywny dashboard ułatwiający implementację wyników na potrzeby raportu. 
Pamiętając o potrzebie walidacji całego procesu oraz korzyści wynikające z klarownego odtworzenia zastosowanej metodyki dokument do dashboardu została dołączona instrukcja w formie strony internetowej \footnote {https://rpubs.com/zamar/757283}. Opisane zostały wykorzystane pakiety oraz opisane zostały zastosowane funkcje. Z tego względu autorzy dokumentu zalecają odtworzenie procesu w sposób kompleksowy w środowisku R minimalizując możliwość popełnienia błędu przez migrację danych pomiędzy różnymi programami.


**Zastosowane narzędzia**

Procedura odczytu i rekonstrukcji indywidualnych danych pacjenta przeprowadzono przy użyciu pakietu _IPDfromKM_. Jest to metoda odtworzenia pierwotnych danych na podstawie opublikowanych krzywych przeżycia Kaplana-Meiera. Wtórną analizę przeżycia oraz wizualizację otrzymanych wyników przeprowadzono wykorzystując pakiet _survminer_. Jest to pakiet dedykowany do przeprowadzania analiz przeżycia zawierający funkcje służące do tworzenia gotowych do publikacji wykresów przeżycia.
Analizę pola pod zrekonstruowanymi krzywymi przeżycia Kaplana-Meiera przeprowadzono korzystając z pakietu _survRM2_ służącego do porównania ograniczonego średniego czasu przeżycia pomiędzy grupami. Natomiast do przeprowadzenia ekstrapolacji wykorzystano pakiet _survHE_ zawierający zestaw funkcji do analizy przeżycia w ekonomii zdrowia w oparciu o rozkłady parametryczne. 
Na potrzeby publikacji wyników wykorzystano pojedycze funkcje służące do przetwarzania danych i wykresów nie mających wpływu na przeprowadzony proces analityczny.


\pagebreak

## **Epidemiologia ostrej białaczki szpikowej (AML)**

Ostra białaczka szpikowa *(ang. acute myeloblastic leukemia, AML)* jest nowotworem złośliwym układu białkokrwinkowego o kodzie ICD-10: C92.0   \footnote{Statystyczna Klasyfikacja Chorób i Problemów Zdrowotnych. Rewizja dziesiąta 2008}. Źródłem danych epidemiologicznych są opublikowane wyniki badania Global Burden of Diseaase 2019 (GBD).
Badanie Global Burden of Diseaase 2019 jest globalnym badaniem prowadzonym przez Institute for Health Metrics and Evaluation *(IHME)* analizującym stopnień obciążenia zdrowotnego na skutek chorób oraz ich czynników. Wyniki badania dostarczają danych epidemiologicznych dotyczących chorobowości, zapadalności oraz umieralności. Uzyskane na podstawie badania dane są opublikowane w formie otwartego źródła danych, które dostępne są na stronie internetowej badania \footnote{http://ghdx.healthdata.org/}. 
Na podstawie danych GBD 2019 przedstawiono obciążenie zdrowotne dla pacjentów z AML. Dane dotyczą lat 1990–2019 i obejmują liczbę zgonów, zapadalność i zachorowalność. Analizie poddano liczbę zgonów *(deaths)*, liczbę nowych przypadków *(incidence)* oraz liczbę chorych *(prevalence)* w latach 1990-2019. Na wykresie przedstawiono pacjentów w 5-letnich grupach wiekowych 20-95+. Zaobserwować można, że największe obciążenie zdrowotne wśród pacjentów z AML dotyka osoby pomiędzy 65, a 69 rokiem życia. W ostatnich latach zaobserwowano wzrost liczby zgonów na skutek AML. W 2019 r. liczba zgonów wyniosła 214 osób. 

**Wykres. Epidemiologia zgonów z powodu ostrej białaczki szpikowej (AML) w Polsce w latach 1990-2019**

```{r echo=F,  message=F, warning=F, warning=FALSE}
gbd_agegbd_age %>%
    filter(metric_id == 1 & measure_id %in% c(1,5,6) & age_id > 10 & sex_id == "3" ) %>%
    ggplot(aes(year, val, color = age_name))+
    geom_line(aes(year))+
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=age_name), alpha = 0.5) +
    facet_grid(measure_name~age_name, scales = "free_y")+
    theme_ipsum()+
    theme(legend.position = "none")+
    theme( panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           panel.grid.minor.x = element_blank(),
           text = element_text(size = 6.5))+
    scale_x_continuous(n.breaks = 2, limits = c(1990,2019))+
    guides(x=guide_axis(angle = 90))+
    xlab(element_blank())+
    ylab(element_blank())
```

*Źródło:Opracowanie własne na podstawie badania GBD 2019*

\vspace{5mm}

Jednym ze wskaźników obciążenia zdrowotnego jest wskaźnik DALY *(ang. disability adjusted life-years)* \footnote{WHO methods and data sources for global burden of disease estimates 2000-2019. WHO 2020}. DALY jest sumą lat życia utraconych z powodu wczesnej śmiertelności *(ang. Years of Life Lost, YLL)* oraz lat życia skorygowanych niepełnosprawnością *(ang. Years Lived with Disability, YLD)*. Chcąc oszacować stosunek obciążenia wynikającego z powodu zgonu obliczono rozkład wskaźnika DALY wśród pacjentów z AML w latach 1990-2019. W latach objętych badaniem dostrzegalny jest wzrost obciążenia zdrowotnego związanego z ostrą białaczką szpikową. Zaobserwować można wysoki wpływ wskaźnika YLL na obciążenie zdrowotne mierzone wskaźnikiem DALY. Oznacza to, że dominującym czynnikiem wpływającym na obciążenie zdrowotne wśród osób z AML jest ten wynikający z lat utraconych z powodu wczesnego zgonu.

\vspace{5mm}


**Wykres. Rozkład obciążenia zdrowotnego według wskaźnika DALY dla ostrej białaczki szpikowej AML w Polsce w latach 1990-2019**

```{r echo=F,  message=F, warning=F, fig.height= 4, fig.width=10, warning=FALSE}
gbd_age %>% 
    filter(metric_id == 1 & measure_id %in% c(3,4)) %>%
    group_by(year, measure_name, measure_id) %>%
    summarise(val = round(sum(val),1)) %>%
    ggplot(aes(year, val, fill = measure_name))+
    geom_area(alpha = 0.7, color = "white", size = 1)+
    theme_classic()+
    labs(x = element_blank(),
         y = "DALYs")+
    theme(legend.position = "top",
          legend.title = element_blank())+
    scale_x_continuous(n.breaks = 20)+
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed",  size = 0.5),
          panel.grid.minor.x = element_line(linetype = "solid", size = 0.1))+
    scale_fill_manual(values=c("deepskyblue4","#E69F00"))+
    theme(text = element_text(size = 12))+
  theme(axis.line = element_line(size = 0.5))

```
*Źródło:Opracowanie własne na podstawie badania GBD 2019*

\vspace{5mm}

Znając przybliżony wpływ utraconych lat życia z powodu AML na obciążenie zdrowotne przeanalizowano rozkład wskaźnika YLL w 5-letnich grupach wiekowych pomiędzy analizowanymi latami. Intensywność koloru wskazuje na wyższy poziom wskaźnika YLL. Obciążenie zdrowotne wynikające z utraconych lat życia pacjentów z AML dotykała w głównej mierze osoby pomiędzy 50, a 84 rokiem życia. W ostatnich latach auważalny jest wzrost wskaźnika YLL wśród pacjentów w wieku 65-69 lat.


\vspace{5mm}

**Wykres. Rozkład wskaźnika YLD dla ostrej białaczki szpikowej AML w Polsce w latach 1990-2019**

```{r echo=F,  message=F, warning=F, fig.height= 5, fig.width=10, warning=FALSE}
gbd_age %>%
  filter(metric_name %in% c("Number", "Percent") & measure_id %in% c(3)) %>%
  group_by(year, measure_name, age_name, age_id) %>%
  summarise(val=sum(val), lower = mean(lower), upper = mean(upper)) %>%
  ggplot(aes(year, age_name, fill =val))+
  geom_tile(color= "white",size=0.1) +
  facet_grid(~measure_name)+
  scale_fill_viridis_b(name="YLD",option ="D")+
  theme_minimal(base_size = 10)+
  theme(legend.position = "right")+
  theme(plot.title=element_blank())+
  theme(strip.background = element_rect(colour="white"))+
  theme(axis.ticks=element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(text = element_text(size = 10))
```

*Źródło:Opracowanie własne na podstawie badania GBD 2019*



#r lek` 

<p>&nbsp;</p>


## **1. Krótka charakterystyka produktu**
 
** lek`** *(` substancja`)*  wskazanie`





<p>&nbsp;</p>

## **Schemat opisywania danych GBD w raportach TLI/TLK w części oceny niezaspokojonej potrzeby zdrowotnej**    

 

Produkt leczniczy **[lek]** stosuje się w terapii [rozpoznanie]. Zgodnie z badaniem GBD 2019, prowadzonym przez Institute for Health Metrics and Evaluation (IHME) przy Uniwersytecie Waszyngtońskim (Seattle, USA)  ten problem zdrowotny należy do kategorii [nazwa rozpoznania wg. GBD (ICD-10)]. W Polsce w 2019 roku z powodu tego rozpoznania odnotowano łącznie [liczba przypadków] nowych przypadków co stanowi [wskaźnik/100 tys. ludności], chorobowość związana z [nazwa rozpoznania wg. GBD] wynosiła odpowiednio [wskaźnik na 100 tys.] u obu płci.  

Obciążenie chorobowe (tj. utrata zdrowia wynikająca z sumy lat utraconych w wyniku przedwczesnego zgonu oraz lat spędzonych z niesprawnością spowodowaną tą przyczyną)  w Polsce w 2019 roku wynosiło:   

Wskaźnik łącznej utraty zdrowia DALY (Lata życia skorygowane niesprawnością; ang. Disability Adjusted Life-Years,), współczynnik na 100 tys.: 

Ogółem: XXX; 

Kobiety: XXX; 

Mężczyźni: XXX; 

W tym:  

Utracone lata życia z powodu przedwczesnego (Utracone lata życia; ang. Years of Life Lost, YLL),, współczynnik na 100 tys.: 

Ogółem: XXX; 

Kobiety: XXX; 

Mężczyźni: XXX; 

 

Wykres trendu dla DALY i YLL [do zbiorczego wygenerowania z bazy GBD2019]. 


Wagi niesprawności: Tabela 


Tabela dla wartości bezwzględnej DALY i YLL  

 

Wartości (bezwzględne) YLL oraz DALY szacowane dla roku 2019 dla pacjentów z [nazwa rozpoznania] 

\pagebreak


## **Rekontrukcja indywidualnych danych pacjentów przy użyciu algorytmu iteracyjnego**

\vspace{5mm}

Procedurę odczytu i odwzorowania przebiegu krzywych przeżycia Kaplana-Meiera oraz analizy z niej wynikające przeprowadzono na podstawie opublikowanego wykresu źródłowego pozyskanego z [Charakterystyki Produktu Leczniczego, aneks I, Daurismo 25, 100mg](https://www.ema.europa.eu/en/documents/product-information/daurismo-epar-product-information_en.pdf). Daurismo jest lekiem przeciwnowotworowym zawierającym substancję czynną glasdegib. Daurismo stosuje się w połączeniu z innym lekiem przeciwnowotworowym-cytarabiną (LDAC – Low Dose Cytarabine) w leczeniu dorosłych pacjentów z nowo zdiagnozowaną ostrą białaczką szpikową. 

**Wykres 1. Wykres źródłowy przedstawiający estymator przeżycia Kaplana-Meiera u pacjentów z AML - wykres źródłowy**

![](daurismo.png)

*Źródło: Charakterystyka produktu leczniczego Daurismo 25, 100mg; aneks I*



### **Procedura odczytu i odwzorowania przebiegu krzywych przeżycia - pakiet IPDfromKM**

   <br>
   
Procedura odczytu i rekonstrukcji indywidualnych danych pacjenta przeprowadzono przy użyciu pakietu _IPDfromKM_. Jest to metoda odtworzenia pierwotnych danych na podstawie opublikowanych krzywych przeżycia Kaplana-Meiera. W celu zwiększenia precyzji rekonstrukcji danych pakiet wykorzystuje algorytm iteracyjny. W procedurze odczytu współrzędnych i rekonstrukcji danych na poziomie pacjenta miały miejsce poniższe etapy:

 1. Ekstrakcja współrzędnych punktów z krzywych przeżycia Kaplana-Meiera
    
 2. Wstępne przetwarzanie odczytanych współrzędnych

 3. Rekonstrukcja indywidualnych danych pacjentów



**Tabela 1. Charakterystyka funkcji pakietu IPDfromKM**

|Funkcja    |Zbiór          |Zmienne                                                      |Etap                         |
|:------------|:----------------|:----------------------------------------------------------|:----------------------------|
|getpoints  |getpoints      |x, y                                                        |Oznaczanie współrzędne        |         
|preprocess |preprocessdat  |time, surv, id, interval                                    |Wstępne przetwarzanie         |         |preprocess |intervalIndex |interval, lower, upper, t.risk, n.risk                                 |Wstępne przetwarzanie |
|preprocess |inputdat      |time, surv                                                             |Wstępne przetwarzanie |
|getIPD     |IPD           |time, status, treat                                                    |Rekonstrukcja danych  |
|getIPD     |points        |time, surv, id, interval, risk censor,event, estsurv, diff             |Rekonstrukcja danych  |
|getIPD     |riskmat       |interval, lower, upper, trisk, nrisk, nrisk.hat, censor.hat, event.hat |Ocena rekonstrukcji   |  
|getIPD     |kstest        |D, p-value                                                             |Ocena rekonstrukcji   |
|getIPD     |precision     |RMSE, mean_abserror,  max_abserror                                     |Ocena rekonstrukcji   |
|survreport |survtime      |time, 0.95LCI, 0.95UCI                                                 |Czas przeżycia        |
|survreport |survprob      |Surv, SE, 0.95LCI, 0.95UCI                                             |Prawdopodobieństwo przeżycia |
*Źródło: Opracowanie własne*

\vspace{5mm}


### **Ekstrakcja współrzędnych i wstępne przetwarzanie odczytanych danych**

Wykres źródłowy został zaimportowany do środowiska R funkcją *getpoints()*. Jako argumenty funkcji wprowadzono odczytane z wykresu wartości maksymalne i minimalne dla osi x i y. Następnie zdefiniowano dwa obiekty reprezentujące krzywe przeżycia nadając im nazwy zgodne z zastosowanym schematem leczenia.W wyniku zastosowania funkcji obraz w postaci wykresu z krzywymi przeżycia został wczytany do okna wykresów w celu ręcznego oznaczenia współrzędnych punktów przebiegu krzywych przeżycia. Dla każdej z krzywych przeżycia oznaczono wartości minimalne i maksymalne osi x i y oraz współrzędne punktów poprzez kliknięcia na krzywej, dążąc do oznaczenia wszystkich punktów załamania krzywej. Wynikiem funkcji był dwukolumnowy zestaw współrzędnych punktów oznaczonych na krzywej przeżycia Kaplana-Meiera wymagany przez funkcję *preprocess()*.

---

```
Reconstruct individual patient data (IPD) from Scanned Kaplan-Meier(K-M) curves
    
input:  image file
output: coordinates extracted from a K-M curve 

getpoints_dau <- getpoints("daurismo.png",
                            x1 = 0, 
                            x2 = 40,
                            y1 = 0,  
                            y2 = 1)
```
 

Do wstępnego przetwarzania danych wykorzystano funkcję *preprocess()*. Dla zdefiniowanych wcześniej obiektów wprowadzono ręcznie dane dotyczące interwałów czasowych przedstawionych na osi y oraz dane dotyczące liczby pacjentów w zagrożeniu dla każdego interwału oraz początkową liczbę pacjentóww, liczbę pacjentów  w zagrożeniu oraz początkową liczbę pacjentów - 78 osób dla schematu Daurismo 100 mg + LDAC i 38 pacjentów dla schematu LDAC w monoterapii. 

---

```
Preprocess the read-in coordinates

input: getpoints_dau, number of risk, time risk, total patients
output: preprocessdat, intervalIndex, inputdat

preprocess_dau <- reprocess(getpoint_dau, 
                            trisk = c(0,5,10,15,20,25,30,35,40),   
                            nrisk = c(78,45,30,20,11,5,1,0,0),     
                            totalpts = 78,                        
                            maxy = 1)
```           


### **Rekonstrukcja indywidualnych danych pacjentów**

Rekonstrukcja pierwotnych danych odbyła się z wykorzystaniem funkcji *getIPD()*. Funkcja wykorzystuje algorytm iteracyjny wpływający na prezycję odtworzenia uzyskanych danych. Szczegółowy schemat algorytmu przedstawiono w tabeli. Głównym przedmiotem zainteresowania jest zbiór *$IPD* przedstawiający oszacowane dane na poziomie jednego pacjenta. Zbiór tworzą zmienne *surv* i *time* przedstawiające kolejno prawdopodobieństwo i czas wystąpienia zdarzenia. W przypadku prawdopodobieństwa dla dwóch grup terapeutycznych funkcja zwraca również indeks przypisanej grupy terapeutycznej w postaci zmiennej *treat*.

---

```
Reconstruct individual patient data (IPD) from Scanned Kaplan-Meier(K-M) curves
input: preprocess_dau, number of ID
output: $IPD, $Points, $riskmat $kstest, $precission

getIPD_dau <- getIPD(prep = preprocess_dau,   
                        armID = 0) 
```           

**Ryc. Schemat algorytmu iteracyjnego stosowanego w procesie rekonstrukcji danych IPD**

![](algorytm.png){ width=50% }

*Źródło: Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves. P. Guyot 2012*




\vspace{5mm}

### **Ocena dokładności rekonstrukcji danych**


W celu oceny dokładności procesu rekonstrukcji przeprowadzono test Kołmogorowa-Smirnowa korzystajc z fukncji *summary()* podsumowującej wyniki dla uzyskanego obiektu *getIPD*. Zastosowany Test Kołogomorowa-Smirnowa dla dwóch prób jest nieparametrycznym testem oceniającym zgodność badanego rozkładu z rozkładem referencyjnym. \footnote{Biostatistics. A Foundation for Analysis in the Health Sciences 10th Edition. W. Wayne 2018.}. Hipoteza zerowa stanowi, że testowane rozkłady są identyczne. Miarą różnicy pomiędzy rozładami jest statystyka D. 
Obliczono pierwiastkowy błąd średniokwadratowy *(ang. RMSE, Root Mean Squared Error*), średni błąd bezwzględny *(ang. MAE, mean absolute error)* i maksymalny błąd bezwzględny *(ang. max absolute error)*, mierzące różnice między oszacowanym i odczytanym prawdopodobieństwem przeżycia. Pierwiastkowy błąd średniokwadratowy oraz średni błąd bezwzględny  informuje o wielkości zachodzących różnic pomiędzy testowanymi rozkładami. Przy właściwie przeprowadzonej analizie wartości RMSE oraz MAE bowinny być zbliżone. Wartości bliższe zeru są bardziej skoncentrowane wokół wartości wynikających z rzeczywistego rozkładu, zatem porządanym jest uzyskanie małych wartości tych błędów. 
Uzyskana w wyniku zastosowania testu Kołomogorowa-Smirnowa wartość *p* powyżej 0.05 nie dostarcza dowodow na istotne rozbierzności pomiędzy badanymi rozkładami. Statystyka *D* oraz małe wartości uzyskanych błędów wskazują na brak istotnych różnic między rozkładami. Na podstawie przyjętych założeń przyjęto, że pomiędzy testowanymi rozkładami nie zachodzą istotne różnice.

---

```{r echo=F , eval=F, comment= "", fig.width=10}
summary(getIPD_dau)

```

```
summary(getIPD_dau)

The function read in  112 points from the K-M curve, and 7 numbers of patients 
at risk. Thus the read-in points are divided into  7 time intervals. 

 interval lower upper trisk nrisk nrisk.hat censor.hat event.hat
        1     1    51     0    78        78          4        29
        2    52    80     5    45        45          0        15
        3    81    95    10    30        30          1         9
        4    96   102    15    20        20          6         3
        5   103   107    20    11        11          5         1
        6   108   111    25     5         5          2         2
        7   112   112    30     1         1          1         0

The root-mean-square error between estimated and read-in survival probabilities
is 0.007 
The mean absolute error between estimated and read-in survival probabilities
is 0.005
The max absolute error between estimated and read-in survival probabilities
is 0.014 

The Kolmogorov-Smirnov test: 
Test statistics D= 0.02679       p-value= 1 

Null hypothesis: distributions of the read-in and estimated survival 
probabilities are the same.
```



\vspace{5mm}



Celem weryfikacji przyjętych założeń dokonano wizualnej oceny przebiegu zrekonstruowanych rozkładów przeżycia porównano liczbę pacjentów uzyskaną w procedurze rekonstrukcji z liczbą pacjentów wprowadzoną ręcznie liczbę pacjentów w zagrożeniu. 
Wykres *Compare KM curves* reprezentuje rozkład prawdopodobieństwa wynikający z przebiegu punktów oznaczonych na wykresie źródłowym oraz oszacowany rozkład uzyskany w procedurze rekonstrukcji. Podobieństwo pomiędzy rozkładami świadczy o wyższej precyzji rekontrukcji danych. W analogiczny sposób należy interpretować dane na wykresie *Compare numbers at risk* przedstawiający oszacowaną liczbę pacjentów z ręcznie wprowadzoną liczbą pacjentów narażonych na ryzyko. 
Wykres *difference between estimated and read-in survival probabilities* przedstawia różnice pomiędzy danymi zrekonstruowanymi, a danymi z oznaczonych współrzędnych. 
Przebieg współrzędnej 0 oznaczony czerwoną linią można interpretować jako rozkład *referencyjny*, natomiast czarne punkty przedstawiają rozkład prawdopodobieństwa wynikający z oznaczonych współrzędnych. Wartości bliższe czerwonej linii świadczą o braku istotnych różnic pomiędzy rozkładami. Wykres dla obiektu *getIPD* uzyskujemy za pomocą funkcji *plot()*.



*-liczba osób narażonych na ryzyko*

**Wykres 1. Porównanie rozkładu prawdopodobieństwa wynikającego z oznaczonych wspołrzędnych z rozkładem oszacowanym w procedurze rekonstrukcji**



```{r echo = F}
plot(getIPD_dau)
```

*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\vspace{5mm}

## **Wtórna analiza przeżycia i wizualizacja wyników**


\vspace{5mm}




Korzyści wynikające z odtworzenia danych do poziomu pacjenta pozwalają oszacować pełniejszą ocenę efektu zastosowanej interwencji. Wtórna analiza przeprowadzona na podstawie odtworzonych danych pacjentów dotyczy odtworzenia przebiegu krzywych Kaplana-Meiera oraz funkcji skumulowanej zapadalności wraz z 95-procentowymi przedziałami ufności. Oszacowano medianowy czas przeżycia. Prawdopodobieństwo zgonu mierzonego jako proporcje hazardu uzyskano przy użyciu modelu proporcjonalnego hazardu Coxa. Do wizualizacji wyników wykorzystano funckję *survreport()*. Poza zrekonstruowanymi danymi IPD argumenty funkcji stanowi liczba interwałów czasowych dla prawdopodobieństwa przeżycia *(interval*) oraz prawdopodobieństwo przeżycia dla trzeciego, drugiego i pierwszego kwartylu pacjentów *(s)*.
*estymacji prawdopodobieństwa przeżycia, skumulowany hazard (funkcja skumulowanej zapadalności), proporcje hazardu, oraz ryzyka skumulowanego*

\vspace{5mm}

---

```
Survival analysis on the reconstructed IPD

input: $IPD, number of arms, intervals, quantiles
output: $survtime, $survprob

survreport_dau <- survreport(ipd1 = getIPD_dau$IPD,
                             ipd2 = getIPD_komparator$IPD,
                             arms = 2,             
                             interval = 6,
                             s = c(0.75.0.5,0.25)
```

\vspace{5mm}



Utworzone zmienne *surv*, *time* oraz *treat* stanowią podstawowy zakres danych do budowy modeli właściwych dla analiz przeżycia. W większości pakietów dedykowanych do przeprowadzenia tego typu analiz w środowisku R modele budowane są na obiektach *survival*, gdzie dane wejściowe obejmują zmienne w standardzie otrzymanym w procedurze rekonstrukcji. Stąd też po zrekonstruowaniu danych dobrą praktyką jest utworzenie formuły, na podstawie której budowane będą modele w dalszym procesie analitycznym. 
Dzięki zdefiniowanej formule nie będzie konieczne przypisywanie zmiennych oraz źródła danych przy każdorazowym budowaniu modelu, co może zredukować możliwość popełnienia błędu związanego ze składnią kodu. Dodatkowo, zastosowanie formuły pozwoli zaoszczędzić czas oraz poprawi czytelność kodu. Formułę tworzymy przez przypisanie zmiennych zgodnie z formatem wymaganym przez stosowane funkcje. Indeks grupy terapeutycznej (zmienna *treat*) przekształcony został na typ czynnikowy (nazywany inaczej kategorycznym) przy użyciu funkcji *as.factor()*. Zastosowana funkcja przekształca zmienne liczbowe (indeks 0 i 1) na zmienne jakościowe.  \footnote{Mając na celu zapewnienie odpowiedniej czytelności funkcji i wyników w dalszej części publikacji oraz załączonym dashboardzie zmiennej *treat* nadano nazwę *group*, a indeksy grup przypisano do poziomów "daurismo" i "komparator"}. 


---

```
time = time
status = status
group.by = treat
data = ipd_daurismo
formula = Surv(time, status) ~ as.factor(group)

Create a Survival Object

Call: survfit(formula = formula)

                             n events median 0.95LCL 0.95UCL
as.factor(group)=daurismo   78     59   8.36    6.61   12.35
as.factor(group)=komparator 38     36   4.31    1.97    6.54
```



### **Estymator Kaplana-Meiera**

Na podstawie odtworzonych danych na poziomie pacjentów utworzono dwa wykresy prezentujące krzywe przeżycia Kaplana-Meiera oraz funkcję skumulowanej zapadalności. W stosunku do wykresu źródłowego nowo otrzymano estymację przedziałową z oszacowanymi 95-procentowymi przedziałami ufności. Zastosowana funkcja wyświetliła wynik testu log-rank oceniający istotność różnić pomiędzy krzywymi przeżycia. Hipoteza zerowa zakłada brak różnic w rozkładzie przeżycia pomiędzy krzywymi reprezentującymi grupy terapeutyczne. W wyniku zastosowanego testu uzyskano wartość p mniejszą niż 0,05. Oznacza to, że nie mamy dowodów any przyjąż h0 stanowiącej, że pomiędzy grupami zachodzą różnice.

\vspace{5mm}


**Wykres 2. Krzywe przeżycia Kaplana-Meiera oszacowany na podstawie zrekonstruowanych danych IPD**

```{r, echo = F, message=F, eval = T,fig.height=5, fig.width=10, warning=F}
time = ipd_daurismo$time
status = ipd_daurismo$status
group = ipd_daurismo$group

fit <- survfit(Surv(time, status) ~ group, data = ipd_daurismo)
ggsurvplot(fit,   
data = ipd_daurismo, 
    pval = TRUE,   
pval.method = T,
    conf.int = TRUE,          
    conf.int.style = "ribbon",   
    xlab = "Time",
    ylab = "Survival propability",
    break.time.by = 5, 
    ggtheme = theme_pubclean(),  
    surv.median.line = "hv",  
    palette = c("darkslategray4","orange4"),
    xlim = c(0, 40))
```
*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\vspace{5mm}

Zastosowana funkcja *survreport()* poza wizualizacją opisanych wyżej estymatorów zwraca dwa zbiory danych - prawdopodobieństwo przeżycia dla zdefiniowanych w funkcji punktów czasowych oraz oszacowany czas przeżycia dla wskazanego udziału pacjentów. Dostęp do danych uzyskujemy wywołaniem zbioru z obiektu utworzonego funkcją *survreport()* przy użyciu znaku dolara w stosunku do ramienia badania *(arm)*: *`survreport_dau$arm1$survtime`*

Czas przeżycia oszacowano dla trzeciego, drugiego i pierwszego kwartylu pacjentów. Uzyskane wyniki przedstawiono dla łącznej liczby pacjentów (*overall*) oraz w poszczególnych grupach terapeutycznych. Mediana czasu przeżycia grup była wyższa wśród pacjentów otrzymujących leczenie w schemacie daurismo 100mg + LDAC i wyniosła  8.36 miesiąca [6.61-12.35 CI]. Mediana dla grupy otrzymującej cytarabinę w monoterapii wyniosła 4.31 miesiąca [1.97-6.54 CI]. 


**Tabela. Czas przeżycia dla trzeciego, drugiego i pierwszego kwartylu pacjentów**
```{r echo = F, message=F, warning=F}

list(survfit(Surv(time, status) ~ 1, ipd_daurismo),
     survfit(Surv(time, status) ~ group, ipd_daurismo)) %>%
    tbl_survfit(statistic = "{estimate}", prob = c(0.75, 0.50,0.25), label_header = '**{prob*100} % kwantyl**') %>%
    as_flextable()

```
*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\vspace{5mm}

W analizie przestawiono wyniki oszacowanego prawdopodobieńśtwa przeżycia w półrocznych punktach czasowych. Uzyskane wyniki przedstawiono jako przeżycie całkowite (*overall*) dla wszystkich pacjentów oraz prawdopodobieństwo przeżycia dla obu grup łącznie oraz prawdopodobieńśtwo przeżycia dla poszczególnych grup łącznie. Łączne przeżycie w pierwszym roku wyniosło 29% oraz 14%  po upływie kolejnych 12 miesięcy. Dostodobnie jak w przypadku czasu przeżycia ęp do zbioru danych uzyskujemy analogicznie jak w przypadku zbioru *time*: *`survreport_dau$arm1$survprob`*


**Tabela. Prawdopodobieństwo przeżycia w 6-miesięcznych punktach czasowych dla zrekonstruowanych danych IPD**
```{r echo = F, message=F, warning=F}

list(survfit(Surv(time, status) ~ 1, ipd_daurismo),
     survfit(Surv(time, status) ~ group, ipd_daurismo)) %>%
    tbl_survfit(statistic = "{estimate}", times = seq(6,30,6), label = "Grupa", label_header = "**{time} miesiąc**") %>%
  as_flextable()

```
*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\vspace{5mm}


*- proporcje hazardu
- czym jest stopa hazardu - do częstośći zgonu w danym czasie
- stop określa cęstość zgonów w zestawieniu dla grupy w danym czasie
- HR - proporcje tych stóp
- częstość nie prawdopodobieństwo*

### **Model proporcjonalnego hazardu Coxa**

Model proporcjonalnego hazardu Coxa określa stosunek częstości zgonów pomiędzy grupami w danym czasie wystąpienia zdarzenia zależnego jest od zmiennych objaśniającycych - w tym przypadku zastosowanego schematu leczenia. W celu oceny wpływu ocenianej interwencji na prawdopodobieństwo wystąpienia zgonu na podstawie zrekostruowanych danych oszacowano współczynik ryzyka w postaci hazardu względnego *(HR - hazard ratio)*. W środowisku R do modelowania proporcjonalnego hazardu Coxa wykorzystano funkcję *coxph()* zaimplementowaną do pakietu *survminer*. 

\vspace{5mm}





```{r echo = F, eval = F}
coxph(Surv(time, status) ~ relevel(factor(group),
      ref = "komparator"), 
      data = ipd_daurismo)
```

-----

\newpage


```
Fit Proportional Hazards Regression Model

                                   
Call:
coxph(formula = Surv(time, status) ~ relevel(factor(group), 
ref = "komparator"))

                coef    exp(coef) se(coef)   z      p
groupdaurismo -0.8207    0.4401   0.2189  -3.749 0.000177

Likelihood ratio test=13.07  on 1 df, p=0.0003003
n= 116, number of events= 95                                    
                                
```
-----




\vspace{5mm}

Proporcjonalny hazard stanowi o stosunku zagrożenia pomiędzy ocenianą interwencją, a komparatorem. Wartość hazardu względnego poniżej 1 wskazuje na zmniejszone ryzyko wystąpienia zgonu, natomiast wartość powyżej 1 informuje o wzroście ryzyka. 
Hazard względny na poziomie 0.44 oznacza, że prawdopodobieństwo wystąpienia zgonu jest 0.44 razy niższa na jednostkę czasu w populacji referencyjnej w grupie kontrolnej. Inaczej mówiąc, zastosowanie ocenianej interwencji zmniejsza ryzyko zgonu o 56%. Oszacowaną funkcję skumulowanego hazardu oraz wartość logarytmiczną stopy hazardu uzyskano przy zastosowaniu funkcji _survreport()_

*- zmienić funcję coxph, relevel 
- hazard częstość nie prop - stopa
- resztki
- coxph
- nie prrb KM tylko np krzywe
oraz skumulowane ryzyko*

**Wykres 3. Funkcja skumulowanego hazardu oszacowana na podstawie zrekonstruowanych danych IPD**

```{r, echo = F, fig.height=5, fig.width=10, warning=F}
cumhaz <- ggsurvplot(fit,    
                     fun = "cumhaz",
                     conf.int = TRUE,          
                     conf.int.style = "ribbon",   
                     xlab = "Time",
                     ylab = "Survival propability",
                     break.time.by = 5, 
                     ggtheme = theme_pubclean(),  
                     palette = c("darkslategray4","orange4"),
                     xlim = c(0, 40))

cumhaz$plot+
  annotate("text",
           x = 7, y = 1,
           vjust = 1, hjust = 1,
           label = "logHR = 0.821",
           size = 6)
```

*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*



```{r echo=F, eval=F}
broom::tidy(res.cox,
            exp = T) %>%
    select(-term) %>%
    kable(col.names  = c("Hazard ratio", "SE", "Statistic", "p-value"), digits = 5, format = "pipe")
```


```{r echo=F, eval = F, message=F, warning=F}
gtsummary::tbl_regression(res.cox, exp = F) %>%
  as_flextable()
```

```{r echo=F, eval = F,message=F, warning=F}
gtsummary::tbl_regression(res.cox, exp = T) %>%
  as_flextable()
```

\vspace{5mm}

Porównując stopę hazardu oszacowanego na podstawie zrekonstruowanych danych z wartością wynikającą ze źródłowych danych można zaobserwować różnice. Związane jest to z właściwością samego współczynnika, który jest średnią ważoną dla całego okresu obserwacji \footnote{https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-12-9}. 
Stąd też współczynnik HR może różnić się od oryginalnego pomimo korzystnego dopasowania przebiegu estymatorów Kaplana-Meiera oraz mediany przeżycia, które są estymatorami punktowymi. Co istotne, wskaźnik HR pochodządzy z wykresu źródłowego mieści się w granicach dolnego przedzialu ufności wskaźnika uzyskanego przy wykorzystaniu zrekonstruowanych danych.




```{r echo = F, eval=F,, fig.height= 3}
#**Wykres. Porównanie hazard względnego oszacowanego na podstawie zrekonstruowanych danych oraz hazardu względnego dostępny na wykresie źródłowym**
row <- data.frame("term" = "Hazard ratio \n0.46  [0.3 - 0.72]", "model" = "Model 2ss", "estimate" = 0.463, conf.low = 0.299, conf.high = 0.717)
coef_map <- c('groupkomparator' = 'Hazard ratio \n0.82 [0.39 - 1.2]')
background <- list(geom_vline(xintercept = 0.463,color = 'deepskyblue4'),
                   annotate("rect", alpha = .1,
                            xmin = 0.299, xmax = 0.717, 
                            ymin = -Inf, ymax = Inf),
                   geom_point(aes(y = term, x = estimate), alpha = 0.3, 
                              color = "darkslategray4", shape = 'square'))
modelplot(res.cox, linetype = 'dotted', background = background, coef_map = coef_map, add_rows = row) + 
    scale_color_manual(values = c("orange4","dodgerblue4"),
                       labels = c("Zrekonstruowane dane IPD","Wykres źródłowy"))+
  theme(legend.position="top")
#*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

```


\vspace{5mm}


### **Analiza pola pod krzywymi przeżycia Kaplana-Meiera**

<br>

*posłużyła do analizy różnic pomiędzy grupami HR jest relatywnym wskaźnikiem, rmst okrećla całość dopisać TAU*

W związku z niepewnością dotyczącą oszacowania hazardu względnego jako alternatywną metodę zastosowano analizę pola pod krzywymi Kaplana-Meiera z wykorzystaniem ograniczonego średniego czasu przeżycia.
Analiza pola pod krzywymi posłużyła do analizy różnic pomiędzy grupami. Ograniczony średni czas przeżycia (RMST) jest miarą oszacowania efektu leczenia. W odróżnieniu od stopy hazardu, która jest wskaźnikiem relatywnym, ograniczona średnia szacuje czas przeżycia dla całego rozkładu. Mierzy ona średni czas przeżycia od początku obserwacji (początku obserwacji) do określonego punktu definiowanego jako czas obcięcia *(truncation time, tau)*. Efekt leczenia stanowi różnicę pomiędzy średnim czasem przeżycia pomiędzy dwoma badanymi grupami.^5^ 
Ograniczony średni czas przeżycia definiuje pole znajdujące się poniżej krzywej przeżycia Kaplana-Meiera, natomiast obszar powyżej krzywej reprezentuje ograniczony średni czas utracony (RMTL) i przedstawia czas utracony do ustalonego czasu obcięcia.
Dane wejściowe obejmują zrekonstruowane dane w postaci zmiennych: time, status oraz treat. 

\vspace{5mm}


---

```
Comparing restricted mean survival time

rmean_dau <- 
      rmst2(time = ipd_daurismo$time,  
                   ipd_daurismo$status,             
                   ipd_daurismo$treat)

The truncation time, tau, was not specified. Thus, the default tau  20.16  is used. 

Restricted Mean Survival Time (RMST) by arm 
              Est.    se lower .95 upper .95
RMST (arm=1) 5.310 0.797     3.747     6.873
RMST (arm=0) 9.967 0.833     8.334    11.600

Restricted Mean Time Lost (RMTL) by arm 
               Est.    se lower .95 upper .95
RMTL (arm=1) 14.848 0.797    13.285    16.411
RMTL (arm=0) 10.191 0.833     8.558    11.824

Between-group contrast 
                       Est. lower .95 upper .95 p
RMST (arm=1)-(arm=0) -4.657    -6.917    -2.397 0
RMST (arm=1)/(arm=0)  0.533     0.380     0.746 0
RMTL (arm=1)/(arm=0)  1.457     1.203     1.765 0
```
---


\vspace{5mm}



**Wykres. Ograniczony średni czas przeżycia - RMST**

```{r echo = F, eval=T}
plot(rmean_dau, 
     xlab="Czas w miesiącach",             
     ylab="Prawdopodobieństwo przeżycia",
     col = "lightgrey",
     col.RMST = "darkslategray4",
     col.RMTL = "steelblue",
     density = 75,
     angle = 10)
```

```{r echo=F, eval=F, message=F, warning=F, fig.height= 6, fig.width=10, warning=FALSE}

rbind(data.frame("group" = "daurismo", getIPD_dau$Points),
      data.frame("group" = "komparator", getIPD_komparator$Points)) %>%
  filter(time < 19.99) %>%
  mutate("rmst" = surv) %>%
  mutate("rmtl" = 1-surv) %>%
  ggplot(aes(time, surv, fill = group)) +
  geom_area(position = "identity", color = "white", size = 2)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "deepskyblue4", color = "white"))+
  coord_cartesian(xlim =c(0, 20), ylim = c(0, 1), expand = F)+
  theme(legend.position = "none")+
    scale_fill_manual(values=c("#E69F00", "#E69F00")) + 
    facet_grid(~group)+
    theme(panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x  = element_blank())+
    geom_label(label = "RMTL", y = 0.75, x = 15, size = 10,color = "white", fill = "deepskyblue4")+
    geom_label(label = "RMST", y = 0.2, x = 4, size = 10, color = "white", fill = "#E69F00")+
    ylab("Survival") + 
    xlab("Survival")
    
```

*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\vspace{5mm}



**Tabela. Różnica RMST, iloraz RMST oraz współczynnik RMTL pomiędzy grupami**
```{r echo = F}
rmean_dau$unadjusted.result %>%
    kbl(align = "c",
        longtable = T,
        booktabs = T,
        digits = c(2,2,2,5),
        col.names = c("Time", "LCI 0.95", "UCI 0.95", "p-value")) 
```
*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\pagebreak

## **Ekstrapolacja**


Dokładne oszacowanie korzyści w zakresie przeżycia wynikające z zastosowania ocenianej interwencji utrudnione jest z powodu ograniczonego czasu badania. W celu oszacowania pełniejszych korzyści konieczna jest ekstrapolacja funkcji przeżycia poza horyzont czasowy wynikający z badania. 
Istnieje wiele metod przeprowadzania ekstrapolacji, które mogą prowadzić do różnych szacunków przeżycia. Dobór odpowiedniego rozkładu ma istotny wpływ na oszacowaną funkcję przeżycia, na podstawie której szacowane są korzyści ekonomiczne wynikające z zastosowanej interwencji \footnote{Survival Analysis for Economic Evaluations Alongside Clinical Trials—Extrapolation with Patient-Level Data: Inconsistencies, Limitations, and a Practical Guide. Latimer 2013}

W związku zapewnieniem właściwego podejścia do  ekstrapolacji National Institute for Health and Care Excellence (NICE) opublikował dokument wsparcia technicznego dotyczący właściwego doboru metod przy przeprowadzania parametrycznej ekstrapolacji funkcji przeżycia w procesie oceny technologii medycznych ^2^. Ekstrapolację przeprowadzodo w oparciu o zalecenia zawarte w dokumencie.
Do najczęstszych metod szacowania średniej długości przeżycia w analizach HTA należą: modelowanie przy użyciu rozkładów parametrycznych (zastosowane w 71% raportach HTA), proporcjonalny hazard (19%), ograniczony średni czas przeżycia (38%) oraz szacowanie na podstawie zewnętrznych źródeł danych (9%). Pozostałe metody określone jako hybrydowe miały zastosowanie w 4% raportów^2^.


**Tabela. Rozkłady parametryczne stosowane w raportach HTA**

|Nazwa rozkładu parametrycznego     |Udział w raportach HTA|
|:------------------------------|:-----------------------------:|
|Weibulls     |51% |
|Expotential  |44% |
|Gompertz     |13% |
|Log-logistic |20% |
|Log normal   |13% |
|Log-logistic |4%  |
|Pozostałe    |2%  |
*Źródło: Opracowanie własne na podstawie  NICE DSU Technical Support Document 14*

\vspace{5mm}

### **Dobór rozkładu parametrycznego**

W przeprowadzonej ekstrapolacji funkcji przeżycia zastosowano główne rozkłady parametryczne stosowane w analizach HTA: rozkład wykładniczy, rozkład Weibulla, rozkład gamma, rozkład logarytmiczny normalny, rozkład log-logistyczny oraz rozkład Gompertza. Przy doborze odpowiedniego rozkładu parametrycznego posłużono się kryterium informacyjnym Akaikego *(ang. Akaike Information Criterion, AIC)*. Przyjęto założenie, że niższa wartość AIC stanowi o lepszym dopasowaniu rozkładu. Zgodnie z zaleceniami NICE zastosowano ten sam typ rozkładu dla obydwu analizowanych grup. Na podstawie przeprowadzonej analizy najniższą wartość AIC uzyskano przy modelu z rozkładem wykładniczym. Jednocześnie jest to rozkład o najniższej wartości logarytmu wiarygodności *(log-likelihood)*.




**Tabela. Porównanie modeli parametrycznych zastosowanych w ekstrapolacji**

```{r, echo = F, eval=F}
mods <- c("exp", "weibull", "gamma", "lnorm", "llogis", "gompertz")    

formula <- Surv(time, status) ~ as.factor(group)  

m1 <- fit.models(formula,              
                 data = ipd_daurismo,  
                 distr = mods)         
  
  cm <- c('as.factor(group)komparator'  = 'group komparator',
          'AIC' = 'AIC',
          'BIC' = 'BIC',
          'log-likelihood' = 'log-likelihood',
          'DF' = 'DF')
  
  gm <- list(
      list("raw" = "AIC", "clean" = "AIC", "fmt" = 2),
      list("raw" = "BIC", "clean" = "BIC", "fmt" = 2),
      list("raw" = "df", "clean" = "DF", "fmt" = 0),
      list("raw" = "logLik", "clean" = "log-likelihood", "fmt" = 2))
  
  modelsummary(m1$models, 
               statistic = NULL,
               coef_omit = "rate|shape|scale|sdlog|meanlog|censored|events|N",
               gof_map = gm,
               coef_map = cm,
               output = "kableExtra") %>%
      kable_paper(full_width = T)

```

```{r, echo = F}
mods <- c("exp", "weibull", "gamma", "lnorm", "llogis", "gompertz")    

formula <- Surv(time, status) ~ as.factor(group)  

m1 <- fit.models(formula,              
                 data = ipd_daurismo,  
                 distr = mods)         
  
  cm <- c('rate' = 'rate',
          'shape' = 'shape',
          'scale' = 'scale',
          'sdlog' = 'SD log',
          'meanlog' = 'mean log',
          'as.factor(group)komparator'  = 'group komparator',
          'AIC' = 'AIC',
          'BIC' = 'BIC',
          'log-likelihood' = 'log-likelihood',
          'DF' = 'DF')
  
  gm <- list(
      list("raw" = "AIC", "clean" = "AIC", "fmt" = 2),
      list("raw" = "BIC", "clean" = "BIC", "fmt" = 2),
      list("raw" = "df", "clean" = "DF", "fmt" = 0),
      list("raw" = "logLik", "clean" = "log-likelihood", "fmt" = 2))
  
  modelsummary(m1$models, 
               statistic = NULL,
               coef_omit = "censored|events|N",
               gof_map = gm,
               coef_map = cm,
               output = "kableExtra") %>%
      kable_paper(full_width = T)

```

*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\vspace{5mm}

```{r, eval = F, echo = F}


cm <- c('as.factor(group)komparator'  = 'group komparator')

gm <- list(
    list("raw" = "df", "clean" = "df", "fmt" = 2),
    list("raw" = "AIC", "clean" = "AIC", "fmt" = 2),
    list("raw" = "BIC", "clean" = "BIC", "fmt" = 2),
    list("raw" = "logLik", "clean" = "log-likelihood", "fmt" = 2))

modelsummary(m1$models, 
             statistic = NULL,
             coef_omit = "rate|shape|scale|sdlog|meanlog|censored|events|N",
             gof_map = gm,
             coef_map = cm,
             output = "kableExtra") %>%
    kable_paper(full_width = T)

```



\vspace{5mm}

### **Wizualna ocena dopasowania rozkładów parametrycznych**


Na wykresie przedstawiono 3 rozkłady o najniższym kryterium AIC - rozkład wykładniczy, rozkład log-logistyczny oraz rozkład Weibulla. Na podstawie oceny wizualnej wykluczono rozkład log-logistyczny. Kryterium AIC dla rozkładu Gompertza było wyższe o 1.3. Zgodnie z metodologiczną zasadą ekonomii myślenia, tzw. *brzytwą Ockhama" ekstrapolację funkcji przeżycia przeprowadzono przy użyciu rozkładu wykładniczego charakteryzującego się mniejszą liczbą parametrów \footnote {Fatyga B. Praktyki badawcze, Warszawa 2015}
Dobór odpowiedniego modelu wiąże się właściwym dla niego rozkładem. Zastosowanie niewłaściwego rozkładu może skutkować nieprezycyjną oceną ekonomiczną w dalszym horyzoncie czasu. Dlatego wybór właściwego rozkładu powinien być poprzedzony opinią ekspertów specjalizujących się danej dziedzinie oraz uzyskaniem informacji na podstawie dostępnej literatury. 
Przy ocenie wizualnej warto również skorzystać z interaktywnych narzędzi ułatwiających precyzyjniejsze dopasowanie przebiegu rozkładów. Przykład takiego narzędzia w postaci interaktywnego wykresu utworzonego przy użyciu pakietu **plotly** zaprezentowano w załączonym dashboardzie. 


```{r echo = F}
      plot(m1,
     mods = c(1,5,6),
     add.km = T,
     lab.profile = c("Daurismo","Komparator"),
     ylab = "Prawdopodobieństwo \nprzeżycia",
     xlab = "Czas (miesiącach)")+
  theme_survminer()+
  theme(legend.direction = "horizontal")+
  theme(legend.text = element_text(size = 10),
        legend.title = element_blank())
 
```

*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*
  
\vspace{5mm}

```{r, eval = F, echo=FALSE}

df <- c(m1$models$Exponential$npars, m1$models$`Weibull (AFT)`$npars, m1$models$Gamma$npars,m1$models$`log-Normal`$npars, m1$models$`log-Logistic`$npars, m1$models$Gompertz$npars)
aic <- data.frame(Distribution, AIC = m1$model.fitting$aic, BIC = m1$model.fitting$bic, DF = df)

p_ex <- flexsurvreg(formula, 
            data = ipd_daurismo,
            dist = "exp")%>%
  plot(ci = T,
       col.obs = "red",
       lwd.obs = 2,
       col = "darkslategray",
       cl = 0.95, 
       col.ci = "gray7",
       lwd.ci = 0.8,
       lty.ci = "dotted",
       lty = "dashed",
       lwd = 3,
       est = T,
       xlab = "Expotential",
       bty = "n",
       xaxt="n",
       yaxt = "n")



aic %>%
    arrange(AIC) %>%
    kbl(digits = 2, booktabs = T, centering = T) %>%
    kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
    footnote(general = "", 
             footnote_as_chunk = T, title_format = c("italic"),
             general_title = "Źródło: Opracowanie własne na podstawie zrekonstruowanych danych") 


```


\vspace{5mm}


### **Ekstrapolacja**


Przy ekstrapolacji przyjęto horyzont czasowy wynoszący 100 miesięcy (5 lat). Po upływie 60 miesięcy zaobserwować można wypłaszczenie ekstrapolowanej funkcji do oszacowanego prawdopodobieństwa przeżycia wynoszącego 0.

\vspace{5mm}



**Wykres. Parametryczna ekstrapolacja przeżycia na podstawie zrekonstruowanych danych**

```{r echo = F, eval=T, fig.width= 10}
plot(m1, mods = c(1), add.km = T,
           nsim = 1000, t = seq(.1, 100), 
           lab.profile = c("Daurismo","Komparator"),
           annotate = T,
           legend.text = element_text(size = 12),
           legend.title = element_text(size = 13, face = "bold"),
           ylab = "Prawdopodobieństwo \nprzeżycia",
           xlab = "Czas (miesiącach)")
```

*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*

\vspace{5mm}

Propabilistyczna analiza wrażliwości *(ang. Probabilistic sensitivity analysis, PSA)* pozwoliła ocenić wizualnie uzyskaną funkcję przeżycia dla 1000 możliwych symulacji. Uzyskane modele są ze sobą zbierzne.

### **Propabilistyczna analiza wrażliwości**

```{r echo = F, eval=T, fig.width= 10}
psa <- psa <- make.surv(fit = m1, nsim = 1000, t = seq(.1, 100))
psa.plot(psa, xlab = "Czas (w miesiącach)", ylab = "Prawdopodobieństwo \nprzeżycia", 
         alpha = 0.2, col = c("dark grey", "black"))
```

*Źródło: Opracowanie własne na podstawie danych zrekonstruowanych przy użyciu pakietu IPDfromKM*


\vspace{5mm}


------
#### **Bibliografia**


^1^

^2^ [NICE DSU Technical Support Document 14: undertaking survival analysis for economic evaluations alongside clinical trials—extrapolation with patient-level data. Latimer 2011](http://nicedsu.org.uk/wp-content/uploads/2016/03/NICE-DSU-TSD-Survival-analysis.updated-March-2013.v2.pdf)

^3^ [Survival Analysis for Economic Evaluations Alongside Clinical Trials—Extrapolation with Patient-Level Data: Inconsistencies, Limitations, and a Practical Guide. Latimer 2013](https://journals.sagepub.com/doi/10.1177/0272989X12472398?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed)

^4^ https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-12-9

^5^ https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-13-152

^6^ https://www2.karlin.mff.cuni.cz/~pesta/NMFM404/ph.html#Model_diagnostics___Assessing_overall_model_fit
<style/>
