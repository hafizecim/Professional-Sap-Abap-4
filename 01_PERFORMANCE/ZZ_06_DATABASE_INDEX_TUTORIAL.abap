*---------------------------------------------------------------------*
* PROGRAM: ZZ_05_DATABASE_INDEX_TUTORIAL
* AMAÇ   : Veritabanı Index kullanımını göstermek
* AÇIKLAMA: 
* Bu programda hem DATABASE INDEX hem de BINARY SEARCH konusu          *
* tek bir rapor içinde örneklerle anlatılmaktadır.                     *
* Amaç: ABAP'ta veriye erişim yöntemlerini göstermek.                  *
*---------------------------------------------------------------------*

REPORT ZZ_05_DATABASE_INDEX_TUTORIAL.

*---------------------------------------------------------------------*
* 📌 Veritabanı (Database) Index Kullanımı
*---------------------------------------------------------------------*
* Veritabanındaki bir tabloya anahtar alanlarla erişmek istediğiniz 
* zaman performans sorunu yaşamazsınız. Çünkü tablolar anahtar alanlara 
* göre sıralıdır, bu nedenle erişim hızlı olur. 
* 
* Eğer anahtar alanlar ile erişmiyorsanız ve o tabloda kayıt miktarı 
* fazla ise SELECT komutunuz çok yavaş sürebilir. 
* 
* Bu durumda erişmek istediğimiz alanlar için sanal anahtar alanlar 
* oluşturabilirsek erişimi daha hızlı yapabiliriz. 
* Index’leme tam olarak bunu yapıyor.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* 📌 Senaryo: MSEG tablosu üzerinde index olmadan okuma
* SE16N işlem kodundan MSEG tablosundaki kayıt sayısına bakalım.
* Giriş sayısı butonuna tıklarız.
* Toplam 1.434.931 kayıt olduğu görülür.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* 📘 KONU: DATABASE INDEX KULLANIMI                                    *
*---------------------------------------------------------------------*
* Veritabanındaki bir tabloya anahtar alanlarla erişmek istediğinizde *
* performans sorunu yaşamazsınız. Çünkü tablolar anahtar alanlara     *
* göre sıralıdır. Eğer anahtar alanlarla erişmiyorsanız ve kayıt      *
* miktarı fazlaysa SELECT çok yavaş çalışabilir.                      *
*                                                                    *
* Çözüm: INDEX oluşturarak sanal anahtar alanlar yaratabiliriz.       *
* Böylece erişim hızlanır.                                            *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* PROGRAM 1 : Index OLMADAN veri çekme                                *
*---------------------------------------------------------------------*

DATA : GT_MSEG LIKE TABLE OF MSEG WITH HEADER LINE,
       T1 TYPE I,
       T2 TYPE I,
       T3 TYPE P DECIMALS 3,
       LINE_COUNT TYPE I.

* Başlangıç zamanı ölçülüyor
GET RUN TIME FIELD T1.

* MATBF alanına göre okuma yapılıyor (anahtar değil!)
SELECT * INTO TABLE GT_MSEG FROM MSEG
   WHERE MATBF BETWEEN '000000000100000000' AND
                       '000000000400000000'.

* Bitiş zamanı ölçülüyor
GET RUN TIME FIELD T2.

T3 = ( T2 - T1 ) / 1000000.
DESCRIBE TABLE GT_MSEG LINES LINE_COUNT.


WRITE : / '--- PROGRAM 1 : Index OLMADAN ---'.
WRITE : / 'Okunan kayıt sayısı' , 30 ':', LINE_COUNT LEFT-JUSTIFIED.
WRITE : / 'Hız (Saniye)', 30 ':', T3 LEFT-JUSTIFIED.

*---------------------------------------------------------------------*
* 📌 Program Çıktısı:
* Yaklaşık 42 saniye sürdüğünü gösteriyor.
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
* 📘 Şimdi MSEG-MATBF alanı için INDEX oluşturalım:                   *
*---------------------------------------------------------------------*
* - SE11 işlem kodundan MSEG tablosunu açıyoruz                       *
* - Indexes butonuna basıyoruz                                        *
* - Create Index dedikten sonra ZI1 adında index ekranına giriyoruz   *
* - MANDT ve MATBF alanlarını index alanlarına yazıyoruz              *
* - Kaydedip aktifleştiriyoruz                                        *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* 📌 Dikkat: 
* Bu tür fazla veriye sahip tablolarda index anında yaratılmaz. 
* Çünkü index’leme işlemi uzun sürebilir. 
* Bu nedenle “Index does not exist in database …” uyarısı alınır.
*
* Çözüm: SE14 işlem kodundan index veritabanında yaratılır.
* - SE14 → Edit → Indexes
* - Direct modu seçilmez (kayıt çoksa uzun sürebilir ve hata olabilir)
* - Background modu seçmek daha güvenlidir
* - Create database index butonuna basılır
* - Sakla dediğimizde index arka planda bir JOB ile oluşur
* - JOB adı: DB-INDXMSEGZI1
* - SM37 işlem kodundan JOB takip edilir
* - SE14 ekranından index’in yaratıldığını görebiliriz
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* 📌 Index yaratma işlemi bittiğinde program tekrar yürütülür.
* Önceki program 42 saniye sürmüştü,
* index sonrası yaklaşık 5 saniye sürüyor.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* PROGRAM 2 : Index ile ama sadece İSTENEN alanları çekme             *
*---------------------------------------------------------------------*

DATA : BEGIN OF GT_MSEG OCCURS 0,  " sadece istenen alanlar tanımlanır
         MBLNR LIKE MSEG-MBLNR,
         MJAHR LIKE MSEG-MJAHR,
         ZEILE LIKE MSEG-ZEILE,
       END OF GT_MSEG,
       T1 TYPE I,
       T2 TYPE I,
       T3 TYPE P DECIMALS 3,
       LINE_COUNT TYPE I.

GET RUN TIME FIELD T1.

* Sadece istenen alanlar internal tabloya alınır → performans artar
SELECT MBLNR MJAHR ZEILE
  INTO TABLE GT_MSEG FROM MSEG
   WHERE MATBF BETWEEN '000000000100000000' AND
                       '000000000400000000'.

GET RUN TIME FIELD T2.

T3 = ( T2 - T1 ) / 1000000.
DESCRIBE TABLE GT_MSEG LINES LINE_COUNT.

WRITE : / '--- PROGRAM 2 : Index + Alan Seçimi ---'.
WRITE : / 'Okunan kayıt sayısı' , 30 ':', LINE_COUNT2 LEFT-JUSTIFIED.
WRITE : / 'Hız (Saniye)', 30 ':', T3_2 LEFT-JUSTIFIED.

*---------------------------------------------------------------------*
* 📌 Program Çıktısı:
* Yaklaşık 1 saniye sürdüğünü gösteriyor.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* 📌 Sonuç:
* - Index kullanarak ve sadece istenen alanları çekerek,
*   42 saniyeden 1 saniyeye kadar inebildik.
* - Programlarımızda birçok defa veritabanına erişim yapıldığını
*   düşünürsek, bu sayede saatlerce çalışan programlar 
*   çok küçük işlemlerle dakikalara indirilebilir.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* 📌 Ekstra: 
* Programlarımızın performansını daha net ölçmek için 
* SE30 - Runtime Analysis işlem kodu kullanılabilir.
*---------------------------------------------------------------------*
