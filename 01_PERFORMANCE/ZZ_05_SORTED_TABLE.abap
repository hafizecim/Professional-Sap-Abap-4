*--------------------------------------------------------------------*
* Program: ZZ_05_SORTED_TABLE_DETAILED
* Amaç   : ABAP'ta Sorted Table kullanımını ve mantığını öğretmek
* Açıklama:
* - Sorted Table nedir, ne zaman kullanılır, avantajları nelerdir
* - READ TABLE ve Binary Search mantığı
* - Nested Loop ve Parallel Cursor ile performans optimizasyonu
*--------------------------------------------------------------------*

REPORT ZZ_05_SORTED_TABLE_DETAILED.

*--------------------------------------------------------------------*
* 🔹 Sorted Table Nedir?
*--------------------------------------------------------------------*
* Sorted Table, ABAP içindeki internal tabloların özel bir türüdür.
* - Veriler, belirlenen alanlara göre sürekli sıralı tutulur.
* - Internal tablodaki veri ekleme, değiştirme veya silme işlemleri sırasında ABAP sıralamayı otomatik sağlar.
* - Bu sayede tablo üzerinde yapılan aramalar (READ TABLE) çok hızlı olur.
* - Binary Search (ikili arama) algoritması Sorted Table'larda otomatik çalışır.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 Avantajları:
*--------------------------------------------------------------------*
* 1. Hızlı veri erişimi: READ TABLE BINARY SEARCH komutu olmadan ikili arama yapabilir.
* 2. Performans: Çok büyük internal tablolarda döngüleri optimize eder.
* 3. Tekil veya tekrar eden anahtarlarla veri yönetimi: 
*    - UNIQUE KEY: Aynı anahtara sahip ikinci kayıt eklenemez.
*    - NON-UNIQUE KEY: Aynı anahtara sahip birden fazla kayıt eklenebilir.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 Ne Zaman Kullanılır?
*--------------------------------------------------------------------*
* 1. Eğer internal tablodan tek bir kayıt arayacaksanız
*    - READ TABLE ile sorgulama yapılır, Binary Search otomatik çalışır.
*    - WITH TABLE KEY kullanılır. BINARY SEARCH yazmaya gerek yoktur.
* 2. Eğer internal tablodan birden fazla kayıt veya iç içe döngü ile okuma yapacaksanız
*    - Sorted Table kullanmak performansı artırır.
*    - Parallel Cursor algoritması ile iki tabloyu eşleştirip performanslı şekilde okuyabilirsiniz.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 Sorted Table Tanımlama Örnekleri
*--------------------------------------------------------------------*
* UNIQUE KEY: Aynı anahtara sahip ikinci kayıt eklenemez
DATA: ITAB_SORTED_UNIQUE TYPE SORTED TABLE OF BKPF_TAB
      WITH UNIQUE KEY BUKRS BELNR GJAHR.

* NON-UNIQUE KEY: Aynı anahtara sahip birden fazla kayıt olabilir
DATA: ITAB_SORTED_NONUNIQUE TYPE SORTED TABLE OF BKPF_TAB
      WITH NON-UNIQUE KEY BUKRS BELNR GJAHR.

*--------------------------------------------------------------------*
* 🔹 READ TABLE Kullanımı
*--------------------------------------------------------------------*
* Sorted Table ile tek bir satır okumak için:
* READ TABLE ITAB_SORTED_UNIQUE
*   WITH TABLE KEY BUKRS = '1000'
*                  BELNR = '0001'
*                  GJAHR = '2025'. " BINARY SEARCH yazılmaz
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 Nested Loop ve Parallel Cursor
*--------------------------------------------------------------------*
* İç içe döngüler performansı düşürür. Bazen kaçınılamaz.
* Sorted Table kullanmak ve Parallel Cursor mantığı ile performansı artırabiliriz.
* Örnek: BKPF ve BSEG tablolarını eşleştirip kalemleri okumak
*--------------------------------------------------------------------*

DATA: BSEG_INDEX TYPE I,
      BKPF_READS TYPE I VALUE 0,
      BSEG_READS TYPE I VALUE 0.

BSEG_INDEX = 1.

LOOP AT BKPF_TAB INTO DATA(BKPF_LIN).
  BKPF_READS = BKPF_READS + 1.

  LOOP AT BSEG_TAB INTO DATA(BSEG_LIN) FROM BSEG_INDEX.
    IF BSEG_LIN-BUKRS <> BKPF_LIN-BUKRS OR
       BSEG_LIN-BELNR <> BKPF_LIN-BELNR OR
       BSEG_LIN-GJAHR <> BKPF_LIN-GJAHR.
       
       "Farklı kayıt geldi, index kaydedilir ve döngüden çıkılır
       BSEG_INDEX = SY-TABIX.
       EXIT.
    ELSE.
       "Eşleşen kalem bulundu
       BSEG_READS = BSEG_READS + 1.
       WRITE: / 'Belge:', BKPF_LIN-BELNR,
               'Kalem:', BSEG_LIN-BUZEI,
               'Tutar:', BSEG_LIN-WRBTR.
    ENDIF.
  ENDLOOP.
ENDLOOP.

WRITE: / 'Toplam BKPF Satır Sayısı:', LINES(BKPF_TAB),
       / 'Toplam BSEG Satır Sayısı:', LINES(BSEG_TAB),
       / 'Toplam BKPF Okuma:', BKPF_READS,
       / 'Toplam BSEG Okuma:', BSEG_READS.

*--------------------------------------------------------------------*
* 🔹 Özet:
*--------------------------------------------------------------------*
* - Sorted Table, verileri sıralı tutar ve hızlı arama sağlar
* - Binary Search otomatik çalışır
* - Tek bir kayıt okumak için WITH TABLE KEY kullanılır
* - İç içe döngülerde performans için Parallel Cursor kullanılır
* - UNIQUE ve NON-UNIQUE KEY ile veri bütünlüğü sağlanır
*--------------------------------------------------------------------*
