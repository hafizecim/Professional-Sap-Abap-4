*--------------------------------------------------------------------*
* Program: ZZ_04_02_BINARY_SEARCH_TUTORIAL_EXAMPLE
* Yazar : HSENYIL
* Amaç  : ABAP’ta Binary Search (İkili Arama) algoritmasını göstermek
* Tarih  : 2025-08-16
* Açıklama: Bu program örnek veri seti üzerinde READ TABLE 
*            BINARY SEARCH kullanımını öğretmek için hazırlanmıştır.
*--------------------------------------------------------------------*

REPORT ZZ_04_02_BINARY_SEARCH_TUTORIAL_EXAMPLE.
*--------------------------------------------------------------------*
DATA : BEGIN OF GT_DATA OCCURS 0,
        EBELN LIKE EKKO-EBELN,     "Satın alma belgesi
        EBELP LIKE EKPO-EBELP,     "Satın alma belgesi kalemi
        BEDAT LIKE EKKO-BEDAT,     "Satın alma belgesinin tarihi
        CPUDT LIKE EKBE-CPUDT,     "Muhasebe belgesinin giriş tarihi
        MENGE LIKE EKPO-MENGE,     "Satın alma siparişi miktarı
        MEINS LIKE EKPO-MEINS,     "Satın alma siparişi ölçü birimi
        MATNR LIKE EKPO-MATNR,     "Malzeme Numarası
        MAKTX LIKE MAKT-MAKTX,     "Malzeme Tanımı
        MTART LIKE MARA-MTART,     "Malzeme Türü
        MTBEZ LIKE T134T-MTBEZ,    "Malzeme Türü Tanımı
        VGABE LIKE EKBE-VGABE,     "İşlem Türü
        GJAHR LIKE RBKP-GJAHR,     "Fatura Belgesi Mali Yılı
        BELNR LIKE RBKP-BELNR,     "Fatura Belgesi
        BUZEI LIKE RSEG-BUZEI,     "Fatura Belgesi Kalemi
        WRBTR LIKE RSEG-WRBTR,     "Fatura Tutarı
        WAERS LIKE RBKP-WAERS,     "Fatura Tutar Birimi
        MJAHR LIKE MKPF-MJAHR,     "Mal Giriş Belgesi Yılı
        MBLNR LIKE MKPF-MBLNR,     "Mal Giriş Belgesi
        ZEILE LIKE MSEG-ZEILE,     "Mal Giriş Belgesi Kalemi
        MENGEX LIKE MSEG-MENGE,    "Mal Giriş Miktarı
       END OF GT_DATA.

* Faturalar için internal tablo
DATA : BEGIN OF GT_INVOICE OCCURS 0,
        GJAHR LIKE RBKP-GJAHR,     "Fatura Belgesi Mali Yılı
        BELNR LIKE RBKP-BELNR,     "Fatura Belgesi
        BUZEI LIKE RSEG-BUZEI,     "Fatura Belgesi Kalemi
        WRBTR LIKE RSEG-WRBTR,     "Fatura Tutarı
        WAERS LIKE RBKP-WAERS,     "Fatura Tutar Birimi
       END OF GT_INVOICE.

* Mal girişleri için internal tablo
DATA : BEGIN OF GT_GOODSMOV OCCURS 0,
        MJAHR LIKE MKPF-MJAHR,     "Mal Giriş Belgesi Yılı
        MBLNR LIKE MKPF-MBLNR,     "Mal Giriş Belgesi
        ZEILE LIKE MSEG-ZEILE,     "Mal Giriş Belgesi Kalemi
        MENGE LIKE MSEG-MENGE,     "Mal Giriş Miktarı
        SJAHR LIKE MSEG-SJAHR,     "Ters kayıt belgesi yılı
        SMBLN LIKE MSEG-SMBLN,     "Ters kayıt belgesi
        SMBLP LIKE MSEG-SMBLP,     "Ters kayıt belgesi kalemi
       END OF GT_GOODSMOV.

* Çalışma Süresi Analizi için
DATA : GV_T1 TYPE P DECIMALS 3,
       GV_T2 TYPE P DECIMALS 3,
       GV_T3 TYPE P DECIMALS 3.

* Okunan kayıt sayısı
DATA : GV_LINE_COUNT TYPE I.

*--------------------------------------------------------------------*
* Seçim Ekranının Tanımlanması
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_EBELN FOR EKKO-EBELN,    "Satın alma belgesi
                 S_BEDAT FOR EKKO-BEDAT,    "Satınalma belgesinin tarihi
                 S_MATNR FOR EKPO-MATNR,    "Malzeme Numarası
                 S_MTART FOR MARA-MTART,    "Malzeme Türü
                 S_CPUDT FOR EKBE-CPUDT,    "Giriş tarihi
                 S_BELNR FOR RBKP-BELNR,    "Fatura Belgesi
                 S_MBLNR FOR MKPF-MBLNR.    "Mal Giriş Belgesi
SELECTION-SCREEN END OF BLOCK B1.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*
* Program başlatılıyor...
*--------------------------------------------------------------------*
* Aşağıdaki SELECT ‘e görüldüğü üzere JOIN’imizde
* olabildiğince tabloyu birleştiriyoruz.
GET RUN TIME FIELD GV_T1.

SELECT
  EKKO~EBELN,    "Satın alma belgesi
  EKPO~EBELP,    "Satın alma belgesi kalemi
  EKBE~CPUDT,    "Muhasebe belgesinin giriş tarihi
  EKKO~BEDAT,    "Satın alma belgesinin tarihi
  EKPO~MENGE,    "Satın alma siparişi miktarı
  EKPO~MEINS,    "Satın alma siparişi ölçü birimi
  EKPO~MATNR,    "Malzeme Numarası
  MAKT~MAKTX,    "Malzeme Tanımı
  MARA~MTART,    "Malzeme Türü
  EKBE~VGABE,    "İşlem Türü
  EKBE~BELNR,    "Belge Numarası
  EKBE~GJAHR,    "Belge Yılı
  EKBE~BUZEI,    "Belge Kalemi
  T134T~MTBEZ     "Malzeme Türü Tanımı
  INTO CORRESPONDING FIELDS OF TABLE GT_DATA
  FROM EKKO
  INNER JOIN EKPO
          ON EKPO~EBELN EQ EKKO~EBELN
  INNER JOIN MARA
          ON MARA~MATNR EQ EKPO~MATNR
  INNER JOIN MAKT
          ON MAKT~MATNR EQ MARA~MATNR AND
             MAKT~SPRAS EQ SY-LANGU   " login dilinde tanımı alıyoruz
  INNER JOIN T134T
          ON T134T~MTART EQ MARA~MTART AND
             T134T~SPRAS EQ SY-LANGU   " login dilinde tanımı alıyoruz
  LEFT OUTER JOIN EKBE
               ON EKBE~EBELN EQ EKPO~EBELN AND
                  EKBE~EBELP EQ EKPO~EBELP
  WHERE EKKO~EBELN IN S_EBELN AND
        EKKO~BEDAT IN S_BEDAT AND
        EKPO~MATNR IN S_MATNR AND
        MARA~MTART IN S_MTART.

* EKBE tablosu INNER JOIN ile eklenseydi
* ‘EKBE~CPUDT IN S_CPUDT’ ifadesini WHERE koşulumuza
* ekleyebilirdik. Ancak LEFT OUTER JOIN ile eklendiğinden
* koşula bu tablo ile ilgili bir koşul eklenemez,
* LEFT OUTER JOIN’de bu tür durumlarda
* çoğu zaman EKBE tablosu JOIN’den çıkarılır
* Ancak bu kesinlikle yanlıştır,
* EKBE tablosunu JOIN’den çıkarmak performansı
* düşürür. Bu nedenle EKBE için filtrelenen alanları
* aşağıdaki gibi filtrelemek daha mantıklıdır
DELETE GT_DATA WHERE CPUDT NOT IN S_CPUDT.

* Şimdi Faturaları ve Mal girişlerini
* tek SELECT komutu ve for all entries ile alalım
* Önce KEY tablolarımızı oluşturalım...
DATA : LT_INV_KEY LIKE TABLE OF GT_INVOICE WITH HEADER LINE,
       LT_GOODS_KEY LIKE TABLE OF GT_GOODSMOV WITH HEADER LINE.

REFRESH : LT_INV_KEY, LT_GOODS_KEY.

LOOP AT GT_DATA.
  CLEAR LT_INV_KEY.
  IF GT_DATA-VGABE EQ '1'.
    * Mal girişleri için KEY tablosu
    LT_GOODS_KEY-MJAHR = GT_DATA-GJAHR.
    LT_GOODS_KEY-MBLNR = GT_DATA-BELNR.
    LT_GOODS_KEY-ZEILE = GT_DATA-BUZEI.
    COLLECT LT_GOODS_KEY.
  ELSEIF GT_DATA-VGABE EQ '2'.
    * Faturalar için key tablosu
    LT_INV_KEY-GJAHR = GT_DATA-GJAHR.
    LT_INV_KEY-BELNR = GT_DATA-BELNR.
    LT_INV_KEY-BUZEI = GT_DATA-BUZEI.
    COLLECT LT_INV_KEY.
  ENDIF.
ENDLOOP.

* Faturalar okunuyor...
IF LT_INV_KEY[] IS NOT INITIAL.
  REFRESH : GT_INVOICE.
  SELECT
      RSEG~GJAHR
      RSEG~BELNR
      RSEG~BUZEI
      RSEG~WRBTR
      RBKP~WAERS
      INTO TABLE GT_INVOICE
      FROM RSEG
      INNER JOIN RBKP
              ON RBKP~GJAHR EQ RSEG~GJAHR AND
                 RBKP~BELNR EQ RSEG~BELNR AND
                 RBKP~STBLG EQ ''   "Ters kayıt belgeleri gelmesin
      FOR ALL ENTRIES IN LT_INV_KEY
      WHERE RSEG~BELNR EQ LT_INV_KEY-BELNR AND
            RSEG~GJAHR EQ LT_INV_KEY-GJAHR AND
            RSEG~BUZEI EQ LT_INV_KEY-BUZEI.
ENDIF.

* Satır indexleri için
DATA : LV_TABIX LIKE SY-TABIX.

* Mal Girişleri okunuyor...
IF LT_GOODS_KEY[] IS NOT INITIAL.
  REFRESH : GT_GOODSMOV.
  SELECT
       MSEG~MJAHR
       MSEG~MBLNR
       MSEG~ZEILE
       MSEG~MENGE
       MSEG~SJAHR
       MSEG~SMBLN
       MSEG~SMBLP
       INTO TABLE GT_GOODSMOV
       FROM MSEG
       INNER JOIN MKPF
               ON MKPF~MJAHR EQ MSEG~MJAHR AND
                  MKPF~MBLNR EQ MSEG~MBLNR
       FOR ALL ENTRIES IN LT_GOODS_KEY
       WHERE MSEG~MJAHR EQ LT_GOODS_KEY-MJAHR AND
             MSEG~MBLNR EQ LT_GOODS_KEY-MBLNR AND
             MSEG~ZEILE EQ LT_GOODS_KEY-ZEILE.
  SORT GT_GOODSMOV BY MJAHR MBLNR ZEILE.

  LOOP AT GT_GOODSMOV WHERE SMBLN IS NOT INITIAL.
    LV_TABIX = SY-TABIX.
    READ TABLE GT_GOODSMOV WITH KEY MJAHR = GT_GOODSMOV-SJAHR
                                        MBLNR = GT_GOODSMOV-SMBLN
                                        ZEILE = GT_GOODSMOV-SMBLP
                                        BINARY SEARCH
                                        TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      DELETE GT_GOODSMOV INDEX LV_TABIX.
    ENDIF.
  ENDLOOP.
  DELETE GT_GOODSMOV WHERE SMBLN IS NOT INITIAL.
ENDIF.

* Toplam 3 SELECT komutunda Satın alma Belgeleri, Faturalar
* ve Mal Girişleri okunmuş oldu. Bundan sonra BINARY SEARCH
* yardımıyla GT_DATA içindeki ek alanları doldurmamız
* yeterli olacaktır.
* BINARY SEARCH için tabloları sıralıyoruz
SORT GT_INVOICE BY GJAHR BELNR BUZEI.
SORT GT_GOODSMOV BY MJAHR MBLNR ZEILE.

LOOP AT GT_DATA.
  LV_TABIX = SY-TABIX.
  IF GT_DATA-VGABE EQ '1'.
    * Mal girişi
    READ TABLE GT_GOODSMOV WITH KEY MJAHR = GT_DATA-GJAHR
                                    MBLNR = GT_DATA-BELNR
                                    ZEILE = GT_DATA-BUZEI
                                    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DATA-MENGEX = GT_GOODSMOV-MENGE.
    ENDIF.
  ELSEIF GT_DATA-VGABE EQ '2'.
    * Faturalar
    READ TABLE GT_INVOICE WITH KEY GJAHR = GT_DATA-GJAHR
                                      BELNR = GT_DATA-BELNR
                                      BUZEI = GT_DATA-BUZEI
                                      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DATA-WRBTR = GT_INVOICE-WRBTR.
      GT_DATA-WAERS = GT_INVOICE-WAERS.
    ENDIF.
  ENDIF.
  MODIFY GT_DATA INDEX LV_TABIX.
ENDLOOP.

GET RUN TIME FIELD GV_T2.
GV_T3 = ( GV_T2 - GV_T1 ) / 1000000.
WRITE : / 'Toplam Çalışma Süresi (saniye):' , GV_T3 LEFT-JUSTIFIED .

GV_LINE_COUNT = LINES( GT_INVOICE ).
WRITE : / 'Toplam okunan Fatura Sayısı :' , GV_LINE_COUNT LEFT-JUSTIFIED.

GV_LINE_COUNT = LINES( GT_GOODSMOV ).
WRITE : / 'Toplam okunan Mal Girişi Sayısı :' , GV_LINE_COUNT LEFT-JUSTIFIED.

GV_LINE_COUNT = LINES( GT_DATA ).
WRITE : / 'Toplam okunan Kayıt Sayısı :' , GV_LINE
```
