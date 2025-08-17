REPORT ZZ_03_01_FOR_ALL_ENTRIES.

*---------------------------------------------------------------------*
* PERFORMANS - FOR ALL ENTRIES ANLATIMI
*---------------------------------------------------------------------*
* Temel kurallardan biri döngü içerisinde veritabanı erişimi yapmamaktır.
* FOR ALL ENTRIES ile bu temel kuralı uygulayabiliriz.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* JOIN yapmadan vazgeçmememiz gerektiği önceki aşmalarda anlatılmıştır.
* Ancak bazı tablolar için JOIN yapılamayabilir.
* Örneğin BSEG (FI Belge Kalemleri) tablosu bir CLUSTER tablodur ve JOIN yapılamaz.
* Ayrıca program mantığı gereği de JOIN yapmak mümkün olmayabilir.
* Bu tür durumlarda FOR ALL ENTRIES devreye girer.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* NASIL UYGULANIR
*---------------------------------------------------------------------*
* Öncelikle okunacak verilerden bir anahtar tablosu oluşturulur.
* Daha sonra bu anahtar tablodaki tüm kayıtlar için SELECT komutu tek adımda çalıştırılır.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* NELERE DİKKAT ETMELİYİZ
*---------------------------------------------------------------------*
* Eğer anahtar tablonuz boşsa tüm kayıtlar okunur.
* Bu yüzden FOR ALL ENTRIES kullanmadan önce anahtar tablonun boş olup olmadığını kontrol edin.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* ÖRNEK UYGULAMA 1: Satın alma siparişi bazlı mal girişleri ve faturalar
*---------------------------------------------------------------------*
* Kullanacağımız tablolar:
* EKBE  : Satın alma Belge Akışı
* MKPF-MSEG : Malzeme belgeleri Başlık-Kalem Tablosu
* RBKP-RSEG : MM Fatura Belgeleri Başlık-Kalem Tablosu
*---------------------------------------------------------------------*
* EKBE tablosunda VGABE (İşlem türü) alanı:
*  - '1' ise referans belge mal girişidir
*  - '2' ise referans belge fatura belgesidir
* Mal girişleri MKPF-MSEG tablolarından okunur
* Faturalar RBKP-RSEG tablolarından okunur
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* Kullanılmaması gereken algoritma
* Bu yöntemde her satır için DB'ye gidilir ve döngü uzun sürer
*---------------------------------------------------------------------*
SELECT * FROM ekbe
  INTO TABLE gt_ekbe
  WHERE budat IN s_budat.

LOOP AT gt_ekbe.
  IF gt_ekbe-vgabe EQ '1'. " Malzeme belgesi
    " Burada her satır için MSEG tablosuna SELECT yapılır
    " SELECT ... FROM mseg
  ELSEIF gt_ekbe-vgabe EQ '2'. " Fatura belgesi
    " Burada her satır için RSEG tablosuna SELECT yapılır
    " SELECT ... FROM rseg
  ENDIF.
ENDLOOP.

*---------------------------------------------------------------------*
* FOR ALL ENTRIES ile performanslı yöntem
*---------------------------------------------------------------------*
* Satır sayısı artabilir ama hız çok daha iyi olur.
* Yukarıdaki yöntemde n defa DB erişimi olabilirken burada en fazla 3 defa erişim olur.
*---------------------------------------------------------------------*
SELECT * FROM ekbe
  INTO TABLE gt_ekbe
  WHERE budat IN s_budat.

* FOR ALL ENTRIES için kullanılacak tablo yapısı
DATA: BEGIN OF s_key,
        gjahr LIKE ekbe-gjahr,
        belnr LIKE ekbe-belnr,
        buzei LIKE rseg-buzei,
      END OF s_key,
      gt_key1 LIKE TABLE OF s_key WITH HEADER LINE,
      gt_key2 LIKE TABLE OF s_key WITH HEADER LINE.

* Anahtar tablolar dolduruluyor
LOOP AT gt_ekbe.
  IF gt_ekbe-vgabe EQ '1'. " Mal girişi
    MOVE-CORRESPONDING gt_ekbe TO gt_key1. " Anahtar tabloya ilgili alanları kopyalar. Malzeme belgesi anahtarı
    COLLECT gt_key1.
  ELSEIF gt_ekbe-vgabe EQ '2'. " Fatura
    MOVE-CORRESPONDING gt_ekbe TO gt_key2. " Fatura belgesi anahtarı
    COLLECT gt_key2.
  ENDIF.
ENDLOOP.

* Anahtar tablo boş mu kontrol edilir ve mal girişleri tek adımda okunur
IF gt_key1[] IS NOT INITIAL.
  SELECT * INTO TABLE gt_mseg FROM mseg
    FOR ALL ENTRIES IN gt_key1
    WHERE mjahr EQ gt_key1-gjahr
      AND mblnr EQ gt_key1-belnr
      AND zeile EQ gt_key1-buzei(4).
ENDIF.

* Tüm faturalar tek adımda okunur
IF gt_key2[] IS NOT INITIAL.
  SELECT * INTO TABLE gt_rseg FROM rseg
    FOR ALL ENTRIES IN gt_key2
    WHERE gjahr EQ gt_key2-gjahr
      AND belnr EQ gt_key2-belnr
      AND buzei EQ gt_key2-buzei.
ENDIF.

*---------------------------------------------------------------------*
* ÖRNEK UYGULAMA 2: Malzeme listesi ve tüm özelliklerinin okunması
*---------------------------------------------------------------------*
* GT_DATA-MATNR alanında malzeme numaraları vardır
* Bu malzemelere ait özellikler MARA tablosundan okunacaktır
*---------------------------------------------------------------------*

* Kullanılmaması gereken yöntem (her satırda SELECT)
LOOP AT gt_data.
  SELECT SINGLE * FROM mara
    WHERE matnr EQ gt_data-matnr.
  IF sy-subrc EQ 0.
    " Burada MARA tablosundan istenen alanlar gt_data içine atılır
  ENDIF.
ENDLOOP.

*---------------------------------------------------------------------*
* FOR ALL ENTRIES yöntemi ile performanslı çözüm
*---------------------------------------------------------------------*
DATA: gt_mara LIKE TABLE OF mara WITH HEADER LINE.
DATA: lt_matnr LIKE TABLE OF mara-matnr WITH HEADER LINE.

* Anahtar tablo dolduruluyor
LOOP AT gt_data.
  lt_matnr = gt_data-matnr.
  COLLECT lt_matnr.
ENDLOOP.

* Anahtar tablodaki tüm malzemeler için MARA tablosu tek seferde okunur
SELECT * INTO TABLE gt_mara FROM mara
  FOR ALL ENTRIES IN lt_matnr
  WHERE matnr EQ lt_matnr-table_line.

* GT_DATA içindeki istenen alanlar güncellenir
LOOP AT gt_data.
  READ TABLE gt_mara WITH KEY matnr = gt_data-matnr.
  IF sy-subrc EQ 0.
    " Burada MARA tablosundan istenen alanlar gt_data içine atılır
  ENDIF.
ENDLOOP.
