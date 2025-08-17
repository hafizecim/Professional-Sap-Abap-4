*---------------------------------------------------------------------*
* 1. MARA Tablosundan Veriye INDEX ile Erişim                          *
*---------------------------------------------------------------------*

DATA: gt_mara TYPE TABLE OF mara,   " MARA tablosu için internal tablo
      gs_mara TYPE mara.            " MARA satırı için work area

* MARA tablosundan ilk 20 kaydı çekelim (örnek amaçlı LIMIT)
SELECT * FROM mara UP TO 20 ROWS
         INTO TABLE gt_mara.

WRITE: / '--- DATABASE INDEX ÖRNEĞİ ---'.

* Tabloyu LOOP ederek yazdıralım
LOOP AT gt_mara INTO gs_mara.
  WRITE: / gs_mara-matnr, gs_mara-ersda.
ENDLOOP.

*---------------------------------------------------------------------*
* 2. INTERNAL TABLODA BINARY SEARCH KULLANIMI                          *
*---------------------------------------------------------------------*

DATA: gt_data TYPE TABLE OF mara-matnr,  " Sadece malzeme numaraları
      gv_matnr TYPE mara-matnr,
      gv_index TYPE sy-tabix.

* Malzeme numaralarını ayrı tabloya alalım
LOOP AT gt_mara INTO gs_mara.
  APPEND gs_mara-matnr TO gt_data.
ENDLOOP.

* Binary Search kullanmak için internal tabloyu SORT etmemiz gerekir
SORT gt_data.

* Aranacak malzeme numarasını belirleyelim (örnek: ilk kayıttaki matnr)
READ TABLE gt_data WITH KEY table_line = gt_data[ 1 ]
     BINARY SEARCH
     TRANSPORTING NO FIELDS.

IF sy-subrc = 0.
  gv_index = sy-tabix.
  WRITE: / '--- BINARY SEARCH ÖRNEĞİ ---'.
  WRITE: / 'Aranan malzeme bulundu. Index:', gv_index.
ELSE.
  WRITE: / 'Aranan malzeme bulunamadı!'.
ENDIF.

*---------------------------------------------------------------------*
* SONUÇ                                                                *
*---------------------------------------------------------------------*
* Bu rapor iki şeyi göstermektedir:                                   *
* 1) DATABASE INDEX -> Veritabanı tablosuna erişim                     *
* 2) BINARY SEARCH -> Internal tabloda hızlı arama                     *
*---------------------------------------------------------------------*