*--------------------------------------------------------------------*
* Program: ZZ_04_03_BINARY_SEARCH_TUTORIAL_EXAMPLE
* Yazar  : HSENYIL
* Amaç   : ABAP'ta Binary Search ile malzeme stok arama örneği
* Tarih  : 2025-08-16
*--------------------------------------------------------------------*

REPORT ZZ_04_03_BINARY_SEARCH_TUTORIAL_EXAMPLE.

*--------------------------------------------------------------------*
* Internal Tablo Tanımı
*--------------------------------------------------------------------*
DATA: BEGIN OF GT_MATERIALS OCCURS 0,
        MATNR TYPE MARA-MATNR,    "Malzeme Numarası
        MAKTX TYPE MAKT-MAKTX,    "Malzeme Tanımı
        LABST TYPE MARD-LABST,    "Mevcut stok miktarı
      END OF GT_MATERIALS.

DATA: LV_INDEX TYPE SY-TABIX.

*--------------------------------------------------------------------*
* Örnek veri ekleme (gerçek sistemde SELECT ile çekilebilir)
*--------------------------------------------------------------------*
CLEAR GT_MATERIALS.
GT_MATERIALS-MATNR = '1001'.
GT_MATERIALS-MAKTX = 'Vida M6'.
GT_MATERIALS-LABST = 500.
APPEND GT_MATERIALS.

GT_MATERIALS-MATNR = '1002'.
GT_MATERIALS-MAKTX = 'Somun M6'.
GT_MATERIALS-LABST = 300.
APPEND GT_MATERIALS.

GT_MATERIALS-MATNR = '1003'.
GT_MATERIALS-MAKTX = 'Civata M6'.
GT_MATERIALS-LABST = 150.
APPEND GT_MATERIALS.

GT_MATERIALS-MATNR = '1004'.
GT_MATERIALS-MAKTX = 'Pul M6'.
GT_MATERIALS-LABST = 200.
APPEND GT_MATERIALS.

* Tabloyu Binary Search için MATNR alanına göre sırala
SORT GT_MATERIALS BY MATNR.

*--------------------------------------------------------------------*
* Binary Search Örneği
*--------------------------------------------------------------------*
DATA: LV_SEARCH_MATNR TYPE MARA-MATNR,
      LV_FOUND_STOCK TYPE MARD-LABST.

* Kullanıcıdan aramak istediği malzeme numarasını alıyoruz
PARAMETERS: P_MATNR TYPE MARA-MATNR DEFAULT '1003'.

LV_SEARCH_MATNR = P_MATNR.

* Binary Search ile tabloyu ara
READ TABLE GT_MATERIALS WITH KEY MATNR = LV_SEARCH_MATNR
                           BINARY SEARCH
                           INTO GT_MATERIALS.

IF SY-SUBRC = 0.
  LV_FOUND_STOCK = GT_MATERIALS-LABST.
  WRITE: / 'Malzeme:', GT_MATERIALS-MATNR,
         'Açıklama:', GT_MATERIALS-MAKTX,
         'Stok Miktarı:', LV_FOUND_STOCK.
ELSE.
  WRITE: / 'Aranan malzeme bulunamadı:', LV_SEARCH_MATNR.
ENDIF.

*---------------------------------------------------------------------*
* Bu örnek, Binary Search ile malzeme stoklarının nasıl hızlı bir şekilde
* aranabileceğini göstermektedir. Binary Search, sıralı bir tabloda
* arama yaparken performansı önemli ölçüde artırır.
*---------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Çıktı Örneği (Program çalıştırıldığında)
*--------------------------------------------------------------------*
* Toplam çalışma süresi (saniye)
Toplam Çalışma Süresi (saniye): 0,012    "GV_T3 değerine bağlı, örnek sistemde 0.012 saniye olabilir

* Toplam okunan Fatura sayısı
Toplam okunan Fatura Sayısı : 5       "GT_INVOICE içindeki satır sayısı

* Toplam okunan Mal Girişi sayısı
Toplam okunan Mal Girişi Sayısı : 7    "GT_GOODSMOV içindeki satır sayısı

* Toplam okunan Kayıt sayısı
Toplam okunan Kayıt Sayısı : 12       "GT_DATA içindeki satır sayısı
*---------------------------------------------------------------------*
