REPORT ZZ_01_PERFORMANCE_OPTIMIZATION.

*---------------------------------------------------------------------*
* Aşama 1: Olabildiğince Az Veritabanına Erişmek
*---------------------------------------------------------------------*
* Birden fazla kayıt okumak için SELECT-ENDSELECT komutu ile DB döngüye
* sokulur ve teker teker kayıtlar okunur. Bu yöntem kolay olmakla beraber
* çok yavaş çalışır. Çünkü her okunan kayıt için SELECT tekrar çalıştırılır.
* Eğer tabloda çok az kayıt olduğundan emin isek kullanabiliriz.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* SELECT-ENDSELECT Örneği
*---------------------------------------------------------------------*
* Döngü içerisinde VBAK tablosundan siparişler tek tek okunur
*---------------------------------------------------------------------*
SELECT * FROM VBAK
  WHERE ERDAT IN S_ERDAT.
  " … işlemler burada yapılır …
ENDSELECT.

*---------------------------------------------------------------------*
* Tek adımda tüm kayıtları internal tabloya almak
*---------------------------------------------------------------------*
DATA: gt_vbak TYPE TABLE OF vbak.

SELECT * FROM VBAK
  INTO TABLE gt_vbak
  WHERE ERDAT IN S_ERDAT.

*---------------------------------------------------------------------*
* Aşama 2: Sadece İhtiyacımız Olan Alanları Okumak
*---------------------------------------------------------------------*
* Bellek ve performans kazanımı için yalnızca gerekli alanları okuyalım.
*---------------------------------------------------------------------*
* Örnek 1: INTO CORRESPONDING FIELDS OF TABLE kullanımı
DATA: gt_vbak TYPE TABLE OF vbak.

SELECT vbeln
       erdat
       auart
       kunnr
  INTO CORRESPONDING FIELDS OF TABLE gt_vbak
  FROM vbak
  WHERE erdat IN s_erdat.

*---------------------------------------------------------------------*
* Örnek 2: Internal tabloyu sadece gerekli alanlarla tanımlayarak maksimum performans
*---------------------------------------------------------------------*
DATA: BEGIN OF gt_vbak_occurs OCCURS 0,
        vbeln LIKE vbak-vbeln,
        erdat LIKE vbak-erdat,
        auart LIKE vbak-auart,
        kunnr LIKE vbak-kunnr,
      END OF gt_vbak_occurs.

SELECT vbeln
       erdat
       auart
       kunnr
  INTO TABLE gt_vbak_occurs
  FROM vbak
  WHERE erdat IN s_erdat.
