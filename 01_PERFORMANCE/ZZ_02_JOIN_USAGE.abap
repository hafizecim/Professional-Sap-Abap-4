REPORT ZZ_02_JOIN_USAGE

*---------------------------------------------------------------------*
* Rapor: ZZ_02_JOIN_USAGE
* Amacı: ABAP'ta JOIN kullanımını ve performans optimizasyonunu göstermek
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* Internal tablo tanımı
* Alanlar: Müşteri No, Müşteri Adı, Sipariş No, Sipariş Tarihi, Sipariş Türü
*---------------------------------------------------------------------*
DATA: BEGIN OF gt_data OCCURS 0,
        kunnr LIKE vbak-kunnr,
        name1 LIKE kna1-name1,
        vbeln LIKE vbak-vbeln,
        erdat LIKE vbak-erdat,
        auart LIKE vbak-auart,
      END OF gt_data.

*---------------------------------------------------------------------*
* Seçim ekranı parametreleri
*---------------------------------------------------------------------*
PARAMETERS: s_kunnr TYPE RANGE OF kna1-kunnr,
            s_erdat TYPE RANGE OF vbak-erdat.

*---------------------------------------------------------------------*
* 1. KULLANILMAMASI GEREKEN YÖNTEM: SELECT-ENDSELECT
* Performans: Çok Yavaş (her kayıt için ayrı SELECT çalıştırılır)
*---------------------------------------------------------------------*
SELECT * FROM kna1
  WHERE kunnr IN s_kunnr.
  
  SELECT * FROM vbak
    WHERE kunnr EQ kna1-kunnr.

    CLEAR gt_data.
    MOVE-CORRESPONDING kna1 TO gt_data.
    MOVE-CORRESPONDING vbak TO gt_data.
    APPEND gt_data.

  ENDSELECT.
ENDSELECT.

*---------------------------------------------------------------------*
* 2. INNER JOIN: Sadece siparişi olan müşteriler
* Performans: Hızlı (tek SQL sorgusu ile birleştirme yapılır)
*---------------------------------------------------------------------*
SELECT a~kunnr
       a~name1
       b~vbeln
       b~erdat
       b~auart
  INTO TABLE gt_data
  FROM kna1 AS a
  INNER JOIN vbak AS b
    ON a~kunnr = b~kunnr
  WHERE a~kunnr IN s_kunnr.

*---------------------------------------------------------------------*
* 3. LEFT OUTER JOIN: Tüm müşteriler, sipariş olmayabilir
* Performans: Hızlı (tek SQL sorgusu, bazı satırlar boş olabilir)
*---------------------------------------------------------------------*
SELECT a~kunnr
       a~name1
       b~vbeln
       b~erdat
       b~auart
  INTO TABLE gt_data
  FROM kna1 AS a
  LEFT OUTER JOIN vbak AS b
    ON a~kunnr = b~kunnr
  WHERE a~kunnr IN s_kunnr.

*---------------------------------------------------------------------*
* LEFT OUTER JOIN sonrası filtreleme
* ABAP, LEFT OUTER JOIN yapılan tablodaki alanlar için WHERE koşulu
* eklemeye izin vermez. Bu nedenle internal tablodan filtreleme yapılır.
*---------------------------------------------------------------------*
DELETE gt_data WHERE erdat NOT IN s_erdat.

*---------------------------------------------------------------------*
* Internal tablodaki verileri işleme
*---------------------------------------------------------------------*
LOOP AT gt_data INTO DATA(ls_data).
  " Örnek işlem: listeleme
  WRITE: / ls_data-kunnr, ls_data-name1,
          ls_data-vbeln, ls_data-erdat, ls_data-auart.
ENDLOOP.

*---------------------------------------------------------------------*
* Örnek: LEFT OUTER JOIN ve SQL koşulu eklenememesi durumu
*---------------------------------------------------------------------*
* Aşağıdaki SQL çalıştırılamaz:
 SELECT A~KUNNR
        A~NAME1
        B~VBELN
        B~ERDAT
        B~AUART
   INTO TABLE GT_DATA
   FROM KNA1 AS A
   LEFT OUTER JOIN VBAK AS B
     ON A~KUNNR EQ B~KUNNR
   WHERE A~KUNNR IN S_KUNNR
     AND B~ERDAT IN S_ERDAT.
* Hata verir çünkü LEFT OUTER JOIN yapılan tablonun alanı WHERE şartına eklenemez.
* Çözüm: LEFT OUTER JOIN’den çıkarmak yerine, internal tablodan koşula uymayan satırları silmek daha mantıklıdır.
