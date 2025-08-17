REPORT ZZ_03_02_FOR_ALL_ENTRIES_EXAMPLE.

*---------------------------------------------------------------------*
* SENARYO: Müşteri siparişleri ve sevkiyat bilgileri
*---------------------------------------------------------------------*
* Şirket, seçilen tarihler arasında oluşturulmuş siparişleri ve
* bu siparişlere ait sevkiyat kayıtlarını raporlamak istiyor.
* Hedef: Performanslı veri okuma ve FOR ALL ENTRIES kullanımı.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* ÖNEMLİ KURAL:
* Döngü içerisinde SELECT yapmak performansı düşürür.
* Bu yüzden FOR ALL ENTRIES kullanacağız.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* Kullanılacak tablolar:
*  - VBAK : Satış Siparişleri (Başlık)
*  - LIKP : Sevkiyat (Başlık)
*  - LIPS : Sevkiyat (Kalemler)
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* Kullanılmaması gereken yöntem (her sipariş için DB sorgusu):
*---------------------------------------------------------------------*
SELECT * FROM vbak
  INTO TABLE gt_vbak
  WHERE erdat IN s_erdat.

LOOP AT gt_vbak.
  " Her sipariş için ayrı ayrı sevkiyat sorgusu yapılacak
  " Bu yöntem yavaş çalışır
  " SELECT * FROM likp WHERE vbeln EQ gt_vbak-vbeln.
ENDLOOP.

*---------------------------------------------------------------------*
* FOR ALL ENTRIES ile performanslı yöntem
*---------------------------------------------------------------------*
SELECT * FROM vbak
  INTO TABLE gt_vbak
  WHERE erdat IN s_erdat.

*---------------------------------------------------------------------*
* Anahtar tablo yapısı
*---------------------------------------------------------------------*
DATA: BEGIN OF s_key,
        vbeln LIKE vbak-vbeln,
      END OF s_key,
      gt_key LIKE TABLE OF s_key WITH HEADER LINE.

*---------------------------------------------------------------------*
* Anahtar tablo dolduruluyor (sipariş numaraları)
*---------------------------------------------------------------------*
LOOP AT gt_vbak.
  MOVE-CORRESPONDING gt_vbak TO gt_key. " Anahtar tabloya sadece gerekli alanları kopyala
  COLLECT gt_key. " Aynı sipariş tekrarını önler
ENDLOOP.

*---------------------------------------------------------------------*
* Anahtar tablo boş mu kontrol edilir
* Eğer boşsa tüm tablo okunabilir, bu yüzden dikkat!
*---------------------------------------------------------------------*
IF gt_key[] IS NOT INITIAL.
  " FOR ALL ENTRIES ile tek seferde tüm sevkiyat başlıkları okunuyor
  SELECT * INTO TABLE gt_likp FROM likp
    FOR ALL ENTRIES IN gt_key
    WHERE vbeln EQ gt_key-vbeln.
ENDIF.

*---------------------------------------------------------------------*
* Sevkiyat kalemleri de FOR ALL ENTRIES ile okunabilir
*---------------------------------------------------------------------*
IF gt_likp[] IS NOT INITIAL.
  DATA: gt_lips LIKE TABLE OF lips WITH HEADER LINE.
  SELECT * INTO TABLE gt_lips FROM lips
    FOR ALL ENTRIES IN gt_likp
    WHERE vbeln EQ gt_likp-vbeln.
ENDIF.

*---------------------------------------------------------------------*
* GT_VBAK ve GT_LIKP verilerini birleştirip raporlayabiliriz
* Artık performanslı şekilde tüm siparişler ve sevkiyat bilgileri elimizde
*---------------------------------------------------------------------*
LOOP AT gt_vbak.
  " Sipariş bilgilerini ekrana veya dosyaya yazdır
  " READ TABLE gt_likp WITH KEY vbeln = gt_vbak-vbeln.
  " Eğer varsa ilgili sevkiyat bilgilerini ekle
ENDLOOP.
