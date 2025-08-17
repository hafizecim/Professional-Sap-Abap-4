REPORT ZZ_07_SE30_RUNTIME_ANALYSIS_DEMO.

************************************************************************
* Program Adı  : Z_SE30_RUNTIME_ANALYSIS_DEMO
* Amaç         : SE30 - Runtime Analysis kullanımı hakkında bilgilendirici
*                bir rapor niteliğinde örnek kod.
* Açıklama     : Bu rapor, ABAP geliştiricisinin SE30 ile nasıl analiz
*                yapabileceğini adım adım anlatan bir eğitim örneğidir.
* Telif        : Eğitim amaçlı, özgün olarak hazırlanmıştır.
************************************************************************

START-OF-SELECTION.

*---------------------------------------------------------------
* 1) SE30 işlem kodu nedir?
*---------------------------------------------------------------
WRITE: / 'SE30, ABAP programlarının performansını analiz etmek için kullanılan',
       / 'bir SAP işlem kodudur.'.
WRITE: / 'Bu araç ile:',
       / '- Program parçalarının ne kadar sürdüğü',
       / '- Veritabanına (DB) erişim süreleri',
       / '- ABAP kodlarının çalıştırılma süreleri',
       / 'gibi bilgiler görülebilir.'.

ULINE.

*---------------------------------------------------------------
* 2) Analiz yapılacak program seçimi
*---------------------------------------------------------------
WRITE: / 'Örneğin, analiz edilmek istenen program ZMS_TEST olsun.'.
WRITE: / 'SE30 ekranında "Program" alanına ZMS_TEST yazılır ve',
       / 'Execute (Çalıştır) butonu ile analiz başlatılır.'.

ULINE.

*---------------------------------------------------------------
* 3) Programın yürütülmesi ve çıkış
*---------------------------------------------------------------
WRITE: / 'Program çalıştırılır, istenilen işlemler yapılır ve',
       / 'işlem tamamlandıktan sonra EXIT komutu ile çıkılır.'.

ULINE.

*---------------------------------------------------------------
* 4) Sonuçların değerlendirilmesi
*---------------------------------------------------------------
WRITE: / 'Analiz tamamlandığında, "Evaluate" seçeneği ile',
       / 'programın veritabanı ve ABAP performans raporları incelenebilir.'.
WRITE: / 'Grafik üzerinde ABAP kısmı ne kadar yüksekse,',
       / 'program o kadar performanslı çalışıyor demektir.'.

ULINE.

*---------------------------------------------------------------
* 5) Performans problemlerinin tespiti
*---------------------------------------------------------------
WRITE: / 'Hit List seçeneği ile programın hangi parçalarının',
       / 'ne kadar sürdüğü görülebilir.'.
WRITE: / 'Örneğin, Net kolonuna göre sıralama yapılırsa,',
       / 'en uzun süreyi alan işlemler en üstte görüntülenir.'.
WRITE: / 'Bu sayede hangi kod veya tablo erişiminin problemli olduğu anlaşılır.'.

ULINE.

*---------------------------------------------------------------
* 6) Örnek senaryo
*---------------------------------------------------------------
WRITE: / 'Örneğin, analiz sonucunda MSEG tablosuna yapılan okuma',
       / 'işleminin fazla zaman aldığı tespit edilmişse,',
       / 'optimizasyon bu kısımda yapılmalıdır.'.

ULINE.

*---------------------------------------------------------------
* 7) Sonuç
*---------------------------------------------------------------
WRITE: / 'SE30 aracı sayesinde, programın hangi noktalarının',
       / 'performans sorununa yol açtığı bulunabilir ve',
       / 'gerekli iyileştirmeler yapılabilir.'.

************************************************************************
* Bu rapor örneği, sadece öğretici amaçlı hazırlanmıştır.
* SE30 ile gerçek analiz yapılırken, sistemdeki program doğrudan
* SE30 üzerinden çalıştırılarak ölçümler elde edilir.
************************************************************************
