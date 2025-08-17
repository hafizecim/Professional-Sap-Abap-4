REPORT ZZ_04_01_BINARY_SEARCH_TUTORIAL.


*--------------------------------------------------------------------*
* Amaç: Bu rapor ikili arama algoritmasının mantığını ve ABAP'ta
*        nasıl uygulanacağını adım adım anlatmak için hazırlanmıştır.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 İkili Arama Algoritması
*   - Bir dizide belirli bir değeri bulmak için geliştirilmiş bir yöntemdir.
*   - Önemli şart: Dizi veya tablo mutlaka sıralı olmalıdır.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 Çalışma Mantığı
*   - Sürekli olarak dizinin ortasındaki değer referans alınır.
*   - Aranan değerle karşılaştırılır; küçükse sol, büyükse sağ tarafa gidilir.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Örnek Senaryo:
*  Dizi: 4,12,16,17,22,26,30,40
*  Aranan: 16
*  Eleman Sayısı: 8
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 1️⃣ Adım
*   - Dizi uzunluğu: 8
*   - Ortanca indeks: 8 / 2 = 4 → 4. indeks (22)
*   - 16 < 22 → Sol alt diziye geçilir
*   - Yeni alt dizi: 4,12,16,17
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 2️⃣ Adım
*   - Alt dizinin ortası: 4 / 2 = 2 → 2. indeks (16)
*   - Aranan değer: 16 → Bulundu!
*   - Yeni alt dizi: 4,12
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 Algoritmanın Hızı
*   - Her adımda arama alanını ikiye böler
*   - Performans: O(log2(n))
*   - Örnek: 1 milyar kayıt içinden sadece ~30 karşılaştırma ile bulunabilir
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 ABAP’ta Performans
*   - Tek kayıt okumak için READ TABLE
*   - Birden fazla kayıt için LOOP AT ... ENDLOOP
*   - Binary Search ile READ TABLE çok daha hızlı olur
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 ABAP Binary Search Mantığı
*   1. Önce arama yapılacak tablo sıralanır
      SORT itab BY V1 V2 V3.
*   2. READ TABLE komutu sonuna BINARY SEARCH eklenir
      READ TABLE itab WITH KEY V1 = p_v1
                               V2 = p_v2
                               V3 = p_v3
                             BINARY SEARCH.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 🔹 Normal Uygulama Örneği
*   - GT_DATA içindeki alanları GT_MARA’dan çekelim:
      LOOP AT gt_data.
        READ TABLE gt_mara WITH KEY matnr = gt_data-matnr.
        IF sy-subrc = 0.
          " MARA tablosundaki alanları GT_DATA'ya aktar
            ls_data-maktx = ls_mara-maktx.
            ls_data-mtart = ls_mara-mtart.

          " Güncellenmiş satırı GT_DATA tablosuna geri yaz
            MODIFY gt_data FROM ls_data.
        ENDIF.
      ENDLOOP.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
* 🔹 Binary Search ile Uygulama 
*--------------------------------------------------------------------*
* 1️⃣ Önce internal tablo sıralanır
SORT gt_data BY matnr.

* 2️⃣ Ardından READ TABLE sonuna BINARY SEARCH eklenir
LOOP AT gt_data.
  READ TABLE gt_mara
       WITH KEY matnr = gt_data-matnr
       BINARY SEARCH.
  IF sy-subrc = 0.
    " MARA tablosundaki alanları GT_DATA'ya aktar
    ls_data-maktx = ls_mara-maktx.
    ls_data-mtart = ls_mara-mtart.

    " Güncellenmiş satırı GT_DATA tablosuna geri yaz
    MODIFY gt_data FROM ls_data.
  ENDIF.
ENDLOOP.

*--------------------------------------------------------------------*
* 🔹 Önemli Not
*   - Eğer GT_MARA tablosunda aynı malzemeden birden fazla kayıt olursa
*     LOOP + BINARY SEARCH hata verebilir
*   - Bu durumda çözüm SORTED veya HASHED tablolar kullanmaktır
*   - SORTED ve HASHED tablolar performans açısından birbirine yakındır
*--------------------------------------------------------------------*