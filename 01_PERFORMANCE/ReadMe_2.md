# 📘 ABAP Performans ve İyi Kodlama Prensipleri

## 1. Giriş
ABAP programlarında performans, çoğunlukla **veritabanına erişim** ve **döngü işlemleri** sırasında belirleyici olur.  
İyi bir ABAPçı, programını hem doğru hem de hızlı çalışacak şekilde tasarlamalıdır.

---

## 2. Olabildiğince Az Veritabanına Erişmek
- Gereksiz `SELECT` komutlarından kaçın.  
- Mümkünse verileri **tek seferde** oku.  
- `SELECT *` yerine sadece ihtiyacın olan alanları çek.  

---

## 3. Join Kullanımı
Join, ilişkili tabloları tek seferde çekmek için kullanılır.  

✅ Avantaj: Çoklu `SELECT`’lere göre daha hızlı.  
⚠️ Dezavantaj: Çok karmaşık join’ler performansı düşürebilir.  

### 3.1 Sık Kullanılan Join Çeşitleri
- **INNER JOIN** → Ortak kayıtları döndürür.  
- **LEFT OUTER JOIN** → Sol tabloyu tamamen alır, sağda olmayanlar boş gelir.  
- **RIGHT OUTER JOIN** → Sağ tabloyu tamamen alır.  
- **FULL JOIN** → Hem sağ hem sol kayıtları döndürür (nadiren kullanılır).  

---

## 4. For All Entries (FAE)
Bir tablodaki değerleri diğer tabloda filtrelemek için kullanılır.  

### 4.1 Nasıl Uygulanır
```abap
SELECT * 
  FROM mara
  INTO TABLE @DATA(it_mara)
  FOR ALL ENTRIES IN @it_matnr
  WHERE matnr = @it_matnr-matnr.

import pypandoc

```

## 4.2 Nelere Dikkat Etmeliyiz
- FAE’den (FOR ALL ENTRIES) önce tablo **boş olmamalı**.
- Duplicate (çift) kayıtlar oluşabilir → `DISTINCT` kullanılmalı.

---

## 5. İkili Arama Algoritması (Binary Search)

### 5.1 Nasıl Çalışır
- Liste sıralanmış olmalı.
- Orta eleman seçilir → aranılan değer büyükse sağa, küçükse sola gidilir.

### 5.2 Neden Hızlıdır
- Normal arama → **O(n)**
- Binary Search → **O(log n)**

### 5.3 Örnek
```abap
READ TABLE itab WITH KEY id = 100 BINARY SEARCH.
```

## 6. Sorted Table
- İçinde veri sıralı olarak tutulur.
- Binary search otomatik yapılır.
- “READ TABLE … BINARY SEARCH” yazmana gerek yok.
Ne zaman kullanmalıyız?
- Çok sık arama yapılacaksa.
- Ama INSERT işlemleri yavaştır (çünkü tablo sıralamayı korumak zorunda).

## 7. İç İçe Döngüler (Nested Loops)
- Performansı çok düşürür.
- Alternatif: Join, FAE, Hash Table.

## 8. Veritabanı Index Kullanımı
- Index, tabloya hızlı erişim sağlar.
- Primary key zaten bir index’tir.
- Gereksiz index, INSERT/UPDATE işlemlerini yavaşlatır.

## 9. Program Analizi
- SE30 – Runtime Analysis
- Kodun hangi satırının ne kadar zaman aldığını gösterir.
- Alternatif: SAT, ST05 (SQL Trace).


## 10. Internal Table Tipleri: Hashed / Standard / Sorted
- Standard Table → Arama LINEAR SEARCH ile yapılır → büyük veride yavaştır.
- Sorted Table → Tablo sıralı tutulur → arama BINARY SEARCH ile yapılır → daha hızlıdır.
- Hashed Table → “Hash algoritması” kullanır → UNIQUE KEY üzerinden çok hızlı erişim sağlar (neredeyse O(1)).
➡️ Kullanım: performans ve veri yapısı ihtiyacına göre seçim yapılır.

## 11.Buffering (Tablo Önbellekleme)
- SAP, DDIC tablolarında buffering seçeneği sunar (Single record / Generic area / Full buffering).
- Amaç: sık kullanılan tablolara veritabanına gitmeden ABAP katmanından erişmek → performans artışı.
Örn: T001 (şirket kodları) gibi çok değişmeyen tablolar genelde buffered edilir.

## 12.Field Symbol Kullanımı
- C dilindeki pointer gibi düşünülebilir.
Bellekteki alanı kopyalamadan erişmemizi sağlar → performans artışı.
Örn:
FIELD-SYMBOLS <fs> TYPE any.
LOOP AT it_data ASSIGNING <fs>.
  <fs>-field = 'X'. " direkt bellekte değiştirilir, kopya oluşturulmaz
ENDLOOP.

## 13.Parallel Processing
- Büyük işler background job veya RFC parallel task ile parçalara bölünerek paralel yapılabilir.
Örn: milyon satırlık veriyi tek bir SELECT’te değil, paketlere ayırıp farklı task’lara dağıtmak.
Transaction: SPBT (Parallel RFC).

## 14.ST05 SQL Trace
- SAP’nin performans analiz aracıdır.
- Bir programın hangi SQL’leri attığını, indeks kullanıp kullanmadığını, ne kadar süre harcadığını görebilirsin.
- Özellikle SELECT optimizasyonunda kritik.

## 15.Package Size ile SELECT
- Çok büyük veri çekerken hepsini birden almak belleği patlatabilir.
- Bunun yerine paketleme yapılır:

SELECT * FROM mara
       INTO TABLE @DATA(lt_mara)
       PACKAGE SIZE 1000.
  " Her 1000 kayıtta bir işleme sok
  PERFORM process_data USING lt_mara.
  CLEAR lt_mara.
ENDSELECT.

- Amaç: batch halinde işlemek → hem bellek hem performans kazanımı.