# Giriş – ABAP’ta Performans

SAP ABAP programları genellikle test ortamında hızlı çalışır. Ancak canlı kullanıma geçtikten sonra, özellikle raporlarda ciddi yavaşlamalar ortaya çıkabilir. Örneğin 100.000 belge okuyan bir rapor saatler sürebilir; fakat doğru optimizasyon teknikleriyle bu süre birkaç dakikaya indirilebilir.  

Performans çoğu zaman geliştiriciler tarafından göz ardı edilir. Bunun sebebi, performans problemlerinin genellikle müşteri tarafından daha sonra fark edilmesi veya konunun zahmetli/zor görülmesidir. Oysa aslında performans geliştirmeleri oldukça kolay ve zevkli olabilir.  

Unutulmamalıdır ki yazdığımız her satır kod, bir programcı olarak imzamızdır. Bu yüzden performansa her zaman önem vermeliyiz.  

---

## Temel Kurallar

- Program ve veritabanı arasındaki trafiği minimum seviyeye indirmek (***)  
- Veritabanından yalnızca ihtiyaç duyulan alanları okumak (*)  
- Tabloları ayrı ayrı okumak yerine **JOIN** kullanmak (***)  
- Döngü içerisinde veritabanı erişiminden kaçınmak (ör. `LOOP` içinde `SELECT` kullanmamak) (***)  
- Internal tablolarda okuma yaparken veya iç içe (nested loop) döngülerde sıralama algoritmalarını kullanmak (***)  
- Anahtar alanlar dışında erişim yapılan büyük tablolarda mutlaka **index** kullanmak (***)  
- Index oluşturulamayan tablolarda (örn: **BSEG**), ara tablolardan faydalanıp anahtar alanlarla erişim yapmak (***)  
- Döngülerde `INTO` yerine `ASSIGNING` kullanmak (*). (Performansa etkisi azdır ama iyi bir pratiktir.)  

---
