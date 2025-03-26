# Cok Dilli Hesap Makinesi Yorumlayicisi

### BIL 395
#### Adi Soyadi:
Hayrettin Kaan Ozsoy

#### Ogrenci No:
201104086

Bu proje, bes farkli programlama dilinde basit bir aritmetik ifade yorumlayicisinin implementasyonunu icerir.

## Desteklenen Ozellikler

- Aritmetik islemler: toplama (+), cikarma (-), carpma (*), bolme (/), us alma (^)
- Parantezli ve karmasik ifadeleri cozumleme
- Sifira bolme kontrolu
- Sozdizimi hatasi yakalama

## Kullanilan Diller

Proje asagidaki programlama dillerinde yorumlayici implementasyonlari icerir:

1. Rust  
2. Ada  
3. Perl  
4. Scheme  
5. Prolog

## Kurulum ve Calistirma

Her dil icin ayri klasorlerde kodlar bulunmaktadir. Her klasorde ilgili dile ozel `README.md` dosyasinda detayli kurulum ve calistirma adimlari yer almaktadir.

### Rust
```bash
cd rust
cargo build
cargo run
```

### ADA
```bash
cd ada
gnatmake calculator.adb
./calculator
```

### Perl
```bash
cd perl
perl calculator.pl
```

### Scheme
```bash
cd scheme
guile calculator.scm
```

### Prolog
```bash
cd prolog
swipl calculator.pl
```

## Ornek Kullanim

Her dil icin ornek giris ve cikis senaryolari, kendi klasoru altindaki README.md dosyasinda mevcuttur. 

## Hata Yonetimi

Tum implementasyonlarda asagidaki hata durumlari kontrol edilmektedir:

- Sifira bolme hatasi

- Gecersiz karakter veya operator kullanimi

- Parantez eksikligi veya yapisal hatalar
