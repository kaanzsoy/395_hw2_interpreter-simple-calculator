# Rust Hesap Makinesi Yorumlayicisi

Bu klasor, basit bir hesap makinesi yorumlayicisinin Rust implementasyonunu icerir.

## Ozellikler

- Aritmetik islemler: toplama (+), cikarma (-), carpma (*), bolme (/), us alma (^)
- Parantezli ve karmasik ifadeleri cozer
- Sifira bolme kontrolu
- Gecersiz ifade durumlarinda hata mesaji verir
- Bos giris yapildiginda uygulamayi sonlandirir

## Gereksinimler

- Rust (1.70.0 veya uzeri)
- Cargo

## Kurulum

```bash
cargo build
```

## Calistirma

```bash
cargo run
```

## Ornek Kullanim

```
> 3 + 4
7
> 2 * (5 + 1)
12
> 8 / 2 + 2 * 3
10
> 2 ^ 3
8
> ((2 + 3) * 4) / 2
10
> 
(cikis)
```

## Hata Yonetimi

- Sifira bolme durumunda uyari verir

- Tanimsiz operator veya parantez hatasi varsa hata mesaji dondurur

- Sozdizimi hatalarinda 'Invalid expression' mesaji gosterir