# Prolog Hesap Makinesi Yorumlayicisi

Bu klasor, basit bir hesap makinesi yorumlayicisinin Prolog implementasyonunu icerir.

## Ozellikler

- Aritmetik islemler: toplama (+), cikarma (-), carpma (*), bolme (/), us alma (^)
- Parantezli ve karmasik ifadeleri destekler
- Gecersiz ifade kontrolu
- Sifira bolme kontrolu
- Bos giriste uygulamayi sonlandirma

## Gereksinimler

- SWI-Prolog (8.0 veya uzeri)

## Calistirma

```bash
swipl calculator.pl
```

## Ornek Kullanim

```
> 5 + 3
8
> 2 * (4 + 1)
10
> 8 / 2 + 3 * 2
10.0
> 2 ^ 3
8
> ((5 + 3) * 2) / 4
4.0
> 
(cikis)
```

## Hata Yonetimi

- Sifira bolme girildiginde uyari verir

- Gecersiz karakter veya operator girildiginde hata mesaji verir

- Parantez hatalari ve sozdizimi hatalari kontrol edilir