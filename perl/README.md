# Perl Hesap Makinesi Yorumlayicisi

Bu klasor, basit bir hesap makinesi yorumlayicisinin Perl implementasyonunu icerir.

## Ozellikler

- Aritmetik islemler: toplama (+), cikarma (-), carpma (*), bolme (/), us alma (^)
- Parantezli ifadeleri cozumleyebilme
- Gecersiz ifade kontrolu
- Bos giris yapildiginda uygulamayi sonlandirma

## Gereksinimler

- Perl 5.30 veya uzeri

## Calistirma

```bash
perl calculator.pl
```

## Ornek Kullanim

```
> 2 + 3
5
> 4 * (2 + 1)
12
> 10 / 2 + 3 * 2
11
> 2 ^ 3
8
> ((5 + 3) * 2) / 4
4
> 
(cikis)
```

## Hata Yonetimi

- Sifira bolme girildiginde uyari verir
- Gecersiz karakter veya operator kullanimi durumunda hata mesaji verir
- Parantez hatalari ve sozdizimi hatalari kontrol edilir