# Scheme Hesap Makinesi Yorumlayicisi

Bu klasor, basit bir hesap makinesi yorumlayicisinin Scheme (Guile) implementasyonunu icerir.

## Ozellikler

- Aritmetik islemler: toplama (+), cikarma (-), carpma (*), bolme (/), us alma (^)
- Parantezli ve karmasik ifadeleri destekler
- Gecersiz karakter ve operator hatalarini algilar
- Sifira bolme kontrolu yapar
- Bos giriste uygulamayi sonlandirir

## Gereksinimler

- Guile Scheme (3.0 veya uzeri)

## Calistirma

```bash
guile calculator.scm
```

## Ornek Kullanim

```
> 2 + 3
5
> 4 * (2 + 1)
12
> 10 / 2 + 3 * 2
11.0
> 2 ^ 3
8
> ((5 + 3) * 2) / 4
4.0
> 
(cikis)
```

## Hata Yonetimi

- Sifira bolme durumunda hata verir

- Gecersiz karakter veya parantez hatasinda uyari mesaji gosterir

- Tanimsiz ifade yapilarinda "Invalid expression" hatasi dondurur