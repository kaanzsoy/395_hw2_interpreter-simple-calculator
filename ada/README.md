# ADA Hesap Makinesi Yorumlayicisi

Bu klasor, basit hesap makinesi yorumlayicisinin ADA implementasyonunu icerir.

## Ozellikler

- Aritmetik islemler: toplama (+), cikarma (-), carpma (*), bolme (/), us alma (^)
- Gecersiz ifade kontrolu
- Sifira bolme hatasi kontrolu
- Girdiyi yorumlayip sonucu dondurme
- Bos giris ile uygulamayi sonlandirma

## Gereksinimler

- GNAT (GNU Ada Translator)
- make

## Kurulum

```bash
gnatmake calculator.adb
```

## Calistirma

```bash
./calculator
```

## Ornek Kullanim

```
> 5 + 3
8
> 10 * 2
20
> 18 / 3
6
> 7 - 12
-5
> 
(cikis)
```

## Hata Yonetimi

- Sifira bolme girildiginde uyari mesaji

- Gecersiz ifade girildiginde hata mesaji