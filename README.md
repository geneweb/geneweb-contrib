# GeneWeb-contrib

Useful scripts for geneweb not included in official release

## Build status

|            | Linux                       | macOS                        | Windows (mingw64)
| ---:       | :---:                       | :---:                        | :---:
| OCaml 4.05 | [![linux-405]][travis-link] | [![macosx-405]][travis-link] | -
| OCaml 4.06 | [![linux-406]][travis-link] | [![macosx-406]][travis-link] | -
| OCaml 4.07 | [![linux-407]][travis-link] | [![macosx-407]][travis-link] | [![win-407]][appveyor-link]
| OCaml 4.08 | [![linux-408]][travis-link] | [![macosx-408]][travis-link] | -

[linux-405]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/1
[linux-406]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/2
[linux-407]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/3
[linux-408]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/4
[macosx-405]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/5
[macosx-406]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/6
[macosx-407]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/7
[macosx-408]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/geneweb-contrib/branches/master/8
[win-407]:https://ci.appveyor.com/api/projects/status/5a5yk7jvxk332pxu/branch/master?svg=true
[travis-link]:https://travis-ci.org/geneweb/geneweb
[appveyor-link]:https://ci.appveyor.com/project/geneweb/geneweb

## Build instruction

```
    opam pin add geneweb https://github.com/geneweb/geneweb.git --no-action
    opam depext geneweb
    opam install geneweb yojson
    make exe
```

## Executables list

| Executable | Description
| ---: | :---
|check_base/check_base.exe|
|dag2html/main.exe|
|gwFix/gwFixBase.exe|
|gwFix/gwFixBurial.exe|
|gwFix/gwFixEvtSrc.exe|
|gwFix/gwFixFromFile.exe|
|gwFix/gwFixFromFileAlias.exe|
|gwFix/gwFixFromFileDomicile.exe|
|gwFix/gwFixY.exe|
|gwbase/chkimg.exe|
|gwbase/clavier.exe|
|gwbase/consmoy.exe|
|gwbase/geneanet.exe|
|gwbase/hist.exe|
|gwbase/lune.exe|
|gwbase/nbdesc.exe|
|gwbase/probot.exe|
|gwbase/selroy.exe|
|gwbase/titres.exe|
|gwpublic/gwiftitles.exe|
|gwpublic/gwprivate.exe|
|gwpublic/gwpublic.exe|
|gwpublic/gwpublic1.exe|
|gwpublic/gwpublic2.exe|
|gwpublic/gwpublic2priv.exe|
|history/convert_hist.exe|
|history/fix_hist.exe|
|history/is_gw_plus.exe|
|i18n_check/i18n_check.exe|
|jsonExport/gwJsonExport.exe|
|lex/lex_utils.exe|
|misc/lower_string.exe|
|oneshot/gwBaseCompatiblePlus.exe|
|oneshot/gwExportAscCSV.exe|
|oneshot/gwFixAddEvent.exe|
|oneshot/gwFixDateText.exe|
|oneshot/gwMostAsc.exe|
|oneshot/gwRemoveImgGallery.exe|
