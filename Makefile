.DEFAULT_GOAL = exe
.PHONY: clean exe

EXE = \
	check_base/check_base.exe \
	compact/compact.exe \
	dag2html/main.exe \
	gwFix/gwFixBase.exe \
	gwFix/gwFixBurial.exe \
	gwFix/gwFixEvtSrc.exe \
	gwFix/gwFixFromFile.exe \
	gwFix/gwFixFromFileAlias.exe \
	gwFix/gwFixFromFileDomicile.exe \
	gwFix/gwFixY.exe \
	gwbase/chkimg.exe \
	gwbase/clavier.exe \
	gwbase/consmoy.exe \
	gwbase/geneanet.exe \
	gwbase/hist.exe \
	gwbase/lune.exe \
	gwbase/nbdesc.exe \
	gwbase/probot.exe \
	gwbase/selroy.exe \
	gwbase/titres.exe \
	gwpublic/gwiftitles.exe \
	gwpublic/gwprivate.exe \
	gwpublic/gwpublic.exe \
	gwpublic/gwpublic1.exe \
	gwpublic/gwpublic2.exe \
	gwpublic/gwpublic2priv.exe \
	history/convert_hist.exe \
	history/fix_hist.exe \
	history/is_gw_plus.exe \
	i18n_check/i18n_check.exe \
	jsonExport/gwJsonExport.exe \
	lex/lex_utils.exe \
	misc/lower_string.exe \
	oneshot/gwBaseCompatiblePlus.exe \
	oneshot/gwExportAscCSV.exe \
	oneshot/gwFixAddEvent.exe \
	oneshot/gwFixDateText.exe \
	oneshot/gwMostAsc.exe \
	oneshot/gwRemoveImgGallery.exe \

%.exe:
	dune build $@

exe:
	dune build $(EXE)

clean:
	dune clean
