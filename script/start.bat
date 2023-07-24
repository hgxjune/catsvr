@echo off
pushd 

cd %~dp0
cd ..

set P=262144
set name=catsvr_001@127.0.0.1
set setcookie=zy34w8hwp3bw3f34
set boot=start_sasl
set kernel=
set config=config/sys.config
set hidden=true
set detached=
set mnesia=mnesia


set c=erl

IF DEFINED P (
	set c=%c% +P %P%
)
IF DEFINED name (
	set c=%c% -name %name%
)
IF DEFINED setcookie (
	set c=%c% -setcookie %setcookie%
)
IF DEFINED boot (
	set c=%c% -boot %boot%
)
IF DEFINED kernel (
	set c=%c% -kernel %kernel%
)
IF DEFINED config (
	set c=%c% -config %config%
)
IF DEFINED hidden (
	set c=%c% -hidden %hidden%
)
IF DEFINED detached (
	set c=%c% -detached
)
IF DEFINED mnesia (
	set c=%c% -mnesia dir %mnesia%
)
set c=%c% -s main start

set c=%c% -pa _build\\default\\lib\\bson\\ebin
set c=%c% -pa _build\\default\\lib\\cowboy\\ebin
set c=%c% -pa _build\\default\\lib\\cowlib\\ebin
set c=%c% -pa _build\\default\\lib\\gamechat\\ebin
set c=%c% -pa _build\\default\\lib\\goldrush\\ebin
set c=%c% -pa _build\\default\\lib\\gpb\\ebin
set c=%c% -pa _build\\default\\lib\\jsx\\ebin
set c=%c% -pa _build\\default\\lib\\lager\\ebin
set c=%c% -pa _build\\default\\lib\\metronome\\ebin
set c=%c% -pa _build\\default\\lib\\mongodb\\ebin
set c=%c% -pa _build\\default\\lib\\pbkdf2\\ebin
set c=%c% -pa _build\\default\\lib\\poolboy\\ebin
set c=%c% -pa _build\\default\\lib\\ranch\\ebin
set c=%c% -pa _build\\default\\lib\\catsvr\\ebin

rem for /d %%i in (_build\\default\\lib\\*) do ( echo %%i\\ebin )

echo %c%
call %c%

popd
@echo on
