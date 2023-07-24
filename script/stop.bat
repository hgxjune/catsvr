@echo off
pushd 

cd %~dp0
cd ..

set name=catsvr_stop@127.0.0.1
set setcookie=zy34w8hwp3bw3f34

set c=erl

IF DEFINED name (
	set c=%c% -name %name%
)
IF DEFINED setcookie (
	set c=%c% -setcookie %setcookie%
)

set c=%c% -eval "rpc:call('catsvr_001@127.0.0.1',main, stop, []),timer:sleep(1000),halt(0)."

call %c%

popd
@echo on


