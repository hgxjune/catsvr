@echo off
pushd 

cd %~dp0
cd ..

set name=catsvr_ctrl@127.0.0.1
set setcookie=zy34w8hwp3bw3f34

set c=erl
IF DEFINED name (
	set c=%c% -remsh %name%
)
IF DEFINED setcookie (
	set c=%c% -setcookie %setcookie%
)
set c=%c% -name remsh@127.0.0.1

call %c%

popd
@echo on
