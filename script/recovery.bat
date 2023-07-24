@echo off
pushd 

cd %~dp0
cd ..

rmdir /S /Q _build
rem rmdir /S /Q logs
del /F /S /Q erl_crash.dump
del /F /S /Q rebar.lock

popd
@echo on

