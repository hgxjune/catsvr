@echo off
pushd 

cd %~dp0
cd ..

rebar3 clean
rem echo ==== Cleaning out rebar3_gpb_plugin...
rem rmdir /S /Q .\\_build\\default\\plugins\\rebar3_gpb_plugin\\ebin
rem del /F /S /Q .\\src\\proto\\pb2\\*_pb2.erl
del /F /S /Q .\\rebar.lock


popd
@echo on

