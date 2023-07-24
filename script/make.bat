@echo off
pushd 

cd %~dp0
cd ..

rem del /F /S /Q .\\src\\proto\\pb2\\*_pb2.erl
rebar3 compile

popd
@echo on

