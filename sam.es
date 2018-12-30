#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%%! -i 



% test path
main([Mode,Input,Test,Out]) when Mode =:= "test" -> 
    io:format("Leex: ~s~n",[Input]),
    {ok, L_File} = leex:file(Input),
    io:format("Compile: ~s~n",[L_File]),
    {ok,Module} = c:c(L_File),
    io:format("Testing:     ~s~n",[Module]),
    io:format("   input:    ~s~n",[Test]),
    {ok,Text} = file:read_file(Test),
    {ok,Tokens,_} = Module:string(binary:bin_to_list(Text)),
    io:format("   output:   ~s~n",[Out]),
    case file:open(Out,[write,delayed_write]) of 
        {ok,File} ->
            io:format("   processing ..."),
            Module:write_html(File,Tokens),
            file:close(File),
            io:format(" done.~n");
        {error,Reason} ->
            io:format("ERROR: ~p~n",[Reason])
        end,
    halt(0);

% test path
main([Mode,Input,Test]) when Mode =:= "test" -> 
    io:format("Leex: ~s~n",[Input]),
    {ok, L_File} = leex:file(Input),
    io:format("Compile lexer: ~s~n",[L_File]),
    {ok,Module} = c:c(L_File),
    io:format("Testing:     ~s~n",[Module]),
    io:format("   input:    ~s~n",[Test]),
    {ok,Text} = file:read_file(Test),
    {ok,Tokens,_} = Module:string(binary:bin_to_list(Text)),
    Module:write_html(Tokens),
    halt(0);

% build an erl file from xlr
main([Mode,Input]) when Mode =:= "leex" ->
    io:format("Leex: ~s\n",[Input]),
    leex:file(Input);

% build an erl file from xlr
main([Mode,Input]) when Mode =:= "compile" ->
    io:format("Compile: ~s\n",[Input]),
    c:c(Input);

% default path
main (_) ->
    usage().

% describing usage
usage () -> 
    io:format("SAM Erlang Parser Generator\n"),
    io:format("usage: <mode> [args]\n"),
    io:format("else this info is printed.\n"),
    halt(0).

