%%% File: sam.xrl
%%% Author: Benno Falkner
%%% Purpose: Token definition for SAM

Definitions.
PARENS     = [\(\)]
BPARENS    = [\[\]]
L          = [A-Za-z_\$]
D          = [0-9]
F          = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
HEX        = 0x[0-9]+

WS         = ([\000-\s]|%.*)
SPACE      = (\s|\t)
NWS        = [^\000-\s]

EOF        = \000
LINEBREAK  = \n\000
COMMENT    = %
HEADER     = \#
ITALIC     = \*
BOLD       = \*\*
MATH       = \$
INCLUDE    = @
BLOCK      = >

CODE       = `
CODE_BLOCK = ```

OLIST      = ({SPACE}*{D}+\.\s)
ULIST      = ({SPACE}*\*+\s)
ESCAPE_SEQ = \\
              
SPLIT    = {LINEBREAK}{ESCAPE_SEQ}{ITALIC}{CODE}{COMMENT}{HEADER}
TEXT     = ^{SPLIT}


Rules. 


% LineSpliter
{HEADER}+                                  : {token,{header,TokenLine,length(TokenChars)}}.
({SPACE}*{D}+\.\s)                         : {token,{ordered_list,TokenLine, TokenChars}}.   % ordererd list
({SPACE}*\*+\s)                            : {token,{unordered_list,TokenLine, TokenChars}}. % unordered list
`([^`]|```)+`                              : {token,{code_inline, TokenLine, TokenChars}}.  % code inline
{CODE_BLOCK}([^`]|`[^`])+{CODE_BLOCK}      : {token,{code_block, TokenLine, TokenChars}}.   % code bock
{BOLD}                                     : {token,{bold,TokenLine,TokenChars}}.
{ITALIC}                                   : {token,{italic,TokenLine,TokenChars}}.
{ESCAPE_SEQ}.                              : {token,{escape,TokenLine,TokenChars}}.
[{TEXT}]+                                  : {token,{text,TokenLine,TokenChars}}.
{COMMENT}.*                                : skip_token.
[{LINEBREAK}]                              : {token,{lineend,TokenLine,TokenChars}}.    


Erlang code.
%% added code for sam
%%
-export([write_html/1,write_html/2,version/0]).

%% Version Output
version() -> {ok,"0.01a"}.

% convert to html
write_html(TokenList) -> 
    write_html(standard_io,TokenList).
write_html(Device,TokenList) ->
    State = {Device,[]}, % Device, cmd stack
    io:format(Device,"<!-- sam (~p)-->~n",[version()]),
    Res = lists:foldl(fun html/2,State,TokenList),
    io:format(Device,"<!-- done. stack: ~p-->~n",[Res]),
    ok.

%% HTML section 
%%
%%


 
% Handle header
html({header,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- header [~p]-->~n<h~w>",[Line,Data]),
    {Device,[{header,Line,Data}|Stack]};
html({lineend,Line,Data},{Device,[{header,_,Val}|Stack]}) ->
    io:format(Device,"</h~w>",[Val]),
    html({lineend,Line,Data},{Device,Stack});

% handle ordered lists
html({ordered_list,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        [{ordered_list,_,_}|_] -> Stack;
        _ -> 
            io:format(Device,"<!-- ordered [~p]-->~n<ol>~n",[Line]),
            [{ordered_list,Line,start}|Stack]
    end,
    io:format(Device,"<li>",[]),
    {Device,[{ordered_list,Line,Data}|Stack1]};
html({lineend,Line,Data},{Device,[{ordered_list,_,Data1}|Stack]}) ->
    case Data1 of 
        start ->
            io:format(Device,"</ol>",[]),
            html({lineend,Line,Data},{Device,Stack});
        _ -> 
            io:format(Device,"</li>~n",[]),
            {Device,Stack}
    end;

% handle unordered lists
html({unordered_list,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        [{unordered_list,_,_}|_] -> Stack;
        _ -> 
            io:format(Device,"<!-- unordered [~p] -->~n<ul>~n",[Line]),
            [{unordered_list,Line,start}|Stack]
    end,
    io:format(Device,"<li>",[]),
    {Device,[{unordered_list,Line,Data}|Stack1]};
html({lineend,Line,Data},{Device,[{unordered_list,_,Data1}|Stack]}) ->
    case Data1 of 
        start -> 
            io:format(Device,"</ul>",[]),
            html({lineend,Line,Data},{Device,Stack});
        _ -> 
            io:format(Device,"</li>~n",[]),
            {Device,Stack}
    end;

% Escape sequence
html({escape,_,Data},{Device,Stack}) ->
    io:format(Device,"~s",[Data]),
    {Device,Stack};

% italic
html({italic,_,_},{Device,[{italic,_,_}|Stack]}) ->
    io:format(Device,"</i>",[]),
    {Device,Stack};
html({italic,Line,Data},{Device,Stack}) ->
    io:format(Device,"<i>",[]),
    {Device,[{italic,Line,Data}|Stack]};

% bold
html({bold,_,_},{Device,[{bold,_,_}|Stack]}) ->
    io:format(Device,"</b>",[]),
    {Device,Stack};
html({bold,Line,Data},{Device,Stack}) ->
    io:format(Device,"<b>",[]),
    {Device,[{bold,Line,Data}|Stack]};


% Handle text
html({text,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        []->
            io:format(Device,"<p>",[]),         
            [{paragraph,Line,start}|Stack];
        _ -> Stack
    end,
    io:format(Device,"~s",[Data]),
    {Device,Stack1};
html({lineend,Line,Data},{Device,[{paragraph,_,_}|Stack]}) ->
    io:format(Device,"</p>",[]),
    html({lineend,Line,Data},{Device,Stack});

% Handle line ends
html({lineend,Line,_},{Device,[H|Stack]}) ->
    io:format(Device,"<!--end line: ~p last stack ~p-->~n",[Line,H]),
    {Device,[H|Stack]};
html({lineend,_,_},{Device,Stack}) ->
    io:format(Device,"~n",[]),
    {Device,Stack};

% Handle code block
html({code_inline,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- code_inline [~p]--><code>~s</code>",[Line,Data]),
    {Device,Stack};
html({code_block,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- code_block [~p]-->~n<pre><code>~s</code></pre>~n",[Line,Data]),
    {Device,Stack};

% Development output
html({Type,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- UNKOWN TOKEN [~p] ~p: ~p-->~n",[Line,Type,Data]),
    {Device,Stack}.
    


    




