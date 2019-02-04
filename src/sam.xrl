%%% File: sam.xrl
%%% Author: Benno Falkner
%%% Purpose: Token definition for SAM

%%====================================================================
%% Definitions
%%====================================================================
Definitions.
MARK_START = \[ 
MARK_END   = \]
ATT_START  = \(
ATT_END    = \)
MARK       = {MARK_START}([^{MARK_END}]|\\{MARK_END})*{MARK_END}
ATT        = {ATT_START}([^{ATT_END}]|\\{ATT_END})*{ATT_END} 
ATT_SEQ    = {MARK}\s*{ATT}       
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
BLOCK      = \>

CODE       = `
CODE_BLOCK = ```

OLIST      = ({SPACE}*{D}+\.\s)
ULIST      = ({SPACE}*\*+\s)
ESCAPE_SEQ = \\
              
SPLIT    = {LINEBREAK}{ESCAPE_SEQ}{ITALIC}{CODE}{COMMENT}{HEADER}{BLOCK}{MATH}{MARK_START}{MARK_END}
TEXT     = ^{SPLIT}


%%====================================================================
%% Rules
%%====================================================================
Rules. 


% LineSpliter
{HEADER}+                                             : {token,{header,TokenLine,length(TokenChars)}}.
({SPACE}*{BLOCK}+\s)                                  : {token,{block,TokenLine, TokenChars}}.   % block quote 
({MATH})([^{MATH}]|(\\{MATH}))*({MATH})               : {token,{math_inline,TokenLine, TokenChars}}.   % block quote 
({MATH}{MATH})([^{MATH}]|(\\{MATH}))*({MATH}{MATH})   : {token,{math_block,TokenLine, TokenChars}}.   % block quote 
({SPACE}*{D}+\.\s)                                    : {token,{ordered_list,TokenLine, TokenChars}}.   % ordererd list
({SPACE}*\*+\s)                                       : {token,{unordered_list,TokenLine, TokenChars}}. % unordered list
{CODE}([^{CODE}]|{CODE_BLOCK})+{CODE}                 : {token,{code_inline, TokenLine, TokenChars}}.  % code inline
{CODE_BLOCK}([^{CODE}]|{CODE}[^{CODE}])+{CODE_BLOCK}  : {token,{code_block, TokenLine, TokenChars}}.   % code bock
{MARK_START}                                          : {token,{mark_start,TokenLine,TokenChars}}.
{MARK_END}\s*{ATT}                                    : {token,{mark_end_att,TokenLine,TokenChars}}.
{MARK_END}\s*[^{ATT_START}]                           : {token,{mark_end,TokenLine,TokenChars}}.  % image for md compatibility
{BOLD}                                                : {token,{bold,TokenLine,TokenChars}}.
{ITALIC}                                              : {token,{italic,TokenLine,TokenChars}}.
{ESCAPE_SEQ}.                                         : {token,{escape,TokenLine,TokenChars}}.
[{TEXT}]+                                             : {token,{text,TokenLine,TokenChars}}.
[{LINEBREAK}]                                         : {token,{lineend,TokenLine,TokenChars}}.    
{COMMENT}.*                                           : skip_token.


Erlang code.
%% added code for sam
%%


-export([parse/2,parse_file/2,parse_file/3]).

-export([write_html/1,write_html/2]).
-export([new_buffer/0,buffer_init/0,buffer_dump/1]).



%%====================================================================
%% API functions
%%====================================================================

%% new api 

-type doc() :: {atom(),list(),list()}.



%% Version Output
-spec version() -> string().
version() -> "0.1.1".

%% parse input text
-spec parse(atom(),iodata()) -> doc() | tuple().
parse(html,Input) -> 
    case string(Input) of 
        {ok,Tokens,_} -> 
            Doc = {ok,[["<!-- sam (",version(),")-->\n"]],[]}, % define start doc
            case lists:foldl(fun html/2,Doc,Tokens) of
                {ok,Html,_} -> 
                    {ok,lists:flatten(lists:reverse(Html)),[]};
                {error,_,Reason} -> {error,lists:flatten(Reason)}
            end;
        R -> R
    end.

%% parse file
-spec parse_file(atom(),string()) -> doc() | tuple().
parse_file(Mode,Filen) -> 
    case file:read_file(Filen) of
        {ok,Txt} -> parse(Mode,binary:bin_to_list(Txt));
        R -> R
    end.

-spec parse_file(atom(),string(),string()) -> atom() | tuple().
parse_file(Mode,Filen,OutFilen) -> 
    case parse_file(Mode,Filen) of
        {ok,Doc,_} -> 
            file:write_file(OutFilen,Doc);
        R -> R
    end. 



%%====================================================================
%% Internal functions
%%====================================================================
%% html implementation
-spec html({atom(),integer(),list()},doc()) -> doc().

%% handle error ignore any input
html(_,{error,Output,Reason}) -> 
    {error,Output,Reason};

%% Marked 
html({mark_start,Line,_},{State,Output,Stack}) ->
    %io:format("~p: mark start~n",[Line]),
    {State,[],[{mark_start,Line,Output}|Stack]};
html({mark_start_img,Line,_},{State,Output,Stack}) ->
    {State,[],[{mark_start_img,Line,Output}|Stack]};
html({mark_end_att,Line,Data},Doc) ->
    %io:format("~p~n",[Doc]),
    {ok,Output,Stack} = html({lineend,Line,[]},Doc),
    %io:format(">>>~p~n",[Stack]),
    case Stack of 
        [{mark_start,_,OutOld}|Stack2] -> html_att(OutOld,Stack2,Output,trim_att(Data)); 
        [{mark_start_img,_,OutOld}|Stack2] -> html_att_img(OutOld,Stack2,Output,trim_att(Data));
        _ -> {error,Output,io_lib:format("closing block which was never started (~p)~n  ~w~n",[Line,Stack])} 
    end;
html({mark_end,Line,_},Doc) ->
    {State,Output,Stack} = html({lineend,Line,[]},Doc),
    case Stack of 
        [{mark_start,_,OutOld}|Stack2] -> {State,[["[",Output,"]"]|OutOld],Stack2};
        _ -> {error,Output,io_lib:format("closing block which was never started (~p)",[Line])} 
    end;

%% Header
html({header,Line,Data},{State,Output,Stack}) ->
    Txt = io_lib:format("~n<h~w>",[Data]),
    {State,[Txt|Output],[{header,Line,Data}|Stack]};
html({lineend,Line,Data},{State,Output,[{header,_,Data1}|Stack]}) ->
    Txt = io_lib:format("</h~w>~n",[Data1]),
    html({lineend,Line,Data},{State,[Txt|Output],Stack});

%% Blockquote
html({block,Line,Data},{State,Output,Stack}) ->
    case Stack of 
        [{block,_,_}|_] -> {State,Output,[{block,Line,Data}|Stack]};
        _ -> {State,["<blockquote>\n"|Output],[{block,Line,Data},{block,Line,start}|Stack]}
    end;
html({lineend,Line,Data},{State,Output,[{block,_,Data1}|Stack]}) ->
    case Data1 of
        start -> html({lineend,Line,Data},{State,["</blockquote>\n"|Output],Stack});
        _ -> {State,["<br />\n"|Output],Stack} 
    end;

%% Ordered list
html({ordered_list,Line,Data},{State,Output,Stack}) ->
    case Stack of 
        [{ordered_list,_,_}|_] -> {State,["<li>"|Output],[{ordered_list,Line,Data}|Stack]};
        _ -> {State,["<ol>\n<li>"|Output],[{ordered_list,Line,Data},{ordered_list,Line,start}|Stack]}
    end;
html({lineend,_,_},{State,Output,[{ordered_list,_,Data1}|Stack]}) ->
    case Data1 of
        start -> {State,["</ol>"|Output],Stack};
        _ -> {State,["</li>\n"|Output],Stack} 
    end;

%% Unordered list
html({unordered_list,Line,Data},{State,Output,Stack}) ->
    case Stack of 
        [{unordered_list,_,_}|_] -> {State,["<li>"|Output],[{unordered_list,Line,Data}|Stack]};
        _ -> {State,["<ul>\n<li>"|Output],[{unordered_list,Line,Data},{unordered_list,Line,start}|Stack]}
    end;
html({lineend,_,_},{State,Output,[{unordered_list,_,Data1}|Stack]}) ->
    case Data1 of
        start -> {State,["</ul>"|Output],Stack};
        _ -> {State,["</li>\n"|Output],Stack} 
    end;


%% Escape sequence
html({escape,_,Data},{State,Output,Stack}) when Data =:= "\\n"->
    {State,["<br />"|Output],Stack};
html({escape,_,Data},{State,Output,Stack}) ->
    {State,[Data|Output],Stack};

%% Italic
html({italic,_,_},{State,Output,[{italic,_,_}|Stack]}) ->
    {State,["</i>"|Output],Stack};
html({italic,Line,Data},{State,Output,Stack}) ->
    {State,["<i>"|Output],[{italic,Line,Data}|Stack]};

%% Bold
html({bold,_,_},{State,Output,[{bold,_,_}|Stack]}) ->
    {State,["</b>"|Output],Stack};
html({bold,Line,Data},{State,Output,Stack}) ->
    {State,["<b>"|Output],[{bold,Line,Data}|Stack]};

%% Text
html({text,Line,Data},{State,Output,Stack}) ->
    case Stack of
        [] -> {State,[["<p>",Data]|Output],[{paragraph,Line,start}|Stack]};
        _ ->  {State,[Data|Output],Stack}
    end; 
html({lineend,Line,Data},{State,Output,[{paragraph,_,_}|Stack]}) ->
    html({lineend,Line,Data},{State,["</p>"|Output],Stack});

%% Lineend 
html({lineend,_,_},{State,Output,[H|Stack]}) ->
    {State,Output,[H|Stack]};
html({lineend,_,_},{State,Output,[]}) ->
    {State,["\n"|Output],[]};

%% Math (TeX)
html({math_inline,_,Data},{State,Output,Stack}) ->
    {State,[Data|Output],Stack};
html({math_block,_,Data},{State,Output,Stack}) ->
    {State,["\n",Data,"\n"|Output],Stack};

%% Code Block
html({code_inline,_,Data},{State,Output,Stack}) ->
    Txt = io_lib:format("<code>~s</code>",[string:trim(Data,both,"`")]),
    {State,[Txt|Output],Stack};
html({code_block,_,Data},{State,Output,Stack}) ->
    Txt = io_lib:format("<pre><code>~s</code></pre>~n",[string:trim(Data,both,"`\r\n")]),
    {State,[Txt|Output],Stack};

%% Unkown token
html({Type,Line,Data},{State,Output,Stack}) ->
    io:format("UNKOWN TOKEN [Line: ~p] ~p: ~p~n",[Line,Type,Data]),
    Txt = io_lib:format("<!-- UNKOWN TOKEN [Line: ~p] ~p: ~p-->~n",[Line,Type,Data]),
    {State,[Txt|Output],Stack}.





%%% attributes
%%%
%%%
-spec html_att(iolist(),list(),iolist(),iolist()) -> doc().
html_att(Output,Stack,Txt,Data) -> 
    html_att_link(Output,Stack,Txt,Data).
html_att_link(Output,Stack,Txt,Data) -> 
    {ok,[["<a href=\"",Data,"\">",Txt,"</a>"]|Output],Stack}.
html_att_img(Output,Stack,Txt,Data) -> 
    {ok,[["<img src=\"",Data,"\" alt=\"",Txt,"\" />"]|Output],Stack}.


trim_att(Data) -> 
    string:trim(Data,both,":]()").






%%====================================================================
%% API functions
%%====================================================================
% convert to html
write_html(TokenList) -> 
    write_html(standard_io,TokenList).
write_html(Device,TokenList) ->
    %io:format("~p~n",[TokenList]),
    State = {Device,[]}, % Device, cmd stack
    io:format(Device,"<!-- sam (~p)-->~n",[version()]),
    Res = lists:foldl(fun html_o/2,State,TokenList),
    io:format(Device,"<!-- done. stack: ~p-->~n",[Res]),
    ok.

% buffer handler
new_buffer() -> 
    spawn(?MODULE,buffer_init,[]).

buffer_dump(Buffer) -> 
    Buffer ! {io_dump,self()},
    receive
        Data -> Data
    end.
buffer_init() -> 
    buffer_run(self(),[]).



%%====================================================================
%% Internal functions
%%====================================================================
%% HTML section 
%%
%%

% Handle Marked
html_o({mark_start,Line,_},{Device,Stack}) ->
    io:format(Device,"<!-- marked [~p]-->",[Line]),
    {new_buffer(),[{mark_start,Line,Device}|Stack]};
html_o({mark_end_att,Line,Data},{Device,Stack}) ->
    {Device2,Stack2} = html_o({lineend,Line,[]},{Device,Stack}),
    case Stack2 of 
        [{mark_start,_,Device_old}|Stack3] ->
            { ok,R } = buffer_dump(Device2),
            io:format(Device_old,"<!-- Marked ~s -->",[Data]),
            att_handle(Device_old,Data,R), 
            {Device_old,Stack3};
        [{mark_start_img,_,Device_old}|Stack3] ->
            { ok,R } = buffer_dump(Device2),
            io:format(Device_old,"<!-- Marked ~s -->",[Data]),
            att_handle(Device_old,"img " ++ Data,R), 
            {Device_old,Stack3};
        [H|_] -> 
            io:format(Device2, "<!-- ERROR closing block with no opening (~p) stack: ~p-->", [Line,H]),
            {Device2,Stack2};
        [] -> 
            io:format(Device2, "<!-- ERROR closing block with no opening (~p) empty stack -->", [Line]),
            {Device2,Stack2}            
    end;
html_o({mark_end,Line,_},{Device,Stack}) ->
    {Device2,Stack2} = html_o({lineend,Line,[]},{Device,Stack}),
    case Stack2 of 
        [{mark_start,_,Device_old}|Stack3] ->
            { ok,R } = buffer_dump(Device2),
            io:format(Device_old,"[~s]",R),
            {Device_old,Stack3};
        _ -> 
            io:format(Device2, "<!-- ERROR closing block with no opening (~p) -->", [Line]),
            {Device2,Stack2}            
    end;


 
% Handle header
html_o({header,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- header [~p]-->~n<h~w>",[Line,Data]),
    {Device,[{header,Line,Data}|Stack]};
html_o({lineend,Line,Data},{Device,[{header,_,Val}|Stack]}) ->
    io:format(Device,"</h~w>",[Val]),
    html_o({lineend,Line,Data},{Device,Stack});


% handle blockquote
html_o({block,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        [{block,_,_}|_] -> Stack;
        _ -> 
            io:format(Device,"<!-- block [~p]-->~n<blockquote>~n",[Line]),
            [{block,Line,start}|Stack]
    end,
    {Device,[{block,Line,Data}|Stack1]};
html_o({lineend,Line,Data},{Device,[{block,_,Data1}|Stack]}) ->
    case Data1 of 
        start ->
            io:format(Device,"</blockquote>",[]),
            html_o({lineend,Line,Data},{Device,Stack});
        _ -> 
            io:format(Device,"<br />~n",[]),
            {Device,Stack}
    end;

% handle ordered lists
html_o({ordered_list,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        [{ordered_list,_,_}|_] -> Stack;
        _ -> 
            io:format(Device,"<!-- ordered [~p]-->~n<ol>~n",[Line]),
            [{ordered_list,Line,start}|Stack]
    end,
    io:format(Device,"<li>",[]),
    {Device,[{ordered_list,Line,Data}|Stack1]};
html_o({lineend,Line,Data},{Device,[{ordered_list,_,Data1}|Stack]}) ->
    case Data1 of 
        start ->
            io:format(Device,"</ol>",[]),
            html_o({lineend,Line,Data},{Device,Stack});
        _ -> 
            io:format(Device,"</li>~n",[]),
            {Device,Stack}
    end;

% handle unordered lists
html_o({unordered_list,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        [{unordered_list,_,_}|_] -> Stack;
        _ -> 
            io:format(Device,"<!-- unordered [~p] -->~n<ul>~n",[Line]),
            [{unordered_list,Line,start}|Stack]
    end,
    io:format(Device,"<li>",[]),
    {Device,[{unordered_list,Line,Data}|Stack1]};
html_o({lineend,Line,Data},{Device,[{unordered_list,_,Data1}|Stack]}) ->
    case Data1 of 
        start -> 
            io:format(Device,"</ul>",[]),
            html_o({lineend,Line,Data},{Device,Stack});
        _ -> 
            io:format(Device,"</li>~n",[]),
            {Device,Stack}
    end;

% Escape sequence
html_o({escape,_,Data},{Device,Stack}) when Data =/= "\\n" ->
    io:format(Device,"<br />~n",[]),
    {Device,Stack};
html_o({escape,_,Data},{Device,Stack}) ->
    io:format(Device,"~s",[Data]),
    {Device,Stack};

% italic
html_o({italic,_,_},{Device,[{italic,_,_}|Stack]}) ->
    io:format(Device,"</i>",[]),
    {Device,Stack};
html_o({italic,Line,Data},{Device,Stack}) ->
    io:format(Device,"<i>",[]),
    {Device,[{italic,Line,Data}|Stack]};

% bold
html_o({bold,_,_},{Device,[{bold,_,_}|Stack]}) ->
    io:format(Device,"</b>",[]),
    {Device,Stack};
html_o({bold,Line,Data},{Device,Stack}) ->
    io:format(Device,"<b>",[]),
    {Device,[{bold,Line,Data}|Stack]};


% Handle text
html_o({text,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        []->
            io:format(Device,"<p>",[]),         
            [{paragraph,Line,start}|Stack];
        _ -> Stack
    end,
    io:format(Device,"~s",[Data]),
    {Device,Stack1};
html_o({lineend,Line,Data},{Device,[{paragraph,_,_}|Stack]}) ->
    io:format(Device,"</p>",[]),
    html_o({lineend,Line,Data},{Device,Stack});

% Handle line ends
html_o({lineend,Line,_},{Device,[H|Stack]}) ->
    io:format(Device,"<!--end line: ~p last stack ~p-->~n",[Line,H]),
    {Device,[H|Stack]};
html_o({lineend,_,_},{Device,Stack}) ->
    io:format(Device,"~n",[]),
    {Device,Stack};

% Handle math
html_o({math_inline,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- math_inline [~p]-->i~s",[Line,Data]),
    {Device,Stack};
html_o({math_block,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- math_block [~p]-->~n~s~n",[Line,Data]),
    {Device,Stack};

% Handle code block
html_o({code_inline,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- code_inline [~p]--><code>~s</code>",[Line,string:trim(Data,both,"`")]),
    {Device,Stack};
html_o({code_block,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- code_block [~p] Data=\"\"-->~n<pre><code>~s</code></pre>~n",[Line,string:trim(Data,both,"`\r\n")]),
    {Device,Stack};

% Development output
html_o({Type,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- UNKOWN TOKEN [~p] ~p: ~p-->~n",[Line,Type,Data]),
    {Device,Stack}.
    


    
%%% Buffer 
buffer_run(Pid,Buffer) -> 
    receive
        {io_dump,From} ->
            From ! {ok,Buffer};
        {io_request,From,Ref,{put_chars,_Encoding,Module,Func,Args}} ->
            Res = apply(Module,Func,Args),
            io:format("~p Result: ~s~n",[Pid,Res]),
            Buffer2 = Buffer ++ Res,
            From ! {io_reply,Ref,ok},
            buffer_run(Pid,Buffer2);
        Data -> 
            io:format("~p Received: ~p~n",[Pid,Data]),
            buffer_run(Pid,Buffer)
    end.



%%% Handle Attributes
att_handle(Device,Att,Data) -> 
    A = string:split(string:trim(Att,both,":]()"),"\s",leading),
    case A of 
        [S|R] when S =:= "img" -> 
            io:format(Device,"<img href=\"~s\" />~n<div>~s</div>~n",[R,Data]);
        [S|R] when S =:= "div"-> 
            io:format(Device,"<div ~s>~s</div>",[R,Data]);
        [U]   -> 
            io:format(Device,"<a href=\"~s\">~s</a>",[U,Data]);
        []    -> 
            io:format(Device,"[~s]",[Data]);
        _     ->
            io:format(Device,"[~s]",[Data])
    end.

