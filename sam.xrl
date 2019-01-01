%%% File: sam.xrl
%%% Author: Benno Falkner
%%% Purpose: Token definition for SAM

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
-export([write_html/1,write_html/2,version/0]).
-export([new_buffer/0,buffer_init/0,buffer_dump/1]).

%% Version Output
version() -> {ok,"0.01a"}.

% convert to html
write_html(TokenList) -> 
    write_html(standard_io,TokenList).
write_html(Device,TokenList) ->
    %io:format("~p~n",[TokenList]),
    State = {Device,[]}, % Device, cmd stack
    io:format(Device,"<!-- sam (~p)-->~n",[version()]),
    Res = lists:foldl(fun html/2,State,TokenList),
    io:format(Device,"<!-- done. stack: ~p-->~n",[Res]),
    ok.

%% HTML section 
%%
%%

% Handle Marked
html({mark_start,Line,_},{Device,Stack}) ->
    io:format(Device,"<!-- marked [~p]-->",[Line]),
    {new_buffer(),[{mark_start,Line,Device}|Stack]};
html({mark_end_att,Line,Data},{Device,Stack}) ->
    {Device2,Stack2} = html({lineend,Line,[]},{Device,Stack}),
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
html({mark_end,Line,_},{Device,Stack}) ->
    {Device2,Stack2} = html({lineend,Line,[]},{Device,Stack}),
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
html({header,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- header [~p]-->~n<h~w>",[Line,Data]),
    {Device,[{header,Line,Data}|Stack]};
html({lineend,Line,Data},{Device,[{header,_,Val}|Stack]}) ->
    io:format(Device,"</h~w>",[Val]),
    html({lineend,Line,Data},{Device,Stack});


% handle blockquote
html({block,Line,Data},{Device,Stack}) ->
    Stack1 = case Stack of 
        [{block,_,_}|_] -> Stack;
        _ -> 
            io:format(Device,"<!-- block [~p]-->~n<blockquote>~n",[Line]),
            [{block,Line,start}|Stack]
    end,
    {Device,[{block,Line,Data}|Stack1]};
html({lineend,Line,Data},{Device,[{block,_,Data1}|Stack]}) ->
    case Data1 of 
        start ->
            io:format(Device,"</blockquote>",[]),
            html({lineend,Line,Data},{Device,Stack});
        _ -> 
            io:format(Device,"<br />~n",[]),
            {Device,Stack}
    end;

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
html({escape,_,Data},{Device,Stack}) when Data =/= "\\n" ->
    io:format(Device,"<br />~n",[]),
    {Device,Stack};
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

% Handle math
html({math_inline,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- math_inline [~p]-->i~s",[Line,Data]),
    {Device,Stack};
html({math_block,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- math_block [~p]-->~n~s~n",[Line,Data]),
    {Device,Stack};

% Handle code block
html({code_inline,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- code_inline [~p]--><code>~s</code>",[Line,string:trim(Data,both,"`")]),
    {Device,Stack};
html({code_block,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- code_block [~p] Data=\"\"-->~n<pre><code>~s</code></pre>~n",[Line,string:trim(Data,both,"`\r\n")]),
    {Device,Stack};

% Development output
html({Type,Line,Data},{Device,Stack}) ->
    io:format(Device,"<!-- UNKOWN TOKEN [~p] ~p: ~p-->~n",[Line,Type,Data]),
    {Device,Stack}.
    


    
%%% Buffer 

new_buffer() -> 
    spawn(?MODULE,buffer_init,[]).

buffer_dump(Buffer) -> 
    Buffer ! {io_dump,self()},
    receive
        Data -> Data
    end.

buffer_init() -> 
    buffer_run(self(),[]).

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

