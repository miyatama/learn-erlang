%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%  
%%  日本語版の『プログラミングErlang』、およびサンプルコードについては、
%%   http://ssl.ohmsha.co.jp/cgi-bin/menu.cgi?ISBN=978-4-274-06714-3 を確認してください。
%% ---
-module(test1).

-export([fac/1]).

fac(0) -> 1;
fac(N) -> N*fac(N-1).
