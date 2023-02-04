with Ada.Text_IO;

with Binancebot.Net;

procedure Run is
begin
   Ada.Text_IO.Put_Line("Binance bot running");
   Binancebot.Net.Get_Exchange_Info;
end Run;
