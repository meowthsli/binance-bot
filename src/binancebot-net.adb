with Ada.Text_IO;

with AWS.Response;
with AWS.Resources;
with AWS.Messages;

with Binancebot.Util;

package body Binancebot.Net is

   procedure Get_Exchange_Info is
      use Binancebot.Util;

      Data : AWS.Response.Data;
   begin
      Data := AWS.Client.Get
        (URL => "https://data.binance.com/api/v3/exchangeInfo");
      Ada.Text_IO.Put_Line(AWS.Response.Message_Body (Data));

   end Get_Exchange_Info;

end Binancebot.Net;
