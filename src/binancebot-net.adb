with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Float_Text_IO;

with Interfaces;

with GNAT.SHA256;

with AWS.Response;
with AWS.Resources;
with AWS.Messages;
with AWS.Headers;
with AWS.Translator;

with League.JSON.Objects;
with League.JSON.Documents;
with League.JSON.Values;

with Calendar_Conversions;

with Binancebot.Util;
with Binancebot.Parse;
with Binancebot.Settings;

package body Binancebot.Net is

   procedure Get_Exchange_Info
     (ExchangeInfo : out Binancebot.Parse.Type_ExchangeInfo)
   is
      use Binancebot.Util;

      Data : AWS.Response.Data;
   begin
      Data :=
        AWS.Client.Get (URL => "https://data.binance.com/api/v3/exchangeInfo");
      --Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Data)'Img);
      --if AWS.Response.Status_Code (Result) in AWS.Messages.S200 then
      --Ada.Text_IO.Put_Line(AWS.Response.Message_Body (Data));
      ExchangeInfo := Binancebot.Parse.Parse_ExchangeInfo (Data);
   end Get_Exchange_Info;

   function Get_Ticker_Price (Symbol : String) return Long_Float is
      use Binancebot.Util;

      Data : AWS.Response.Data;

   begin
      --Ada.Text_IO.Put_Line("Req Ticker: " & Symbol);
      Data :=
        AWS.Client.Get
          (URL =>
             "https://api1.binance.com/api/v3/ticker/price?symbol=" & Symbol);
      --Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Data)'Img);
      --Ada.Text_IO.Put_Line(AWS.Response.Message_Body (Data));
      if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
         --Ada.Text_IO.Put_Line(AWS.Response.Message_Body (Data));
         return Binancebot.Util.U2F(Binancebot.Parse.Parse_TickerPrice (Data));
      else
         return 0.0;
      end if;
   end Get_Ticker_Price;

   function Get_Ticker_Price (Symbol : in String;
                             String : out League.Strings.Universal_String) return Long_Float is
      use Binancebot.Util;

      Data : AWS.Response.Data;

   begin
      --Ada.Text_IO.Put_Line("Req Ticker: " & Symbol);
      Data :=
        AWS.Client.Get
          (URL =>
             "https://api1.binance.com/api/v3/ticker/price?symbol=" & Symbol);
      --Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Data)'Img);
      --Ada.Text_IO.Put_Line(AWS.Response.Message_Body (Data));
      if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
         --Ada.Text_IO.Put_Line(AWS.Response.Message_Body (Data));
         String := Binancebot.Parse.Parse_TickerPrice (Data);
         return Binancebot.Util.U2F(String);
      else
         return 0.0;
      end if;
   end Get_Ticker_Price;

   function Get_Ticker_Price
     (Poll : in Binancebot.Parse.Type_TokInfo_Pull) return League.JSON.Arrays.JSON_Array
   is
      use Binancebot.Util;

      Data   : AWS.Response.Data;
      Result : Binancebot.Parse.Type_TickerPair;
      Symbols : String := "%5B%22" & Binancebot.Util.U2S(Poll.First_Pair) & "%22,%22"
        & Binancebot.Util.U2S(Poll.Second_Pair) & "%22,%22" & Binancebot.Util.U2S(Poll.Third_Pair) & "%22%5D";
      Nul : League.JSON.Arrays.JSON_Array; -- !!!!!!!
   begin
      --Ada.Text_IO.Put_Line("Req Ticker: " & Symbol);
      Data :=
        AWS.Client.Get
          (URL =>
             "https://api1.binance.com/api/v3/ticker/price?symbols=" & Symbols);
      --Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Data)'Img);
      if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then

         return Binancebot.Parse.Parse_TickerPricePoll(Data);
      else
         return Nul;
      end if;
   end Get_Ticker_Price;

   function Create_Limit_Order
     (Scrip : String; Quantity : Binancebot.Util.FFloat; Limit : String; Action : String)
      return Boolean
   is
      use Binancebot.Util;
      use Binancebot.Settings;
      use Ada.Calendar;

      Now : String :=
        Interfaces.Integer_64'Image
          (Calendar_Conversions.Ada_To_Unix_Time_No_Leap (Clock));

      Text_Quantity : String := Quantity'Image; -- f2s (Quantity, 8);

      Url : String := "symbol=" & Scrip & "&" & Action & "&type=LIMIT&timeInForce=GTC&quantity=" & Text_Quantity(2..Text_Quantity'Length-2) & "&price=" & Limit & "&recvWindow=5000&timestamp=" & Now (Now'First + 1 .. Now'Last) & "000";

      Connection : AWS.Client.HTTP_Connection;
      Data       : AWS.Response.Data;
      Headers    : AWS.Headers.List;

      C : GNAT.SHA256.Context := GNAT.SHA256.HMAC_Initial_Context (Global_Config.Private_Key);
   begin
      Headers.Add ("X-MBX-APIKEY", Global_Config.API_Token);

      AWS.Client.Create (Connection, "https://api.binance.com");
      GNAT.SHA256.Update (C, Url);
      declare
         Sign : String := GNAT.SHA256.Digest (C);
      begin
         Ada.Text_IO.Put_Line(Global_Config.Private_Key);
         AWS.Client.Post (Headers    => Headers, Data => "signature=" & Sign, Result => Data, Connection => Connection, URI => "/api/v3/order/test?" & Url);
         Ada.Text_IO.Put_Line (Url);
         Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Data)'Img);
         Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Data));
         Ada.Text_IO.Put_Line ("==");
      end;
      return True;
   end Create_Limit_Order;
end Binancebot.Net;
