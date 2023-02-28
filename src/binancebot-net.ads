with AWS;
with AWS.Client;

with League.Strings;
with League.JSON.Arrays;

with Binancebot.Parse;
with Binancebot.Util;

package Binancebot.Net is

   procedure Get_Exchange_Info
     (ExchangeInfo : out Binancebot.Parse.Type_ExchangeInfo);
   function Get_Ticker_Price (Symbol : String) return Long_Float;
   function Get_Ticker_Price (Symbol : in String;
                              String : out League.Strings.Universal_string) return Long_Float;
   function Get_Ticker_Price
     (Poll : in Binancebot.Parse.Type_TokInfo_Pull) return League.JSON.Arrays.JSON_Array;
   function Create_Limit_Order
     (Scrip : String; Quantity : Binancebot.Util.FFloat; Limit : String; Action : String)
      return Boolean;

end Binancebot.Net;
