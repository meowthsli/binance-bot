with Ada.Characters.Conversions;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Wide_Wide_Text_IO;

with AWS.Response;
with AWS.Client;

with League.Strings;

with Binancebot.Net;
with Binancebot.Parse;
with Binancebot.Triangular;
with Binancebot.Settings;
with Binancebot.Cmdargs;
with Binancebot.Util;

procedure Run is
   use Ada.Containers;
   use Ada.Characters.Conversions;

   ExchangeInfo : Binancebot.Parse.Type_ExchangeInfo;
   -- Binancebot_Config.Crate_Version
begin
   Ada.Text_IO.Put_Line ("Binance bot (version 00.roll.2702) is running");
   Binancebot.Settings.Get_Settings_From_File(Binancebot.Settings.Global_Config);
   Ada.Text_IO.Put_Line ("===================");
   Binancebot.Net.Get_Exchange_Info (ExchangeInfo);

   Ada.Text_IO.Put_Line ("Was founded "
                         & Count_Type'Image (ExchangeInfo.Trading_Pull.Length)
                         & " polls for trading");

   for Pair of ExchangeInfo.Trading_Pull loop
      declare
         use Binancebot.Util;

         -- GInfo : String := " Generic " & To_String(Pair.Pair.To_Wide_Wide_String) & " minQty - " & f2s(Pair.MinQTY,10) & " stepSize - " & f2s(Pair.StepSize,10) & " maxQty - " & f2s(Pair.MaxQty,10);
         Info : String := " First " & To_String(Pair.First_Pair.To_Wide_Wide_String) & " minQty - " & f2s(Pair.First_MinQTY,10) & " stepSize - " & f2s(Pair.First_StepSize,10) & " maxQty - " & f2s(Pair.First_MaxQty,10);
         Info2 : String := " Second " & To_String(Pair.Second_Pair.To_Wide_Wide_String) & " minQty - " & f2s(Pair.Second_MinQTY,10) & " stepSize - " & f2s(Pair.Second_StepSize,10) & " maxQty - " & f2s(Pair.Second_MaxQty,10);
         Info3 : String := " Third " & To_String(Pair.Third_Pair.To_Wide_Wide_String) & " minQty - " & f2s(Pair.Third_MinQTY,10) & " stepSize - " & f2s(Pair.Third_StepSize,10) & " maxQty - " & f2s(Pair.Third_MaxQty,10);
         Result : Boolean;
      begin
         -- Ada.Text_IO.Put_Line(GInfo);
         -- Ada.Text_IO.Put_Line(Info);
         -- Ada.Text_IO.Put_Line(Info2);
         -- Ada.Text_IO.Put_Line(Info3);
         -- Ada.Text_IO.Put_Line("----");

         Binancebot.Triangular.Perform_Triangular_Arbitrage(Poll => Pair,
                                                            Arbitrage_type =>  1,
                                                            Initial_investment =>  Binancebot.Settings.Global_Config.INVESTMENT_AMOUNT_DOLLARS,
                                                            Transaction_brokerage => Binancebot.Settings.Global_Config.BROKERAGE_PER_TRANSACTION_PERCENT,
                                                            Min_Profit => Binancebot.Settings.Global_Config.MIN_PROFIT_DOLLARS);
         -- delay 1.0;

         Binancebot.Triangular.Perform_Triangular_Arbitrage(Poll => Pair,
                                                            Arbitrage_type =>  2,
                                                            Initial_investment =>  Binancebot.Settings.Global_Config.INVESTMENT_AMOUNT_DOLLARS,
                                                            Transaction_brokerage => Binancebot.Settings.Global_Config.BROKERAGE_PER_TRANSACTION_PERCENT,
                                                            Min_Profit => Binancebot.Settings.Global_Config.MIN_PROFIT_DOLLARS);
      end;
   end loop;
   --Ada.Text_IO.Put_Line("==================");
   --Ada.Text_IO.Put_Line("Get Ticker Price");
   --Binancebot.Net.Get_Ticker_Price;
   --Ada.Text_IO.Put_Line("Price: "& TMP'Image);
end Run;
