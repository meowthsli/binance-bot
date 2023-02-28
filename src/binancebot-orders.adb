with Binancebot.Net;
with Binancebot.Triangular;
with Binancebot.Util;

package body Binancebot.Orders is

   use Binancebot.Triangular;

   procedure Place_Buy_Order (Scrip : String;
                              Quantity : Binancebot.Util.FFloat;
                              Limit : String)
   is
      Result : Boolean :=
        Binancebot.Net.Create_Limit_Order (Scrip, Quantity, Limit, "side=buy");
   begin
      null;
   end Place_Buy_Order;
   procedure Place_Sell_Order (Scrip : String;
                               Quantity : Binancebot.Util.FFloat;
                               Limit : String)
   is
      Result : Boolean :=
        Binancebot.Net.Create_Limit_Order
          (Scrip, Quantity, Limit, "side=sell");
   begin
      null;
   end Place_Sell_Order;
   procedure Place_Trade_Orders
     (OrderType    : in Arbitrage; Poll : Binancebot.Parse.Type_TokInfo_Pull; Initial_amount : Long_Float)
   is
      Final_Amount : Long_Float := 0.0;
      S1_quantity  : Binancebot.Util.FFloat;
      S2_quantity  : Binancebot.Util.FFloat;
      S3_quantity  : Binancebot.Util.FFloat;
      Tmp_S1_quantity  : Long_Float;
      Tmp_S2_quantity  : Long_Float;
      Tmp_S3_quantity  : Long_Float;

      function LF2FF(Long : in Long_Float) return Binancebot.Util.FFloat is
         Result : Binancebot.Util.FFloat := Binancebot.Util.FFloat(Long);
      begin
         return Result;
      end LF2FF;
   begin
      if (OrderType = 1) then
         Tmp_S1_quantity := Initial_amount / Binancebot.Util.U2F(Poll.First_Price_Txt);

         Tmp_S2_quantity := Tmp_S1_quantity / Binancebot.Util.U2F(Poll.Second_Price_Txt);

         Tmp_S3_quantity := Tmp_S2_quantity;

         if Tmp_S1_quantity > Poll.First_MinQTY and Tmp_S2_quantity > Poll.Second_MinQTY
           and Tmp_S3_quantity > Poll.Third_MinQTY  then

            S1_quantity := Util.Find_Multiple(LF2FF(Tmp_S1_quantity),LF2FF(Poll.First_StepSize));
            S2_quantity := Util.Find_Multiple(LF2FF(Tmp_S2_quantity),LF2FF(Poll.Second_StepSize));
            S3_quantity := Util.Find_Multiple(LF2FF(Tmp_S3_quantity),LF2FF(Poll.Third_StepSize));

            Place_Buy_Order (Binancebot.Util.U2S(Poll.First_Pair), S1_quantity, Binancebot.Util.U2S(Poll.First_Price_Txt));
            Place_Buy_Order (Binancebot.Util.U2S(Poll.Second_Pair), S2_quantity, Binancebot.Util.U2S(Poll.Second_Price_Txt));
            Place_Sell_Order (Binancebot.Util.U2S(Poll.Third_Pair), S3_quantity, Binancebot.Util.U2S(Poll.Third_Price_Txt));

         end if;

      elsif (OrderType = 2) then
         Tmp_S1_quantity := Initial_amount / Binancebot.Util.U2F(Poll.First_Price_Txt);

         Tmp_S2_quantity := Tmp_S1_quantity;

         Tmp_S3_quantity := Tmp_S2_quantity * Binancebot.Util.U2F(Poll.Second_Price_Txt);

         if Tmp_S1_quantity > Poll.First_MinQTY and Tmp_S2_quantity > Poll.Second_MinQTY
           and Tmp_S3_quantity > Poll.Third_MinQTY  then

            S1_quantity := Util.Find_Multiple(LF2FF(Tmp_S1_quantity),LF2FF(Poll.First_StepSize));
            S2_quantity := Util.Find_Multiple(LF2FF(Tmp_S2_quantity),LF2FF(Poll.Second_StepSize));
            S3_quantity := Util.Find_Multiple(LF2FF(Tmp_S3_quantity),LF2FF(Poll.Third_StepSize));

            Place_Buy_Order (Binancebot.Util.U2S(Poll.First_Pair), S1_quantity, Binancebot.Util.U2S(Poll.First_Price_Txt));
            Place_Sell_Order (Binancebot.Util.U2S(Poll.Second_Pair), S2_quantity, Binancebot.Util.U2S(Poll.Second_Price_Txt));
            Place_Sell_Order (Binancebot.Util.U2S(Poll.Third_Pair), S3_quantity, Binancebot.Util.U2S(Poll.Third_Price_Txt));

         end if;
      end if;
   end Place_Trade_Orders;

end Binancebot.Orders;
