with Binancebot.Triangular;
with Binancebot.Parse;
with Binancebot.Util;

package Binancebot.Orders is
   procedure Place_Buy_Order (Scrip : String; Quantity : Binancebot.Util.FFloat; Limit : String);
   procedure Place_Sell_Order
     (Scrip : String; Quantity : Binancebot.Util.FFloat; Limit : String);
   procedure Place_Trade_Orders
     (OrderType    : Binancebot.Triangular.Arbitrage;
      Poll : Binancebot.Parse.Type_TokInfo_Pull; Initial_amount : Long_Float);
end Binancebot.Orders;
